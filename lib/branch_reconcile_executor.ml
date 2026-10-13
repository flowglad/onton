(* @archlint.module shell
   @archlint.domain branch-reconcile *)

open Base
module G = Git_observation

type io = { git : string list -> int * string * string }

exception Probe_failed of string
exception Mutation_failed of string
exception Unsupported_destination of string
exception History_unclear of string

let checked ?(mutation = false) io args =
  let code, out, err = io.git args in
  if code = 0 then String.strip out
  else
    let detail =
      Printf.sprintf "git %s (exit %d): %s"
        (String.concat ~sep:" " args)
        code (String.strip err)
    in
    raise (if mutation then Mutation_failed detail else Probe_failed detail)

let commit s =
  match Branch_reconcile.Commit.make s with
  | Some sha -> sha
  | None -> raise (Probe_failed "invalid commit observation")

let resolve io name =
  checked io [ "rev-parse"; "--verify"; name ^ "^{commit}" ] |> commit

let git_path io name =
  checked io [ "rev-parse"; "--path-format=absolute"; "--git-path"; name ]

let read_optional ?(strip = true) io name =
  let path = git_path io name in
  try
    let ic =
      Unix.openfile path [ Unix.O_RDONLY ] 0 |> Unix.in_channel_of_descr
    in
    Some
      (Stdlib.Fun.protect
         ~finally:(fun () -> Stdlib.close_in_noerr ic)
         (fun () ->
           let contents = Stdlib.In_channel.input_all ic in
           if strip then String.strip contents else contents))
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> None
  | exn -> raise (Probe_failed (Stdlib.Printexc.to_string exn))

let required io name =
  match read_optional io name with
  | Some s -> s
  | None -> raise (Probe_failed ("missing sequencer state: " ^ name))

let observe_checkout io =
  let head = resolve io "HEAD" |> Branch_reconcile.Commit.to_string in
  let code, branch, err =
    io.git [ "symbolic-ref"; "--quiet"; "--short"; "HEAD" ]
  in
  let branch =
    match code with
    | 0 -> Some (String.strip branch)
    | 1 -> None
    | _ -> raise (Probe_failed ("cannot inspect checkout branch: " ^ err))
  in
  let sequencer =
    match
      ( read_optional io "rebase-merge/onto",
        read_optional io "rebase-apply/onto" )
    with
    | Some target, _ ->
        G.Rebase
          {
            target;
            original = required io "rebase-merge/orig-head";
            step = required io "rebase-merge/msgnum";
            head_ref = required io "rebase-merge/head-name";
            merge_heads =
              Option.value_map (read_optional io "MERGE_HEAD") ~default:[]
                ~f:(fun text ->
                  String.split_lines text
                  |> List.map ~f:(fun sha ->
                      Branch_reconcile.Commit.to_string (commit sha)));
          }
    | None, Some target ->
        G.Rebase
          {
            target;
            original = required io "rebase-apply/orig-head";
            step = required io "rebase-apply/next";
            head_ref = required io "rebase-apply/head-name";
            merge_heads = [];
          }
    | None, None -> (
        match
          (read_optional io "MERGE_HEAD", read_optional io "CHERRY_PICK_HEAD")
        with
        | Some target, _ -> G.Merge { target }
        | None, Some target -> G.Cherry_pick { target }
        | None, None -> G.None_active)
  in
  let code, status, err =
    io.git [ "status"; "--porcelain=v1"; "-z"; "--untracked-files=all" ]
  in
  if code <> 0 then raise (Probe_failed ("cannot inspect index: " ^ err));
  let checkout = G.of_porcelain ~branch ~head ~sequencer status in
  if not checkout.valid then raise (Probe_failed "malformed index observation");
  checkout

let ancestor io a b =
  let code, _, err =
    io.git
      [
        "merge-base";
        "--is-ancestor";
        Branch_reconcile.Commit.to_string a;
        Branch_reconcile.Commit.to_string b;
      ]
  in
  match code with
  | 0 -> true
  | 1 -> false
  | _ -> raise (Probe_failed ("ancestry probe failed: " ^ err))

let topology io local = function
  | None -> Branch_reconcile.Unproven
  | Some remote when Branch_reconcile.Commit.equal local remote ->
      Branch_reconcile.Equal
  | Some remote ->
      if ancestor io remote local then Branch_reconcile.Includes
      else if ancestor io local remote then Branch_reconcile.Behind
      else Branch_reconcile.Diverged

let destination io =
  let urls =
    checked io [ "remote"; "get-url"; "--push"; "--all"; "origin" ]
    |> String.split_lines
  in
  match Branch_reconcile.publication_destination urls with
  | Ok url -> url
  | Error reason -> raise (Unsupported_destination reason)

let remote_head_at io ~destination ~branch =
  try
    let ref_name = "refs/heads/" ^ branch in
    let code, out, err =
      io.git
        [ "ls-remote"; "--exit-code"; "--refs"; "--"; destination; ref_name ]
    in
    match code with
    | 2 -> Ok None
    | 0 -> (
        match Worktree_parser.parse_ls_remote_sha ~ref_name out with
        | None -> raise (Probe_failed "cannot decode remote observation")
        | Some s -> Ok (Some (commit s)))
    | _ -> raise (Probe_failed ("remote observation failed: " ^ err))
  with
  | Probe_failed reason -> Error reason
  | exn when Process_tree.has_cancellation exn -> raise exn
  | exn -> Error ("remote observation unavailable: " ^ Exn.to_string exn)

let remote_head ?operation io ~branch =
  try
    let destination = destination io in
    let authority =
      match operation with
      | None -> Ok ()
      | Some operation ->
          Branch_reconcile.check_observation_destination operation
            ~observed:(Branch_reconcile.Remote_id.of_destination destination)
    in
    match authority with
    | Error reason -> Error reason
    | Ok () -> remote_head_at io ~destination ~branch
  with
  | Probe_failed reason | Unsupported_destination reason -> Error reason
  | exn when Process_tree.has_cancellation exn -> raise exn
  | exn -> Error ("remote observation unavailable: " ^ Exn.to_string exn)

let remote_base io ~branch =
  (* Reconciliation fetches its target through origin's fetch URL. Publication
     may deliberately use a different push URL, so this must not use destination. *)
  remote_head_at io ~destination:"origin" ~branch

let remote io ~destination branch =
  match remote_head_at io ~destination ~branch with
  | Error reason -> raise (Probe_failed reason)
  | Ok None -> None
  | Ok (Some sha) ->
      ignore
        (checked io
           [
             "fetch";
             "--no-write-fetch-head";
             "--";
             destination;
             Branch_reconcile.Commit.to_string sha;
           ]
          : string);
      Some sha

let shared_ancestor io source target =
  let code, out, err =
    io.git
      [
        "merge-base";
        "--all";
        Branch_reconcile.Commit.to_string source;
        Branch_reconcile.Commit.to_string target;
      ]
  in
  match code with
  | 0 -> (
      match String.split_lines (String.strip out) with
      | [ revision ] -> Some (commit revision)
      | [] -> raise (Probe_failed "shared ancestor probe returned no revision")
      | _ :: _ :: _ -> None)
  | 1 -> None
  | _ -> raise (Probe_failed ("shared ancestor probe failed: " ^ err))

let observation io ~destination ~branch ~intent ~policy ~boundaries ~target
    ~original_source =
  let checkout = observe_checkout io in
  let source = resolve io ("refs/heads/" ^ branch) in
  let target =
    match target with
    | Some target -> target
    | None -> (
        match intent.Branch_reconcile.purpose with
        | Branch_reconcile.Integrate_revision { revision; _ } -> (
            match checkout.sequencer with
            | G.Rebase r -> commit r.target
            | G.Merge r -> commit r.target
            | G.Cherry_pick r -> commit r.target
            | G.None_active ->
                let revision_text =
                  Branch_reconcile.Commit.to_string revision
                in
                ignore
                  (checked io
                     [
                       "fetch"; "--no-write-fetch-head"; "origin"; revision_text;
                     ]
                    : string);
                resolve io revision_text)
        | Branch_reconcile.Publish_revision source -> source
        | Branch_reconcile.Publish_session _
        | Branch_reconcile.Verify_publication ->
            source
        | Branch_reconcile.Provision_checkout _
        | Branch_reconcile.Reconcile_base | Branch_reconcile.Reconcile_request _
        | Branch_reconcile.Reconcile_scoped _ -> (
            match checkout.sequencer with
            | G.Rebase r -> commit r.target
            | G.Merge r -> commit r.target
            | G.Cherry_pick r -> commit r.target
            | G.None_active ->
                resolve io
                  ("refs/remotes/origin/" ^ intent.Branch_reconcile.base)))
  in
  let remote_sha = remote io ~destination branch in
  (* With a unique shared ancestor, a reachable previous base can refine the
     replay range. Unrelated or ambiguous histories go to the agent, even when
     a previous-base hint exists; imported branches need no such hint. *)
  let rec select evidence =
    match Branch_reconcile.choose_boundary ~candidates:boundaries ~evidence with
    | Branch_reconcile.Chosen boundary -> boundary
    | Branch_reconcile.Probe revision ->
        select ((revision, ancestor io revision source) :: evidence)
  in
  let boundary =
    match shared_ancestor io source target with
    | None -> Branch_reconcile.Plain
    | Some shared -> (
        match select [] with
        | Branch_reconcile.Plain -> Branch_reconcile.Inferred shared
        | ( Branch_reconcile.Recorded _ | Inferred _ | Patch_equivalent _
          | Subject_inferred _ | Reconstructed _ ) as boundary ->
            boundary)
  in
  let sequencer =
    match checkout.sequencer with
    | G.None_active -> None
    | G.Rebase _ | G.Merge _ | G.Cherry_pick _ -> Some (G.progress_key checkout)
  in
  ( {
      Branch_reconcile.destination =
        Branch_reconcile.Remote_id.of_destination destination;
      Branch_reconcile.head = commit checkout.head;
      source;
      target;
      remote = remote_sha;
      boundary;
      topology = topology io source remote_sha;
      clean = G.clean checkout;
      sequencer;
      conflicts = List.length checkout.conflicts;
      target_included = ancestor io target source;
      base_contains_source =
        (match intent.purpose with
        | Branch_reconcile.Publish_revision _
        | Branch_reconcile.Publish_session _
          when Option.is_none original_source -> (
            match remote io ~destination intent.base with
            | Some base -> ancestor io source base
            | None -> false)
        | Branch_reconcile.Publish_revision _
        | Branch_reconcile.Provision_checkout _
        | Branch_reconcile.Publish_session _
        | Branch_reconcile.Verify_publication | Branch_reconcile.Reconcile_base
        | Branch_reconcile.Reconcile_request _
        | Branch_reconcile.Reconcile_scoped _
        | Branch_reconcile.Integrate_revision _ ->
            false);
      completed_integration =
        Option.value_map original_source ~default:false ~f:(fun original ->
            Branch_reconcile.completion_proven ~policy
              ~source_included:(ancestor io original source)
              ~reflog_receipt:
                (Option.value_map
                   (read_optional ~strip:false io ("logs/refs/heads/" ^ branch))
                   ~default:false
                   ~f:(fun reflog ->
                     G.completed_integration ~branch
                       ~source:(Branch_reconcile.Commit.to_string original)
                       ~target:(Branch_reconcile.Commit.to_string target)
                       ~head:(Branch_reconcile.Commit.to_string source)
                       ~reflog)));
    },
    checkout )

let pin io ~prefix token label sha =
  let ref_name =
    Printf.sprintf "%s/%d/%d/%s" prefix token.Branch_reconcile.operation
      token.command label
  in
  let sha = Branch_reconcile.Commit.to_string sha in
  let code, _, _ = io.git [ "update-ref"; ref_name; sha; "" ] in
  if
    code <> 0
    && not (String.equal (checked io [ "rev-parse"; "--verify"; ref_name ]) sha)
  then raise (Probe_failed "recovery ref contention")

let mutation_result io ~(operation : Branch_reconcile.operation) code err =
  let checkout = observe_checkout io in
  match checkout.sequencer with
  | G.Rebase _ | G.Merge _ | G.Cherry_pick _ ->
      Branch_reconcile.stopped_integration_result checkout ~detail:err
  | G.None_active ->
      if code = 0 then
        let candidate = commit checkout.head in
        let target_preserved =
          Option.value_map operation.target ~default:false ~f:(fun target ->
              ancestor io target candidate)
        in
        let source_preserved =
          match Branch_reconcile.execution_policy operation with
          | Branch_reconcile.Preserve_ancestry ->
              Option.value_map operation.source ~default:false ~f:(fun source ->
                  ancestor io source candidate)
          | Branch_reconcile.Rewrite -> false
        in
        Branch_reconcile.integration_result operation ~candidate
          ~target_preserved ~source_preserved
      else Branch_reconcile.stopped_integration_result checkout ~detail:err

let ancestry_preserved io operation candidate =
  List.for_all (Branch_reconcile.ancestry_requirements operation)
    ~f:(fun required -> ancestor io required candidate)

let completed_merge io ~branch (capture : Branch_reconcile.merge_completion)
    (checkout : G.t) =
  if
    (not (G.clean checkout))
    || (not (String.equal (G.progress_key checkout) capture.resumed_sequencer))
    || not
         (match checkout.sequencer with
         | G.Rebase r ->
             String.equal r.head_ref ("refs/heads/" ^ branch)
             && Option.is_none checkout.branch
         | G.Merge _ | G.Cherry_pick _ | G.None_active -> false)
  then None
  else
    let parents =
      checked io
        [
          "show";
          "--no-patch";
          "--no-show-signature";
          "--format=%P";
          checkout.head;
        ]
      |> String.split ~on:' '
    in
    let expected =
      List.map
        (capture.head :: capture.parents)
        ~f:Branch_reconcile.Commit.to_string
    in
    if not (List.equal String.equal parents expected) then None
    else
      let tree revision =
        let oid =
          checked io [ "rev-parse"; "--verify"; revision ^ "^{tree}" ]
        in
        if Option.is_none (Branch_reconcile.Commit.make oid) then
          raise
            (Probe_failed "merge completion returned an invalid tree identity");
        oid
      in
      if
        String.equal
          (tree (Branch_reconcile.Commit.to_string capture.head))
          (tree checkout.head)
      then Some (commit checkout.head)
      else None

(* Initial publication may have completed before acknowledgement or between
   local configuration writes. Confirm and recovery inspection both finish this
   idempotent local setup, using the operation's original absent-ref lease. *)
let complete_initial_tracking io ~destination ~branch ~operation ~remote =
  match Branch_reconcile.initial_publication_candidate operation ~remote with
  | None -> ()
  | Some candidate -> (
      let fetch_destination =
        checked io [ "remote"; "get-url"; "origin" ] |> String.strip
      in
      let config suffix =
        let code, out, err =
          io.git
            [ "config"; "--local"; "--get-all"; "branch." ^ branch ^ suffix ]
        in
        match code with
        | 0 -> String.split_lines out
        | 1 -> []
        | _ ->
            raise
              (Probe_failed
                 ("cannot inspect initial tracking configuration: " ^ err))
      in
      match
        Branch_reconcile.initial_tracking_edits ~branch
          ~base:operation.Branch_reconcile.intent.base ~fetch_destination
          ~push_destination:destination ~remotes:(config ".remote")
          ~merges:(config ".merge")
      with
      | None -> ()
      | Some edits ->
          let ref_name = "refs/remotes/origin/" ^ branch in
          let code, out, err =
            io.git [ "rev-parse"; "--verify"; "--quiet"; ref_name ]
          in
          let previous =
            match code with
            | 0 ->
                let previous = String.strip out in
                if not (Git_oid.valid previous) then
                  raise (Probe_failed "invalid initial tracking ref");
                previous
            | 1 -> ""
            | _ ->
                raise
                  (Probe_failed ("cannot inspect initial tracking ref: " ^ err))
          in
          let confirmed = Branch_reconcile.Commit.to_string candidate in
          if not (String.equal previous confirmed) then
            ignore
              (checked io [ "update-ref"; ref_name; confirmed; previous ]
                : string);
          List.iter edits ~f:(fun (key, value) ->
              ignore (checked io [ "config"; "--local"; key; value ] : string)))

let execute ~io ~prefix ~branch ~(operation : Branch_reconcile.operation)
    (command : Branch_reconcile.command) =
  if Branch_reconcile.is_provisioning operation.intent.purpose then
    Branch_reconcile.Needs_diagnosis "provisioning_requires_checkout_handler"
  else if
    not
      (Branch_reconcile.command_allowed_for_purpose operation.intent.purpose
         command.kind)
  then Branch_reconcile.Needs_diagnosis "command_not_allowed_for_purpose"
  else
    try
      if
        not
          (Option.value_map operation.pending ~default:false
             ~f:(Branch_reconcile.equal_command command))
      then
        raise (Probe_failed "command is not the checkpointed pending command");
      let destination = destination io in
      (match
         Branch_reconcile.check_destination operation
           ~observed:(Branch_reconcile.Remote_id.of_destination destination)
       with
      | Ok () -> ()
      | Error reason -> raise (Unsupported_destination reason));
      ignore (checked io [ "check-ref-format"; prefix ^ "/probe" ] : string);
      let pin_captured () =
        List.iter
          [
            ("source", operation.source);
            ("target", operation.target);
            ("candidate", operation.candidate);
            ("remote", operation.expected);
            ("deferred-remote", operation.deferred_remote);
            ( "replay-upstream",
              Option.map operation.remote_replay ~f:(fun r ->
                  r.Branch_reconcile.upstream) );
          ]
          ~f:(fun (label, revision) ->
            Option.iter revision ~f:(pin io ~prefix command.token label));
        (match operation.boundary with
        | Branch_reconcile.Reconstructed { original; upstream } ->
            pin io ~prefix command.token "boundary-original" original;
            pin io ~prefix command.token "boundary-upstream" upstream
        | Recorded upstream
        | Inferred upstream
        | Patch_equivalent upstream
        | Subject_inferred upstream ->
            pin io ~prefix command.token "boundary-upstream" upstream
        | Plain -> ());
        List.iter (operation.recovery_revisions @ operation.retained_revisions)
          ~f:(fun revision ->
            pin io ~prefix command.token
              ("recovery-required/" ^ Branch_reconcile.Commit.to_string revision)
              revision);
        Option.iter operation.merge_progress ~f:(fun progress ->
            let capture, result =
              match progress with
              | Branch_reconcile.Pending_merge capture -> (capture, None)
              | Branch_reconcile.Completed_merge { capture; head } ->
                  (capture, Some head)
            in
            List.iteri (capture.head :: capture.parents) ~f:(fun i sha ->
                pin io ~prefix command.token
                  ("merge-parent-" ^ Int.to_string i)
                  sha);
            Option.iter result
              ~f:(pin io ~prefix command.token "completed-merge"))
      in
      let refresh_base () =
        match operation.intent.purpose with
        | Branch_reconcile.Reconcile_base | Reconcile_request _
        | Reconcile_scoped _ ->
            ignore (checked io [ "fetch"; "origin" ] : string)
        | Provision_checkout _ | Publish_revision _ | Publish_session _
        | Verify_publication | Integrate_revision _ ->
            ()
      in
      match command.kind with
      | Branch_reconcile.Observe ->
          refresh_base ();
          let o, checkout =
            observation io ~destination ~branch ~intent:operation.intent
              ~policy:(Branch_reconcile.execution_policy operation)
              ~boundaries:(Branch_reconcile.observation_boundaries operation)
              ~target:None ~original_source:None
          in
          Branch_reconcile.inspection_result operation ~branch o checkout
      | Branch_reconcile.Inspect
        when match operation.candidate_origin with
             | Branch_reconcile.Capture_agent_repair _ -> true
             | Deterministic | Accepted_agent_repair _ -> false ->
          let turn =
            match operation.candidate_origin with
            | Branch_reconcile.Capture_agent_repair turn -> turn
            | Deterministic | Accepted_agent_repair _ ->
                raise (Probe_failed "repair completion receipt missing")
          in
          let code, actual_branch, err =
            io.git [ "symbolic-ref"; "--quiet"; "--short"; "HEAD" ]
          in
          if code <> 0 && code <> 1 then
            raise
              (Probe_failed ("cannot inspect repair checkout branch: " ^ err));
          if code = 1 || not (String.equal (String.strip actual_branch) branch)
          then
            Branch_reconcile.Recovery_required "repair_capture_checkout_changed"
          else
            let ref_name =
              Printf.sprintf "%s/%d/%d/accepted-agent-repair" prefix
                turn.operation turn.command
            in
            let code, _, err =
              io.git [ "show-ref"; "--verify"; "--quiet"; ref_name ]
            in
            let head =
              match code with
              | 0 -> resolve io ref_name
              | 1 ->
                  let head = resolve io "HEAD" in
                  pin io ~prefix turn "accepted-agent-repair" head;
                  head
              | _ ->
                  raise
                    (Probe_failed ("repair capture ref probe failed: " ^ err))
            in
            Branch_reconcile.Integrated head
      | Branch_reconcile.Inspect
        when Option.exists operation.candidate
               ~f:(Branch_reconcile.agent_candidate operation) ->
          pin_captured ();
          let candidate =
            match operation.candidate with
            | Some candidate -> candidate
            | None -> raise (Probe_failed "accepted repair candidate missing")
          in
          let actual_branch =
            checked io [ "symbolic-ref"; "--quiet"; "--short"; "HEAD" ]
          in
          let actual_head = resolve io "HEAD" in
          if
            not
              (String.equal actual_branch branch
              && Branch_reconcile.Commit.equal actual_head candidate)
          then
            Branch_reconcile.Recovery_required
              "accepted_repair_checkout_changed"
          else
            let sha = remote io ~destination branch in
            Option.iter sha ~f:(pin io ~prefix command.token "confirmed-remote");
            Branch_reconcile.Remote
              { sha; topology = topology io candidate sha }
      | Branch_reconcile.Inspect ->
          pin_captured ();
          if Option.is_none operation.source then refresh_base ();
          let o, checkout =
            observation io ~destination ~branch ~intent:operation.intent
              ~policy:(Branch_reconcile.execution_policy operation)
              ~boundaries:(Branch_reconcile.observation_boundaries operation)
              ~target:operation.target ~original_source:operation.source
          in
          if
            o.clean && Option.is_none o.sequencer
            && Option.exists operation.candidate ~f:(fun candidate ->
                (not (Branch_reconcile.agent_candidate operation candidate))
                && not (ancestry_preserved io operation candidate))
          then Branch_reconcile.Recovery_required "ancestry_not_preserved"
          else (
            complete_initial_tracking io ~destination ~branch ~operation
              ~remote:o.remote;
            match operation.merge_progress with
            | Some (Branch_reconcile.Pending_merge capture) -> (
                match completed_merge io ~branch capture checkout with
                | Some head -> Branch_reconcile.Merge_completed head
                | None ->
                    Branch_reconcile.inspection_result operation ~branch o
                      checkout)
            | None | Some (Branch_reconcile.Completed_merge _) ->
                Branch_reconcile.inspection_result operation ~branch o checkout)
      | Branch_reconcile.Verify_recovery ->
          pin_captured ();
          let o, checkout =
            observation io ~destination ~branch ~intent:operation.intent
              ~policy:(Branch_reconcile.execution_policy operation)
              ~boundaries:(Branch_reconcile.observation_boundaries operation)
              ~target:operation.target ~original_source:operation.source
          in
          pin io ~prefix command.token
            ("recovery-head/" ^ Branch_reconcile.Commit.to_string o.head)
            o.head;
          pin io ~prefix command.token
            ("recovery-branch/" ^ Branch_reconcile.Commit.to_string o.source)
            o.source;
          Option.iter o.remote ~f:(fun remote ->
              pin io ~prefix command.token
                ("observed-remote/" ^ Branch_reconcile.Commit.to_string remote)
                remote);
          ignore (checkout : Git_observation.t);
          Branch_reconcile.Recovery_observed o
      | Branch_reconcile.Plan_remote_replay request -> (
          pin_captured ();
          match
            Branch_reconcile.check_checkout ~operation command ~branch
              (observe_checkout io)
          with
          | Error reason -> Branch_reconcile.Recovery_required reason
          | Ok () -> (
              match shared_ancestor io request.incoming request.preserved with
              | Some upstream ->
                  Branch_reconcile.Remote_replay_selected
                    (Branch_reconcile.Inferred upstream)
              | None -> Branch_reconcile.Recovery_required "no_shared_ancestor")
          )
      | Branch_reconcile.Checkout_remote _ ->
          (* Retain old checkpoint syntax, but never reset the managed checkout
             to an unsolicited remote contribution. *)
          pin_captured ();
          Branch_reconcile.Recovery_required
            "legacy_remote_checkout_requires_recovery"
      | Branch_reconcile.Commit_merge _ -> (
          pin_captured ();
          match
            Branch_reconcile.check_checkout ~operation command ~branch
              (observe_checkout io)
          with
          | Error reason -> Branch_reconcile.Recovery_required reason
          | Ok () ->
              let code, _, err =
                io.git [ "-c"; "core.editor=true"; "commit"; "--no-edit" ]
              in
              if code = 0 then
                Branch_reconcile.Merge_completed (resolve io "HEAD")
              else Branch_reconcile.Attempt_failed err)
      | Branch_reconcile.Pin _ ->
          pin_captured ();
          Branch_reconcile.Pinned
      | Branch_reconcile.Integrate { source; target; boundary; policy } -> (
          pin_captured ();
          let checkout = observe_checkout io in
          match
            Branch_reconcile.check_checkout ~operation command ~branch checkout
          with
          | Error reason -> Branch_reconcile.Recovery_required reason
          | Ok () -> (
              match operation.remote_replay with
              | Some replay ->
                  let incoming =
                    Branch_reconcile.Commit.to_string replay.incoming
                  in
                  let args =
                    match policy with
                    | Branch_reconcile.Rewrite
                      when not (ancestor io replay.preserved replay.incoming) ->
                        [
                          "merge";
                          "--squash";
                          "--no-commit";
                          "--no-autostash";
                          "--no-rerere-autoupdate";
                          incoming;
                        ]
                    | Branch_reconcile.Rewrite ->
                        [ "merge"; "--ff-only"; "--no-autostash"; incoming ]
                    | Branch_reconcile.Preserve_ancestry ->
                        [ "merge"; "--no-edit"; "--no-autostash"; incoming ]
                  in
                  let code, _, err = io.git args in
                  if code = 0 && List.mem args "--squash" ~equal:String.equal
                  then
                    let staged, _, detail =
                      io.git [ "diff"; "--cached"; "--quiet" ]
                    in
                    match staged with
                    | 0 -> mutation_result io ~operation 0 err
                    | 1 ->
                        let code, _, err =
                          io.git
                            [ "commit"; "-m"; "Integrate remote patch work" ]
                        in
                        mutation_result io ~operation code err
                    | _ ->
                        raise
                          (Probe_failed
                             ("cannot inspect remote integration index: "
                            ^ detail))
                  else mutation_result io ~operation code err
              | None ->
                  if ancestor io target source then
                    Branch_reconcile.Integrated source
                  else
                    let target = Branch_reconcile.Commit.to_string target in
                    let args =
                      match (policy, boundary) with
                      | Branch_reconcile.Preserve_ancestry, _ ->
                          [ "merge"; "--no-edit"; "--no-autostash"; target ]
                      | Branch_reconcile.Rewrite, boundary ->
                          let upstream =
                            match boundary with
                            | Branch_reconcile.Recorded revision
                            | Inferred revision
                            | Patch_equivalent revision
                            | Subject_inferred revision
                            | Reconstructed { upstream = revision; _ } ->
                                Some revision
                            | Plain -> shared_ancestor io source (commit target)
                          in
                          let upstream =
                            match upstream with
                            | Some revision ->
                                Branch_reconcile.Commit.to_string revision
                            | None ->
                                raise (History_unclear "no_shared_ancestor")
                          in
                          (* Ordinary rebase drops merge-only edits. Let the
                             agent reconstruct merged replay ranges instead. *)
                          let merges =
                            checked io
                              [
                                "rev-list";
                                "--merges";
                                upstream ^ ".."
                                ^ Branch_reconcile.Commit.to_string source;
                              ]
                          in
                          if not (String.is_empty (String.strip merges)) then
                            raise
                              (History_unclear "merged_history_requires_agent");
                          [
                            "rebase";
                            "--merge";
                            "--no-update-refs";
                            "--no-autostash";
                            "--no-rebase-merges";
                            "--no-autosquash";
                            "--no-fork-point";
                            "--onto";
                            target;
                            upstream;
                          ]
                    in
                    let code, _, err = io.git args in
                    mutation_result io ~operation code err))
      | Branch_reconcile.Continue { head = _; target; sequencer } -> (
          pin_captured ();
          let checkout = observe_checkout io in
          match
            Branch_reconcile.check_checkout ~operation command ~branch checkout
          with
          | Error reason -> Branch_reconcile.Recovery_required reason
          | Ok () ->
              if
                match checkout.sequencer with
                | G.Rebase _ -> true
                | G.Merge _ | G.Cherry_pick _ | G.None_active -> false
              then
                Branch_reconcile.Recovery_required
                  "paused_rebase_requires_agent"
              else
                let next = Branch_reconcile.continuation operation checkout in
                if G.equal_continuation next G.Complete_merge then
                  match checkout.sequencer with
                  | G.Rebase r ->
                      Branch_reconcile.Merge_completion_needed
                        {
                          head = commit checkout.head;
                          target;
                          parents = List.map r.merge_heads ~f:commit;
                          sequencer;
                          resumed_sequencer =
                            G.sequencer_key
                              (G.Rebase { r with merge_heads = [] });
                        }
                  | G.Merge _ | G.Cherry_pick _ | G.None_active ->
                      Branch_reconcile.Recovery_required
                        "merge_completion_context_missing"
                else if G.equal_continuation next G.Blocked then
                  Branch_reconcile.Conflict
                    {
                      head = commit checkout.head;
                      sequencer;
                      conflicts = List.length checkout.conflicts;
                    }
                else
                  let args =
                    match checkout.sequencer with
                    | G.Rebase _ ->
                        [
                          "-c";
                          "core.editor=true";
                          "rebase";
                          (if G.equal_continuation next G.Skip_empty_replay then
                             "--skip"
                           else "--continue");
                        ]
                    | G.Merge _ ->
                        [ "-c"; "core.editor=true"; "merge"; "--continue" ]
                    | G.Cherry_pick _ ->
                        [
                          "-c"; "core.editor=true"; "cherry-pick"; "--continue";
                        ]
                    | G.None_active -> []
                  in
                  let code, _, err = io.git args in
                  mutation_result io ~operation code err)
      | Branch_reconcile.Publish { candidate; expected } -> (
          pin_captured ();
          let checkout = observe_checkout io in
          match
            Branch_reconcile.check_checkout ~operation command ~branch checkout
          with
          | Error reason -> Branch_reconcile.Recovery_required reason
          | Ok () -> (
              if
                (not (Branch_reconcile.agent_candidate operation candidate))
                && not (ancestry_preserved io operation candidate)
              then Branch_reconcile.Recovery_required "ancestry_not_preserved"
              else if
                (not (Branch_reconcile.agent_candidate operation candidate))
                && not
                     (Option.for_all expected ~f:(fun remote ->
                          if ancestor io remote candidate then true
                          else if
                            Option.value_map
                              (Branch_reconcile.rewrite_publication_input
                                 operation ~candidate ~expected:remote)
                              ~default:false ~f:(fun (source, upstream) ->
                                ancestor io upstream remote
                                && ancestor io remote source)
                          then true
                          else if
                            Branch_reconcile.equal_policy
                              (Branch_reconcile.execution_policy operation)
                              Branch_reconcile.Preserve_ancestry
                          then false
                          else
                            match
                              Git_publication_evidence.rewrite_authority
                                ~git:io.git ~branch
                                ~local_sha:
                                  (Branch_reconcile.Commit.to_string candidate)
                                ~remote_sha:
                                  (Branch_reconcile.Commit.to_string remote)
                            with
                            | Error reason -> raise (Probe_failed reason)
                            | Ok None -> false
                            | Ok (Some evidence) ->
                                Rewrite_lineage.authorizes evidence ~branch
                                  ~local_sha:
                                    (Branch_reconcile.Commit.to_string candidate)
                                  ~remote_sha:
                                    (Branch_reconcile.Commit.to_string remote)))
              then
                Branch_reconcile.Recovery_required
                  "remote_work_not_incorporated"
              else
                let lease =
                  Option.value_map expected ~default:""
                    ~f:Branch_reconcile.Commit.to_string
                in
                let code, out, err =
                  io.git
                    [
                      "push";
                      "--porcelain";
                      "--force-with-lease=refs/heads/" ^ branch ^ ":" ^ lease;
                      "--";
                      destination;
                      Branch_reconcile.Commit.to_string candidate
                      ^ ":refs/heads/" ^ branch;
                    ]
                in
                match
                  Worktree_parser.classify_push_result ~code ~stdout:out
                    ~stderr:err
                with
                | Worktree_parser.Push_ok | Worktree_parser.Push_up_to_date ->
                    Branch_reconcile.Published
                | Worktree_parser.Push_rejected rejection ->
                    Branch_reconcile.Publication_rejected rejection
                | Worktree_parser.Push_error detail ->
                    Branch_reconcile.Attempt_failed detail))
      | Branch_reconcile.Confirm candidate ->
          pin_captured ();
          if
            Branch_reconcile.equal_purpose operation.intent.purpose
              Branch_reconcile.Verify_publication
            && not
                 (Branch_reconcile.verification_checkout_valid ~branch
                    ~candidate (observe_checkout io))
          then
            Branch_reconcile.Needs_diagnosis
              "legacy_publication_checkout_changed"
          else if
            (not (Branch_reconcile.agent_candidate operation candidate))
            && not (ancestry_preserved io operation candidate)
          then Branch_reconcile.Recovery_required "ancestry_not_preserved"
          else
            let sha = remote io ~destination branch in
            Option.iter sha ~f:(pin io ~prefix command.token "confirmed-remote");
            complete_initial_tracking io ~destination ~branch ~operation
              ~remote:sha;
            Branch_reconcile.Remote
              { sha; topology = topology io candidate sha }
    with
    | Mutation_failed reason -> Branch_reconcile.Attempt_failed reason
    | History_unclear reason -> Branch_reconcile.Recovery_required reason
    | Unsupported_destination reason -> Branch_reconcile.Needs_diagnosis reason
    | Probe_failed reason ->
        Branch_reconcile.Retryable { reason; retry_after = None }
    | exn when Process_tree.has_cancellation exn -> raise exn
    | exn ->
        Branch_reconcile.Retryable
          {
            reason = "executor outcome uncertain: " ^ Exn.to_string exn;
            retry_after = None;
          }

let capture_commit ~io ~ref_name =
  try Ok (resolve io ref_name) with Probe_failed reason -> Error reason

let materialization ~io ~prefix =
  let read suffix =
    let name = prefix ^ suffix in
    let code, _, err = io.git [ "show-ref"; "--verify"; "--quiet"; name ] in
    match code with
    | 0 -> Some (resolve io name)
    | 1 -> None
    | _ -> raise (Probe_failed ("materialization probe failed: " ^ err))
  in
  try
    match (read "/materialized-base", read "/materialized-existing") with
    | Some head, None -> Ok (Some (Branch_reconcile.New_branch head))
    | None, Some head -> Ok (Some (Branch_reconcile.Adopted_branch head))
    | None, None -> Ok None
    | Some _, Some _ -> Error "contradictory_materialization_receipts"
  with Probe_failed reason -> Error reason

let pin_materialization_intent ~io ~prefix head =
  let ref_name = prefix ^ "/materializing-base" in
  let code, _, err = io.git [ "show-ref"; "--verify"; "--quiet"; ref_name ] in
  match code with
  | 0 -> (
      try Ok (resolve io ref_name) with Probe_failed reason -> Error reason)
  | 1 ->
      let code, _, err =
        io.git
          [ "update-ref"; ref_name; Branch_reconcile.Commit.to_string head; "" ]
      in
      if code = 0 then Ok head
      else Error ("materialization intent pin failed: " ^ err)
  | _ -> Error ("materialization intent probe failed: " ^ err)

let record_materialization ~io ~prefix ~branch ~new_branch_from =
  match materialization ~io ~prefix with
  | Error _ as error -> error
  | Ok (Some _) as recorded -> recorded
  | Ok None -> (
      try
        let checkout = observe_checkout io in
        if not (Option.equal String.equal checkout.branch (Some branch)) then
          raise (Probe_failed "materialization_branch_changed");
        let head = commit checkout.head in
        if
          not
            (Option.for_all new_branch_from
               ~f:(Branch_reconcile.Commit.equal head))
        then raise (Probe_failed "materialization_revision_changed");
        let suffix =
          if Option.is_some new_branch_from then "/materialized-base"
          else "/materialized-existing"
        in
        let code, _, err =
          io.git
            [
              "update-ref";
              prefix ^ suffix;
              Branch_reconcile.Commit.to_string head;
              "";
            ]
        in
        if code <> 0 then
          raise (Probe_failed ("materialization pin failed: " ^ err));
        materialization ~io ~prefix
      with Probe_failed reason -> Error reason)

let recover_materialization ~io ~prefix ~branch =
  match materialization ~io ~prefix with
  | Ok (Some _) as result -> result
  | Error _ as error -> error
  | Ok None -> (
      let code, _, err =
        io.git
          [ "show-ref"; "--verify"; "--quiet"; prefix ^ "/materializing-base" ]
      in
      match code with
      | 1 -> Ok None
      | 0 -> (
          try
            let head = resolve io (prefix ^ "/materializing-base") in
            record_materialization ~io ~prefix ~branch
              ~new_branch_from:(Some head)
          with Probe_failed reason -> Error reason)
      | _ -> Error ("materialization intent probe failed: " ^ err))

let make_io ~process_mgr ~clock ~path =
  {
    git =
      (fun args ->
        match
          Eio.Time.with_timeout clock 120. (fun () ->
              Ok
                (Process_tree.run ~process_mgr ~clock
                   ~env:(Git_env.clean_env ())
                   ("git" :: "-C" :: path :: args)))
        with
        | Ok result -> result
        | Error `Timeout ->
            ( 124,
              "",
              "Git command outcome uncertain after timeout: "
              ^ String.concat ~sep:" " args ));
  }

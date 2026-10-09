(* @archlint.module shell
   @archlint.domain branch-reconcile *)

open Base
module G = Git_observation

type io = { git : string list -> int * string * string }

exception Probe_failed of string
exception Unsupported_destination of string

let checked io args =
  let code, out, err = io.git args in
  if code = 0 then String.strip out
  else
    raise
      (Probe_failed
         (Printf.sprintf "git %s (exit %d): %s"
            (String.concat ~sep:" " args)
            code (String.strip err)))

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

let reconstruct_boundary io ~source boundaries =
  let recorded =
    List.filter_map boundaries ~f:(function
      | Branch_reconcile.Recorded original ->
          Some
            ( original,
              checked io
                [
                  "rev-parse";
                  "--verify";
                  Branch_reconcile.Commit.to_string original ^ "^{tree}";
                ] )
      | Inferred _ | Reconstructed _ | Patch_equivalent _ | Subject_inferred _
      | Plain ->
          None)
  in
  if List.is_empty recorded then None
  else
    let code, history, err =
      io.git
        [
          "log";
          "--first-parent";
          "--no-show-signature";
          "--format=%H%x00%P%x00%T";
          Branch_reconcile.Commit.to_string source;
          "--";
        ]
    in
    if code <> 0 then
      raise (Probe_failed ("boundary reconstruction failed: " ^ err));
    match
      Branch_reconcile.reconstructed_boundary ~source ~recorded ~history
    with
    | Ok boundary -> boundary
    | Error reason -> raise (Probe_failed reason)

let observation io ~destination ~branch ~intent ~policy ~boundaries ~target
    ~original_source =
  let checkout = observe_checkout io in
  (match (checkout.branch, checkout.sequencer) with
  | Some actual, G.None_active when not (String.equal actual branch) ->
      raise (Probe_failed "managed branch is not checked out")
  | Some _, G.None_active | _, (G.Rebase _ | G.Merge _ | G.Cherry_pick _) -> ()
  | None, G.None_active -> raise (Probe_failed "unexpected detached checkout"));
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
  let rec select evidence =
    match Branch_reconcile.choose_boundary ~candidates:boundaries ~evidence with
    | Branch_reconcile.Chosen boundary -> boundary
    | Branch_reconcile.Probe sha ->
        let reachable = ancestor io sha source in
        select ((sha, reachable) :: evidence)
  in
  let boundary =
    match select [] with
    | Branch_reconcile.Plain
      when Option.is_none original_source
           && Branch_reconcile.equal_policy policy Rewrite
           &&
           match intent.purpose with
           | Branch_reconcile.Reconcile_base | Reconcile_request _
           | Reconcile_scoped _ ->
               true
           | Provision_checkout _ | Publish_revision _ | Publish_session _
           | Verify_publication | Integrate_revision _ ->
               false -> (
        match reconstruct_boundary io ~source boundaries with
        | Some boundary -> boundary
        | None -> (
            let history =
              let code, out, err =
                io.git
                  [
                    "log";
                    "--cherry-mark";
                    "--right-only";
                    "--topo-order";
                    "--no-show-signature";
                    "--format=%m%x00%H%x00%P";
                    Branch_reconcile.Commit.to_string target
                    ^ "..."
                    ^ Branch_reconcile.Commit.to_string source;
                    "--";
                  ]
              in
              if code <> 0 then
                raise (Probe_failed ("patch-equivalence probe failed: " ^ err));
              out
            in
            match
              Branch_reconcile.patch_equivalent_boundary ~source history
            with
            | Ok (Some upstream) -> Branch_reconcile.Patch_equivalent upstream
            | Ok None -> (
                match Branch_reconcile.subject_scope intent.purpose with
                | None -> Branch_reconcile.Plain
                | Some (project, ancestors) -> (
                    let code, text, err =
                      io.git
                        [
                          "log";
                          "--right-only";
                          "--topo-order";
                          "--no-show-signature";
                          "--format=%H%x00%P%x00%s";
                          Branch_reconcile.Commit.to_string target
                          ^ "..."
                          ^ Branch_reconcile.Commit.to_string source;
                          "--";
                        ]
                    in
                    if code <> 0 then
                      raise (Probe_failed ("subject probe failed: " ^ err));
                    match
                      Branch_reconcile.subject_boundary ~source ~project
                        ~ancestors text
                    with
                    | Ok (Some upstream) ->
                        Branch_reconcile.Subject_inferred upstream
                    | Ok None -> Branch_reconcile.Plain
                    | Error reason -> raise (Probe_failed reason)))
            | Error reason -> raise (Probe_failed reason)))
    | ( Branch_reconcile.Recorded _ | Inferred _ | Patch_equivalent _
      | Subject_inferred _ | Reconstructed _ | Plain ) as boundary ->
        boundary
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

let preserved_revision io ~branch ~original ~candidate =
  ancestor io original candidate
  ||
  match
    Git_publication_evidence.rewrite_authority ~git:io.git ~branch
      ~local_sha:(Branch_reconcile.Commit.to_string candidate)
      ~remote_sha:(Branch_reconcile.Commit.to_string original)
  with
  | Error reason -> raise (Probe_failed reason)
  | Ok None -> false
  | Ok (Some evidence) ->
      Rewrite_lineage.authorizes evidence ~branch
        ~local_sha:(Branch_reconcile.Commit.to_string candidate)
        ~remote_sha:(Branch_reconcile.Commit.to_string original)

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
          let code, _, err =
            io.git [ "show-ref"; "--verify"; "--quiet"; ref_name ]
          in
          (match code with
          | 0 -> ()
          | 1 ->
              let code, _, err =
                io.git
                  [
                    "update-ref";
                    ref_name;
                    Branch_reconcile.Commit.to_string candidate;
                    "";
                  ]
              in
              if code <> 0 then
                let exists, _, _ =
                  io.git [ "show-ref"; "--verify"; "--quiet"; ref_name ]
                in
                if exists <> 0 then
                  raise
                    (Probe_failed ("cannot install initial tracking ref: " ^ err))
          | _ ->
              raise
                (Probe_failed ("cannot inspect initial tracking ref: " ^ err)));
          List.iter edits ~f:(fun (key, value) ->
              ignore (checked io [ "config"; "--local"; key; value ] : string)))

let execute ~io ~prefix ~branch ~(operation : Branch_reconcile.operation)
    (command : Branch_reconcile.command) =
  if Branch_reconcile.is_provisioning operation.intent.purpose then
    Branch_reconcile.Permanent "provisioning_requires_checkout_handler"
  else if
    not
      (Branch_reconcile.command_allowed_for_purpose operation.intent.purpose
         command.kind)
  then Branch_reconcile.Permanent "command_not_allowed_for_purpose"
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
        List.iter operation.recovery_revisions ~f:(fun revision ->
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
                not (ancestry_preserved io operation candidate))
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
          let preserved original =
            if ancestor io original o.source then true
            else if
              Branch_reconcile.equal_policy
                (Branch_reconcile.execution_policy operation)
                Branch_reconcile.Preserve_ancestry
            then false
            else
              match
                Git_publication_evidence.rewrite_authority ~git:io.git ~branch
                  ~local_sha:(Branch_reconcile.Commit.to_string o.source)
                  ~remote_sha:(Branch_reconcile.Commit.to_string original)
              with
              | Error reason -> raise (Probe_failed reason)
              | Ok None -> false
              | Ok (Some evidence) ->
                  Rewrite_lineage.authorizes evidence ~branch
                    ~local_sha:(Branch_reconcile.Commit.to_string o.source)
                    ~remote_sha:(Branch_reconcile.Commit.to_string original)
          in
          let ready = G.clean checkout && Option.is_none o.sequencer in
          Branch_reconcile.Recovery_verified
            {
              observation = o;
              source_preserved =
                (ready
                && List.for_all operation.recovery_revisions ~f:preserved
                && Option.value_map
                     (match operation.candidate with
                     | Some _ as candidate -> candidate
                     | None -> operation.source)
                     ~default:false ~f:preserved
                &&
                match operation.repair with
                | Some
                    {
                      mode = Branch_reconcile.History_recovery { baseline; _ };
                      _;
                    } ->
                    Option.for_all baseline ~f:preserved
                | Some { mode = Branch_reconcile.Content_repair; _ } | None ->
                    false);
              remote_preserved =
                ready
                && Option.for_all operation.expected ~f:preserved
                && Option.for_all o.remote ~f:preserved;
            }
      | Branch_reconcile.Plan_remote_replay request -> (
          pin_captured ();
          List.iteri request.boundaries ~f:(fun i boundary ->
              match boundary with
              | Branch_reconcile.Recorded sha
              | Branch_reconcile.Inferred sha
              | Branch_reconcile.Patch_equivalent sha
              | Branch_reconcile.Subject_inferred sha
              | Branch_reconcile.Reconstructed { upstream = sha; original = _ }
                ->
                  pin io ~prefix command.token
                    ("boundary-" ^ Int.to_string i)
                    sha
              | Branch_reconcile.Plain -> ());
          match
            Branch_reconcile.check_checkout command ~branch
              (observe_checkout io)
          with
          | Error reason -> Branch_reconcile.Recovery_required reason
          | Ok () -> (
              let rec select evidence =
                match
                  Branch_reconcile.choose_boundary
                    ~candidates:request.boundaries ~evidence
                with
                | Branch_reconcile.Chosen boundary -> boundary
                | Branch_reconcile.Probe sha ->
                    let usable =
                      ancestor io sha request.incoming
                      && preserved_revision io ~branch ~original:sha
                           ~candidate:request.preserved
                    in
                    select ((sha, usable) :: evidence)
              in
              match select [] with
              | ( Branch_reconcile.Recorded _ | Branch_reconcile.Inferred _
                | Branch_reconcile.Patch_equivalent _
                | Branch_reconcile.Subject_inferred _
                | Branch_reconcile.Reconstructed _ ) as boundary ->
                  Branch_reconcile.Remote_replay_selected boundary
              | Branch_reconcile.Plain -> (
                  let code, out, err =
                    io.git
                      [
                        "merge-base";
                        "--all";
                        Branch_reconcile.Commit.to_string request.preserved;
                        Branch_reconcile.Commit.to_string request.incoming;
                      ]
                  in
                  if code = 1 then
                    Branch_reconcile.Recovery_required
                      "remote_replay_no_common_ancestor"
                  else if code <> 0 then
                    raise
                      (Probe_failed ("remote replay merge-base failed: " ^ err))
                  else
                    match String.split_lines (String.strip out) with
                    | [ common ] when not (String.is_empty common) ->
                        Branch_reconcile.Remote_replay_selected
                          (Branch_reconcile.Inferred (commit common))
                    | [] | [ _ ] ->
                        raise
                          (Probe_failed
                             "remote replay merge-base returned no evidence")
                    | _ :: _ :: _ ->
                        Branch_reconcile.Recovery_required
                          "remote_replay_ambiguous_common_ancestor")))
      | Branch_reconcile.Checkout_remote replay -> (
          pin_captured ();
          let checkout = observe_checkout io in
          match Branch_reconcile.check_checkout command ~branch checkout with
          | Error reason -> Branch_reconcile.Recovery_required reason
          | Ok () ->
              if not (ancestor io replay.upstream replay.incoming) then
                Branch_reconcile.Recovery_required
                  "remote_replay_boundary_unreachable"
              else
                let preserved =
                  preserved_revision io ~branch ~original:replay.upstream
                    ~candidate:replay.preserved
                in
                if not preserved then
                  Branch_reconcile.Recovery_required
                    "remote_replay_prior_work_unverified"
                else (
                  ignore
                    (checked io
                       [
                         "reset";
                         "--keep";
                         Branch_reconcile.Commit.to_string replay.incoming;
                       ]
                      : string);
                  let after = observe_checkout io in
                  if
                    G.clean after
                    && Option.equal String.equal after.branch (Some branch)
                    && String.equal after.head
                         (Branch_reconcile.Commit.to_string replay.incoming)
                    && G.equal_sequencer after.sequencer G.None_active
                  then Branch_reconcile.Remote_checked_out
                  else
                    Branch_reconcile.Recovery_required
                      "remote_replay_checkout_changed"))
      | Branch_reconcile.Commit_merge capture -> (
          pin_captured ();
          match
            Branch_reconcile.check_checkout command ~branch
              (observe_checkout io)
          with
          | Error reason -> Branch_reconcile.Recovery_required reason
          | Ok () -> (
              let code, _, err = io.git [ "commit"; "--no-edit" ] in
              if code <> 0 then
                Branch_reconcile.Retryable { reason = err; retry_after = None }
              else
                match
                  completed_merge io ~branch capture (observe_checkout io)
                with
                | Some head -> Branch_reconcile.Merge_completed head
                | None ->
                    Branch_reconcile.Recovery_required
                      "merge_completion_unverified"))
      | Branch_reconcile.Pin _ ->
          pin_captured ();
          Branch_reconcile.Pinned
      | Branch_reconcile.Integrate { source; target; boundary; policy } -> (
          pin_captured ();
          let checkout = observe_checkout io in
          match Branch_reconcile.check_checkout command ~branch checkout with
          | Error reason -> Branch_reconcile.Recovery_required reason
          | Ok () ->
              if ancestor io target source then
                Branch_reconcile.Integrated source
              else
                let target = Branch_reconcile.Commit.to_string target in
                let args =
                  match (policy, boundary) with
                  | Branch_reconcile.Preserve_ancestry, _ ->
                      [ "merge"; "--no-edit"; target ]
                  | ( Branch_reconcile.Rewrite,
                      ( Branch_reconcile.Recorded upstream
                      | Branch_reconcile.Inferred upstream
                      | Branch_reconcile.Patch_equivalent upstream
                      | Branch_reconcile.Subject_inferred upstream
                      | Branch_reconcile.Reconstructed
                          { upstream; original = _ } ) ) ->
                      [ "rebase"; "--no-update-refs"; "--no-autostash" ]
                      @ (if Option.is_some operation.remote_replay then
                           [ "--rebase-merges" ]
                         else [])
                      @ [
                          "--onto";
                          target;
                          Branch_reconcile.Commit.to_string upstream;
                        ]
                  | Branch_reconcile.Rewrite, Branch_reconcile.Plain ->
                      [ "rebase"; "--no-update-refs"; "--no-autostash"; target ]
                in
                let code, _, err = io.git args in
                mutation_result io ~operation code err)
      | Branch_reconcile.Continue { head = _; target; sequencer } -> (
          pin_captured ();
          let checkout = observe_checkout io in
          match Branch_reconcile.check_checkout command ~branch checkout with
          | Error reason -> Branch_reconcile.Recovery_required reason
          | Ok () ->
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
                          G.sequencer_key (G.Rebase { r with merge_heads = [] });
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
                      [ "-c"; "core.editor=true"; "cherry-pick"; "--continue" ]
                  | G.None_active -> []
                in
                let code, _, err = io.git args in
                mutation_result io ~operation code err)
      | Branch_reconcile.Publish { candidate; expected } -> (
          pin_captured ();
          let checkout = observe_checkout io in
          match Branch_reconcile.check_checkout command ~branch checkout with
          | Error reason -> Branch_reconcile.Recovery_required reason
          | Ok () -> (
              if not (ancestry_preserved io operation candidate) then
                Branch_reconcile.Recovery_required "ancestry_not_preserved"
              else if
                not
                  (Option.for_all expected ~f:(fun remote ->
                       if ancestor io remote candidate then true
                       else if
                         Option.value_map
                           (Branch_reconcile.rewrite_publication_input operation
                              ~candidate ~expected:remote) ~default:false
                           ~f:(fun (source, upstream) ->
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
                    if Push_reject_classify.is_permanent rejection then
                      Branch_reconcile.Permanent
                        (Push_reject_classify.short_label rejection)
                    else
                      Branch_reconcile.Retryable
                        { reason = err; retry_after = None }
                | Worktree_parser.Push_error _ ->
                    Branch_reconcile.Retryable
                      { reason = err; retry_after = None }))
      | Branch_reconcile.Confirm candidate ->
          pin_captured ();
          if
            Branch_reconcile.equal_purpose operation.intent.purpose
              Branch_reconcile.Verify_publication
            && not
                 (Branch_reconcile.verification_checkout_valid ~branch
                    ~candidate (observe_checkout io))
          then Branch_reconcile.Permanent "legacy_publication_checkout_changed"
          else if not (ancestry_preserved io operation candidate) then
            Branch_reconcile.Recovery_required "ancestry_not_preserved"
          else
            let sha = remote io ~destination branch in
            complete_initial_tracking io ~destination ~branch ~operation
              ~remote:sha;
            Branch_reconcile.Remote
              { sha; topology = topology io candidate sha }
    with
    | Unsupported_destination reason -> Branch_reconcile.Permanent reason
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

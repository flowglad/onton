(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
open Onton_test_support.Branch_reconcile_git_fixture

let () =
  timed ~name:"publication-recovery" (fun () ->
      Eio_main.run @@ fun env ->
      let make_io dir =
        E.make_io
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~path:dir
      in
      (* Pending deterministic publication checks actual ancestry. Previously
     confirmed publication stays settled when importing an old checkpoint. *)
      iter_cases ~name:"publication-recovery-1"
        (fun (stage, safe, probe_failure) ->
          Git.with_temp_repo (fun remote ->
              let base = commit remote "base" "base\n" "base" in
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  let source = commit dir "local" "local\n" "local" in
                  ignore (commit remote "upstream" "upstream\n" "upstream");
                  let io = make_io dir in
                  let reply state result =
                    let command = Option.get (B.pending state) in
                    fst
                      (B.step state
                         (B.Result { token = command.token; at = 100.; result }))
                  in
                  let rec prepare_candidate fuel state =
                    if fuel = 0 then
                      failwith "fixture did not reach publication";
                    let command = Option.get (B.pending state) in
                    match command.B.kind with
                    | B.Publish _ -> state
                    | B.Integrate _ ->
                        let result =
                          execute io
                            ~operation:(Option.get (B.operation state))
                            command
                        in
                        prepare_candidate (fuel - 1) (reply state result)
                    | B.Observe | B.Pin _ | B.Inspect | B.Verify_recovery
                    | B.Continue _ | B.Commit_merge _ | B.Plan_remote_replay _
                    | B.Checkout_remote _ | B.Confirm _ ->
                        prepare_candidate (fuel - 1)
                          (reply state
                             (execute io
                                ~operation:(Option.get (B.operation state))
                                command))
                  in
                  let state =
                    let initial =
                      fst
                        (B.step B.empty
                           (B.Materialized (B.New_branch (sha base))))
                    in
                    prepare_candidate 12
                      (fst
                         (B.step initial
                            (B.Request
                               {
                                 intent with
                                 policy =
                                   (if safe then B.Preserve_ancestry
                                    else B.Rewrite);
                               })))
                  in
                  let candidate =
                    Option.get (Option.get (B.operation state)).candidate
                  in
                  let state =
                    if stage = 0 then state else reply state B.Published
                  in
                  let state =
                    if stage < 2 then state
                    else (
                      Git.run_git ~cwd:dir [ "push"; "origin"; "HEAD:patch" ];
                      reply state
                        (B.Remote { sha = Some candidate; topology = Equal }))
                  in
                  let field name f = function
                    | `Assoc fields ->
                        `Assoc
                          (List.map
                             (fun (key, value) ->
                               (key, if key = name then f value else value))
                             fields)
                    | _ -> failwith "checkpoint object"
                  in
                  let legacy =
                    field "active"
                      (fun active ->
                        let active =
                          field "intent"
                            (field "policy" (fun _ ->
                                 `List [ `String "Preserve_ancestry" ]))
                            active
                        in
                        match active with
                        | `Assoc fields ->
                            let fields =
                              List.remove_assoc "preservation_policy" fields
                            in
                            `Assoc fields
                        | _ -> failwith "active object")
                      (B.yojson_of_t state)
                    |> field "desired"
                         (field "policy" (fun _ ->
                              `List [ `String "Preserve_ancestry" ]))
                  in
                  let state = get (B.decode legacy) in
                  let state =
                    if B.pending state = None then fst (B.step state B.Recover)
                    else state
                  in
                  if stage = 2 && safe then
                    check "confirmed legacy publication stays settled"
                      (B.phase state = Some B.Settled && B.pending state = None)
                  else
                    let operation = Option.get (B.operation state) in
                    let command = Option.get (B.pending state) in
                    let pushes = ref 0 in
                    let observed_io =
                      {
                        E.git =
                          (fun args ->
                            (match args with
                            | "push" :: _ -> incr pushes
                            | _ -> ());
                            if
                              probe_failure
                              && args
                                 = [
                                     "merge-base";
                                     "--is-ancestor";
                                     source;
                                     B.Commit.to_string candidate;
                                   ]
                            then (128, "", "ancestry probe unavailable")
                            else io.E.git args);
                      }
                    in
                    let result = execute observed_io ~operation command in
                    if probe_failure then (
                      check
                        "failed ancestry probe retries without spending repair"
                        (result
                        = B.Retryable
                            {
                              reason =
                                "ancestry probe failed: ancestry probe \
                                 unavailable";
                              retry_after = None;
                            });
                      check "probe failure never pushes" (!pushes = 0))
                    else if not safe then (
                      check
                        "legacy candidate cannot bypass ancestry verification"
                        (result = B.Recovery_required "ancestry_not_preserved");
                      check "invalid legacy candidate is never pushed"
                        (!pushes = 0)))))
        [
          (0, false, false);
          (1, false, false);
          (2, false, false);
          (0, false, true);
          (1, false, true);
          (2, true, false);
        ];
      (* A rewrite request adopting a merge retains the merge's ancestry policy.
     A later external rebase cannot impersonate completion through its reflog. *)
      Git.with_temp_repo (fun remote ->
          ignore (commit remote "base" "base\n" "base");
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              let source = commit dir "patch" "patch\n" "patch work" in
              let target =
                commit remote "upstream" "upstream\n" "base advanced"
              in
              Git.run_git ~cwd:dir [ "fetch"; "origin" ];
              Git.run_git ~cwd:dir [ "merge"; "--no-commit"; target ];
              let runtime = make_runtime () and io = make_io dir in
              let checkpoint =
                Filename.temp_file "adopted-merge-authority" ".json"
              in
              Fun.protect
                ~finally:(fun () -> Sys.remove checkpoint)
                (fun () ->
                  (try
                     ignore
                       (R.run ~runtime ~persist:(persist checkpoint)
                          ~patch_id:id
                          ~now:(fun () -> 100.)
                          ~execute:(fun ~operation command ->
                            match command.B.kind with
                            | B.Pin _ -> raise Simulated_stop
                            | B.Observe | B.Integrate _ | B.Inspect
                            | B.Verify_recovery | B.Continue _
                            | B.Commit_merge _ | B.Plan_remote_replay _
                            | B.Checkout_remote _ | B.Publish _ | B.Confirm _ ->
                                execute io ~operation command)
                          (B.Request intent));
                     failwith "expected adoption interruption"
                   with Simulated_stop -> ());
                  let operation = Option.get (B.operation (state runtime)) in
                  check "adopted merge captures stronger policy than request"
                    (operation.intent.policy = B.Rewrite
                    && B.execution_policy operation = B.Preserve_ancestry);
                  Git.run_git ~cwd:dir [ "merge"; "--abort" ];
                  Git.run_git ~cwd:dir [ "rebase"; target ];
                  let rewritten =
                    Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                  in
                  check "external rewrite removes original ancestry"
                    (Git.git_exit_code ~cwd:dir
                       [ "merge-base"; "--is-ancestor"; source; rewritten ]
                    = 1);
                  let runtime =
                    Runtime.create ~gameplan
                      ~main_branch:(Types.Branch.of_string "main")
                      ~snapshot:(get (Persistence.load ~path:checkpoint))
                      ()
                  in
                  let token =
                    match run runtime (persist checkpoint) io B.Recover with
                    | R.Repair_needed token -> token
                    | R.Idle | R.Waiting | R.Intervention _
                    | R.Checkpoint_failed _ ->
                        failwith "rewritten adopted merge must require recovery"
                  in
                  check
                    "reflog alone cannot authorize publishing rewritten merge"
                    (Git.git_exit_code ~cwd:remote
                       [ "show-ref"; "--verify"; "refs/heads/patch" ]
                    <> 0);
                  Git.run_git ~cwd:dir [ "reset"; "--hard"; source ];
                  Git.run_git ~cwd:dir [ "merge"; "--no-edit"; target ];
                  check "restoring required ancestry allows publication"
                    (run runtime (persist checkpoint) io
                       (B.Repair_completed { token; at = 100. })
                    = R.Idle);
                  List.iter
                    (fun revision ->
                      check "published adopted merge retains captured ancestry"
                        (Git.git_exit_code ~cwd:remote
                           [ "merge-base"; "--is-ancestor"; revision; "patch" ]
                        = 0))
                    [ source; target ];
                  check "unapproved rewritten intermediate remains recoverable"
                    (Base.String.is_substring
                       (Git.git_capture ~cwd:dir
                          [ "for-each-ref"; "--format=%(objectname)"; prefix ])
                       ~substring:rewritten))));

      (* Exhausted deterministic integration needs recovery, not an endless
     transport retry. Unrelated histories retain both sides for the agent. *)
      iter_cases ~name:"publication-recovery-2"
        (fun (policy, late_remote) ->
          Git.with_temp_repo (fun remote ->
              ignore (commit remote "base" "base\n" "base");
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  let local = commit dir "local" "local\n" "local work" in
                  Git.run_git ~cwd:remote [ "checkout"; "--orphan"; "patch" ];
                  Git.run_git ~cwd:remote [ "rm"; "-rf"; "." ];
                  let remote_head =
                    commit remote "external" "external\n" "external work"
                  in
                  Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
                  let runtime = make_runtime () and io = make_io dir in
                  let checkpoint =
                    Filename.temp_file "publication-recovery" ".json"
                  in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      let outcome =
                        run runtime (persist checkpoint) io
                          (B.Request
                             {
                               intent with
                               policy;
                               purpose = B.Publish_revision (sha local);
                             })
                      in
                      let token =
                        match outcome with
                        | R.Repair_needed token ->
                            let turn =
                              Option.get
                                (B.repair_turn (state runtime) ~branch:"patch"
                                   token)
                            in
                            check "unproven publication enters history recovery"
                              (match turn.B.mode with
                              | B.History_recovery { reason; _ } ->
                                  reason = "no_shared_ancestor"
                              | B.Content_repair | B.Diagnosis _ -> false);
                            token
                        | R.Idle | R.Waiting | R.Intervention _
                        | R.Checkpoint_failed _ ->
                            failwith
                              ("unproven publication did not offer recovery: "
                              ^ Yojson.Safe.to_string
                                  (B.yojson_of_t (state runtime)))
                      in
                      check "recovery preserves local candidate"
                        (Git.git_capture ~cwd:dir [ "rev-parse"; "patch" ]
                        = local);
                      check "recovery preserves remote work"
                        (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                        = remote_head);
                      (* The recovery turn may change HEAD, but publication still belongs
                 to the owner and requires independently observed preservation. *)
                      Git.run_git ~cwd:dir
                        [
                          "merge";
                          "--allow-unrelated-histories";
                          "--no-edit";
                          remote_head;
                        ];
                      let later =
                        if late_remote then (
                          Git.run_git ~cwd:remote [ "checkout"; "-q"; "patch" ];
                          let later =
                            commit remote "later" "later\n" "later remote work"
                          in
                          Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
                          Some later)
                        else None
                      in
                      let outcome =
                        run runtime (persist checkpoint) io
                          (B.Repair_completed { token; at = 100. })
                      in
                      let outcome =
                        if outcome = R.Waiting then
                          run runtime (persist checkpoint) io (B.Tick 10000.)
                        else outcome
                      in
                      check
                        "lease movement preserves accepted repair and requests \
                         new work"
                        (if late_remote then
                           match outcome with
                           | R.Repair_needed _ -> true
                           | R.Idle | R.Waiting | R.Intervention _
                           | R.Checkpoint_failed _ ->
                               false
                         else outcome = R.Idle);
                      check
                        "remote receives exact accepted HEAD or keeps its \
                         newer lease"
                        (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                        = Option.value later
                            ~default:
                              (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ])
                        );
                      List.iter
                        (fun revision ->
                          check "unproven merge inputs remain recoverable"
                            (List.mem (sha revision)
                               (B.required_revisions (state runtime))))
                        (local :: remote_head :: Option.to_list later)))))
        [ (B.Rewrite, false); (B.Rewrite, true); (B.Preserve_ancestry, true) ];
      iter_cases ~name:"publication-recovery-3"
        (fun (lost_probe, discard, late_work) ->
          Git.with_temp_repo (fun remote ->
              let base = commit remote "base" "base\n" "base" in
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  let source = commit dir "patch" "patch\n" "patch work" in
                  let target =
                    commit remote "upstream" "upstream\n" "base advancement"
                  in
                  let io = make_io dir and runtime = make_runtime () in
                  let checkpoint =
                    Filename.temp_file "agent-recovery" ".json"
                  in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      ignore
                        (run runtime (persist checkpoint) io
                           (B.Materialized (B.New_branch (sha base))));
                      (try
                         ignore
                           (R.run ~runtime ~persist:(persist checkpoint)
                              ~patch_id:id
                              ~now:(fun () -> 100.)
                              ~execute:(fun ~operation command ->
                                match command.B.kind with
                                | B.Integrate _ ->
                                    ignore
                                      (commit dir "extra"
                                         "unacknowledged local work\n"
                                         "interrupted work");
                                    raise Simulated_stop
                                | B.Commit_merge _ | B.Plan_remote_replay _
                                | B.Checkout_remote _ | B.Observe | B.Pin _
                                | B.Inspect | B.Verify_recovery | B.Continue _
                                | B.Publish _ | B.Confirm _ ->
                                    execute io ~operation command)
                              (B.Request intent));
                         failwith "expected interruption"
                       with Simulated_stop -> ());
                      let restore () =
                        Runtime.create ~gameplan
                          ~main_branch:(Types.Branch.of_string "main")
                          ~snapshot:(get (Persistence.load ~path:checkpoint))
                          ()
                      in
                      let runtime = restore () in
                      let token =
                        match run runtime (persist checkpoint) io B.Recover with
                        | R.Repair_needed token -> token
                        | R.Idle | R.Waiting | R.Intervention _
                        | R.Checkpoint_failed _ ->
                            failwith "history recovery was not dispatched"
                      in
                      let token =
                        if late_work then (
                          ignore
                            (commit dir "late" "work arriving between turns\n"
                               "late external work");
                          match
                            run runtime (persist checkpoint) io B.Recover
                          with
                          | R.Repair_needed token -> token
                          | R.Idle | R.Waiting | R.Intervention _
                          | R.Checkpoint_failed _ ->
                              failwith
                                "late work should enter retained history \
                                 recovery")
                        else token
                      in
                      let retained =
                        Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                      in
                      let turn =
                        Option.get
                          (B.repair_turn (state runtime) ~branch:"patch" token)
                      in
                      check "fallback has distinct durable authority"
                        (match turn.B.mode with
                        | B.History_recovery _ -> true
                        | B.Content_repair | B.Diagnosis _ -> false);
                      let calls = ref 0 in
                      let active_token = ref token in
                      let backend =
                        Llm_backend.
                          {
                            name = "history-recovery";
                            run_streaming =
                              (fun ~project_name:_
                                ~cwd:_
                                ~patch_id:_
                                ~prompt:_
                                ~resume_session:_
                                ~session_uuid:_
                                ~complexity:_
                                ~on_event:_
                              ->
                                incr calls;
                                let saved =
                                  get (Persistence.load ~path:checkpoint)
                                in
                                let saved_state =
                                  (Orchestrator.agent saved.Runtime.orchestrator
                                     id)
                                    .Patch_agent.branch_reconcile
                                in
                                check
                                  "history recovery claimed before backend runs"
                                  (Option.is_some
                                     (B.repair_turn saved_state ~branch:"patch"
                                        !active_token));
                                if discard then
                                  Git.run_git ~cwd:dir
                                    [ "reset"; "--hard"; target ]
                                else (
                                  Git.run_git ~cwd:dir
                                    [ "reset"; "--hard"; source ];
                                  Git.run_git ~cwd:dir
                                    [ "rebase"; "--onto"; target; base ]);
                                {
                                  exit_code = 0;
                                  stdout = "";
                                  stderr = "";
                                  got_events = true;
                                  saw_final_result = true;
                                  timed_out = false;
                                });
                          }
                      in
                      let probes = ref 0 in
                      let invoke (turn : B.repair_turn) =
                        active_token := turn.token;
                        Branch_repair_session.run
                          ~resume_session:(Some "patch-session")
                          ~session_uuid:"patch-session"
                          ~on_event:(fun _ -> ())
                          ~context:"" ~guidance:[] ~backend
                          ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
                          ~project_name:"demo" ~patch_id:id ~complexity:None
                          ~turn
                          ~read_head:(fun () ->
                            incr probes;
                            if lost_probe && !probes = 2 then None
                            else
                              Some
                                (Git.git_capture ~cwd:dir
                                   [ "rev-parse"; "HEAD" ]))
                          ~now:(fun () -> 100.)
                      in
                      let outcome =
                        run runtime (persist checkpoint) io (invoke turn)
                      in
                      if lost_probe then
                        check
                          "successful turn does not depend on completion HEAD \
                           probe"
                          (outcome = R.Idle && !calls = 1);
                      if discard then (
                        check
                          "successful agent repair accepts topology beyond \
                           deterministic criteria"
                          (outcome = R.Idle && !calls = 1);
                        check "published revision is the captured agent HEAD"
                          (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                          = target))
                      else (
                        check "verified agent recovery publishes"
                          (outcome = R.Idle);
                        check
                          "restart verifies completed work without another \
                           agent"
                          (!calls = 1);
                        check
                          "only captured patch content survives reconstruction"
                          (Git.git_capture ~cwd:remote
                             [ "diff"; "--name-only"; "main...patch" ]
                          = "patch");
                        List.iter
                          (fun file ->
                            check
                              ("published recovery retains " ^ file)
                              (Git.git_capture ~cwd:remote
                                 [ "show"; "patch:" ^ file ]
                              = Git.git_capture ~cwd:dir
                                  [ "show"; "HEAD:" ^ file ]))
                          [ "patch"; "upstream" ]);
                      let saved = get (Persistence.load ~path:checkpoint) in
                      let agent =
                        Orchestrator.agent saved.Runtime.orchestrator id
                      in
                      check
                        "fallback is separate from implementation and session \
                         failures"
                        (agent.Patch_agent.session_completion = None
                        && agent.Patch_agent.start_attempts_without_pr = 0);
                      check "recovery refs retain captured work"
                        (Base.String.is_substring
                           (Git.git_capture ~cwd:dir
                              [
                                "for-each-ref"; "--format=%(objectname)"; prefix;
                              ])
                           ~substring:retained)))))
        [
          (false, false, false);
          (true, false, false);
          (false, true, false);
          (false, true, true);
        ];
      ())

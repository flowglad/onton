(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
open Onton_test_support.Branch_reconcile_git_fixture

let () =
  timed ~name:"ancestry-repair" (fun () ->
      Eio_main.run @@ fun env ->
      let make_io dir =
        E.make_io
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~path:dir
      in
      iter_cases ~name:"ancestry-repair-1"
        (fun fail_probe ->
          Git.with_temp_repo (fun remote ->
              let base = commit remote "base" "base\n" "base" in
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  ignore (commit dir "patch-work" "patch\n" "patch work");
                  let runtime = make_runtime () and io = make_io dir in
                  let checkpoint =
                    Filename.temp_file "older-boundary" ".json"
                  in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      ignore
                        (run runtime (persist checkpoint) io
                           (B.Materialized (B.New_branch (sha base))));
                      let request name =
                        B.Request
                          { intent with purpose = Reconcile_request name }
                      in
                      let first_target =
                        commit remote "first" "first\n" "first base movement"
                      in
                      check "first reconciliation settles"
                        (run runtime (persist checkpoint) io (request "first")
                        = R.Idle);
                      let first_candidate =
                        Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                      in
                      let second_target =
                        commit remote "second" "second\n" "second base movement"
                      in
                      check "second reconciliation settles"
                        (run runtime (persist checkpoint) io (request "second")
                        = R.Idle);
                      Git.run_git ~cwd:dir
                        [ "reset"; "--hard"; first_candidate ];
                      let runtime, event =
                        if fail_probe then (
                          let failing =
                            E.
                              {
                                git =
                                  (fun args ->
                                    if
                                      args
                                      = [
                                          "merge-base";
                                          "--is-ancestor";
                                          second_target;
                                          first_candidate;
                                        ]
                                    then (128, "", "ancestry probe unavailable")
                                    else io.git args);
                              }
                          in
                          check "failed boundary probe waits without mutation"
                            (run runtime (persist checkpoint) failing
                               (request "third")
                            = R.Waiting);
                          let snapshot =
                            get (Persistence.load ~path:checkpoint)
                          in
                          ( Runtime.create ~gameplan
                              ~main_branch:(Types.Branch.of_string "main")
                              ~snapshot (),
                            B.Recover ))
                        else (runtime, request "third")
                      in
                      let checked = ref false in
                      (try
                         ignore
                           (R.run ~runtime ~persist:(persist checkpoint)
                              ~patch_id:id
                              ~now:(fun () -> 100.)
                              ~execute:(fun ~operation command ->
                                match command.B.kind with
                                | B.Integrate _ ->
                                    check "older reachable receipt selected"
                                      (B.equal_boundary operation.B.boundary
                                         (B.Recorded (sha first_target)));
                                    check "captured reverted source retained"
                                      (operation.B.source
                                      = Some (sha first_candidate));
                                    checked := true;
                                    raise Simulated_stop
                                | B.Commit_merge _ | B.Plan_remote_replay _
                                | B.Checkout_remote _ | B.Observe | B.Pin _
                                | B.Inspect | B.Verify_recovery | B.Continue _
                                | B.Publish _ | B.Confirm _ ->
                                    execute io ~operation command)
                              event);
                         failwith "expected stop before integration"
                       with Simulated_stop -> ());
                      check "older boundary verified before mutation" !checked;
                      check "boundary selection leaves checkout untouched"
                        (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                        = first_candidate)))))
        [ false; true ];
      iter_cases ~name:"ancestry-repair-2"
        (fun external_reset ->
          Git.with_temp_repo (fun remote ->
              ignore (commit remote "base" "base\n" "base");
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  let root_source = commit dir "root" "root\n" "root work" in
                  Git.run_git ~cwd:remote
                    [ "checkout"; "-q"; "-b"; "contributor" ];
                  let captured =
                    commit remote "child" "captured\n" "child work"
                  in
                  let newer =
                    commit remote "later-child" "newer\n" "later child work"
                  in
                  Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
                  let runtime = make_runtime () and io = make_io dir in
                  let pushes = ref 0 and reset_pending = ref external_reset in
                  let lost_ack =
                    E.
                      {
                        git =
                          (fun args ->
                            let result = io.git args in
                            match args with
                            | "merge" :: _ when !reset_pending ->
                                reset_pending := false;
                                Git.run_git ~cwd:dir
                                  [ "reset"; "--hard"; root_source ];
                                result
                            | "push" :: _ ->
                                incr pushes;
                                (128, "", "lost push acknowledgement")
                            | _ -> result);
                      }
                  in
                  let checkpoint =
                    Filename.temp_file "captured-contribution" ".json"
                  in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      let request =
                        B.Request
                          {
                            intent with
                            purpose =
                              Integrate_revision
                                { contributor = "2"; revision = sha captured };
                          }
                      in
                      let outcome =
                        run runtime (persist checkpoint) lost_ack request
                      in
                      let outcome =
                        if external_reset then (
                          let token =
                            match outcome with
                            | R.Repair_needed token -> token
                            | R.Idle | R.Waiting | R.Intervention _
                            | R.Checkpoint_failed _ ->
                                failwith
                                  "external reset was not routed to recovery"
                          in
                          check
                            "failed integration postcondition prevents \
                             publication"
                            (!pushes = 0);
                          Git.run_git ~cwd:dir
                            [ "merge"; "--no-edit"; captured ];
                          run runtime (persist checkpoint) lost_ack
                            (B.Repair_completed { token; at = 100. }))
                        else outcome
                      in
                      check "root publication waits after lost acknowledgement"
                        (outcome = R.Waiting);
                      check
                        "unconfirmed contribution has no publication receipt"
                        (B.publications (state runtime) = []);
                      check "root integration preserves existing root ancestry"
                        (Git.git_exit_code ~cwd:remote
                           [
                             "merge-base"; "--is-ancestor"; root_source; "patch";
                           ]
                        = 0);
                      check "root includes captured descendant revision"
                        (Git.git_exit_code ~cwd:remote
                           [ "merge-base"; "--is-ancestor"; captured; "patch" ]
                        = 0);
                      check
                        "moving descendant name cannot change captured \
                         integration"
                        (Git.git_exit_code ~cwd:remote
                           [ "merge-base"; "--is-ancestor"; newer; "patch" ]
                        = 1);
                      let runtime =
                        Runtime.create ~gameplan
                          ~main_branch:(Types.Branch.of_string "main")
                          ~snapshot:(get (Persistence.load ~path:checkpoint))
                          ()
                      in
                      check
                        "restart confirms root publication without another push"
                        (run runtime (persist checkpoint) lost_ack B.Recover
                         = R.Idle
                        && !pushes = 1);
                      match B.publications (state runtime) with
                      | [ receipt ] ->
                          check "receipt names captured contribution"
                            (receipt.B.published_intent.B.purpose
                            = B.Integrate_revision
                                { contributor = "2"; revision = sha captured })
                      | [] | _ :: _ :: _ ->
                          failwith "missing root publication receipt"))))
        [ false; true ];
      iter_cases ~name:"ancestry-repair-3"
        (fun interrupt ->
          Git.with_temp_repo (fun remote ->
              let base = commit remote "file" "base\n" "base" in
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  ignore (commit dir "file" "local\n" "patch");
                  let old_target =
                    commit remote "file" "upstream\n" "upstream"
                  in
                  Git.run_git ~cwd:dir [ "fetch"; "origin" ];
                  check "fixture has an interrupted rebase"
                    (Git.git_exit_code ~cwd:dir
                       [ "rebase"; "--onto"; old_target; base ]
                    <> 0);
                  let output = open_out (Filename.concat dir "file") in
                  output_string output "resolved\n";
                  close_out output;
                  Git.run_git ~cwd:dir [ "add"; "file" ];
                  let new_target =
                    commit remote "later" "later\n" "newer base"
                  in
                  let runtime = make_runtime () and io = make_io dir in
                  let checkpoint =
                    Filename.temp_file "adopt-integration" ".json"
                  in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      ignore
                        (run runtime (persist checkpoint) io
                           (B.Materialized (B.New_branch (sha base))));
                      let runtime, outcome =
                        if interrupt then (
                          (try
                             ignore
                               (R.run ~runtime ~persist:(persist checkpoint)
                                  ~patch_id:id
                                  ~now:(fun () -> 100.)
                                  ~execute:(fun ~operation command ->
                                    match command.B.kind with
                                    | B.Pin _ -> raise Simulated_stop
                                    | B.Commit_merge _ | B.Plan_remote_replay _
                                    | B.Checkout_remote _ | B.Observe
                                    | B.Integrate _ | B.Inspect
                                    | B.Verify_recovery | B.Continue _
                                    | B.Publish _ | B.Confirm _ ->
                                        execute io ~operation command)
                                  (B.Request intent));
                             failwith
                               "expected interruption before adoption pins"
                           with Simulated_stop -> ());
                          let runtime =
                            Runtime.create ~gameplan
                              ~main_branch:(Types.Branch.of_string "main")
                              ~snapshot:
                                (get (Persistence.load ~path:checkpoint))
                              ()
                          in
                          ( runtime,
                            run runtime (persist checkpoint) io B.Recover ))
                        else
                          ( runtime,
                            run runtime (persist checkpoint) io
                              (B.Request intent) )
                      in
                      let token =
                        match outcome with
                        | R.Repair_needed token -> token
                        | R.Idle | R.Waiting | R.Intervention _
                        | R.Checkpoint_failed _ ->
                            failwith
                              "adopted paused rebase must request agent \
                               recovery"
                      in
                      ignore
                        (run runtime (persist checkpoint) io
                           (B.Repair_started token));
                      Git.run_git ~cwd:dir
                        [ "-c"; "core.editor=true"; "rebase"; "--continue" ];
                      let repaired_head =
                        Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                      in
                      check
                        "agent completes adopted staged rebase and newer base"
                        (run runtime (persist checkpoint) io
                           (B.Repair_completed { token; at = 200. })
                        = R.Idle);
                      check
                        "agent publication remains recorded across the base \
                         refresh"
                        (List.exists
                           (fun receipt ->
                             receipt.B.published_revision = sha repaired_head
                             && receipt.publication_evidence
                                = B.Agent_reported token)
                           (B.publications (state runtime)));
                      check "staged resolution survives adoption"
                        (Git.git_capture ~cwd:remote [ "show"; "patch:file" ]
                        = "resolved");
                      check
                        "newer base is incorporated after original integration"
                        (Git.git_exit_code ~cwd:remote
                           [
                             "merge-base"; "--is-ancestor"; new_target; "patch";
                           ]
                        = 0);
                      match B.integrations (state runtime) with
                      | [ latest; adopted ] ->
                          check
                            "adoption preserves original target without \
                             inventing a base name"
                            (adopted.B.capture.B.base_branch = None
                            && adopted.B.capture.B.target_revision
                               = sha old_target);
                          check "successor integrates the newly observed base"
                            (latest.B.capture.B.base_branch = Some "main"
                            && latest.B.capture.B.target_revision
                               = sha new_target)
                      | [] | [ _ ] | _ :: _ :: _ :: _ ->
                          failwith "expected two integration receipts"))))
        [ false; true ];
      (* An adopted rebase may finish mechanically, but cannot waive ancestry
     requested by a root contribution or an ordinary preserving reconciliation. *)
      iter_cases ~name:"ancestry-repair-4"
        (fun (contributor, interrupted) ->
          Git.with_temp_repo (fun remote ->
              let base = commit remote "file" "base\n" "base" in
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  let source = commit dir "file" "local\n" "patch" in
                  let target = commit remote "file" "upstream\n" "upstream" in
                  Git.run_git ~cwd:dir [ "fetch"; "origin" ];
                  check "preserving request adopts a real conflicting rebase"
                    (Git.git_exit_code ~cwd:dir
                       [ "rebase"; "--onto"; target; base ]
                    <> 0);
                  let output = open_out (Filename.concat dir "file") in
                  output_string output "resolved\n";
                  close_out output;
                  Git.run_git ~cwd:dir [ "add"; "file" ];
                  let request =
                    {
                      intent with
                      policy =
                        (if contributor then B.Rewrite else B.Preserve_ancestry);
                      purpose =
                        (if contributor then
                           B.Integrate_revision
                             { contributor = "2"; revision = sha target }
                         else B.Reconcile_base);
                    }
                  in
                  let runtime = make_runtime () and io = make_io dir in
                  let checkpoint =
                    Filename.temp_file "preserving-adopted-rebase" ".json"
                  in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      let runtime, outcome =
                        if interrupted then (
                          (try
                             ignore
                               (R.run ~runtime ~persist:(persist checkpoint)
                                  ~patch_id:id
                                  ~now:(fun () -> 100.)
                                  ~execute:(fun ~operation command ->
                                    if
                                      command.B.kind
                                      = B.Pin
                                          {
                                            source = sha source;
                                            target = sha target;
                                          }
                                    then raise Simulated_stop
                                    else execute io ~operation command)
                                  (B.Request request));
                             failwith "expected adopted rebase checkpoint"
                           with Simulated_stop -> ());
                          let runtime =
                            Runtime.create ~gameplan
                              ~main_branch:(Types.Branch.of_string "main")
                              ~snapshot:
                                (get (Persistence.load ~path:checkpoint))
                              ()
                          in
                          ( runtime,
                            run runtime (persist checkpoint) io B.Recover ))
                        else
                          ( runtime,
                            run runtime (persist checkpoint) io
                              (B.Request request) )
                      in
                      let token =
                        match outcome with
                        | R.Repair_needed token -> token
                        | R.Idle | R.Waiting | R.Intervention _
                        | R.Checkpoint_failed _ ->
                            failwith
                              "completed adopted rebase must require ancestry \
                               recovery"
                      in
                      let turn =
                        Option.get
                          (B.repair_turn (state runtime) ~branch:"patch" token)
                      in
                      check "fallback is history recovery"
                        (match turn.mode with
                        | B.History_recovery _ -> true
                        | B.Content_repair | B.Diagnosis _ -> false);
                      check "staged resolution survives before fallback"
                        (Git.git_capture ~cwd:dir [ "show"; ":file" ]
                        = "resolved");
                      check "ancestry loss cannot publish"
                        (Git.git_exit_code ~cwd:remote
                           [ "show-ref"; "--verify"; "refs/heads/patch" ]
                        <> 0);
                      let rewritten =
                        Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                      in
                      check "fixture actually lost original source ancestry"
                        (Git.git_exit_code ~cwd:dir
                           [ "merge-base"; "--is-ancestor"; source; rewritten ]
                        = 1);
                      (* Rebuild the exact preserving contract, without retaining
                     an unapproved rewritten intermediate as a merge parent. *)
                      if
                        match (E.observe_checkout io).sequencer with
                        | Git_observation.Rebase _ -> true
                        | Git_observation.None_active | Git_observation.Merge _
                        | Git_observation.Cherry_pick _ ->
                            false
                      then Git.run_git ~cwd:dir [ "rebase"; "--abort" ];
                      Git.run_git ~cwd:dir [ "reset"; "--hard"; source ];
                      ignore
                        (Git.git_exit_code ~cwd:dir
                           [ "merge"; "--no-commit"; target ]);
                      let output = open_out (Filename.concat dir "file") in
                      output_string output "resolved\n";
                      close_out output;
                      Git.run_git ~cwd:dir [ "add"; "file" ];
                      Git.run_git ~cwd:dir
                        [ "commit"; "-qm"; "restore required ancestry" ];
                      check "verified ancestry repair publishes"
                        (run runtime (persist checkpoint) io
                           (B.Repair_completed { token; at = 100. })
                        = R.Idle);
                      List.iter
                        (fun required ->
                          check
                            "publication retains every captured ancestry \
                             obligation"
                            (Git.git_exit_code ~cwd:remote
                               [
                                 "merge-base";
                                 "--is-ancestor";
                                 required;
                                 "patch";
                               ]
                            = 0))
                        [ source; target ];
                      check "repaired publication keeps staged resolution"
                        (Git.git_capture ~cwd:remote [ "show"; "patch:file" ]
                        = "resolved")))))
        [ (false, false); (false, true); (true, false); (true, true) ];
      ())

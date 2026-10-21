(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
open Onton_test_support.Branch_reconcile_git_fixture

let () =
  timed ~name:"checkpoint-interruptions" (fun () ->
      Eio_main.run @@ fun env ->
      let make_io dir =
        E.make_io
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~path:dir
      in
      iter_cases ~name:"checkpoint-interruptions-1"
        (fun (stop_at, duplicate, history) ->
          Git.with_temp_repo (fun remote ->
              let base = commit remote "base" "base\n" "base" in
              if history = 2 || history = 4 then
                ignore (commit remote "prior" "prior\n" "prior publication");
              if history <> 1 && history <> 3 then
                Git.run_git ~cwd:remote [ "branch"; "patch"; "HEAD" ];
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  let candidate =
                    commit dir "local" "local\n" "local candidate"
                  in
                  Git.run_git ~cwd:dir [ "branch"; "unrelated"; candidate ];
                  Git.run_git ~cwd:dir [ "config"; "rebase.updateRefs"; "true" ];
                  Git.run_git ~cwd:dir [ "config"; "rebase.autoStash"; "true" ];
                  let runtime = make_runtime () and io = make_io dir in
                  let raced = ref false
                  and mutations = ref 0
                  and resets = ref 0 in
                  let incoming = ref "" in
                  let racing =
                    E.
                      {
                        git =
                          (fun args ->
                            (match args with
                            | "push" :: _ when not !raced ->
                                raced := true;
                                Git.run_git ~cwd:remote
                                  (if history = 1 || history = 3 then
                                     [ "checkout"; "-qb"; "patch"; "main" ]
                                   else [ "checkout"; "-q"; "patch" ]);
                                if history = 2 || history = 4 then
                                  Git.run_git ~cwd:remote
                                    [ "reset"; "--hard"; base ];
                                ignore
                                  (if duplicate then
                                     commit remote "local" "local\n"
                                       "equivalent remote addition"
                                   else
                                     commit remote "incoming" "incoming\n"
                                       "remote addition");
                                if history >= 5 then (
                                  if history >= 6 then
                                    ignore
                                      (commit remote "base" "main change\n"
                                         "remote main side");
                                  Git.run_git ~cwd:remote
                                    [ "checkout"; "-qb"; "side"; base ];
                                  if history < 7 then
                                    ignore
                                      (commit remote "side" "side\n"
                                         "remote side work");
                                  if history >= 6 then
                                    ignore
                                      (commit remote "base" "side change\n"
                                         "remote other side");
                                  Git.run_git ~cwd:remote
                                    [ "checkout"; "-q"; "patch" ];
                                  if history >= 6 then (
                                    check "fixture creates a merge conflict"
                                      (Git.git_exit_code ~cwd:remote
                                         [
                                           "merge";
                                           "--no-ff";
                                           "--no-edit";
                                           "side";
                                         ]
                                      = 1);
                                    ignore
                                      (commit remote "base"
                                         (if history >= 7 then "main change\n"
                                          else "remote resolution\n")
                                         "resolve remote merge"))
                                  else
                                    Git.run_git ~cwd:remote
                                      [
                                        "merge"; "--no-ff"; "--no-edit"; "side";
                                      ]);
                                incoming :=
                                  Git.git_capture ~cwd:remote
                                    [ "rev-parse"; "HEAD" ];
                                Git.run_git ~cwd:remote
                                  [ "checkout"; "-q"; "main" ]
                            | "reset" :: _ -> incr resets
                            | ("rebase" | "merge" | "cherry-pick") :: _ ->
                                incr mutations
                            | _ -> ());
                            io.git args);
                      }
                  in
                  let checkpoint = Filename.temp_file "remote-replay" ".json" in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      if not (List.mem history [ 2; 3; 4 ]) then
                        ignore
                          (run runtime (persist checkpoint) racing
                             (B.Materialized (B.New_branch (sha base))));
                      check "rewrite race first waits with original candidate"
                        (run runtime (persist checkpoint) racing
                           (B.Request
                              {
                                intent with
                                purpose = Publish_revision (sha candidate);
                              })
                        = R.Waiting);
                      let stopped = ref false in
                      let stage (command : B.command) =
                        match command.kind with
                        | B.Plan_remote_replay _ -> 0
                        | B.Integrate _ -> 2
                        | B.Observe | B.Pin _ | B.Inspect | B.Verify_recovery
                        | B.Continue _ | B.Publish _ | B.Confirm _
                        | B.Commit_merge _ | B.Checkout_remote _ ->
                            -10
                      in
                      (try
                         ignore
                           (R.run ~runtime ~persist:(persist checkpoint)
                              ~patch_id:id
                              ~now:(fun () -> 2000.)
                              ~execute:(fun ~operation command ->
                                if stage command = stop_at then (
                                  stopped := true;
                                  raise Simulated_stop);
                                if stop_at = 4 && stage command = 2 then (
                                  let channel =
                                    open_out_bin (Filename.concat dir "local")
                                  in
                                  output_string channel "late staged edit\n";
                                  close_out channel;
                                  Git.run_git ~cwd:dir [ "add"; "local" ]);
                                let result =
                                  execute racing ~operation command
                                in
                                if
                                  stage command + 1 = stop_at
                                  || (stop_at = 4 && stage command = 2)
                                then (
                                  stopped := true;
                                  raise Simulated_stop);
                                result)
                              (B.Tick 2000.));
                         failwith
                           "expected remote planning/integration interruption"
                       with Simulated_stop -> ());
                      check "remote interruption reached" !stopped;
                      let snapshot = get (Persistence.load ~path:checkpoint) in
                      let restarted =
                        Runtime.create ~gameplan
                          ~main_branch:(Types.Branch.of_string "main")
                          ~snapshot ()
                      in
                      let outcome =
                        run restarted (persist checkpoint) racing B.Recover
                      in
                      check "remote replay never resets the checkout"
                        (!resets = 0);
                      check
                        "unrelated refs survive user updateRefs configuration"
                        (Git.git_capture ~cwd:dir [ "rev-parse"; "unrelated" ]
                        = candidate);
                      if stop_at = 4 then (
                        check "dirty replay reaches owned repair"
                          (match outcome with
                          | R.Repair_needed _ -> true
                          | R.Idle | R.Waiting | R.Intervention _
                          | R.Checkpoint_failed _ ->
                              false);
                        check "dirty replay stops before mutation"
                          (!mutations = 0);
                        check "repair retains the local candidate backup"
                          (List.mem (sha candidate)
                             (B.required_revisions (state restarted)));
                        check "rejected replay leaves remote unchanged"
                          (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                          = !incoming);
                        check "rejected replay retains incoming evidence"
                          (List.mem (sha !incoming)
                             (B.required_revisions (state restarted)));
                        check "dirty replay retains the local candidate"
                          (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                          = candidate);
                        check "late staged edits survive"
                          (Git.git_capture ~cwd:dir [ "show"; ":local" ]
                          = "late staged edit"))
                      else (
                        check
                          "ancestor-based remote replay converges after lost \
                           acknowledgement"
                          (outcome = R.Idle);
                        check
                          (Printf.sprintf
                             "bounded remote replay (history=%d stop=%d \
                              duplicate=%b mutations=%d)"
                             history stop_at duplicate !mutations)
                          (!mutations = 1
                          || (duplicate && stop_at = 3 && !mutations = 2));
                        check
                          "remote receipt records the observed ancestor and \
                           candidate"
                          (match B.remote_integrations (state restarted) with
                          | [ receipt ] ->
                              receipt.B.capture.target_revision = sha candidate
                              && receipt.capture.replay_boundary
                                 = B.Inferred (sha base)
                              && receipt.capture.integration_policy = B.Rewrite
                          | [] | _ :: _ :: _ -> false);
                        check "local candidate retains its identity"
                          (Git.git_exit_code ~cwd:remote
                             [
                               "merge-base"; "--is-ancestor"; candidate; "patch";
                             ]
                          = 0);
                        check "incoming work survives replay"
                          (if duplicate then
                             Git.git_capture ~cwd:remote
                               [ "rev-parse"; "patch" ]
                             = candidate
                           else
                             Git.git_capture ~cwd:remote
                               [ "show"; "patch:incoming" ]
                             = "incoming");
                        if history >= 5 && history < 7 then
                          check "merged remote side work survives replay"
                            (Git.git_capture ~cwd:remote
                               [ "show"; "patch:side" ]
                            = "side");
                        if history >= 6 then
                          check "remote merge resolution survives integration"
                            (Git.git_capture ~cwd:remote
                               [ "show"; "patch:base" ]
                            =
                            if history >= 7 then "main change"
                            else "remote resolution");
                        check "remote replay cannot introduce merge commits"
                          (Git.git_capture ~cwd:remote
                             [ "rev-list"; "--merges"; base ^ "..patch" ]
                          = ""))))))
        [
          (0, false, 0);
          (1, false, 0);
          (2, false, 0);
          (3, false, 0);
          (3, true, 0);
          (4, false, 0);
          (0, false, 1);
          (0, false, 2);
          (0, false, 3);
          (0, false, 4);
          (0, false, 5);
          (3, false, 5);
          (3, false, 6);
          (3, false, 7);
          (3, false, 8);
          (3, false, 9);
          (3, false, 10);
          (3, false, 11);
          (3, false, 12);
        ];
      ())

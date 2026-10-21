(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
open Onton_test_support.Branch_reconcile_git_fixture

let () =
  timed ~name:"capture-recovery" (fun () ->
      Eio_main.run @@ fun env ->
      let make_io dir =
        E.make_io
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~path:dir
      in
      iter_cases ~name:"capture-recovery-1"
        (fun (wrong_target, lost_ack) ->
          Git.with_temp_repo (fun remote ->
              let base = commit remote "base" "base\n" "base" in
              Git.run_git ~cwd:remote [ "checkout"; "-q"; "-b"; "other" ];
              let other = commit remote "other" "other\n" "other target" in
              Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  ignore (commit dir "work" "work\n" "patch work");
                  let source =
                    commit dir "second" "second\n" "second patch work"
                  in
                  let target =
                    commit remote "upstream" "upstream\n" "base advanced"
                  in
                  let io = make_io dir and runtime = make_runtime () in
                  let checkpoint =
                    Filename.temp_file "unclaimed-rebase" ".json"
                  in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      ignore
                        (run runtime (persist checkpoint) io
                           (B.Materialized (B.New_branch (sha base))));
                      let paused =
                        E.
                          {
                            git =
                              (fun args ->
                                match args with
                                | "rebase" :: _ ->
                                    let args =
                                      if wrong_target then
                                        List.map
                                          (fun arg ->
                                            if arg = target then other else arg)
                                          args
                                      else args
                                    in
                                    io.git (args @ [ "--exec"; "false" ])
                                | _ -> io.git args);
                          }
                      in
                      (try
                         let outcome =
                           R.run ~runtime ~persist:(persist checkpoint)
                             ~patch_id:id
                             ~now:(fun () -> 100.)
                             ~execute:(fun ~operation command ->
                               let result = execute paused ~operation command in
                               match command.B.kind with
                               | B.Integrate _ ->
                                   if lost_ack then raise Simulated_stop
                                   else result
                               | B.Observe | B.Pin _ | B.Commit_merge _
                               | B.Plan_remote_replay _ | B.Checkout_remote _
                               | B.Inspect | B.Verify_recovery | B.Continue _
                               | B.Publish _ | B.Confirm _ ->
                                   result)
                             (B.Request intent)
                         in
                         check
                           "acknowledged sequencer stop waits without agent \
                            repair"
                           ((not lost_ack) && outcome = R.Waiting);
                         check "infrastructure stop has no repair claim"
                           (match B.operation (state runtime) with
                           | Some op -> op.repair = None
                           | None -> false)
                       with Simulated_stop ->
                         check "only lost acknowledgement interrupts execution"
                           lost_ack);
                      let checkout = E.observe_checkout io in
                      check "interrupted rebase has no unresolved content"
                        (Git_observation.repair_ready checkout);
                      let todo_path =
                        Git.git_capture ~cwd:dir
                          [
                            "rev-parse";
                            "--git-path";
                            "rebase-merge/git-rebase-todo";
                          ]
                      in
                      let todo_path =
                        if Filename.is_relative todo_path then
                          Filename.concat dir todo_path
                        else todo_path
                      in
                      let ic = open_in_bin todo_path in
                      let original_todo =
                        Fun.protect
                          ~finally:(fun () -> close_in ic)
                          (fun () ->
                            really_input_string ic (in_channel_length ic))
                      in
                      let oc = open_out_bin todo_path in
                      Fun.protect
                        ~finally:(fun () -> close_out oc)
                        (fun () ->
                          output_string oc
                            (Base.String.substr_replace_all original_todo
                               ~pattern:"pick " ~with_:"drop "));
                      let snapshot = get (Persistence.load ~path:checkpoint) in
                      let restored =
                        Runtime.create ~gameplan
                          ~main_branch:(Types.Branch.of_string "main")
                          ~snapshot ()
                      in
                      let continued = ref 0 in
                      let outcome =
                        R.run ~runtime:restored ~persist:(persist checkpoint)
                          ~patch_id:id
                          ~now:(fun () -> 200.)
                          ~execute:(fun ~operation command ->
                            (match command.B.kind with
                            | B.Continue _ -> incr continued
                            | B.Observe | B.Pin _ | B.Integrate _
                            | B.Commit_merge _ | B.Plan_remote_replay _
                            | B.Checkout_remote _ | B.Inspect
                            | B.Verify_recovery | B.Publish _ | B.Confirm _ ->
                                ());
                            execute io ~operation command)
                          B.Recover
                      in
                      if wrong_target then (
                        check "foreign integration target cannot be continued"
                          (!continued = 0);
                        check
                          "foreign integration enters verified history recovery"
                          (match outcome with
                          | R.Repair_needed _ -> true
                          | R.Idle | R.Waiting | R.Intervention _
                          | R.Checkpoint_failed _ ->
                              false);
                        check
                          "foreign integration retains observed and captured \
                           commits"
                          (let refs =
                             Git.git_capture ~cwd:dir
                               [
                                 "for-each-ref";
                                 "--format=%(objectname)";
                                 prefix;
                               ]
                           in
                           List.for_all
                             (fun revision ->
                               Base.String.is_substring refs ~substring:revision)
                             [
                               source;
                               target;
                               Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ];
                             ]);
                        check "foreign integration preserves captured target"
                          (match B.operation (state restored) with
                          | Some op ->
                              op.target = Some (sha target)
                              && op.source = Some (sha source)
                          | None -> false))
                      else
                        let token =
                          match outcome with
                          | R.Repair_needed token -> token
                          | R.Idle | R.Waiting | R.Intervention _
                          | R.Checkpoint_failed _ ->
                              failwith
                                "a paused rebase with changed instructions \
                                 needs agent recovery"
                        in
                        check "changed-todo recovery has not published"
                          (Git.git_exit_code ~cwd:remote
                             [ "show-ref"; "--verify"; "refs/heads/patch" ]
                          <> 0);
                        ignore
                          (run restored (persist checkpoint) io
                             (B.Repair_started token));
                        (* The agent may inspect and complete this Git operation;
                       its successful result needs no instruction certificate. *)
                        let oc = open_out_bin todo_path in
                        Fun.protect
                          ~finally:(fun () -> close_out oc)
                          (fun () ->
                            output_string oc
                              (Base.String.substr_replace_all original_todo
                                 ~pattern:"exec false"
                                 ~with_:"# removed by recovery agent"));
                        Git.run_git ~cwd:dir
                          [ "-c"; "core.editor=true"; "rebase"; "--continue" ];
                        check "agent restores the dropped patch contribution"
                          (Git.git_capture ~cwd:dir [ "show"; "HEAD:second" ]
                          = "second");
                        check
                          "agent-approved continuation publishes the captured \
                           HEAD"
                          (run restored (persist checkpoint) io
                             (B.Repair_completed { token; at = 300. })
                          = R.Idle);
                        check "agent continuation publishes the current branch"
                          (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                          = Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ])))))
        [ (false, true); (true, true); (false, false); (true, false) ];
      Git.with_temp_repo (fun remote ->
          let base = commit remote "base" "base\n" "base" in
          Git.run_git ~cwd:remote [ "branch"; "patch"; base ];
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              let source = commit dir "work" "work\n" "local work" in
              let target = commit remote "upstream" "upstream\n" "base moved" in
              let runtime = make_runtime () and io = make_io dir in
              let checked = ref false in
              let checkpoint = Filename.temp_file "captured-lease" ".json" in
              Fun.protect
                ~finally:(fun () -> Sys.remove checkpoint)
                (fun () ->
                  ignore
                    (run runtime (persist checkpoint) io
                       (B.Materialized (B.New_branch (sha base))));
                  let outcome =
                    R.run ~runtime ~persist:(persist checkpoint) ~patch_id:id
                      ~now:(fun () -> 100.)
                      ~execute:(fun ~operation command ->
                        (match command.B.kind with
                        | B.Integrate _ ->
                            let saved label =
                              Git.git_capture ~cwd:dir
                                [ "rev-parse"; prefix ^ "/1/2/" ^ label ]
                            in
                            check "source pinned before integration"
                              (saved "source" = source);
                            check "target pinned before integration"
                              (saved "target" = target);
                            check "remote lease pinned before integration"
                              (saved "remote" = base);
                            checked := true
                        | B.Commit_merge _ | B.Plan_remote_replay _
                        | B.Checkout_remote _ | B.Observe | B.Pin _ | B.Inspect
                        | B.Verify_recovery | B.Continue _ | B.Publish _
                        | B.Confirm _ ->
                            ());
                        execute io ~operation command)
                      (B.Request intent)
                  in
                  check "captured lease survives reconciliation"
                    (!checked && outcome = R.Idle))));
      ())

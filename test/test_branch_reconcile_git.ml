(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
open Onton_test_support.Branch_reconcile_git_fixture

let () =
  let state, _ =
    B.step B.empty
      (B.Request
         {
           base = "main";
           policy = Rewrite;
           purpose = Provision_checkout "provision";
         })
  in
  match (B.operation state, B.pending state) with
  | Some operation, Some command ->
      let called = ref false in
      let io =
        E.
          {
            git =
              (fun _ ->
                called := true;
                failwith "unexpected Git");
          }
      in
      let result =
        E.execute ~io ~prefix:"refs/onton/reconcile/test" ~branch:"feature"
          ~operation command
      in
      if
        !called
        || not
             (B.equal_result result
                (B.Needs_diagnosis "provisioning_requires_checkout_handler"))
      then failwith "ordinary executor granted provisioning Git authority"
  | _ -> failwith "missing provisioning operation"

let () =
  timed ~name:"startup-publication" (fun () ->
      Eio_main.run @@ fun env ->
      let make_io dir =
        E.make_io
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~path:dir
      in
      iter_cases ~name:"startup-publication-1"
        (fun has_pr ->
          Git.with_temp_repo (fun remote ->
              ignore (commit remote "base" "base\n" "base");
              ignore (commit remote "unstaged" "original\n" "tracked fixture");
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  let original =
                    commit dir "work" "committed\n" "previous session"
                  in
                  Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "patch" ];
                  ignore (commit remote "upstream" "upstream\n" "base advanced");
                  let oc = open_out_bin (Filename.concat dir "work") in
                  output_string oc "unfinished session\n";
                  close_out oc;
                  Git.run_git ~cwd:dir [ "add"; "work" ];
                  let oc = open_out_bin (Filename.concat dir "unstaged") in
                  output_string oc "retained\n";
                  close_out oc;
                  let runtime = make_runtime () and io = make_io dir in
                  Runtime.update_orchestrator runtime (fun orch ->
                      let orch = Orchestrator.set_worktree_path orch id dir in
                      if has_pr then
                        Orchestrator.set_pr_number orch id
                          (Types.Pr_number.of_int 1)
                      else orch);
                  let checkpoint = Filename.temp_file "dirty-session" ".json" in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      check
                        "dirty base reconciliation yields to the unfinished \
                         session"
                        (match
                           run runtime (persist checkpoint) io
                             (B.Request intent)
                         with
                        | R.Repair_needed _ -> true
                        | R.Idle | R.Waiting | R.Intervention _
                        | R.Checkpoint_failed _ ->
                            false);
                      check "checkout unchanged before session"
                        (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                         = original
                        && Git.git_capture ~cwd:dir [ "show"; ":work" ]
                           = "unfinished session");
                      let snapshot = get (Persistence.load ~path:checkpoint) in
                      let restarted =
                        Runtime.create ~gameplan
                          ~main_branch:(Types.Branch.of_string "main")
                          ~snapshot ()
                      in
                      let action =
                        Runtime.read restarted (fun snap ->
                            match
                              Patch_controller.plan_actions
                                snap.Runtime.orchestrator
                                ~patches:gameplan.Types.Gameplan.patches
                            with
                            | [ action ] -> Some action
                            | [] | _ :: _ -> None)
                      in
                      let expected_action =
                        Orchestrator.Reconcile_branch
                          (id, (Option.get (B.operation (state restarted))).id)
                      in
                      check
                        "restarted supervisor schedules owned recovery before \
                         integration"
                        (action = Some expected_action);
                      Runtime.read restarted (fun snap ->
                          let orch = snap.Runtime.orchestrator in
                          let plans orch =
                            Patch_controller.plan_actions orch
                              ~patches:gameplan.Types.Gameplan.patches
                          in
                          if has_pr then (
                            let missing =
                              Orchestrator.mark_pr_missing orch id
                            in
                            check "missing PR cannot block owned local recovery"
                              (plans missing = [ expected_action ]);
                            let rediscovered =
                              Orchestrator.set_pr_number missing id
                                (Types.Pr_number.of_int 1)
                            in
                            check "rediscovered PR retains owned recovery"
                              (plans rediscovered = [ expected_action ]));
                          let inflight =
                            Orchestrator.set_automerge_inflight orch id true
                          in
                          check "recovery waits for an inflight merge"
                            (plans inflight = []);
                          check "failed merge resumes recovery"
                            (plans
                               (Orchestrator.set_automerge_inflight inflight id
                                  false)
                            = [ expected_action ]);
                          let won =
                            Patch_controller.apply_automerge_success inflight id
                          in
                          check "winning merge retains work without dispatch"
                            (plans won = []
                            && B.is_unsettled
                                 (Orchestrator.agent won id).branch_reconcile));
                      check "unfinished work cannot be approved"
                        (Runtime.read restarted (fun snap ->
                             not
                               (Patch_agent.is_approved
                                  (Orchestrator.agent snap.Runtime.orchestrator
                                     id)
                                  ~main_branch:(Types.Branch.of_string "main"))));
                      check
                        "tracked unstaged work survives recovery and restart"
                        (Git.git_capture ~cwd:dir [ "show"; ":unstaged" ]
                         = "original"
                        && Git.git_capture ~cwd:dir [ "diff"; "--"; "unstaged" ]
                           <> ""
                        && In_channel.with_open_bin
                             (Filename.concat dir "unstaged")
                             In_channel.input_all
                           = "retained\n");
                      ignore
                        (commit remote "advance" "advanced\n" "base advances");
                      let token =
                        match
                          run restarted (persist checkpoint) io B.Recover
                        with
                        | R.Repair_needed token -> token
                        | R.Idle | R.Waiting | R.Intervention _
                        | R.Checkpoint_failed _ ->
                            failwith "restart lost recovery turn"
                      in
                      let outcome =
                        R.run_repair ~runtime:restarted
                          ~persist:(persist checkpoint) ~patch_id:id
                          ~with_capacity:(fun f -> f ())
                          ~now:(fun () -> 101.)
                          ~execute:(fun ~agent:_ ~operation command ->
                            execute io ~operation command)
                          ~perform:(fun ~agent:_ ~turn ->
                            Git.run_git ~cwd:dir [ "add"; "unstaged" ];
                            Git.run_git ~cwd:dir
                              [
                                "commit";
                                "-q";
                                "-m";
                                "finish interrupted session";
                              ];
                            Git.run_git ~cwd:dir [ "fetch"; "origin" ];
                            Git.run_git ~cwd:dir
                              [ "merge"; "--no-edit"; "origin/main" ];
                            B.Repair_completed
                              { token = turn.B.token; at = 101. })
                          token
                      in
                      check "recovery settles the deferred base reconciliation"
                        (outcome = R.Idle);
                      check "session work survives subsequent integration"
                        (Git.git_capture ~cwd:dir [ "show"; "HEAD:work" ]
                         = "unfinished session"
                        && Git.git_capture ~cwd:dir [ "show"; "HEAD:unstaged" ]
                           = "retained"
                        && Git.git_capture ~cwd:dir [ "show"; "HEAD:advance" ]
                           = "advanced"
                        && Git.git_capture ~cwd:dir [ "status"; "--porcelain" ]
                           = "")))))
        [ false; true ];
      iter_cases ~name:"startup-publication-2"
        (fun missing_base ->
          Git.with_temp_repo (fun fetch_remote ->
              ignore (commit fetch_remote "base" "base\n" "base");
              let source = commit fetch_remote "work" "work\n" "source" in
              Git.with_temp_repo (fun push_remote ->
                  ignore
                    (commit push_remote "other" "other\n" "destination base");
                  if missing_base then
                    Git.run_git ~cwd:push_remote
                      [ "update-ref"; "-d"; "refs/heads/main" ];
                  Git.with_temp_repo (fun dir ->
                      prepare fetch_remote dir;
                      Git.run_git ~cwd:dir
                        [ "config"; "remote.origin.pushurl"; push_remote ];
                      if missing_base then
                        Git.run_git ~cwd:dir
                          [ "update-ref"; "-d"; "refs/remotes/origin/main" ];
                      let runtime = make_runtime () and io = make_io dir in
                      let checkpoint =
                        Filename.temp_file "destination-base" ".json"
                      in
                      Fun.protect
                        ~finally:(fun () -> Sys.remove checkpoint)
                        (fun () ->
                          check "destination base does not suppress publication"
                            (run runtime (persist checkpoint) io
                               (B.Request
                                  {
                                    intent with
                                    purpose = Publish_revision (sha source);
                                  })
                            = R.Idle);
                          check "source is published to configured destination"
                            (Git.git_capture ~cwd:push_remote
                               [ "rev-parse"; "patch" ]
                            = source))))))
        [ false; true ];
      iter_cases ~name:"startup-publication-3"
        (fun adopted ->
          List.iter
            (fun existing_remote ->
              Git.with_temp_repo (fun remote ->
                  let base = commit remote "base" "base\n" "base" in
                  Git.with_temp_repo (fun dir ->
                      prepare remote dir;
                      if existing_remote then
                        Git.run_git ~cwd:dir [ "push"; "origin"; "patch" ];
                      (* Ahead-of-base means reachability, not merely tip equality. *)
                      ignore
                        (commit remote "later-base" "later\n" "base advances");
                      let runtime = make_runtime () and io = make_io dir in
                      let receipt =
                        if adopted then B.Adopted_branch (sha base)
                        else B.New_branch (sha base)
                      in
                      let checkpoint =
                        Filename.temp_file "empty-adopted" ".json"
                      in
                      Fun.protect
                        ~finally:(fun () -> Sys.remove checkpoint)
                        (fun () ->
                          ignore
                            (run runtime (persist checkpoint) io
                               (B.Materialized receipt));
                          check "empty branch publication settles without work"
                            (run runtime (persist checkpoint) io
                               (B.Request
                                  {
                                    intent with
                                    purpose = B.Publish_revision (sha base);
                                  })
                            = R.Idle);
                          check "empty branch has no published candidate"
                            (match B.operation (state runtime) with
                            | Some op -> op.candidate = None
                            | None -> false);
                          check "empty adopted branch creates no remote ref"
                            (existing_remote
                            || Git.git_exit_code ~cwd:remote
                                 [ "show-ref"; "--verify"; "refs/heads/patch" ]
                               <> 0)))))
            [ false; true ])
        [ false; true ];
      Git.with_temp_repo (fun remote ->
          ignore (commit remote "base" "base\n" "base");
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              let source = commit dir "work" "work\n" "implementation" in
              let runtime = make_runtime () and io = make_io dir in
              let checkpoint =
                Filename.temp_file "reconfirm-publication" ".json"
              in
              Fun.protect
                ~finally:(fun () -> Sys.remove checkpoint)
                (fun () ->
                  check "initial publication settles"
                    (run runtime (persist checkpoint) io
                       (B.Request
                          {
                            intent with
                            purpose = B.Publish_revision (sha source);
                          })
                    = R.Idle);
                  Git.run_git ~cwd:remote
                    [ "update-ref"; "-d"; "refs/heads/patch" ];
                  let runtime =
                    Runtime.create ~gameplan
                      ~main_branch:(Types.Branch.of_string "main")
                      ~snapshot:(get (Persistence.load ~path:checkpoint))
                      ()
                  in
                  check
                    "publication retry reconfirms and restores deleted branch"
                    (run runtime (persist checkpoint) io B.Reconfirm_publication
                    = R.Idle);
                  check "restored remote is the exact captured candidate"
                    (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                    = source))));
      Git.with_temp_repo (fun original_remote ->
          ignore (commit original_remote "base" "base\n" "base");
          Git.with_temp_repo (fun replacement ->
              Git.with_temp_repo (fun dir ->
                  prepare original_remote dir;
                  let source =
                    commit dir "work" "work\n" "retained candidate"
                  in
                  let io = make_io dir and runtime = make_runtime () in
                  let checkpoint =
                    Filename.temp_file "destination-change" ".json"
                  in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      let unavailable =
                        E.
                          {
                            git =
                              (fun args ->
                                match args with
                                | "push" :: _ ->
                                    (128, "", "transport unavailable")
                                | _ -> io.git args);
                          }
                      in
                      check "candidate waits before destination change"
                        (run runtime (persist checkpoint) unavailable
                           (B.Request
                              {
                                intent with
                                purpose =
                                  B.Publish_session "changed-destination";
                              })
                        = R.Waiting);
                      let before = B.operation (state runtime) in
                      check
                        "poll probe accepts the captured endpoint before \
                         configuration changes"
                        (E.remote_head ~owner:(state runtime) io ~branch:"patch"
                        = Ok None);
                      Git.run_git ~cwd:dir
                        [ "remote"; "set-url"; "--push"; "origin"; replacement ];
                      let probed = ref false in
                      let watched =
                        E.
                          {
                            git =
                              (fun args ->
                                (match args with
                                | "ls-remote" :: _ -> probed := true
                                | _ -> ());
                                io.git args);
                          }
                      in
                      check
                        "poll probe rejects changed destination before reading \
                         replacement"
                        (E.remote_head ~owner:(state runtime) watched
                           ~branch:"patch"
                         = Error "publication_destination_changed"
                        && not !probed);
                      let runtime =
                        Runtime.create ~gameplan
                          ~main_branch:(Types.Branch.of_string "main")
                          ~snapshot:(get (Persistence.load ~path:checkpoint))
                          ()
                      in
                      let turns = ref 0 in
                      let rec diagnose remaining outcome =
                        match outcome with
                        | R.Repair_needed token when remaining > 0 ->
                            let outcome =
                              R.run_repair ~runtime
                                ~persist:(persist checkpoint) ~patch_id:id
                                ~with_capacity:(fun f -> f ())
                                ~now:(fun () -> 100.)
                                ~execute:(fun ~agent:_ -> execute io)
                                ~perform:(fun ~agent:_ ~turn ->
                                  incr turns;
                                  check
                                    "destination diagnosis is observation-only"
                                    (match turn.B.mode with
                                    | Diagnosis _ -> true
                                    | Content_repair | History_recovery _ ->
                                        false);
                                  B.Repair_completed
                                    { token = turn.token; at = 100. })
                                token
                            in
                            diagnose (remaining - 1) outcome
                        | R.Idle | R.Waiting | R.Intervention _
                        | R.Checkpoint_failed _ | R.Repair_needed _ ->
                            outcome
                      in
                      check
                        "changed destination receives diagnosis before \
                         intervention"
                        (diagnose 3
                           (run runtime (persist checkpoint) io B.Recover)
                         = R.Intervention "publication_destination_changed"
                        && !turns = 2);
                      check "changed destination receives no candidate"
                        (Git.git_exit_code ~cwd:replacement
                           [ "show-ref"; "--verify"; "refs/heads/patch" ]
                        <> 0);
                      check
                        "explicit resume observes and adopts reviewed \
                         destination"
                        (run runtime (persist checkpoint) io B.Resume = R.Idle);
                      check "resume publishes original candidate"
                        (Git.git_capture ~cwd:replacement
                           [ "rev-parse"; "patch" ]
                        = source);
                      check
                        "poll probe accepts destination after owned resume \
                         checkpoints it"
                        (E.remote_head ~owner:(state runtime) io ~branch:"patch"
                        = Ok (Some (sha source)));
                      check
                        "destination resume retains operation and captured \
                         source"
                        (match (before, B.operation (state runtime)) with
                        | Some before, Some after ->
                            before.id = after.id && before.source = after.source
                        | _ -> false)))));
      iter_cases ~name:"startup-publication-4"
        (fun multiple ->
          Git.with_temp_repo (fun fetch_remote ->
              ignore (commit fetch_remote "base" "base\n" "base");
              Git.with_temp_repo (fun push_remote ->
                  Git.run_git ~cwd:push_remote [ "fetch"; fetch_remote; "main" ];
                  Git.run_git ~cwd:push_remote
                    [ "reset"; "--hard"; "FETCH_HEAD" ];
                  Git.with_temp_repo (fun dir ->
                      prepare fetch_remote dir;
                      let source = commit dir "patch" "patch\n" "patch work" in
                      Git.run_git ~cwd:dir
                        [ "push"; "origin"; "HEAD:refs/heads/patch" ];
                      Git.run_git ~cwd:dir
                        [ "remote"; "set-url"; "--push"; "origin"; push_remote ];
                      if multiple then
                        Git.run_git ~cwd:dir
                          [
                            "config";
                            "--add";
                            "remote.origin.pushurl";
                            fetch_remote;
                          ];
                      let io = make_io dir and runtime = make_runtime () in
                      let pushes = ref 0 in
                      let lost_ack =
                        E.
                          {
                            git =
                              (fun args ->
                                let result = io.git args in
                                match args with
                                | "push" :: _ ->
                                    incr pushes;
                                    (128, "", "lost acknowledgement")
                                | _ -> result);
                          }
                      in
                      let checkpoint =
                        Filename.temp_file "push-destination" ".json"
                      in
                      Fun.protect
                        ~finally:(fun () -> Sys.remove checkpoint)
                        (fun () ->
                          let outcome =
                            run runtime (persist checkpoint) lost_ack
                              (B.Request
                                 {
                                   intent with
                                   purpose = B.Publish_session "destination";
                                 })
                          in
                          if multiple then (
                            check
                              "ambiguous destinations request diagnosis before \
                               publication"
                              ((match outcome with
                                 | R.Repair_needed _ -> true
                                 | R.Idle | R.Waiting | R.Intervention _
                                 | R.Checkpoint_failed _ ->
                                     false)
                              && !pushes = 0);
                            check "ambiguous publication retains local work"
                              (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                              = source))
                          else (
                            check
                              "fetch endpoint equality cannot confirm the push \
                               destination"
                              (outcome = R.Waiting && !pushes = 1);
                            check
                              "explicit absent lease publishes to the actual \
                               push URL"
                              (Git.git_capture ~cwd:push_remote
                                 [ "rev-parse"; "patch" ]
                              = source);
                            let runtime =
                              Runtime.create ~gameplan
                                ~main_branch:(Types.Branch.of_string "main")
                                ~snapshot:
                                  (get (Persistence.load ~path:checkpoint))
                                ()
                            in
                            check
                              "restart confirms actual destination without \
                               repeating push"
                              (run runtime (persist checkpoint) lost_ack
                                 B.Recover
                               = R.Idle
                              && !pushes = 1)))))))
        [ false; true ];
      ())

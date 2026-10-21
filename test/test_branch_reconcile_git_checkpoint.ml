(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
open Onton_test_support.Branch_reconcile_git_fixture

let () =
  timed ~name:"checkpoint-contracts" (fun () ->
      Eio_main.run @@ fun env ->
      let make_io dir =
        E.make_io
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~path:dir
      in
      Git.with_temp_repo @@ fun remote ->
      let base = commit remote "file" "base\n" "base" in
      Git.with_temp_repo @@ fun dir ->
      prepare remote dir;
      let work = commit dir "local" "work\n" "work" in
      let snap = Filename.temp_file "branch-checkpoint" ".json" in
      Fun.protect
        ~finally:(fun () -> Sys.remove snap)
        (fun () ->
          let runtime = make_runtime () in
          let io = make_io dir in
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
          check "materialization checkpoint"
            (run runtime (persist snap) io
               (B.Materialized (B.New_branch (sha base)))
            = R.Idle);
          check "lost push acknowledgement waits"
            (run runtime (persist snap) lost_ack (B.Request intent) = R.Waiting);
          check "remote published captured work"
            (Git.git_capture ~cwd:remote [ "rev-parse"; "refs/heads/patch" ]
            = work);
          Git.run_git ~cwd:remote [ "checkout"; "-q"; "patch" ];
          let advanced =
            commit remote "other" "other work\n" "later remote work"
          in
          Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
          let restored = get (Persistence.load ~path:snap) in
          let runtime =
            Runtime.create ~gameplan
              ~main_branch:(Types.Branch.of_string "main")
              ~snapshot:restored ()
          in
          check "recover successful publication"
            (run runtime (persist snap) lost_ack B.Recover = R.Idle);
          check "lost acknowledgement does not push again" (!pushes = 1);
          check "remote advancement does not mutate the captured checkout"
            (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ] = work);
          check "confirmation retains newer remote work"
            (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ] = advanced);
          check "settled recovery" (B.phase (state runtime) = Some B.Settled);
          check "pinned source retained"
            (Git.git_capture ~cwd:dir [ "rev-parse"; prefix ^ "/1/2/source" ]
            = work));
      (* The repair agent completes both conflicting replay steps before reporting success. *)
      Git.with_temp_repo @@ fun repair_dir ->
      prepare remote repair_dir;
      Git.run_git ~cwd:repair_dir [ "reset"; "--hard"; base ];
      ignore (commit repair_dir "file" "local one\n" "one");
      ignore (commit repair_dir "file" "local two\n" "two");
      ignore (commit remote "file" "upstream\n" "base advanced");
      Git.run_git ~cwd:remote [ "branch"; "-D"; "patch" ];
      let snap = Filename.temp_file "branch-repair" ".json" in
      Fun.protect
        ~finally:(fun () -> Sys.remove snap)
        (fun () ->
          let runtime = make_runtime () and io = make_io repair_dir in
          ignore
            (run runtime (persist snap) io
               (B.Materialized (B.New_branch (sha base))));
          let first = run runtime (persist snap) io (B.Request intent) in
          let repair token content =
            let turn =
              match B.repair_turn (state runtime) ~branch:"patch" token with
              | Some prompt -> prompt
              | None -> failwith "repair was not claimed"
            in
            let backend =
              Llm_backend.
                {
                  name = "staged-repair";
                  run_streaming =
                    (fun ~project_name:_
                      ~cwd:_
                      ~patch_id:_
                      ~prompt:_
                      ~resume_session
                      ~session_uuid:_
                      ~complexity:_
                      ~on_event:_
                    ->
                      check "repair resumes the implementation session"
                        (resume_session = Some "patch-session");
                      let checkpoint = get (Persistence.load ~path:snap) in
                      let checkpoint_state =
                        (Orchestrator.agent checkpoint.orchestrator id)
                          .Patch_agent.branch_reconcile
                      in
                      check "repair claim persisted before backend invocation"
                        (Option.is_some
                           (B.repair_turn checkpoint_state ~branch:"patch" token));
                      let oc = open_out (Filename.concat repair_dir "file") in
                      output_string oc content;
                      close_out oc;
                      Git.run_git ~cwd:repair_dir [ "add"; "file" ];
                      ignore
                        (io.git
                           [ "-c"; "core.editor=true"; "rebase"; "--continue" ]);
                      let oc = open_out (Filename.concat repair_dir "file") in
                      output_string oc "resolved two\n";
                      close_out oc;
                      Git.run_git ~cwd:repair_dir [ "add"; "file" ];
                      Git.run_git ~cwd:repair_dir
                        [ "-c"; "core.editor=true"; "rebase"; "--continue" ];
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
            let result =
              Branch_repair_session.run ~resume_session:(Some "patch-session")
                ~session_uuid:"patch-session"
                ~on_event:(fun _ -> ())
                ~context:"" ~guidance:[] ~backend
                ~cwd:Eio.Path.(Eio.Stdenv.fs env / repair_dir)
                ~project_name:"demo" ~patch_id:id ~complexity:None ~turn
                ~read_head:(fun () ->
                  Some (Git.git_capture ~cwd:repair_dir [ "rev-parse"; "HEAD" ]))
                ~now:(fun () -> 100.)
            in
            let before = E.observe_checkout io in
            check "agent finishes the sequencer before successful completion"
              (Git_observation.equal_sequencer before.sequencer
                 Git_observation.None_active);
            run runtime (persist snap) io result
          in
          let second =
            match first with
            | R.Repair_needed token -> repair token "resolved one\n"
            | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _ ->
                failwith "expected first repair"
          in
          let final =
            match second with
            | R.Repair_needed token -> repair token "resolved two\n"
            | R.Idle -> R.Idle
            | R.Waiting | R.Intervention _ | R.Checkpoint_failed _ ->
                failwith "unexpected repair outcome"
          in
          check "repair published"
            (final = R.Idle && B.phase (state runtime) = Some B.Settled);
          check "remote contains repaired tree"
            (Git.git_capture ~cwd:remote [ "show"; "patch:file" ]
            = "resolved two");
          check "sequencer gone"
            (Git_observation.equal_sequencer (E.observe_checkout io).sequencer
               Git_observation.None_active));
      Git.run_git ~cwd:remote [ "branch"; "-D"; "patch" ];
      Git.with_temp_repo @@ fun race_dir ->
      prepare remote race_dir;
      let candidate =
        commit race_dir "candidate-file" "candidate\n" "candidate"
      in
      let io = make_io race_dir in
      let raced = ref false and pushes = ref 0 and external_sha = ref "" in
      let race_io =
        E.
          {
            git =
              (fun args ->
                (match args with
                | "push" :: _ ->
                    incr pushes;
                    if not !raced then (
                      raced := true;
                      Git.run_git ~cwd:remote
                        [ "checkout"; "-q"; "-b"; "external"; "main" ];
                      external_sha :=
                        commit remote "remote-file" "remote work\n"
                          "external work";
                      Git.run_git ~cwd:remote
                        [ "update-ref"; "refs/heads/patch"; !external_sha ])
                | _ -> ());
                io.git args);
          }
      in
      let runtime = make_runtime () in
      let snap = Filename.temp_file "branch-race" ".json" in
      Fun.protect
        ~finally:(fun () -> Sys.remove snap)
        (fun () ->
          let base =
            Git.git_capture ~cwd:race_dir [ "rev-parse"; "origin/main" ] |> sha
          in
          ignore
            (run runtime (persist snap) race_io
               (B.Materialized (B.New_branch base)));
          check "initial lease race retains candidate"
            (run runtime (persist snap) race_io
               (B.Request
                  {
                    intent with
                    policy = Preserve_ancestry;
                    purpose = Publish_session "race-session";
                  })
            = R.Waiting);
          (try
             ignore
               (R.run ~runtime ~persist:(persist snap) ~patch_id:id
                  ~now:(fun () -> 1000.)
                  ~execute:(fun ~operation command ->
                    match command.B.kind with
                    | B.Integrate _ when operation.B.id = 2 ->
                        raise Simulated_stop
                    | B.Commit_merge _ | B.Plan_remote_replay _
                    | B.Checkout_remote _ | B.Observe | B.Pin _ | B.Integrate _
                    | B.Inspect | B.Verify_recovery | B.Continue _ | B.Publish _
                    | B.Confirm _ ->
                        execute race_io ~operation command)
                  (B.Tick 1000.));
             failwith "expected interruption before remote integration"
           with Simulated_stop -> ());
          let snapshot = get (Persistence.load ~path:snap) in
          let restarted =
            Runtime.create ~gameplan
              ~main_branch:(Types.Branch.of_string "main")
              ~snapshot ()
          in
          check "restart integrates remote work before refreshed lease"
            (run restarted (persist snap) race_io B.Recover = R.Idle);
          check "exactly one rejected push and one recovered push" (!pushes = 2);
          check "published candidate preserved"
            (Git.git_exit_code ~cwd:remote
               [ "merge-base"; "--is-ancestor"; candidate; "patch" ]
            = 0);
          check "published external work preserved"
            (Git.git_exit_code ~cwd:remote
               [ "merge-base"; "--is-ancestor"; !external_sha; "patch" ]
            = 0);
          check "publication race has successor identity"
            (match B.operation (state runtime) with
            | Some op -> op.id = 2
            | None -> false));
      Git.with_temp_repo (fun materialized_dir ->
          let initial =
            commit materialized_dir "initial" "initial\n" "initial"
          in
          let io = make_io materialized_dir in
          let first =
            get
              (E.record_materialization ~io ~prefix ~branch:"main"
                 ~new_branch_from:(Some (sha initial)))
          in
          ignore (commit materialized_dir "later" "later\n" "later work");
          let repeated =
            get
              (E.record_materialization ~io ~prefix ~branch:"main"
                 ~new_branch_from:(Some (sha initial)))
          in
          check "materialization survives later local commits" (first = repeated);
          check "new branch has exact replay boundary"
            (match first with
            | Some receipt ->
                B.materialization_head receipt = sha initial
                && B.materialization_boundary receipt = Some (sha initial)
            | None -> false);
          let adopted =
            get
              (E.record_materialization ~io ~prefix:(prefix ^ "-adopted")
                 ~branch:"main" ~new_branch_from:None)
          in
          check "adopting existing work does not claim a replay boundary"
            (match adopted with
            | Some receipt -> B.materialization_boundary receipt = None
            | None -> false);
          check "failed provenance probe differs from absent evidence"
            (Result.is_error
               (E.materialization
                  ~io:{ git = (fun _ -> (128, "", "probe failed")) }
                  ~prefix)));
      (* Failed durable writes never authorize Git. *)
      let runtime = make_runtime () in
      let executions = ref 0 in
      let outcome =
        R.run ~runtime
          ~persist:(fun _ -> Error "disk unavailable")
          ~patch_id:id
          ~now:(fun () -> 0.)
          ~execute:(fun ~operation:_ _ ->
            incr executions;
            B.Pinned)
          (B.Request intent)
      in
      check "failed checkpoint blocks command"
        (outcome = R.Checkpoint_failed "disk unavailable" && !executions = 0);
      check "failed checkpoint leaves runtime unchanged"
        (B.equal (state runtime) B.empty);
      (* A completed implementation is published as captured even when the base
     advanced. This exercises the production Worktree capability, including its
     private recovery namespace, under an already-held session write lock. *)
      iter_cases ~name:"checkpoint-contracts-1"
        (fun unknown_revision ->
          Git.with_temp_repo (fun publish_remote ->
              ignore (commit publish_remote "base" "base\n" "base");
              Git.with_temp_repo (fun publish_dir ->
                  prepare publish_remote publish_dir;
                  let captured =
                    commit publish_dir "work" "work\n" "implementation"
                  in
                  let newer_base =
                    commit publish_remote "new-base" "new base\n"
                      "base advanced"
                  in
                  if unknown_revision then (
                    let oc =
                      open_out_bin (Filename.concat publish_dir "unfinished")
                    in
                    output_string oc
                      "staged work from the interrupted session\n";
                    close_out oc;
                    Git.run_git ~cwd:publish_dir [ "add"; "unfinished" ]);
                  let unfinished_index =
                    Git.git_capture ~cwd:publish_dir [ "write-tree" ]
                  in
                  let module W =
                    (val Worktree.make ~config:Worktree_lifecycle.git
                           ~fs:(Eio.Stdenv.fs env) ~clock:(Eio.Stdenv.clock env)
                           ~process_mgr:(Eio.Stdenv.process_mgr env)
                           ~repo_root:publish_dir)
                  in
                  let runtime = make_runtime () in
                  let checkpoint =
                    Filename.temp_file "branch-publication" ".json"
                  in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      let escaped = ref None in
                      Runtime.with_patch_ownership runtime ~patch_id:id
                        (fun owner ->
                          escaped := Some owner;
                          let result =
                            R.run_owned ~owner ~persist:(persist checkpoint)
                              ~now:(fun () -> 100.)
                              ~execute:
                                (W.reconcile ~path:publish_dir
                                   ~project_name:
                                     "project with punctuation: [test]"
                                   ~branch:(Types.Branch.of_string "patch"))
                              (B.Request
                                 {
                                   intent with
                                   purpose =
                                     (if unknown_revision then
                                        Publish_session "interrupted-session"
                                      else Publish_revision (sha captured));
                                 })
                          in
                          check "captured session revision published"
                            (result = R.Idle));
                      check "publication retains implementation commit identity"
                        (Git.git_capture ~cwd:publish_remote
                           [ "rev-parse"; "patch" ]
                        = captured);
                      check "publication preserves the staged index"
                        (Git.git_capture ~cwd:publish_dir [ "write-tree" ]
                        = unfinished_index);
                      check "publication does not fold in a moving base"
                        (Git.git_exit_code ~cwd:publish_dir
                           [
                             "merge-base"; "--is-ancestor"; newer_base; "patch";
                           ]
                        = 1);
                      check "expired write ownership is rejected"
                        (match !escaped with
                        | None -> false
                        | Some owner -> (
                            try
                              Runtime.with_owned_patch owner (fun _ _ -> false)
                            with Invalid_argument _ -> true))))))
        [ false; true ];
      ())

(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
open Onton_test_support.Branch_reconcile_git_fixture

let () =
  timed ~name:"repair-restart" (fun () ->
      Eio_main.run @@ fun env ->
      let make_io dir =
        E.make_io
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~path:dir
      in
      (* Recovery owns and completes uncommitted work, including edits introduced
     while its turn waited for capacity. *)
      iter_cases ~name:"repair-restart-1"
        (fun during_wait ->
          Git.with_temp_repo (fun remote ->
              let base = commit remote "base" "base\n" "base" in
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  ignore (commit dir "patch" "patch\n" "patch work");
                  ignore
                    (commit remote "upstream" "upstream\n" "base advancement");
                  let runtime = make_runtime () and io = make_io dir in
                  let checkpoint =
                    Filename.temp_file "dirty-history-recovery" ".json"
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
                                      (commit dir "extra" "external commit\n"
                                         "external work");
                                    raise Simulated_stop
                                | B.Observe | B.Pin _ | B.Inspect
                                | B.Verify_recovery | B.Continue _
                                | B.Commit_merge _ | B.Plan_remote_replay _
                                | B.Checkout_remote _ | B.Publish _
                                | B.Confirm _ ->
                                    execute io ~operation command)
                              (B.Request intent));
                         failwith "expected interruption"
                       with Simulated_stop -> ());
                      let head =
                        Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                      in
                      let dirty () =
                        let write name bytes =
                          let oc = open_out_bin (Filename.concat dir name) in
                          Fun.protect
                            ~finally:(fun () -> close_out oc)
                            (fun () -> output_string oc bytes)
                        in
                        write "extra" "staged external work\n";
                        Git.run_git ~cwd:dir [ "add"; "extra" ];
                        write "extra" "unstaged external work\n";
                        write "untracked" "untracked external work\n"
                      in
                      let read name =
                        let ic = open_in_bin (Filename.concat dir name) in
                        Fun.protect
                          ~finally:(fun () -> close_in ic)
                          (fun () ->
                            really_input_string ic (in_channel_length ic))
                      in
                      if not during_wait then dirty ();
                      let token =
                        match run runtime (persist checkpoint) io B.Recover with
                        | R.Repair_needed token -> token
                        | R.Idle | R.Waiting | R.Intervention _
                        | R.Checkpoint_failed _ ->
                            failwith "expected recovery turn"
                      in
                      let calls = ref 0 in
                      let outcome =
                        R.run_repair ~runtime ~persist:(persist checkpoint)
                          ~patch_id:id
                          ~with_capacity:(fun f ->
                            if during_wait then dirty ();
                            f ())
                          ~now:(fun () -> 100.)
                          ~execute:(fun ~agent:_ ~operation command ->
                            execute io ~operation command)
                          ~perform:(fun ~agent:_ ~turn ->
                            incr calls;
                            check "staged bytes reach the recovery agent"
                              (Git.git_capture ~cwd:dir [ "show"; ":extra" ]
                              = "staged external work");
                            check "unstaged bytes reach the recovery agent"
                              (read "extra" = "unstaged external work\n");
                            check "untracked bytes reach the recovery agent"
                              (read "untracked" = "untracked external work\n");
                            Git.run_git ~cwd:dir [ "add"; "extra"; "untracked" ];
                            Git.run_git ~cwd:dir
                              [ "commit"; "-qm"; "Finish interrupted changes" ];
                            Git.run_git ~cwd:dir
                              [ "merge"; "--no-edit"; "origin/main" ];
                            B.Repair_completed
                              { token = turn.B.token; at = 100. })
                          token
                      in
                      check
                        "dirty recovery completes through one owned agent turn"
                        (outcome = R.Idle && !calls = 1);
                      check "external commit remains pinned"
                        (Base.String.is_substring
                           (Git.git_capture ~cwd:dir
                              [
                                "for-each-ref"; "--format=%(objectname)"; prefix;
                              ])
                           ~substring:head);
                      List.iter
                        (fun (file, bytes) ->
                          check
                            ("recovered publication retains " ^ file)
                            (Git.git_capture ~cwd:remote
                               [ "show"; "patch:" ^ file ]
                            = bytes))
                        [
                          ("extra", "unstaged external work");
                          ("untracked", "untracked external work");
                          ("patch", "patch");
                          ("upstream", "upstream");
                        ]))))
        [ false; true ];
      (* A repair's expected detached HEAD is durable. Losing the post-agent
     probe cannot turn an unexpected agent commit into authorized continuation. *)
      iter_cases ~name:"repair-restart-2"
        (fun mutate_before ->
          Git.with_temp_repo (fun remote ->
              let base = commit remote "file" "base\n" "base" in
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  ignore (commit dir "file" "patch\n" "patch");
                  ignore (commit remote "file" "upstream\n" "upstream");
                  let io = make_io dir and runtime = make_runtime () in
                  let checkpoint =
                    Filename.temp_file "repair-authority" ".json"
                  in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      ignore
                        (run runtime (persist checkpoint) io
                           (B.Materialized (B.New_branch (sha base))));
                      let token =
                        match
                          run runtime (persist checkpoint) io (B.Request intent)
                        with
                        | R.Repair_needed token -> token
                        | R.Idle | R.Waiting | R.Intervention _
                        | R.Checkpoint_failed _ ->
                            failwith "expected a claimed repair"
                      in
                      let turn =
                        Option.get
                          (B.repair_turn (state runtime) ~branch:"patch" token)
                      in
                      let calls = ref 0 in
                      let unexpected_commit () =
                        ignore
                          (commit dir "file" "agent resolution\n"
                             "unexpected agent commit")
                      in
                      if mutate_before then unexpected_commit ();
                      let backend =
                        Llm_backend.
                          {
                            name = "unexpected-commit";
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
                                unexpected_commit ();
                                ignore
                                  (io.git
                                     [
                                       "-c";
                                       "core.editor=true";
                                       "rebase";
                                       "--continue";
                                     ]);
                                if
                                  not
                                    (Git_observation.equal_sequencer
                                       (E.observe_checkout io).sequencer
                                       Git_observation.None_active)
                                then Git.run_git ~cwd:dir [ "rebase"; "--skip" ];
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
                      let event =
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
                            if (not mutate_before) && !probes = 2 then None
                            else
                              Some
                                (Git.git_capture ~cwd:dir
                                   [ "rev-parse"; "HEAD" ]))
                          ~now:(fun () -> 100.)
                      in
                      let outcome = run runtime (persist checkpoint) io event in
                      check
                        "repair preflight rejects a changed HEAD before \
                         backend dispatch"
                        (!calls = if mutate_before then 0 else 1);
                      let unexpected_head =
                        Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                      in
                      check
                        "completion capture trusts a finished repair while \
                         preserving preflight fencing"
                        (if mutate_before then
                           match outcome with
                           | R.Repair_needed _ -> true
                           | R.Idle | R.Waiting | R.Intervention _
                           | R.Checkpoint_failed _ ->
                               false
                         else outcome = R.Idle);
                      check "agent work remains recoverable"
                        (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                        = unexpected_head);
                      check "preflight rejection cannot publish"
                        (if mutate_before then
                           Git.git_exit_code ~cwd:remote
                             [ "show-ref"; "--verify"; "refs/heads/patch" ]
                           <> 0
                         else
                           Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                           = unexpected_head)))))
        [ false; true ];
      (* PR #483: a repair agent has published the candidate, while the local
     tracking ref still names the old tip and Onton's lease has been rejected.
     Only a direct observation can settle publication. An unavailable probe
     must retain the candidate without charging a content-repair attempt. *)
      iter_cases ~name:"repair-restart-3"
        (fun fail_confirmation ->
          Git.with_temp_repo (fun published_remote ->
              ignore (commit published_remote "seed" "seed\n" "seed");
              Git.with_temp_repo (fun published_dir ->
                  prepare published_remote published_dir;
                  let old = commit published_dir "work" "first\n" "first" in
                  Git.run_git ~cwd:published_dir
                    [ "push"; "-q"; "origin"; "patch" ];
                  let candidate =
                    commit published_dir "work" "resolved\n" "resolved"
                  in
                  let io = make_io published_dir in
                  let attempts = ref 0 and probe_unavailable = ref false in
                  let published_by_agent =
                    E.
                      {
                        git =
                          (fun args ->
                            match args with
                            | "push" :: _ ->
                                incr attempts;
                                Git.run_git ~cwd:published_dir
                                  [
                                    "push";
                                    "-q";
                                    "origin";
                                    candidate ^ ":refs/heads/patch";
                                  ];
                                Git.run_git ~cwd:published_dir
                                  [
                                    "update-ref";
                                    "refs/remotes/origin/patch";
                                    old;
                                  ];
                                ( 1,
                                  "!\tHEAD:refs/heads/patch\t[rejected] (stale \
                                   info)\n",
                                  "failed to push: stale lease" )
                            | "ls-remote" :: _ when !probe_unavailable ->
                                (128, "", "remote unavailable")
                            | _ -> io.git args);
                      }
                  in
                  let runtime = make_runtime () in
                  let checkpoint =
                    Filename.temp_file "branch-agent-published" ".json"
                  in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      check
                        "agent publication produces a retryable lease failure"
                        (run runtime (persist checkpoint) published_by_agent
                           (B.Request
                              {
                                intent with
                                purpose = Publish_revision (sha candidate);
                              })
                        = R.Waiting);
                      check "tracking ref remains stale"
                        (Git.git_capture ~cwd:published_dir
                           [ "rev-parse"; "origin/patch" ]
                        = old);
                      let snapshot = get (Persistence.load ~path:checkpoint) in
                      let runtime =
                        Runtime.create ~gameplan
                          ~main_branch:(Types.Branch.of_string "main")
                          ~snapshot ()
                      in
                      probe_unavailable := fail_confirmation;
                      if fail_confirmation then (
                        check
                          "failed remote probe does not claim publication \
                           success"
                          (run runtime (persist checkpoint) published_by_agent
                             B.Recover
                          = R.Waiting);
                        check
                          "failed remote probe retains candidate and repair \
                           budget"
                          (match B.operation (state runtime) with
                          | Some op ->
                              op.candidate = Some (sha candidate)
                              && op.repair = None
                          | None -> false);
                        probe_unavailable := false);
                      check
                        "direct remote confirmation recognizes agent \
                         publication"
                        (run runtime (persist checkpoint) published_by_agent
                           B.Recover
                        = R.Idle);
                      check "confirmation does not repeat push or integration"
                        (!attempts = 1
                        && Git.git_capture ~cwd:published_dir
                             [ "rev-parse"; "HEAD" ]
                           = candidate
                        && B.phase (state runtime) = Some B.Settled);
                      check "remote contains the agent's resolution"
                        (Git.git_capture ~cwd:published_remote
                           [ "show"; "patch:work" ]
                        = "resolved")))))
        [ false; true ];
      ())

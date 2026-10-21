(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
open Onton_test_support.Branch_reconcile_git_fixture

let () =
  timed ~name:"transport-legacy" (fun () ->
      Eio_main.run @@ fun env ->
      let make_io dir =
        E.make_io
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~path:dir
      in
      iter_cases ~name:"transport-legacy-1"
        (fun permission ->
          Git.with_temp_repo (fun remote ->
              ignore (commit remote "base" "base\n" "base");
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  let source =
                    commit dir "patch" "retained work\n" "patch work"
                  in
                  let io = make_io dir and runtime = make_runtime () in
                  let unavailable =
                    E.
                      {
                        git =
                          (fun args ->
                            match args with
                            | "push" :: _ ->
                                ( 128,
                                  "",
                                  if permission then
                                    "remote: Write access to repository not \
                                     granted."
                                  else "transport unavailable" )
                            | _ -> io.git args);
                      }
                  in
                  let checkpoint =
                    Filename.temp_file "publication-availability" ".json"
                  in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove checkpoint)
                    (fun () ->
                      let outcome =
                        run runtime (persist checkpoint) unavailable
                          (B.Request
                             {
                               intent with
                               purpose = B.Publish_session "completed-session";
                             })
                      in
                      let captured = Option.get (B.operation (state runtime)) in
                      check "publication failures retain the captured candidate"
                        (captured.candidate = Some (sha source)
                        && (permission || captured.repair = None));
                      check
                        "publication failure does not increment session \
                         failure accounting"
                        (Runtime.read runtime (fun snap ->
                             let agent =
                               Orchestrator.agent snap.Runtime.orchestrator id
                             in
                             agent.Patch_agent.start_attempts_without_pr = 0
                             && agent.session_completion = None));
                      if permission then
                        let turns = ref 0 in
                        let rec assist remaining outcome =
                          match outcome with
                          | R.Repair_needed token when remaining > 0 ->
                              let outcome =
                                R.run_repair ~runtime
                                  ~persist:(persist checkpoint) ~patch_id:id
                                  ~with_capacity:(fun f -> f ())
                                  ~now:(fun () -> 100.)
                                  ~execute:(fun ~agent:_ -> execute unavailable)
                                  ~perform:(fun ~agent:_ ~turn ->
                                    incr turns;
                                    check
                                      "permission recovery prohibits \
                                       credential changes"
                                      (Base.String.is_substring turn.B.prompt
                                         ~substring:"change credentials");
                                    B.Repair_completed
                                      { token = turn.token; at = 100. })
                                  token
                              in
                              assist (remaining - 1) outcome
                          | R.Idle | R.Waiting | R.Intervention _
                          | R.Checkpoint_failed _ | R.Repair_needed _ ->
                              outcome
                        in
                        check "write denial holds only after two agent turns"
                          ((match assist 3 outcome with
                             | R.Intervention _ -> true
                             | R.Idle | R.Waiting | R.Checkpoint_failed _
                             | R.Repair_needed _ ->
                                 false)
                          && !turns = 2)
                      else (
                        check
                          "transport failure leaves durable publication pending"
                          (outcome = R.Waiting);
                        let moved = dir ^ "-temporarily-unavailable" in
                        Sys.rename dir moved;
                        Fun.protect
                          ~finally:(fun () -> Sys.rename moved dir)
                          (fun () ->
                            check "first unavailable checkout probe waits"
                              (run runtime (persist checkpoint) io (B.Tick 105.)
                              = R.Waiting);
                            check
                              "repeated missing checkout requests diagnosis \
                               without recreating it"
                              ((match
                                  run runtime (persist checkpoint) io
                                    (B.Tick 115.)
                                with
                                 | R.Repair_needed _ -> true
                                 | R.Idle | R.Waiting | R.Intervention _
                                 | R.Checkpoint_failed _ ->
                                     false)
                              && not (Sys.file_exists dir));
                            let op = Option.get (B.operation (state runtime)) in
                            check
                              "missing checkout retains immutable operation \
                               captures"
                              (op.id = captured.id
                              && op.candidate = captured.candidate
                              && op.source = captured.source
                              && op.target = captured.target));
                        check
                          "restored checkout resumes publication without \
                           implementation"
                          (run runtime (persist checkpoint) io B.Recover
                          = R.Idle);
                        check "resumed publication retains exact work"
                          (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                          = source))))))
        [ false; true ];
      Git.with_temp_repo (fun remote ->
          let base = commit remote "file" "base\n" "base" in
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              let source = commit dir "file" "local\n" "patch work" in
              Git.run_git ~cwd:dir [ "push"; "origin"; "HEAD:refs/heads/patch" ];
              let target = commit remote "file" "upstream\n" "upstream work" in
              Git.run_git ~cwd:dir [ "fetch"; "origin" ];
              let builds = ref (Ok []) in
              let main = Types.Branch.of_string "main"
              and branch = Types.Branch.of_string "patch" in
              let pr = Types.Pr_number.of_int 1 in
              let module Forge =
                (val Sourcehut.make_with_builds
                       ~read_builds:(fun () -> !builds)
                       ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
                       ~process_mgr:(Eio.Stdenv.process_mgr env)
                       ~token:"fixture" ~owner:"test" ~repo:"fixture"
                       ~repo_root:dir ~main_branch:main
                       ~changes:[ (Some pr, branch, main) ])
              in
              let observe () =
                let request =
                  Forge_observation.{ id = "sourcehut-fixture"; pr_number = pr }
                in
                let observation = Forge_poll.read (module Forge) request in
                match
                  Forge_observation.validate ~expected:request observation
                with
                | Ok (Poll_outcome.Ok_pr_state state) -> state
                | Ok
                    ( Poll_outcome.Transport_failed _ | Poll_outcome.Timed_out _
                    | Poll_outcome.Http_failed _ | Poll_outcome.Graphql_failed _
                    | Poll_outcome.Json_parse_failed _ ) ->
                    failwith "SourceHut poll failed"
                | Error error -> failwith error
              in
              let before = observe () in
              check "SourceHut conflict identifies exact Git head and base"
                (before.Pr_state.pr_number = Some pr
                && before.head_oid = Some source
                && before.base_oid = Some target
                && before.base_branch = Some main
                && before.merge_state = Pr_state.Conflicting);
              let runtime = make_runtime () and io = make_io dir in
              let checkpoint =
                Filename.temp_file "sourcehut-reconciliation" ".json"
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
                        failwith "SourceHut conflict requires repair"
                  in
                  let oc = open_out (Filename.concat dir "file") in
                  output_string oc "resolved\n";
                  close_out oc;
                  Git.run_git ~cwd:dir [ "add"; "file" ];
                  Git.run_git ~cwd:dir
                    [ "-c"; "core.editor=true"; "rebase"; "--continue" ];
                  check
                    "SourceHut complete repair publishes through common owner"
                    (run runtime (persist checkpoint) io
                       (B.Repair_completed { token; at = 100. })
                    = R.Idle);
                  Git.run_git ~cwd:dir [ "fetch"; "origin" ];
                  let published =
                    Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                  in
                  let after = observe () in
                  check "SourceHut observes exact repaired publication"
                    (after.Pr_state.head_oid = Some published
                    && after.base_oid = Some target
                    && after.merge_state = Pr_state.Mergeable
                    && Git.git_capture ~cwd:remote [ "show"; "patch:file" ]
                       = "resolved");
                  builds :=
                    Error
                      (Sourcehut.Transport_error "build service unavailable");
                  check
                    "SourceHut build probe failure stays distinct from a \
                     negative observation"
                    (match Forge.pr_state pr with
                    | Error (Sourcehut.Transport_error _) -> true
                    | Error
                        ( Sourcehut.Http_error _ | Sourcehut.Api_error _
                        | Sourcehut.Timeout _ | Sourcehut.Git_error _
                        | Sourcehut.Unsupported _ )
                    | Ok _ ->
                        false))));
      Git.with_temp_repo (fun remote ->
          let original = commit remote "base" "base\n" "base" in
          Git.run_git ~cwd:remote [ "branch"; "patch" ];
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              Git.run_git ~cwd:remote [ "checkout"; "-q"; "patch" ];
              let advanced =
                commit remote "remote" "remote\n" "remote advancement"
              in
              let observe () = E.remote_head (make_io dir) ~branch:"patch" in
              check
                "direct remote probe sees advancement without tracking update"
                (observe () = Ok (Some (sha advanced)));
              Git.run_git ~cwd:remote
                [ "update-ref"; "refs/heads/patch"; original ];
              check "direct remote probe accepts head reversion"
                (observe () = Ok (Some (sha original)));
              check "direct remote probe leaves tracking refs untouched"
                (Git.git_capture ~cwd:dir [ "rev-parse"; "origin/patch" ]
                = original);
              check "missing remote ref is a negative observation"
                (E.remote_head (make_io dir) ~branch:"absent" = Ok None);
              check "failed remote probe is not a negative observation"
                (match
                   E.remote_head
                     E.{ git = (fun _ -> (128, "", "transport unavailable")) }
                     ~branch:"patch"
                 with
                | Error _ -> true
                | Ok _ -> false)));
      iter_cases ~name:"transport-legacy-2"
        (fun (dirty, active) ->
          Git.with_temp_repo (fun remote ->
              ignore (commit remote "base" "base\n" "base");
              Git.with_temp_repo (fun dir ->
                  prepare remote dir;
                  let revision =
                    commit dir "work" "committed\n" "legacy patch"
                  in
                  let revision =
                    if active then (
                      let revision =
                        commit dir "base" "local base edit\n" "legacy conflict"
                      in
                      ignore
                        (commit remote "base" "remote base edit\n" "base moved");
                      Git.run_git ~cwd:dir [ "fetch"; "origin" ];
                      check "legacy rebase fixture conflicts"
                        (Git.git_exit_code ~cwd:dir [ "rebase"; "origin/main" ]
                        <> 0);
                      revision)
                    else revision
                  in
                  let initial_checkout = E.observe_checkout (make_io dir) in
                  if dirty then (
                    let write name text =
                      let oc = open_out_bin (Filename.concat dir name) in
                      output_string oc text;
                      close_out oc
                    in
                    write "work" "staged\n";
                    Git.run_git ~cwd:dir [ "add"; "work" ];
                    write "work" "unstaged\n";
                    write "untracked" "untracked\n");
                  let before =
                    Git.git_capture ~cwd:dir [ "status"; "--porcelain=v1" ]
                  in
                  let runtime = make_runtime () in
                  Runtime.update_orchestrator runtime (fun orch ->
                      Orchestrator.set_pr_number orch id
                        (Types.Pr_number.of_int 12));
                  let json =
                    Runtime.read runtime Persistence.snapshot_to_yojson
                  in
                  let rec legacy = function
                    | `Assoc fields ->
                        `Assoc
                          (List.filter_map
                             (fun (key, value) ->
                               if key = "branch_reconcile" then None
                               else
                                 Some
                                   ( key,
                                     if key = "version" then `Int 1
                                     else legacy value ))
                             fields)
                    | `List values -> `List (List.map legacy values)
                    | value -> value
                  in
                  let snapshot =
                    get (Persistence.snapshot_of_yojson (legacy json))
                  in
                  let restored =
                    Runtime.create ~gameplan
                      ~main_branch:(Types.Branch.of_string "main")
                      ~snapshot ()
                  in
                  let path = Filename.temp_file "legacy-publication" ".json" in
                  Fun.protect
                    ~finally:(fun () -> Sys.remove path)
                    (fun () ->
                      let outcome =
                        run restored (persist path) (make_io dir) B.Recover
                      in
                      check
                        "legacy unfinished work settles or offers owned \
                         recovery"
                        (if active then
                           match outcome with
                           | R.Repair_needed _ -> true
                           | R.Idle | R.Waiting | R.Intervention _
                           | R.Checkpoint_failed _ ->
                               false
                         else outcome = R.Idle);
                      if active then (
                        check "legacy active integration cannot publish"
                          (Git.git_exit_code ~cwd:remote
                             [ "show-ref"; "--verify"; "refs/heads/patch" ]
                          <> 0);
                        check "legacy sequencer remains intact"
                          (Git_observation.equal_sequencer
                             initial_checkout.sequencer
                             (E.observe_checkout (make_io dir)).sequencer);
                        check "legacy source still exists"
                          (Git.git_capture ~cwd:dir
                             [ "rev-parse"; "refs/heads/patch" ]
                          = revision))
                      else
                        check
                          "legacy publication captures actual branch revision"
                          (Git.git_capture ~cwd:remote
                             [ "rev-parse"; "refs/heads/patch" ]
                          = revision);
                      check "legacy dirty index and worktree remain untouched"
                        (Git.git_capture ~cwd:dir [ "status"; "--porcelain=v1" ]
                        = before);
                      if dirty then (
                        check "staged content remains staged"
                          (Git.git_capture ~cwd:dir [ "show"; ":work" ]
                          = "staged");
                        let read name =
                          let ic = open_in_bin (Filename.concat dir name) in
                          Fun.protect
                            ~finally:(fun () -> close_in ic)
                            (fun () ->
                              really_input_string ic (in_channel_length ic))
                        in
                        check "unstaged content remains"
                          (read "work" = "unstaged\n");
                        check "untracked content remains"
                          (read "untracked" = "untracked\n"));
                      let checkpoint = get (Persistence.load ~path) in
                      let resumed =
                        Runtime.create ~gameplan
                          ~main_branch:(Types.Branch.of_string "main")
                          ~snapshot:checkpoint ()
                      in
                      let calls = ref 0 in
                      let outcome =
                        R.run ~runtime:resumed ~persist:(persist path)
                          ~patch_id:id
                          ~now:(fun () -> 200.)
                          ~execute:(fun ~operation command ->
                            incr calls;
                            execute (make_io dir) ~operation command)
                          B.Recover
                      in
                      check "migration restarts settled or reinspects recovery"
                        (if active then
                           !calls > 0
                           &&
                           match outcome with
                           | R.Repair_needed _ -> true
                           | R.Idle | R.Waiting | R.Intervention _
                           | R.Checkpoint_failed _ ->
                               false
                         else outcome = R.Idle && !calls = 0);
                      if active then (
                        let token =
                          match outcome with
                          | R.Repair_needed token -> token
                          | R.Idle | R.Waiting | R.Intervention _
                          | R.Checkpoint_failed _ ->
                              failwith "missing migrated recovery turn"
                        in
                        let outcome =
                          R.run_repair ~runtime:resumed ~persist:(persist path)
                            ~patch_id:id
                            ~now:(fun () -> 300.)
                            ~with_capacity:(fun f -> f ())
                            ~execute:(fun ~agent:_ ~operation command ->
                              execute (make_io dir) ~operation command)
                            ~perform:(fun ~agent:_ ~turn ->
                              Git.run_git ~cwd:dir [ "rebase"; "--abort" ];
                              ignore
                                (Git.git_exit_code ~cwd:dir
                                   [
                                     "merge";
                                     "--no-commit";
                                     initial_checkout.head;
                                   ]);
                              let oc =
                                open_out_bin (Filename.concat dir "base")
                              in
                              Fun.protect
                                ~finally:(fun () -> close_out oc)
                                (fun () ->
                                  output_string oc
                                    "local base edit\nremote base edit\n");
                              Git.run_git ~cwd:dir [ "add"; "base" ];
                              Git.run_git ~cwd:dir
                                [
                                  "commit";
                                  "-qm";
                                  "Recover retained legacy integration";
                                ];
                              B.Repair_completed
                                { token = turn.B.token; at = 301. })
                            token
                        in
                        check
                          "successful complete legacy repair publishes its HEAD"
                          (outcome = R.Idle);
                        check
                          "legacy repair publishes the exact agent candidate"
                          (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                          = Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]);
                        List.iter
                          (fun retained ->
                            check
                              "legacy recovery retains original and \
                               interrupted commits without importing them"
                              (List.mem (sha retained)
                                 (B.required_revisions (state resumed))))
                          [ revision; initial_checkout.head ])))))
        [ (false, false); (true, false); (false, true) ];
      ())

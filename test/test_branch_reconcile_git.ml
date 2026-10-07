(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
module B = Branch_reconcile
module E = Branch_reconcile_executor
module R = Branch_reconcile_runner

exception Simulated_stop

module Git = Onton_test_support.Git_env

let get = function Ok v -> v | Error e -> failwith e
let check msg b = if not b then failwith msg

let commit dir file content message =
  let oc = open_out_bin (Filename.concat dir file) in
  output_string oc content;
  close_out oc;
  Git.run_git ~cwd:dir [ "add"; file ];
  Git.run_git ~cwd:dir [ "commit"; "-q"; "-m"; message ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let sha text =
  match B.Commit.make text with
  | Some sha -> sha
  | None -> failwith "invalid fixture SHA"

let id = Types.Patch_id.of_string "1"

let gameplan =
  (get
     (Gameplan_parser.parse_string
        "projectName: demo\n\
         owner: owner\n\
         repo: repo\n\
         problemStatement: test\n\
         solutionSummary: test\n\
         patches:\n\
        \  - number: 1\n\
        \    title: patch\n\
        \    description: patch\n\
        \    dependsOn: []\n\
         dependencyGraph:\n\
        \  - patch: 1\n\
        \    dependsOn: []\n"))
    .Gameplan_parser.gameplan

let make_runtime () =
  Runtime.create ~gameplan ~main_branch:(Types.Branch.of_string "main") ()

let state runtime =
  Runtime.read runtime (fun s ->
      (Orchestrator.agent s.Runtime.orchestrator id)
        .Patch_agent.branch_reconcile)

let prefix = "refs/onton/reconcile/test/patch"
let intent = B.{ base = "main"; policy = Rewrite; purpose = Reconcile_base }

let execute io ~operation command =
  E.execute ~io ~prefix ~branch:"patch" ~operation command

let run runtime persist io event =
  R.run ~runtime ~persist ~patch_id:id
    ~now:(fun () -> 100.)
    ~execute:(execute io) event

let persist path = Persistence.save_snapshot ~path

let prepare remote dir =
  Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
  Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
  Git.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "patch"; "origin/main" ]

let () =
  Eio_main.run @@ fun env ->
  let make_io dir =
    E.make_io
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~clock:(Eio.Stdenv.clock env) ~path:dir
  in
  Git.with_temp_repo (fun original_remote ->
      ignore (commit original_remote "base" "base\n" "base");
      Git.with_temp_repo (fun replacement ->
          Git.with_temp_repo (fun dir ->
              prepare original_remote dir;
              let source = commit dir "work" "work\n" "retained candidate" in
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
                            | "push" :: _ -> (128, "", "transport unavailable")
                            | _ -> io.git args);
                      }
                  in
                  check "candidate waits before destination change"
                    (run runtime (persist checkpoint) unavailable
                       (B.Request
                          {
                            intent with
                            purpose = B.Publish_session "changed-destination";
                          })
                    = R.Waiting);
                  let before = B.operation (state runtime) in
                  Git.run_git ~cwd:dir
                    [ "remote"; "set-url"; "--push"; "origin"; replacement ];
                  let runtime =
                    Runtime.create ~gameplan
                      ~main_branch:(Types.Branch.of_string "main")
                      ~snapshot:(get (Persistence.load ~path:checkpoint))
                      ()
                  in
                  check "restart rejects changed destination"
                    (run runtime (persist checkpoint) io B.Recover
                    = R.Intervention "publication_destination_changed");
                  check "changed destination receives no candidate"
                    (Git.git_exit_code ~cwd:replacement
                       [ "show-ref"; "--verify"; "refs/heads/patch" ]
                    <> 0);
                  check
                    "explicit resume observes and adopts reviewed destination"
                    (run runtime (persist checkpoint) io B.Resume = R.Idle);
                  check "resume publishes original candidate"
                    (Git.git_capture ~cwd:replacement [ "rev-parse"; "patch" ]
                    = source);
                  check
                    "destination resume retains operation and captured source"
                    (match (before, B.operation (state runtime)) with
                    | Some before, Some after ->
                        before.id = after.id && before.source = after.source
                    | _ -> false)))));
  List.iter
    (fun multiple ->
      Git.with_temp_repo (fun fetch_remote ->
          ignore (commit fetch_remote "base" "base\n" "base");
          Git.with_temp_repo (fun push_remote ->
              Git.run_git ~cwd:push_remote [ "fetch"; fetch_remote; "main" ];
              Git.run_git ~cwd:push_remote [ "reset"; "--hard"; "FETCH_HEAD" ];
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
                        "config"; "--add"; "remote.origin.pushurl"; fetch_remote;
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
                          "ambiguous destinations stop before any publication"
                          (outcome = R.Intervention "multiple_push_destinations"
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
                          "explicit absent lease publishes to the actual push \
                           URL"
                          (Git.git_capture ~cwd:push_remote
                             [ "rev-parse"; "patch" ]
                          = source);
                        let runtime =
                          Runtime.create ~gameplan
                            ~main_branch:(Types.Branch.of_string "main")
                            ~snapshot:(get (Persistence.load ~path:checkpoint))
                            ()
                        in
                        check
                          "restart confirms actual destination without \
                           repeating push"
                          (run runtime (persist checkpoint) lost_ack B.Recover
                           = R.Idle
                          && !pushes = 1)))))))
    [ false; true ];
  List.iter
    (fun permission ->
      Git.with_temp_repo (fun remote ->
          ignore (commit remote "base" "base\n" "base");
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              let source = commit dir "patch" "retained work\n" "patch work" in
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
                    && captured.repair = None);
                  check
                    "publication failure does not increment session failure \
                     accounting"
                    (Runtime.read runtime (fun snap ->
                         let agent =
                           Orchestrator.agent snap.Runtime.orchestrator id
                         in
                         agent.Patch_agent.push_failure_count = 0
                         && agent.session_completion = None));
                  if permission then
                    check "explicit write denial is stable intervention"
                      (outcome = R.Intervention "push_permission_denied"
                      && run runtime (persist checkpoint) io B.Recover = outcome
                      )
                  else (
                    check "transport failure leaves durable publication pending"
                      (outcome = R.Waiting);
                    let moved = dir ^ "-temporarily-unavailable" in
                    Sys.rename dir moved;
                    Fun.protect
                      ~finally:(fun () -> Sys.rename moved dir)
                      (fun () ->
                        check "missing checkout waits without recreating it"
                          (run runtime (persist checkpoint) io (B.Tick 105.)
                           = R.Waiting
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
                      (run runtime (persist checkpoint) io (B.Tick 110.)
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
                   ~token:"fixture" ~owner:"test" ~repo:"fixture" ~repo_root:dir
                   ~main_branch:main
                   ~changes:[ (Some pr, branch, main) ])
          in
          let observe () =
            match Forge.pr_state pr with
            | Ok state -> state
            | Error error -> failwith (Sourcehut.show_error error)
          in
          let before = observe () in
          check "SourceHut conflict identifies exact Git head and base"
            (before.Pr_state.head_oid = Some source
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
                | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _
                  ->
                    failwith "SourceHut conflict requires repair"
              in
              let oc = open_out (Filename.concat dir "file") in
              output_string oc "resolved\n";
              close_out oc;
              Git.run_git ~cwd:dir [ "add"; "file" ];
              check "SourceHut staged repair publishes through common owner"
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
                Error (Sourcehut.Transport_error "build service unavailable");
              check
                "SourceHut build probe failure stays distinct from a negative \
                 observation"
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
          check "direct remote probe sees advancement without tracking update"
            (observe () = Ok (Some (sha advanced)));
          Git.run_git ~cwd:remote [ "update-ref"; "refs/heads/patch"; original ];
          check "direct remote probe accepts head reversion"
            (observe () = Ok (Some (sha original)));
          check "direct remote probe leaves tracking refs untouched"
            (Git.git_capture ~cwd:dir [ "rev-parse"; "origin/patch" ] = original);
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
  List.iter
    (fun (dirty, active) ->
      Git.with_temp_repo (fun remote ->
          ignore (commit remote "base" "base\n" "base");
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              let revision = commit dir "work" "committed\n" "legacy patch" in
              let revision =
                if active then (
                  let revision =
                    commit dir "base" "local base edit\n" "legacy conflict"
                  in
                  ignore
                    (commit remote "base" "remote base edit\n" "base moved");
                  Git.run_git ~cwd:dir [ "fetch"; "origin" ];
                  check "legacy rebase fixture conflicts"
                    (Git.git_exit_code ~cwd:dir [ "rebase"; "origin/main" ] <> 0);
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
                  Orchestrator.set_pr_number orch id (Types.Pr_number.of_int 12));
              let json = Runtime.read runtime Persistence.snapshot_to_yojson in
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
                  let expected =
                    if active then
                      R.Intervention "publication_during_integration"
                    else R.Idle
                  in
                  check
                    "legacy publication either settles or retains active \
                     integration"
                    (outcome = expected);
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
                    check "legacy publication captures actual branch revision"
                      (Git.git_capture ~cwd:remote
                         [ "rev-parse"; "refs/heads/patch" ]
                      = revision);
                  check "legacy dirty index and worktree remain untouched"
                    (Git.git_capture ~cwd:dir [ "status"; "--porcelain=v1" ]
                    = before);
                  if dirty then (
                    check "staged content remains staged"
                      (Git.git_capture ~cwd:dir [ "show"; ":work" ] = "staged");
                    let read name =
                      let ic = open_in_bin (Filename.concat dir name) in
                      Fun.protect
                        ~finally:(fun () -> close_in ic)
                        (fun () ->
                          really_input_string ic (in_channel_length ic))
                    in
                    check "unstaged content remains" (read "work" = "unstaged\n");
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
                    R.run ~runtime:resumed ~persist:(persist path) ~patch_id:id
                      ~now:(fun () -> 200.)
                      ~execute:(fun ~operation command ->
                        incr calls;
                        execute (make_io dir) ~operation command)
                      B.Recover
                  in
                  check "settled migration is a restart fixed point"
                    (outcome = expected && !calls = 0);
                  if active then (
                    Git.run_git ~cwd:dir [ "rebase"; "--abort" ];
                    Runtime.update_orchestrator resumed (fun orch ->
                        Orchestrator.reset_intervention_state orch id);
                    let first = ref None in
                    let outcome =
                      R.run ~runtime:resumed ~persist:(persist path)
                        ~patch_id:id
                        ~now:(fun () -> 300.)
                        ~execute:(fun ~operation command ->
                          if Option.is_none !first then
                            first := Some command.B.kind;
                          execute (make_io dir) ~operation command)
                        B.Recover
                    in
                    check "human resume inspects before mutation"
                      (!first = Some B.Inspect);
                    check "human repair resumes captured publication"
                      (outcome = R.Idle);
                    check "human resume retains original patch work"
                      (Git.git_capture ~cwd:remote
                         [ "rev-parse"; "refs/heads/patch" ]
                      = revision))))))
    [ (false, false); (true, false); (false, true) ];
  List.iter
    (fun (wrong_target, lost_ack) ->
      Git.with_temp_repo (fun remote ->
          ignore (commit remote "base" "base\n" "base");
          Git.run_git ~cwd:remote [ "checkout"; "-q"; "-b"; "other" ];
          let other = commit remote "other" "other\n" "other target" in
          Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              let source = commit dir "work" "work\n" "patch work" in
              let target =
                commit remote "upstream" "upstream\n" "base advanced"
              in
              let io = make_io dir and runtime = make_runtime () in
              let checkpoint = Filename.temp_file "unclaimed-rebase" ".json" in
              Fun.protect
                ~finally:(fun () -> Sys.remove checkpoint)
                (fun () ->
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
                       R.run ~runtime ~persist:(persist checkpoint) ~patch_id:id
                         ~now:(fun () -> 100.)
                         ~execute:(fun ~operation command ->
                           let result = execute paused ~operation command in
                           match command.B.kind with
                           | B.Integrate _ ->
                               if lost_ack then raise Simulated_stop else result
                           | B.Observe | B.Pin _ | B.Commit_merge _
                           | B.Plan_remote_replay _ | B.Checkout_remote _
                           | B.Inspect | B.Verify_recovery | B.Continue _
                           | B.Publish _ | B.Confirm _ ->
                               result)
                         (B.Request intent)
                     in
                     check
                       "acknowledged sequencer stop waits without agent repair"
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
                        | B.Observe | B.Pin _ | B.Integrate _ | B.Commit_merge _
                        | B.Plan_remote_replay _ | B.Checkout_remote _
                        | B.Inspect | B.Verify_recovery | B.Publish _
                        | B.Confirm _ ->
                            ());
                        execute io ~operation command)
                      B.Recover
                  in
                  if wrong_target then (
                    check "foreign integration target cannot be continued"
                      (!continued = 0);
                    check "foreign integration enters verified history recovery"
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
                           [ "for-each-ref"; "--format=%(objectname)"; prefix ]
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
                  else (
                    check "ready integration resumes without agent repair"
                      (outcome = R.Idle && !continued = 1);
                    check "resumed rebase retains patch content"
                      (Git.git_capture ~cwd:remote [ "show"; "patch:work" ]
                      = "work");
                    check "resumed rebase retains captured target"
                      (Git.git_exit_code ~cwd:remote
                         [ "merge-base"; "--is-ancestor"; target; "patch" ]
                      = 0);
                    check "resumed rebase retains the patch commit"
                      (Git.git_capture ~cwd:remote
                         [ "rev-list"; "--count"; target ^ "..patch" ]
                      = "1"))))))
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
  List.iter
    (fun (stop_at, duplicate, history) ->
      Git.with_temp_repo (fun remote ->
          let base = commit remote "base" "base\n" "base" in
          if history = 2 || history = 4 then
            ignore (commit remote "prior" "prior\n" "prior publication");
          if history <> 1 && history <> 3 then
            Git.run_git ~cwd:remote [ "branch"; "patch"; "HEAD" ];
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              let candidate = commit dir "local" "local\n" "local candidate" in
              Git.run_git ~cwd:dir [ "config"; "rebase.updateRefs"; "true" ];
              Git.run_git ~cwd:dir [ "config"; "rebase.autoStash"; "true" ];
              let runtime = make_runtime () and io = make_io dir in
              let raced = ref false and resets = ref 0 and rebases = ref 0 in
              let probe_failed = ref false in
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
                                 [ "checkout"; "-q"; "-b"; "patch"; "main" ]
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
                                [ "checkout"; "-q"; "-b"; "side"; base ];
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
                                     [ "merge"; "--no-ff"; "--no-edit"; "side" ]
                                  = 1);
                                ignore
                                  (commit remote "base"
                                     (if history >= 7 then "main change\n"
                                      else "remote resolution\n")
                                     "resolve remote merge"))
                              else
                                Git.run_git ~cwd:remote
                                  [ "merge"; "--no-ff"; "--no-edit"; "side" ]);
                            Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ]
                        | "reset" :: _ ->
                            incr resets;
                            if stop_at = 4 then (
                              let oc =
                                open_out_bin (Filename.concat dir "local")
                              in
                              output_string oc "late staged edit\n";
                              close_out oc;
                              Git.run_git ~cwd:dir [ "add"; "local" ])
                        | "rebase" :: _ ->
                            incr rebases;
                            Git.run_git ~cwd:dir
                              [ "branch"; "unrelated"; "HEAD" ]
                        | _ -> ());
                        match args with
                        | "merge-base" :: "--all" :: _
                          when history = 4 && not !probe_failed ->
                            probe_failed := true;
                            (128, "", "common ancestor probe unavailable")
                        | _ -> io.git args);
                  }
              in
              let checkpoint = Filename.temp_file "remote-replay" ".json" in
              Fun.protect
                ~finally:(fun () -> Sys.remove checkpoint)
                (fun () ->
                  if history = 1 then
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
                  let runtime =
                    if history = 4 then (
                      check "common ancestor probe failure waits"
                        (R.run ~runtime ~persist:(persist checkpoint)
                           ~patch_id:id
                           ~now:(fun () -> 1000.)
                           ~execute:(execute racing) (B.Tick 1000.)
                        = R.Waiting);
                      check "failed planning does not mutate checkout"
                        (!resets = 0 && !rebases = 0);
                      let snapshot = get (Persistence.load ~path:checkpoint) in
                      Runtime.create ~gameplan
                        ~main_branch:(Types.Branch.of_string "main")
                        ~snapshot ())
                    else runtime
                  in
                  let stopped = ref false in
                  let stage command =
                    match command.B.kind with
                    | B.Checkout_remote _ -> 0
                    | B.Integrate _ -> 2
                    | B.Commit_merge _ | B.Plan_remote_replay _ | B.Observe
                    | B.Pin _ | B.Inspect | B.Verify_recovery | B.Continue _
                    | B.Publish _ | B.Confirm _ ->
                        -10
                  in
                  (try
                     ignore
                       (R.run ~runtime ~persist:(persist checkpoint)
                          ~patch_id:id
                          ~now:(fun () -> 2000.)
                          ~execute:(fun ~operation command ->
                            (match command.B.kind with
                            | B.Checkout_remote _ ->
                                check "chosen replay evidence is retained"
                                  (if List.mem history [ 2; 3; 4 ] then
                                     operation.B.boundary
                                     = B.Inferred (sha base)
                                   else
                                     operation.B.boundary
                                     = B.Recorded (sha base))
                            | B.Commit_merge _ | B.Plan_remote_replay _
                            | B.Observe | B.Pin _ | B.Integrate _ | B.Inspect
                            | B.Verify_recovery | B.Continue _ | B.Publish _
                            | B.Confirm _ ->
                                ());
                            if stage command = stop_at then (
                              stopped := true;
                              raise Simulated_stop);
                            let result = execute racing ~operation command in
                            if
                              stage command + 1 = stop_at
                              || (stop_at = 4 && stage command = 0)
                            then (
                              stopped := true;
                              raise Simulated_stop);
                            result)
                          (B.Tick 2000.));
                     failwith "expected replay interruption"
                   with Simulated_stop -> ());
                  check "replay interruption reached" !stopped;
                  let snapshot = get (Persistence.load ~path:checkpoint) in
                  let restarted =
                    Runtime.create ~gameplan
                      ~main_branch:(Types.Branch.of_string "main")
                      ~snapshot ()
                  in
                  if stop_at = 4 then (
                    check "late edits stop replay without losing work"
                      (run restarted (persist checkpoint) racing B.Recover
                      = R.Intervention "dirty_worktree");
                    check "failed checkout retains original revision"
                      (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                      = candidate);
                    check "late staged content survives refused checkout"
                      (Git.git_capture ~cwd:dir [ "show"; ":local" ]
                      = "late staged edit");
                    check "no replay after late edit"
                      (!resets = 1 && !rebases = 0))
                  else
                    let outcome =
                      run restarted (persist checkpoint) racing B.Recover
                    in
                    let restarted, outcome =
                      if history >= 6 then (
                        check "merge replay enters content repair"
                          (match outcome with
                          | R.Repair_needed _ -> true
                          | R.Idle | R.Waiting | R.Intervention _
                          | R.Checkpoint_failed _ ->
                              false);
                        check "conflict occurs at the recreated merge"
                          (Git.git_exit_code ~cwd:dir
                             [ "rev-parse"; "--verify"; "MERGE_HEAD" ]
                          = 0);
                        let head =
                          Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                        in
                        let oc = open_out_bin (Filename.concat dir "base") in
                        output_string oc
                          (if history >= 7 then "main change\n"
                           else "remote resolution\n");
                        close_out oc;
                        Git.run_git ~cwd:dir [ "add"; "base" ];
                        if history >= 7 then
                          check "merge resolution has no staged tree changes"
                            (Git.git_exit_code ~cwd:dir
                               [ "diff"; "--cached"; "--quiet" ]
                            = 0);
                        check "staging merge resolution needs no agent commit"
                          (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                          = head);
                        let snapshot =
                          get (Persistence.load ~path:checkpoint)
                        in
                        let resumed =
                          Runtime.create ~gameplan
                            ~main_branch:(Types.Branch.of_string "main")
                            ~snapshot ()
                        in
                        if history >= 7 then (
                          let interrupted = ref false in
                          (try
                             ignore
                               (R.run ~runtime:resumed
                                  ~persist:(persist checkpoint) ~patch_id:id
                                  ~now:(fun () -> 2000.)
                                  ~execute:(fun ~operation command ->
                                    match command.B.kind with
                                    | B.Commit_merge _ ->
                                        if history = 7 then
                                          ignore
                                            (execute racing ~operation command);
                                        interrupted := true;
                                        raise Simulated_stop
                                    | B.Plan_remote_replay _
                                    | B.Checkout_remote _ | B.Observe | B.Pin _
                                    | B.Integrate _ | B.Inspect
                                    | B.Verify_recovery | B.Continue _
                                    | B.Publish _ | B.Confirm _ ->
                                        execute racing ~operation command)
                                  B.Recover);
                             failwith
                               "expected interruption at merge commit \
                                checkpoint"
                           with Simulated_stop -> ());
                          check "merge commit checkpoint exercised" !interrupted;
                          let snapshot =
                            get (Persistence.load ~path:checkpoint)
                          in
                          let resumed =
                            Runtime.create ~gameplan
                              ~main_branch:(Types.Branch.of_string "main")
                              ~snapshot ()
                          in
                          ( resumed,
                            run resumed (persist checkpoint) racing B.Recover ))
                        else
                          ( resumed,
                            run resumed (persist checkpoint) racing B.Recover ))
                      else (restarted, outcome)
                    in
                    check
                      (Printf.sprintf
                         "remote replay resumes after interruption \
                          (history=%d, stop=%d): %s"
                         history stop_at
                         (Yojson.Safe.to_string
                            (B.yojson_of_t (state restarted))))
                      (outcome = R.Idle);
                    check
                      "remote replay receipt retains captured candidate and \
                       strategy"
                      (match B.remote_integrations (state restarted) with
                      | [ receipt ] ->
                          receipt.B.capture.target_revision = sha candidate
                          && receipt.capture.integration_policy = B.Rewrite
                          && (receipt.capture.replay_boundary
                             =
                             if List.mem history [ 2; 3; 4 ] then
                               B.Inferred (sha base)
                             else B.Recorded (sha base))
                          && receipt.integrated_revision
                             = sha
                                 (Git.git_capture ~cwd:remote
                                    [ "rev-parse"; "patch" ])
                      | [] | _ :: _ :: _ -> false);
                    check
                      "replay does not update unrelated refs despite user \
                       configuration"
                      (match B.remote_integrations (state restarted) with
                      | [ receipt ] ->
                          sha
                            (Git.git_capture ~cwd:dir
                               [ "rev-parse"; "unrelated" ])
                          = receipt.B.capture.source_revision
                      | [] | _ :: _ :: _ -> false);
                    check "remote commits replayed once"
                      (!resets = 1 && !rebases = 1);
                    check "local candidate retains its identity"
                      (Git.git_exit_code ~cwd:remote
                         [ "merge-base"; "--is-ancestor"; candidate; "patch" ]
                      = 0);
                    check "incoming work survives replay"
                      (if duplicate then
                         Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                         = candidate
                       else
                         Git.git_capture ~cwd:remote
                           [ "show"; "patch:incoming" ]
                         = "incoming");
                    if history >= 5 then (
                      if history >= 6 then
                        check "staged merge resolution is published"
                          (Git.git_capture ~cwd:remote [ "show"; "patch:base" ]
                          =
                          if history >= 7 then "main change"
                          else "remote resolution");
                      if history < 7 then
                        check "remote side branch work survives replay"
                          (Git.git_capture ~cwd:remote [ "show"; "patch:side" ]
                          = "side");
                      check "remote merge retains both parents"
                        (List.length
                           (String.split_on_char ' '
                              (Git.git_capture ~cwd:remote
                                 [ "rev-list"; "--parents"; "-n1"; "patch" ]))
                        = 3);
                      List.iter
                        (fun parent ->
                          check
                            "each replayed merge parent includes preserved \
                             candidate"
                            (Git.git_exit_code ~cwd:remote
                               [
                                 "merge-base";
                                 "--is-ancestor";
                                 candidate;
                                 parent;
                               ]
                            = 0))
                        [ "patch^1"; "patch^2" ])
                    else
                      check
                        "linear rewrite race does not introduce merge commits"
                        (Git.git_capture ~cwd:remote
                           [ "rev-list"; "--merges"; base ^ "..patch" ]
                        = "")))))
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
    ];
  List.iter
    (fun fail_probe ->
      Git.with_temp_repo (fun remote ->
          let base = commit remote "base" "base\n" "base" in
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              ignore (commit dir "patch-work" "patch\n" "patch work");
              let runtime = make_runtime () and io = make_io dir in
              let checkpoint = Filename.temp_file "older-boundary" ".json" in
              Fun.protect
                ~finally:(fun () -> Sys.remove checkpoint)
                (fun () ->
                  ignore
                    (run runtime (persist checkpoint) io
                       (B.Materialized (B.New_branch (sha base))));
                  let request name =
                    B.Request { intent with purpose = Reconcile_request name }
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
                  Git.run_git ~cwd:dir [ "reset"; "--hard"; first_candidate ];
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
                      let snapshot = get (Persistence.load ~path:checkpoint) in
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
  List.iter
    (fun external_reset ->
      Git.with_temp_repo (fun remote ->
          ignore (commit remote "base" "base\n" "base");
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              let root_source = commit dir "root" "root\n" "root work" in
              Git.run_git ~cwd:remote [ "checkout"; "-q"; "-b"; "contributor" ];
              let captured = commit remote "child" "captured\n" "child work" in
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
                            failwith "external reset was not routed to recovery"
                      in
                      check
                        "failed integration postcondition prevents publication"
                        (!pushes = 0);
                      Git.run_git ~cwd:dir [ "merge"; "--no-edit"; captured ];
                      run runtime (persist checkpoint) lost_ack
                        (B.Repair_completed { token; at = 100. }))
                    else outcome
                  in
                  check "root publication waits after lost acknowledgement"
                    (outcome = R.Waiting);
                  check "unconfirmed contribution has no publication receipt"
                    (B.publications (state runtime) = []);
                  check "root integration preserves existing root ancestry"
                    (Git.git_exit_code ~cwd:remote
                       [ "merge-base"; "--is-ancestor"; root_source; "patch" ]
                    = 0);
                  check "root includes captured descendant revision"
                    (Git.git_exit_code ~cwd:remote
                       [ "merge-base"; "--is-ancestor"; captured; "patch" ]
                    = 0);
                  check
                    "moving descendant name cannot change captured integration"
                    (Git.git_exit_code ~cwd:remote
                       [ "merge-base"; "--is-ancestor"; newer; "patch" ]
                    = 1);
                  let runtime =
                    Runtime.create ~gameplan
                      ~main_branch:(Types.Branch.of_string "main")
                      ~snapshot:(get (Persistence.load ~path:checkpoint))
                      ()
                  in
                  check "restart confirms root publication without another push"
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
  List.iter
    (fun interrupt ->
      Git.with_temp_repo (fun remote ->
          let base = commit remote "file" "base\n" "base" in
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              ignore (commit dir "file" "local\n" "patch");
              let old_target = commit remote "file" "upstream\n" "upstream" in
              Git.run_git ~cwd:dir [ "fetch"; "origin" ];
              check "fixture has an interrupted rebase"
                (Git.git_exit_code ~cwd:dir
                   [ "rebase"; "--onto"; old_target; base ]
                <> 0);
              let output = open_out (Filename.concat dir "file") in
              output_string output "resolved\n";
              close_out output;
              Git.run_git ~cwd:dir [ "add"; "file" ];
              let new_target = commit remote "later" "later\n" "newer base" in
              let runtime = make_runtime () and io = make_io dir in
              let checkpoint = Filename.temp_file "adopt-integration" ".json" in
              Fun.protect
                ~finally:(fun () -> Sys.remove checkpoint)
                (fun () ->
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
                                | B.Integrate _ | B.Inspect | B.Verify_recovery
                                | B.Continue _ | B.Publish _ | B.Confirm _ ->
                                    execute io ~operation command)
                              (B.Request intent));
                         failwith "expected interruption before adoption pins"
                       with Simulated_stop -> ());
                      let runtime =
                        Runtime.create ~gameplan
                          ~main_branch:(Types.Branch.of_string "main")
                          ~snapshot:(get (Persistence.load ~path:checkpoint))
                          ()
                      in
                      (runtime, run runtime (persist checkpoint) io B.Recover))
                    else
                      ( runtime,
                        run runtime (persist checkpoint) io (B.Request intent)
                      )
                  in
                  check
                    "adopted staged rebase and newer base converge without \
                     another repair"
                    (outcome = R.Idle);
                  check "staged resolution survives adoption"
                    (Git.git_capture ~cwd:remote [ "show"; "patch:file" ]
                    = "resolved");
                  check "newer base is incorporated after original integration"
                    (Git.git_exit_code ~cwd:remote
                       [ "merge-base"; "--is-ancestor"; new_target; "patch" ]
                    = 0);
                  match B.integrations (state runtime) with
                  | [ latest; adopted ] ->
                      check
                        "adoption preserves original target without inventing \
                         a base name"
                        (adopted.B.capture.B.base_branch = None
                        && adopted.B.capture.B.target_revision = sha old_target
                        );
                      check "successor integrates the newly observed base"
                        (latest.B.capture.B.base_branch = Some "main"
                        && latest.B.capture.B.target_revision = sha new_target)
                  | [] | [ _ ] | _ :: _ :: _ :: _ ->
                      failwith "expected two integration receipts"))))
    [ false; true ];
  (* A rewrite request adopting a merge retains the merge's ancestry policy.
     A later external rebase cannot impersonate completion through its reflog. *)
  Git.with_temp_repo (fun remote ->
      ignore (commit remote "base" "base\n" "base");
      Git.with_temp_repo (fun dir ->
          prepare remote dir;
          let source = commit dir "patch" "patch\n" "patch work" in
          let target = commit remote "upstream" "upstream\n" "base advanced" in
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
                   (R.run ~runtime ~persist:(persist checkpoint) ~patch_id:id
                      ~now:(fun () -> 100.)
                      ~execute:(fun ~operation command ->
                        match command.B.kind with
                        | B.Pin _ -> raise Simulated_stop
                        | B.Observe | B.Integrate _ | B.Inspect
                        | B.Verify_recovery | B.Continue _ | B.Commit_merge _
                        | B.Plan_remote_replay _ | B.Checkout_remote _
                        | B.Publish _ | B.Confirm _ ->
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
                | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _
                  ->
                    failwith "rewritten adopted merge must require recovery"
              in
              check "reflog alone cannot authorize publishing rewritten merge"
                (Git.git_exit_code ~cwd:remote
                   [ "show-ref"; "--verify"; "refs/heads/patch" ]
                <> 0);
              Git.run_git ~cwd:dir [ "merge"; "--no-edit"; source ];
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
                [ source; target; rewritten ])));
  (* A completed negative preservation proof needs recovery, not an endless
     transport retry. Keep both histories while asking the agent for help. *)
  Git.with_temp_repo (fun remote ->
      ignore (commit remote "base" "base\n" "base");
      Git.with_temp_repo (fun dir ->
          prepare remote dir;
          let local = commit dir "local" "local\n" "local work" in
          Git.run_git ~cwd:remote [ "checkout"; "-q"; "-b"; "patch" ];
          let remote_head =
            commit remote "external" "external\n" "external work"
          in
          Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
          let runtime = make_runtime () and io = make_io dir in
          let checkpoint = Filename.temp_file "publication-recovery" ".json" in
          Fun.protect
            ~finally:(fun () -> Sys.remove checkpoint)
            (fun () ->
              let outcome =
                run runtime (persist checkpoint) io
                  (B.Request
                     { intent with purpose = B.Publish_revision (sha local) })
              in
              let token =
                match outcome with
                | R.Repair_needed token ->
                    let turn =
                      Option.get
                        (B.repair_turn (state runtime) ~branch:"patch" token)
                    in
                    check "unproven publication enters history recovery"
                      (match turn.B.mode with
                      | B.History_recovery { reason; _ } ->
                          reason = "remote_work_not_incorporated"
                      | B.Content_repair -> false);
                    token
                | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _
                  ->
                    failwith
                      ("unproven publication did not offer recovery: "
                      ^ Yojson.Safe.to_string (B.yojson_of_t (state runtime)))
              in
              check "recovery preserves local candidate"
                (Git.git_capture ~cwd:dir [ "rev-parse"; "patch" ] = local);
              check "recovery preserves remote work"
                (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                = remote_head);
              (* The recovery turn may change HEAD, but publication still belongs
                 to the owner and requires independently observed preservation. *)
              Git.run_git ~cwd:dir [ "merge"; "--no-edit"; remote_head ];
              let outcome =
                run runtime (persist checkpoint) io
                  (B.Repair_completed { token; at = 100. })
              in
              check "verified agent recovery completes publication"
                (outcome = R.Idle);
              check "published recovery retains local work"
                (Git.git_exit_code ~cwd:remote
                   [ "merge-base"; "--is-ancestor"; local; "patch" ]
                = 0);
              check "published recovery retains remote work"
                (Git.git_exit_code ~cwd:remote
                   [ "merge-base"; "--is-ancestor"; remote_head; "patch" ]
                = 0))));
  List.iter
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
              let checkpoint = Filename.temp_file "agent-recovery" ".json" in
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
                  let baseline =
                    Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                  in
                  let token =
                    if late_work then (
                      ignore
                        (commit dir "late" "work arriving between turns\n"
                           "late external work");
                      match run runtime (persist checkpoint) io B.Recover with
                      | R.Repair_needed token -> token
                      | R.Idle | R.Waiting | R.Intervention _
                      | R.Checkpoint_failed _ ->
                          failwith
                            "late work should enter retained history recovery")
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
                    | B.Content_repair -> false);
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
                              (Orchestrator.agent saved.Runtime.orchestrator id)
                                .Patch_agent.branch_reconcile
                            in
                            check "history recovery claimed before backend runs"
                              (Option.is_some
                                 (B.repair_turn saved_state ~branch:"patch"
                                    !active_token));
                            if discard then (
                              Git.run_git ~cwd:dir
                                [
                                  "reset";
                                  "--hard";
                                  (if late_work then baseline else source);
                                ];
                              Git.run_git ~cwd:dir
                                [ "merge"; "--no-edit"; target ])
                            else
                              Git.run_git ~cwd:dir
                                [ "merge"; "--no-edit"; target ];
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
                    Branch_repair_session.run ~backend
                      ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
                      ~project_name:"demo" ~patch_id:id ~complexity:None ~turn
                      ~read_head:(fun () ->
                        incr probes;
                        if lost_probe && !probes = 2 then None
                        else
                          Some
                            (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]))
                      ~now:(fun () -> 100.)
                  in
                  let outcome =
                    run runtime (persist checkpoint) io (invoke turn)
                  in
                  let outcome =
                    if lost_probe then (
                      check "unknown recovery result remains pending"
                        (outcome = R.Waiting);
                      run (restore ()) (persist checkpoint) io (B.Tick 105.))
                    else outcome
                  in
                  if discard then (
                    let token =
                      match outcome with
                      | R.Repair_needed token -> token
                      | R.Idle | R.Waiting | R.Intervention _
                      | R.Checkpoint_failed _ ->
                          failwith
                            "unverified recovery should allow its second \
                             attempt"
                    in
                    let turn =
                      Option.get
                        (B.repair_turn (state runtime) ~branch:"patch" token)
                    in
                    let outcome =
                      run runtime (persist checkpoint) io (invoke turn)
                    in
                    check
                      "agent success alone cannot authorize losing captured \
                       work"
                      (outcome = R.Intervention "recovery_preservation_unproven"
                      && !calls = 2);
                    check "unverified recovery never publishes"
                      (Git.git_exit_code ~cwd:remote
                         [ "show-ref"; "--verify"; "refs/heads/patch" ]
                      <> 0))
                  else (
                    check "verified agent recovery publishes" (outcome = R.Idle);
                    check
                      "restart verifies completed work without another agent"
                      (!calls = 1);
                    check "captured patch commits survive fallback"
                      (Git.git_exit_code ~cwd:dir
                         [ "merge-base"; "--is-ancestor"; source; "HEAD" ]
                      = 0);
                    List.iter
                      (fun file ->
                        check
                          ("published recovery retains " ^ file)
                          (Git.git_capture ~cwd:remote
                             [ "show"; "patch:" ^ file ]
                          = Git.git_capture ~cwd:dir [ "show"; "HEAD:" ^ file ]
                          ))
                      [ "patch"; "extra"; "upstream" ]);
                  let saved = get (Persistence.load ~path:checkpoint) in
                  let agent =
                    Orchestrator.agent saved.Runtime.orchestrator id
                  in
                  check
                    "fallback is separate from implementation and session \
                     failures"
                    (agent.Patch_agent.session_completion = None
                    && agent.Patch_agent.push_failure_count = 0);
                  check "recovery refs retain captured work"
                    (Base.String.is_substring
                       (Git.git_capture ~cwd:dir
                          [ "for-each-ref"; "--format=%(objectname)"; prefix ])
                       ~substring:retained)))))
    [
      (false, false, false);
      (true, false, false);
      (false, true, false);
      (false, true, true);
    ];
  (* History recovery must not hand uncommitted external work to an agent,
     including work introduced while the turn waited for capacity. *)
  List.iter
    (fun during_wait ->
      Git.with_temp_repo (fun remote ->
          let base = commit remote "base" "base\n" "base" in
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              ignore (commit dir "patch" "patch\n" "patch work");
              ignore (commit remote "upstream" "upstream\n" "base advancement");
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
                            | B.Checkout_remote _ | B.Publish _ | B.Confirm _ ->
                                execute io ~operation command)
                          (B.Request intent));
                     failwith "expected interruption"
                   with Simulated_stop -> ());
                  let head = Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ] in
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
                  let outcome =
                    if during_wait then
                      let token =
                        match run runtime (persist checkpoint) io B.Recover with
                        | R.Repair_needed token -> token
                        | R.Idle | R.Waiting | R.Intervention _
                        | R.Checkpoint_failed _ ->
                            failwith "expected recovery turn"
                      in
                      R.run_repair ~runtime ~persist:(persist checkpoint)
                        ~patch_id:id
                        ~with_capacity:(fun f ->
                          dirty ();
                          f ())
                        ~now:(fun () -> 100.)
                        ~execute:(fun ~agent:_ ~operation command ->
                          execute io ~operation command)
                        ~perform:(fun ~agent:_ ~turn:_ ->
                          failwith "dirty work cannot reach recovery agent")
                        token
                    else (
                      dirty ();
                      run runtime (persist checkpoint) io B.Recover)
                  in
                  check "dirty history recovery stops specifically"
                    (outcome = R.Intervention "history_recovery_dirty_worktree");
                  check "new external commit is pinned before intervention"
                    (Base.String.is_substring
                       (Git.git_capture ~cwd:dir
                          [ "for-each-ref"; "--format=%(objectname)"; prefix ])
                       ~substring:head);
                  check "staged external bytes survive"
                    (Git.git_capture ~cwd:dir [ "show"; ":extra" ]
                    = "staged external work");
                  let read name =
                    let ic = open_in_bin (Filename.concat dir name) in
                    Fun.protect
                      ~finally:(fun () -> close_in ic)
                      (fun () -> really_input_string ic (in_channel_length ic))
                  in
                  check "unstaged external bytes survive"
                    (read "extra" = "unstaged external work\n");
                  check "untracked external bytes survive"
                    (read "untracked" = "untracked external work\n");
                  check "unsafe recovery never publishes"
                    (Git.git_exit_code ~cwd:remote
                       [ "show-ref"; "--verify"; "refs/heads/patch" ]
                    <> 0)))))
    [ false; true ];
  (* A repair's expected detached HEAD is durable. Losing the post-agent
     probe cannot turn an unexpected agent commit into authorized continuation. *)
  List.iter
    (fun mutate_before ->
      Git.with_temp_repo (fun remote ->
          let base = commit remote "file" "base\n" "base" in
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              ignore (commit dir "file" "patch\n" "patch");
              ignore (commit remote "file" "upstream\n" "upstream");
              let io = make_io dir and runtime = make_runtime () in
              let checkpoint = Filename.temp_file "repair-authority" ".json" in
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
                    Branch_repair_session.run ~backend
                      ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
                      ~project_name:"demo" ~patch_id:id ~complexity:None ~turn
                      ~read_head:(fun () ->
                        incr probes;
                        if (not mutate_before) && !probes = 2 then None
                        else
                          Some
                            (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]))
                      ~now:(fun () -> 100.)
                  in
                  let outcome = run runtime (persist checkpoint) io event in
                  check
                    "repair preflight rejects a changed HEAD before backend \
                     dispatch"
                    (!calls = if mutate_before then 0 else 1);
                  let unexpected_head =
                    Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                  in
                  let outcome =
                    if mutate_before then outcome
                    else (
                      check "lost HEAD probe defers without charging repair"
                        (outcome = R.Waiting);
                      let snapshot = get (Persistence.load ~path:checkpoint) in
                      let runtime =
                        Runtime.create ~gameplan
                          ~main_branch:(Types.Branch.of_string "main")
                          ~snapshot ()
                      in
                      run runtime (persist checkpoint) io (B.Tick 105.))
                  in
                  check
                    "restart invalidates content authority and enters history \
                     recovery"
                    (match outcome with
                    | R.Repair_needed _ -> true
                    | R.Idle | R.Waiting | R.Intervention _
                    | R.Checkpoint_failed _ ->
                        false);
                  check "unexpected agent work remains recoverable"
                    (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                    = unexpected_head);
                  check "no continuation after unexpected commit"
                    (not
                       (Git_observation.equal_sequencer
                          (E.observe_checkout io).sequencer
                          Git_observation.None_active));
                  check "unverified agent work was not published"
                    (Git.git_exit_code ~cwd:remote
                       [ "show-ref"; "--verify"; "refs/heads/patch" ]
                    <> 0)))))
    [ false; true ];
  (* PR #483: a repair agent has published the candidate, while the local
     tracking ref still names the old tip and Onton's lease has been rejected.
     Only a direct observation can settle publication. An unavailable probe
     must retain the candidate without charging a content-repair attempt. *)
  List.iter
    (fun fail_confirmation ->
      Git.with_temp_repo (fun published_remote ->
          ignore (commit published_remote "seed" "seed\n" "seed");
          Git.with_temp_repo (fun published_dir ->
              prepare published_remote published_dir;
              let old = commit published_dir "work" "first\n" "first" in
              Git.run_git ~cwd:published_dir [ "push"; "-q"; "origin"; "patch" ];
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
                              [ "update-ref"; "refs/remotes/origin/patch"; old ];
                            ( 1,
                              "!\tHEAD:refs/heads/patch\t[rejected] (stale info)\n",
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
                  check "agent publication produces a retryable lease failure"
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
                      "failed remote probe does not claim publication success"
                      (run runtime (persist checkpoint) published_by_agent
                         B.Recover
                      = R.Waiting);
                    check
                      "failed remote probe retains candidate and repair budget"
                      (match B.operation (state runtime) with
                      | Some op ->
                          op.candidate = Some (sha candidate)
                          && op.repair = None
                      | None -> false);
                    probe_unavailable := false);
                  check
                    "direct remote confirmation recognizes agent publication"
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
        (Git.git_capture ~cwd:remote [ "rev-parse"; "refs/heads/patch" ] = work);
      Git.run_git ~cwd:remote [ "checkout"; "-q"; "patch" ];
      let advanced = commit remote "other" "other work\n" "later remote work" in
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
        (Git.git_capture ~cwd:dir [ "rev-parse"; prefix ^ "/1/2/source" ] = work));
  (* A new checkout with two conflicting replay steps. Agents only stage; the
     executor owns continuation and publication, including empty/no-op steps. *)
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
                  check "repair is separate from implementation session"
                    (resume_session = None);
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
          Branch_repair_session.run ~backend
            ~cwd:Eio.Path.(Eio.Stdenv.fs env / repair_dir)
            ~project_name:"demo" ~patch_id:id ~complexity:None ~turn
            ~read_head:(fun () ->
              Some (Git.git_capture ~cwd:repair_dir [ "rev-parse"; "HEAD" ]))
            ~now:(fun () -> 100.)
        in
        let before = E.observe_checkout io in
        check "staged repair is ready without an agent commit"
          (Git_observation.repair_ready before);
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
        (Git.git_capture ~cwd:remote [ "show"; "patch:file" ] = "resolved two");
      check "sequencer gone"
        (Git_observation.equal_sequencer (E.observe_checkout io).sequencer
           Git_observation.None_active));
  Git.run_git ~cwd:remote [ "branch"; "-D"; "patch" ];
  Git.with_temp_repo @@ fun race_dir ->
  prepare remote race_dir;
  let candidate = commit race_dir "candidate-file" "candidate\n" "candidate" in
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
                    commit remote "remote-file" "remote work\n" "external work";
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
        (run runtime (persist snap) race_io (B.Materialized (B.New_branch base)));
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
                | B.Integrate _ when operation.B.id = 2 -> raise Simulated_stop
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
      let initial = commit materialized_dir "initial" "initial\n" "initial" in
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
  List.iter
    (fun unknown_revision ->
      Git.with_temp_repo (fun publish_remote ->
          ignore (commit publish_remote "base" "base\n" "base");
          Git.with_temp_repo (fun publish_dir ->
              prepare publish_remote publish_dir;
              let captured =
                commit publish_dir "work" "work\n" "implementation"
              in
              let newer_base =
                commit publish_remote "new-base" "new base\n" "base advanced"
              in
              if unknown_revision then (
                let oc =
                  open_out_bin (Filename.concat publish_dir "unfinished")
                in
                output_string oc "staged work from the interrupted session\n";
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
                               ~project_name:"project with punctuation: [test]"
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
                       [ "merge-base"; "--is-ancestor"; newer_base; "patch" ]
                    = 1);
                  check "expired write ownership is rejected"
                    (match !escaped with
                    | None -> false
                    | Some owner -> (
                        try Runtime.with_owned_patch owner (fun _ _ -> false)
                        with Invalid_argument _ -> true))))))
    [ false; true ];
  print_endline "branch reconciliation Git/checkpoint contracts: OK"

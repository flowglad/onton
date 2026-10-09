(* @archlint.module test
   @archlint.domain poller-fiber *)

open Onton
open Onton_core
open Types
module Git = Onton_test_support.Git_env

let check message condition = if not condition then failwith message
let get = function Ok value -> value | Error reason -> failwith reason
let pid = Patch_id.of_string "patch"
let main = Branch.of_string "main"

let patch : Patch.t =
  Patch.
    {
      id = pid;
      title = "patch";
      description = "";
      branch = Branch.of_string "patch";
      dependencies = [];
      spec = "";
      acceptance_criteria = [];
      files = [];
      classification = "";
      changes = [];
      test_stubs_introduced = [];
      test_stubs_implemented = [];
      complexity = None;
      precedents = [];
      required_context = [];
    }

let gameplan : Gameplan.t =
  Gameplan.
    {
      project_name = "forge-checkpoint";
      repo_owner = "";
      repo_name = "";
      problem_statement = "";
      architecture_design = None;
      solution_summary = "";
      final_state_spec = "";
      patches = [ patch ];
      operational_considerations = "";
      required_changes = "";
      ordering_constraints = [];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
      functional_changes = [];
      context_resources = [];
      publication = None;
      reachability_traces = [];
    }

type scenario =
  | Valid
  | Stale_head
  | Stale_base
  | Missing_head
  | Missing_base
  | Missing_identity
  | Changed_pr
  | Replaced_request
  | Head_reversion
  | Satisfied_conflict
  | Contradictory
  | Native_present
  | Native_absent of int

type provider = Sourcehut | Github

let run env provider scenario =
  Git.with_temp_repo (fun remote ->
      Git.run_git ~cwd:remote [ "commit"; "--allow-empty"; "-qm"; "base" ];
      Git.run_git ~cwd:remote [ "branch"; "stack-parent"; "main" ];
      Git.with_temp_repo (fun dir ->
          let previous_data = Sys.getenv_opt "ONTON_DATA_DIR" in
          Unix.putenv "ONTON_DATA_DIR" (Filename.concat dir "state");
          Fun.protect
            ~finally:(fun () ->
              Unix.putenv "ONTON_DATA_DIR"
                (Option.value previous_data ~default:""))
            (fun () ->
              Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
              Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
              Git.run_git ~cwd:dir [ "checkout"; "-qb"; "patch"; "origin/main" ];
              Git.run_git ~cwd:dir [ "commit"; "--allow-empty"; "-qm"; "patch" ];
              let older_head =
                Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
              in
              Git.run_git ~cwd:dir
                [ "commit"; "--allow-empty"; "-qm"; "patch follow-up" ];
              Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "patch" ];
              Git.run_git ~cwd:dir [ "checkout"; "-qb"; "idle"; "origin/main" ];
              let head = Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ] in
              let base_sha =
                Git.git_capture ~cwd:remote [ "rev-parse"; "main" ]
              in
              let native =
                match scenario with
                | Native_present | Native_absent _ -> true
                | Valid | Stale_head | Stale_base | Missing_head | Missing_base
                | Missing_identity | Changed_pr | Replaced_request
                | Head_reversion | Satisfied_conflict | Contradictory ->
                    false
              in
              let cycles =
                match scenario with
                | Native_absent n -> n
                | Head_reversion -> 3
                | Satisfied_conflict | Contradictory -> 2
                | Valid | Stale_head | Stale_base | Missing_head | Missing_base
                | Missing_identity | Changed_pr | Replaced_request
                | Native_present ->
                    1
              in
              let runtime = Runtime.create ~gameplan ~main_branch:main () in
              Runtime.update_orchestrator runtime (fun orch ->
                  let orch =
                    Orchestrator.set_pr_number orch pid (Pr_number.of_int 7)
                  in
                  if native then
                    let orch =
                      Orchestrator.set_base_branch orch pid
                        (Branch.of_string "stack-parent")
                    in
                    let orch =
                      Orchestrator.enqueue orch pid Operation_kind.Rebase
                    in
                    match scenario with
                    | Native_absent _ ->
                        Orchestrator.set_native_stack orch pid true
                    | Native_present | Valid | Stale_head | Stale_base
                    | Missing_head | Missing_base | Missing_identity
                    | Changed_pr | Replaced_request | Head_reversion
                    | Satisfied_conflict | Contradictory ->
                        orch
                  else orch);
              let current () =
                Runtime.read runtime (fun snap ->
                    Orchestrator.agent snap.Runtime.orchestrator pid)
              in
              let settled_operation =
                if scenario <> Satisfied_conflict then None
                else
                  let path = Filename.concat dir "linked" in
                  Git.run_git ~cwd:dir
                    [ "worktree"; "add"; "--quiet"; path; "patch" ];
                  Runtime.update_orchestrator runtime (fun orch ->
                      Orchestrator.set_worktree_path orch pid path);
                  let checkpoint =
                    Project_store.snapshot_path gameplan.Gameplan.project_name
                  in
                  Project_store.ensure_dir (Filename.dirname checkpoint);
                  let io =
                    Branch_reconcile_executor.make_io
                      ~process_mgr:(Eio.Stdenv.process_mgr env)
                      ~clock:(Eio.Stdenv.clock env) ~path
                  in
                  let result =
                    Branch_reconcile_runner.run ~runtime ~patch_id:pid
                      ~persist:(Persistence.save_snapshot ~path:checkpoint)
                      ~now:(fun () -> Eio.Time.now (Eio.Stdenv.clock env))
                      ~execute:(fun ~operation command ->
                        Branch_reconcile_executor.execute ~io
                          ~prefix:"refs/onton/reconcile/poller-context"
                          ~branch:"patch" ~operation command)
                      (Branch_reconcile.Request
                         Branch_reconcile.
                           {
                             base = "main";
                             policy = Rewrite;
                             purpose = Reconcile_base;
                           })
                  in
                  check "real Git reconciliation settles before polling"
                    (result = Branch_reconcile_runner.Idle);
                  Branch_reconcile.operation
                    (current ()).Patch_agent.branch_reconcile
              in
              let module Real =
                (val match provider with
                     | Sourcehut ->
                         let module S =
                           (val Sourcehut.make_with_builds
                                  ~read_builds:(fun () -> Ok [])
                                  ~net:(Eio.Stdenv.net env)
                                  ~clock:(Eio.Stdenv.clock env)
                                  ~process_mgr:(Eio.Stdenv.process_mgr env)
                                  ~token:"" ~owner:"fixture" ~repo:"fixture"
                                  ~repo_root:dir ~main_branch:main
                                  ~changes:
                                    [
                                      ( Some (Pr_number.of_int 7),
                                        Branch.of_string "patch",
                                        main );
                                    ])
                         in
                         (module S : Forge.S)
                     | Github ->
                         let module G =
                           (val Github.make ~net:(Eio.Stdenv.net env)
                                  ~clock:(Eio.Stdenv.clock env) ~token:""
                                  ~owner:"fixture" ~repo:"fixture"
                                  ~main_branch:main)
                         in
                         (module G : Forge.S))
              in
              let polled, signal_polled = Eio.Promise.create () in
              let never, _ = Eio.Promise.create () in
              let calls = ref 0 in
              let module Forge = struct
                include Real

                let pr_state number =
                  incr calls;
                  if !calls > cycles then (
                    Eio.Promise.resolve signal_polled ();
                    Eio.Promise.await never)
                  else
                    let snapshot =
                      get
                        (Persistence.load
                           ~path:
                             (Project_store.snapshot_path
                                gameplan.Gameplan.project_name))
                    in
                    let stored =
                      Orchestrator.agent snapshot.Runtime.orchestrator pid
                    in
                    check "poll request is durable before provider dispatch"
                      (Option.is_some
                         (Forge_observation.pending_request
                            (Branch_reconcile.forge_observations
                               stored.Patch_agent.branch_reconcile)));
                    if scenario = Head_reversion && !calls = 3 then (
                      check "stale metadata did not revert the controller head"
                        ((current ()).Patch_agent.head_oid = Some head);
                      Git.run_git ~cwd:remote
                        [ "update-ref"; "refs/heads/patch"; older_head; head ]);
                    let response =
                      match provider with
                      | Sourcehut -> Real.pr_state number
                      | Github -> (
                          if !calls = 2 && native then
                            check
                              "one absent stack response preserves native \
                               ownership"
                              (current ()).Patch_agent.native_stack;
                          let stack =
                            match scenario with
                            | Native_present -> "{\"id\":\"stack-1\"}"
                            | Native_absent _ | Valid | Stale_head | Stale_base
                            | Missing_head | Missing_base | Missing_identity
                            | Changed_pr | Replaced_request | Head_reversion
                            | Satisfied_conflict | Contradictory ->
                                "null"
                          in
                          let body =
                            Printf.sprintf
                              {|{"data":{"repository":{"pullRequest":{"number":7,"state":"OPEN","mergeable":"MERGEABLE","isDraft":true,"mergeStateStatus":"BLOCKED","headRefName":"patch","headRefOid":%S,"baseRefName":%S,"baseRefOid":%S,"headRepositoryOwner":{"login":"fixture"},"commits":{"nodes":[]},"reviewThreads":{"nodes":[]},"mergeCommit":null,"mergeQueueEntry":null,"stack":%s}}}}|}
                              (if scenario = Head_reversion && !calls >= 2 then
                                 older_head
                               else head)
                              (if native then "stack-parent" else "main")
                              base_sha stack
                          in
                          match Github.parse_response ~owner:"fixture" body with
                          | Ok state -> Ok state
                          | Error error -> failwith (Github.show_error error))
                    in
                    match response with
                    | Error error -> failwith (Real.show_error error)
                    | Ok state ->
                        let state =
                          match scenario with
                          | Valid | Native_present | Native_absent _ -> state
                          | Satisfied_conflict ->
                              {
                                state with
                                Pr_state.merge_state = Pr_state.Conflicting;
                                merge_ready = false;
                              }
                          | Contradictory ->
                              {
                                state with
                                Pr_state.merge_state =
                                  (if !calls = 1 then Pr_state.Conflicting
                                   else Pr_state.Mergeable);
                                merge_ready = !calls > 1;
                                is_draft = false;
                              }
                          | Head_reversion ->
                              {
                                state with
                                Pr_state.head_oid =
                                  Some
                                    (if !calls >= 2 then older_head else head);
                              }
                          | Stale_head ->
                              {
                                state with
                                Pr_state.head_oid = Some (String.make 40 'f');
                              }
                          | Stale_base ->
                              {
                                state with
                                Pr_state.base_oid = Some (String.make 40 'f');
                              }
                          | Missing_head ->
                              { state with Pr_state.head_oid = None }
                          | Missing_base ->
                              { state with Pr_state.base_oid = None }
                          | Missing_identity ->
                              { state with Pr_state.pr_number = None }
                          | Replaced_request ->
                              let[@warning "-42"] request :
                                  Forge_observation.request =
                                {
                                  Forge_observation.id = "replacement";
                                  pr_number = number;
                                }
                              in
                              ignore
                                (get
                                   (Forge_poll.prepare ~runtime
                                      ~persist:
                                        (Persistence.save_snapshot
                                           ~path:
                                             (Project_store.snapshot_path
                                                gameplan.Gameplan.project_name))
                                      [ (pid, current (), request) ]));
                              state
                          | Changed_pr ->
                              Runtime.update_orchestrator runtime (fun orch ->
                                  Orchestrator.set_pr_number orch pid
                                    (Pr_number.of_int 8));
                              state
                        in
                        Ok state
              end in
              let module W =
                (val Worktree.make ~fs:(Eio.Stdenv.fs env)
                       ~config:Worktree_lifecycle.git
                       ~clock:(Eio.Stdenv.clock env)
                       ~process_mgr:(Eio.Stdenv.process_mgr env)
                       ~repo_root:dir)
              in
              let event_path = Filename.concat dir "events.jsonl" in
              let module Env = struct
                let runtime = runtime
                let clock = Eio.Stdenv.clock env
                let fs = Eio.Stdenv.fs env
                let process_mgr = Eio.Stdenv.process_mgr env
                let worktree_mutex = Eio.Mutex.create ()
                let hook_mutex = Eio.Mutex.create ()
                let fetch_mutex = Eio.Mutex.create ()
                let user_config = { User_config.on_worktree_create = None }
                let project_name = gameplan.Gameplan.project_name
                let github_owner = "fixture"
                let github_repo = "fixture"
                let main_branch = main
                let poll_interval = 0.001
                let repo_root = dir
                let register_pr_number ~patch_id:_ ~pr_number:_ = ()
                let unregister_pr_number ~patch_id:_ = ()
                let findings_registry = Findings_registry.create ()
                let review_clients = []
                let event_log = Event_log.create ~path:event_path
                let branch_of _ = Branch.of_string "patch"
              end in
              let module Startup = struct
                let discover_pr ~branch:_ = failwith "unexpected PR rediscovery"
              end in
              let module Poller = Poller_fiber.Make (Forge) (W) (Env) in
              Telemetry_dispatch.with_sink ~sink:(Event_log.sink Env.event_log)
                (fun () ->
                  Eio.Time.with_timeout_exn Env.clock 15. (fun () ->
                      Eio.Fiber.first
                        (fun () -> Poller.run (module Startup))
                        (fun () -> Eio.Promise.await polled)));
              let agent = current () in
              (match scenario with
              | Head_reversion ->
                  check "directly confirmed head reversion reaches controller"
                    (agent.Patch_agent.head_oid = Some older_head)
              | Valid | Native_present | Native_absent _ | Satisfied_conflict
              | Contradictory ->
                  check "valid revision pair reaches patch controller"
                    (agent.Patch_agent.head_oid = Some head)
              | Stale_head | Stale_base | Missing_head | Missing_base
              | Missing_identity | Changed_pr | Replaced_request ->
                  check "stale response cannot change controller head"
                    (agent.Patch_agent.head_oid = None);
                  check "stale response cannot grant merge readiness"
                    (not agent.Patch_agent.merge_ready));
              (match scenario with
              | Satisfied_conflict ->
                  check "satisfied conflict does not dispatch duplicate repair"
                    ((not (Patch_agent.has_conflict agent))
                    && not
                         (List.mem Operation_kind.Merge_conflict
                            agent.Patch_agent.queue));
                  check "polling preserves the settled operation"
                    (Branch_reconcile.operation
                       agent.Patch_agent.branch_reconcile
                    = settled_operation);
                  check "satisfied conflict visibly waits for forge refresh"
                    (Base.String.is_substring
                       (In_channel.with_open_bin event_path In_channel.input_all)
                       ~substring:"waiting for forge mergeability refresh")
              | Contradictory ->
                  check
                    "mergeable metadata cannot erase unresolved conflict \
                     evidence"
                    (Patch_agent.has_conflict agent);
                  check "contradictory status cannot authorize merge"
                    (not (Patch_agent.is_approved agent ~main_branch:main));
                  check "positive metadata reaches the merge gate"
                    agent.Patch_agent.merge_ready;
                  check "conflict is the sole remaining approval blocker"
                    (Patch_agent.is_pr_present agent
                    && Patch_agent.forge_revision_pair_confirmed agent
                    && (not agent.Patch_agent.busy)
                    && (not
                          (Branch_reconcile.is_unsettled
                             agent.Patch_agent.branch_reconcile))
                    && Option.is_none
                         (Branch_reconcile.unobserved_publication
                            agent.Patch_agent.branch_reconcile)
                    && (not (Patch_agent.needs_intervention agent))
                    && (not agent.Patch_agent.is_draft)
                    && (not agent.Patch_agent.branch_blocked)
                    && agent.Patch_agent.base_branch = Some main)
              | Valid | Stale_head | Stale_base | Missing_head | Missing_base
              | Missing_identity | Changed_pr | Replaced_request
              | Head_reversion | Native_present | Native_absent _ ->
                  ());
              if native then (
                let expected =
                  match scenario with
                  | Native_absent 2 -> false
                  | Native_absent _ | Native_present | Valid | Stale_head
                  | Stale_base | Missing_head | Missing_base | Missing_identity
                  | Changed_pr | Replaced_request | Head_reversion
                  | Satisfied_conflict | Contradictory ->
                      true
                in
                check
                  "native stack ownership follows confirmed absence threshold"
                  (agent.Patch_agent.native_stack = expected);
                let actions =
                  Runtime.read runtime (fun snap ->
                      Patch_controller.plan_actions snap.Runtime.orchestrator
                        ~patches:[ patch ])
                in
                check
                  "native base mismatch blocks dispatch until stack absence is \
                   confirmed"
                  (if expected then actions = [] else actions <> []);
                let _, effects =
                  Runtime.read runtime (fun snap ->
                      Patch_controller.reconcile_all snap.Runtime.orchestrator
                        ~project_name:Env.project_name ~gameplan)
                in
                let changes_base =
                  List.exists
                    (function
                      | Patch_controller.Set_pr_base _ -> true
                      | Patch_controller.Set_pr_draft _ -> false)
                    effects
                in
                check
                  "native stack base is changed only after confirmed absence"
                  (changes_base = not expected));
              match scenario with
              | Valid | Native_present | Native_absent _ | Head_reversion
              | Satisfied_conflict | Contradictory ->
                  check "accepted poll emitted an observation event"
                    (Sys.file_exists event_path)
              | Stale_head | Stale_base | Missing_head | Missing_base
              | Missing_identity | Changed_pr | Replaced_request ->
                  ())))

let () =
  Eio_main.run (fun env ->
      List.iter
        (fun provider ->
          List.iter (run env provider)
            [
              Valid;
              Stale_head;
              Stale_base;
              Missing_head;
              Missing_base;
              Missing_identity;
              Changed_pr;
              Replaced_request;
              Head_reversion;
              Satisfied_conflict;
              Contradictory;
            ])
        [ Sourcehut; Github ];
      List.iter (run env Github)
        [ Native_present; Native_absent 1; Native_absent 2 ]);
  print_endline "poller runtime revision identity and delayed context: OK"

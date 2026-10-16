(* @archlint.module test
   @archlint.domain poller-fiber *)

(* F-001 review fixture: real local Git confirmations, counted containment
   effects, and stable queued repair across unrelated PR observations. *)
open Onton
open Onton_core
open Base
open Types
module Git = Onton_test_support.Git_env

let fail message = failwith message
let check message condition = if not condition then fail message
let main = Branch.of_string "main"

let patch id dependencies : Patch.t =
  let id = Patch_id.of_string id in
  Patch.
    {
      id;
      title = Patch_id.to_string id;
      description = "";
      branch = Branch.of_string (Patch_id.to_string id);
      dependencies;
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

let make_gameplan extra_count =
  let a = Patch_id.of_string "a" in
  let b = Patch_id.of_string "b" in
  let extras =
    List.init extra_count ~f:(fun i ->
        Patch_id.of_string ("x" ^ Int.to_string i))
  in
  let patches =
    [ patch "a" []; patch "b" []; patch "c" [ a; b ] ]
    @ List.map extras ~f:(fun id -> patch (Patch_id.to_string id) [])
  in
  ( Gameplan.
      {
        project_name = "forge-reconcile-probe";
        repo_owner = "";
        repo_name = "";
        problem_statement = "";
        architecture_design = None;
        solution_summary = "";
        final_state_spec = "";
        patches;
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
      },
    extras )

let pr_state ~number ~branch ~base ~head_sha ~base_sha : Pr_state.t =
  {
    Pr_state.pr_number = Some number;
    status = Open;
    is_draft = true;
    merge_state = Mergeable;
    merge_ready = true;
    merge_ready_divergence = None;
    review_decision = None;
    check_status = Passing;
    ci_checks = [];
    ci_checks_truncated = false;
    comments = [];
    unresolved_comment_count = 0;
    findings = [];
    node_id = None;
    merge_queue_required = false;
    merge_queue_entry = None;
    native_stack = false;
    head_branch = Some (Branch.of_string branch);
    base_oid = Some base_sha;
    head_oid = Some head_sha;
    merge_commit_sha = None;
    base_branch = Some (Branch.of_string base);
    is_fork = false;
  }

type ancestry = Present | Absent | Unavailable

let run_case env ~request_count ~ancestry ~skip =
  let slow = request_count = 3 && not skip in
  let gameplan, extras = make_gameplan (Int.max 0 (request_count - 1)) in
  Git.with_temp_repo (fun remote ->
      Git.run_git ~cwd:remote [ "commit"; "--allow-empty"; "-qm"; "base" ];
      let main_sha = Git.git_capture ~cwd:remote [ "rev-parse"; "HEAD" ] in
      let branch_names =
        [ "a"; "b"; "c" ] @ List.map extras ~f:(fun id -> Patch_id.to_string id)
      in
      List.iter branch_names ~f:(fun branch ->
          Git.run_git ~cwd:remote [ "branch"; branch; main_sha ]);
      Git.with_temp_repo (fun dir ->
          let old_data = Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" in
          Unix.putenv "ONTON_DATA_DIR" (Stdlib.Filename.concat dir "state");
          Stdlib.Fun.protect
            ~finally:(fun () ->
              Unix.putenv "ONTON_DATA_DIR" (Option.value old_data ~default:""))
            (fun () ->
              Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
              Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
              Git.run_git ~cwd:dir [ "checkout"; "-qb"; "main"; "origin/main" ];
              let runtime = Runtime.create ~gameplan ~main_branch:main () in
              let sha_a = Git.git_capture ~cwd:remote [ "rev-parse"; "a" ] in
              Runtime.update_orchestrator runtime (fun orch ->
                  let orch =
                    Orchestrator.mark_merged orch (Patch_id.of_string "a")
                  in
                  Orchestrator.set_merge_commit_sha orch
                    (Patch_id.of_string "a") (Some sha_a));
              let pr_agents =
                if request_count = 0 then []
                else Patch_id.of_string "c" :: extras
              in
              let prs =
                List.mapi pr_agents ~f:(fun index pid ->
                    let pr = Pr_number.of_int (101 + index) in
                    (pid, pr))
              in
              Runtime.update_orchestrator runtime (fun orch ->
                  List.fold prs ~init:orch ~f:(fun orch (pid, pr) ->
                      Orchestrator.set_pr_number orch pid pr));
              (* A stable pending-repair state prevents the first applied PR
                 from changing sibling generations when reconciliation notices
                 missing integration receipts. The fixture's base refs already
                 contain the merged dependency commit. *)
              Runtime.update_orchestrator runtime (fun orch ->
                  List.fold prs ~init:orch ~f:(fun orch (pid, _) ->
                      Orchestrator.enqueue orch pid Operation_kind.Rebase));
              let state_by_pr = Hashtbl.create (module Int) in
              List.iter prs ~f:(fun (pid, pr) ->
                  let branch = Patch_id.to_string pid in
                  let base =
                    if Patch_id.equal pid (Patch_id.of_string "c") then "b"
                    else "main"
                  in
                  let head_sha =
                    Git.git_capture ~cwd:remote [ "rev-parse"; branch ]
                  in
                  let base_sha =
                    Git.git_capture ~cwd:remote [ "rev-parse"; base ]
                  in
                  Hashtbl.set state_by_pr ~key:(Pr_number.to_int pr)
                    ~data:
                      (pr_state ~number:pr ~branch ~base ~head_sha ~base_sha));
              let ancestor_calls = ref 0 in
              let oracle_errors = ref [] in
              let expected_real_root = Unix.realpath dir in
              let module Real_worktree =
                (val Worktree.make ~fs:(Eio.Stdenv.fs env)
                       ~config:Worktree_lifecycle.git
                       ~clock:(Eio.Stdenv.clock env)
                       ~process_mgr:(Eio.Stdenv.process_mgr env)
                       ~repo_root:dir)
              in
              let module W = struct
                include Real_worktree

                let is_ancestor ~path ~ancestor ~descendant =
                  ancestor_calls := !ancestor_calls + 1;
                  (match try Some (Unix.realpath path) with _ -> None with
                  | Some actual when String.equal actual expected_real_root ->
                      ()
                  | Some actual ->
                      oracle_errors :=
                        ("unexpected repository root: " ^ actual)
                        :: !oracle_errors
                  | None ->
                      oracle_errors :=
                        "cannot resolve repository root" :: !oracle_errors);
                  if
                    not
                      (Int.equal (String.length ancestor) 40
                      || String.equal ancestor "main")
                  then
                    oracle_errors :=
                      "malformed fixture merge SHA" :: !oracle_errors;
                  if not (String.equal descendant "b") then
                    oracle_errors :=
                      ("unexpected ancestry descendant: " ^ descendant)
                      :: !oracle_errors;
                  match ancestry with
                  | Present -> true
                  | Absent -> false
                  | Unavailable -> failwith "injected ancestry probe failure"
              end in
              let module Real_forge =
                (val Github.make ~net:(Eio.Stdenv.net env)
                       ~clock:(Eio.Stdenv.clock env) ~token:"" ~owner:"fixture"
                       ~repo:"fixture" ~main_branch:main)
              in
              let polled, signal_polled = Eio.Promise.create () in
              let release_slow, signal_slow = Eio.Promise.create () in
              let calls = ref 0 in
              let module Forge = struct
                include Real_forge

                let pr_state number =
                  calls := !calls + 1;
                  if !calls = request_count then
                    Eio.Promise.resolve signal_polled ();
                  if slow && Pr_number.to_int number = 100 + request_count then
                    Eio.Promise.await release_slow;
                  if !calls > request_count then
                    fail "poller dispatched more than one batch"
                  else
                    match
                      Hashtbl.find state_by_pr (Pr_number.to_int number)
                    with
                    | Some state -> Ok { state with Pr_state.is_fork = skip }
                    | None -> fail "unexpected fake Forge PR number"
              end in
              let event_path = Stdlib.Filename.concat dir "events.jsonl" in
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
                let poll_interval = 60.
                let repo_root = dir
                let register_pr_number ~patch_id:_ ~pr_number:_ = ()
                let unregister_pr_number ~patch_id:_ = ()
                let findings_registry = Findings_registry.create ()
                let review_clients = []
                let event_log = Event_log.create ~path:event_path
                let branch_of id = Branch.of_string (Patch_id.to_string id)
              end in
              let module Startup = struct
                let discover_pr ~branch:_ = fail "unexpected PR rediscovery"
              end in
              let module Poller = Poller_fiber.Make (Forge) (W) (Env) in
              let observations () =
                Runtime.read runtime (fun snap ->
                    List.map prs ~f:(fun (pid, _) ->
                        let agent =
                          Orchestrator.agent snap.Runtime.orchestrator pid
                        in
                        let history =
                          Branch_reconcile.forge_observations
                            agent.Patch_agent.branch_reconcile
                        in
                        ( Patch_id.to_string pid,
                          Patch_agent.forge_revision_pair_confirmed agent,
                          Yojson.Safe.to_string
                            (Forge_observation.yojson_of_history history),
                          Yojson.Safe.to_string
                            (Forge_observation.yojson_of_scope
                               (Patch_agent.forge_scope agent)) )))
              in
              let all_applied () =
                List.for_all (observations ()) ~f:(fun (_, confirmed, _, _) ->
                    confirmed)
              in
              let all_resolved () =
                Runtime.read runtime (fun snap ->
                    List.for_all prs ~f:(fun (pid, _) ->
                        let agent =
                          Orchestrator.agent snap.Runtime.orchestrator pid
                        in
                        Option.is_none
                          (Forge_observation.pending_request
                             (Branch_reconcile.forge_observations
                                agent.Patch_agent.branch_reconcile))))
              in
              (try
                 Eio.Time.with_timeout_exn Env.clock 15. (fun () ->
                     Eio.Fiber.first
                       (fun () -> Poller.run (module Startup))
                       (fun () ->
                         if request_count > 0 then Eio.Promise.await polled;
                         if slow then (
                           let rec wait_for_fast () =
                             let confirmed =
                               Runtime.read runtime (fun snap ->
                                   Patch_agent.forge_revision_pair_confirmed
                                     (Orchestrator.agent
                                        snap.Runtime.orchestrator
                                        (Patch_id.of_string "c")))
                             in
                             if not confirmed then (
                               Eio.Time.sleep Env.clock 0.001;
                               wait_for_fast ())
                           in
                           wait_for_fast ();
                           check
                             "fast response applies before slow provider \
                              completes"
                             (not (all_resolved ()));
                           check
                             "unfinished batch performs no repeated global \
                              probes"
                             (!ancestor_calls = 0);
                           Runtime.read runtime (fun snap ->
                               check
                                 "fan-in readiness stays fail-closed until \
                                  global verification"
                                 (not
                                    (Orchestrator.agent
                                       snap.Runtime.orchestrator
                                       (Patch_id.of_string "c"))
                                      .Patch_agent.base_contains_merged_siblings));
                           Eio.Promise.resolve signal_slow ());
                         let rec wait_for_resolution () =
                           if all_resolved () && !ancestor_calls > 0 then ()
                           else (
                             Eio.Time.sleep Env.clock 0.001;
                             wait_for_resolution ())
                         in
                         wait_for_resolution ()))
               with Eio.Time.Timeout ->
                 Stdlib.Printf.eprintf "timeout calls=%d resolved=%b\n%!" !calls
                   (all_resolved ());
                 List.iter (observations ())
                   ~f:(fun (pid, confirmed, history, scope) ->
                     Stdlib.Printf.eprintf
                       "PR %s confirmed=%b current_scope=%s history=%s\n%!" pid
                       confirmed scope history);
                 raise Eio.Time.Timeout);
              check "poller fetched the complete request batch"
                (Int.equal !calls request_count);
              if not skip then
                check "all exact provider revision pairs were accepted"
                  (all_applied ());
              check
                ("mocked ancestry adapter observed invalid calls: "
                ^ String.concat ~sep:"; " (List.rev !oracle_errors))
                (List.is_empty !oracle_errors);
              check
                "one global containment pass regardless of unrelated PR count"
                (Int.equal !ancestor_calls
                   (match ancestry with
                   | Present -> 1
                   | Absent | Unavailable -> 4));
              Runtime.read runtime (fun snap ->
                  let orch = snap.Runtime.orchestrator in
                  let child =
                    Orchestrator.agent orch (Patch_id.of_string "c")
                  in
                  check
                    "negative and failed probes keep containment fail-closed"
                    (Bool.equal child.Patch_agent.base_contains_merged_siblings
                       (match ancestry with
                       | Present -> true
                       | Absent | Unavailable -> false));
                  List.iter (Orchestrator.all_agents orch) ~f:(fun agent ->
                      check
                        "polling cannot dispatch implementation or bypass \
                         owned repair"
                        ((not agent.Patch_agent.busy)
                        && List.for_all agent.Patch_agent.queue
                             ~f:(Operation_kind.equal Operation_kind.Rebase))));
              Stdlib.Printf.printf
                "requests=%d accepted_pairs=%d ancestry_probes=%d\n%!"
                request_count
                (List.length
                   (List.filter (observations ())
                      ~f:(fun (_, confirmed, _, _) -> confirmed)))
                !ancestor_calls)))

let () =
  Eio_main.run (fun env ->
      List.iter [ Present; Absent; Unavailable ] ~f:(fun ancestry ->
          List.iter [ false; true ] ~f:(fun skip ->
              List.iter [ 0; 1; 3; 17 ] ~f:(fun request_count ->
                  run_case env ~request_count ~ancestry ~skip))));
  Stdlib.print_endline
    "poller batch containment, fail-closed and skipped-response controls: OK"

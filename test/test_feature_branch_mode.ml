(* @archlint.module test
   @archlint.domain orchestrator *)
open Base
open Onton
open Onton_core
open Types

let id n = Patch_id.of_string (Int.to_string n)
let branch n = Branch.of_string ("patch-" ^ Int.to_string n)
let main = Branch.of_string "main"

let patch n deps =
  Patch.
    {
      id = id n;
      branch = branch n;
      dependencies = List.map deps ~f:id;
      title = "patch";
      description = "";
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

let patches = [ patch 1 []; patch 2 [ 1 ]; patch 3 [ 1 ]; patch 4 [ 3 ] ]
let graph = Graph.of_patches patches

let mode =
  match Execution_mode.infer graph with Ok m -> m | Error e -> failwith e

let gp =
  Gameplan.
    {
      project_name = "feature-test";
      repo_owner = "test";
      repo_name = "test";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches;
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
      functional_changes = [];
      context_resources = [];
      publication = None;
      reachability_traces = [];
    }

let check label value = if not value then failwith label

let ready_branch orch n =
  Orchestrator.mark_branch_published orch (id n) |> fun o ->
  Orchestrator.set_pr_body_delivered o (id n) true |> fun o ->
  Orchestrator.set_automerge_enabled o (id n) true |> fun o ->
  Orchestrator.set_base_branch o (id n) (branch 1) |> fun o ->
  fst (Orchestrator.apply_rebase_result o (id n) Worktree.Noop (branch 1))
  |> fun o ->
  Patch_controller.apply_branch_observation o (id n)
    ~head_sha:("head" ^ Int.to_string n)
    ~checks:
      [
        Ci_check.
          {
            name = "test";
            conclusion = "success";
            details_url = None;
            description = None;
            started_at = None;
            app_id = None;
            check_suite_id = None;
            id = Some n;
          };
      ]

let ready_root orch =
  Orchestrator.set_pr_number orch (id 1) (Pr_number.of_int 1) |> fun o ->
  Orchestrator.set_is_draft o (id 1) true |> fun o ->
  Orchestrator.set_base_branch o (id 1) main |> fun o ->
  fst (Orchestrator.apply_rebase_result o (id 1) Worktree.Noop main) |> fun o ->
  Orchestrator.set_pr_body_delivered o (id 1) true |> fun o ->
  Orchestrator.set_checks_passing o (id 1) true |> fun o ->
  Orchestrator.set_head_oid o (id 1) (Some "root-head") |> fun o ->
  Orchestrator.set_mergeability_unknown o (id 1) false

let fresh () =
  Orchestrator.create ~patches ~main_branch:main |> fun o ->
  Orchestrator.set_execution_mode o mode |> ready_root

let publication_lifecycle () =
  let get = function Ok x -> x | Error e -> failwith e in
  let publication =
    get
      (Gameplan_publication.create ~directory:"gameplans"
         ~project_name:gp.Gameplan.project_name ~yaml:false ~content:"{}")
  in
  let gameplan = get (Gameplan.publish gp publication) in
  let mode = get (Execution_mode.infer_gameplan gameplan) in
  let runtime = Runtime.create ~gameplan ~main_branch:main () in
  Runtime.update_orchestrator runtime (fun o ->
      Orchestrator.set_execution_mode o mode);
  let orch () = Runtime.read runtime (fun s -> s.Runtime.orchestrator) in
  let starts () =
    Patch_controller.plan_actions (orch ()) ~patches:gameplan.Gameplan.patches
    |> List.filter_map ~f:(function
      | Orchestrator.Start (p, b) -> Some (p, b)
      | Orchestrator.Respond _ | Orchestrator.Rebase _ -> None)
  in
  let check_starts label expected =
    check label
      (List.equal
         (fun (p, b) (p', b') -> Patch_id.equal p p' && Branch.equal b b')
         (starts ()) expected)
  in
  let resume () =
    let snapshot =
      get
        (Persistence.snapshot_of_yojson
           (Runtime.read runtime Persistence.snapshot_to_yojson))
    in
    let mode =
      get
        (Execution_mode.restore_gameplan snapshot.Runtime.gameplan
           (Some (id 1)))
    in
    Runtime.update_orchestrator runtime (fun _ ->
        Orchestrator.set_execution_mode snapshot.Runtime.orchestrator mode)
  in
  check_starts "only publication starts against main" [ (id 0, main) ];
  Runtime.update_orchestrator runtime (fun o ->
      Orchestrator.set_pr_number o (id 0) (Pr_number.of_int 10) |> fun o ->
      Orchestrator.set_base_branch o (id 0) main |> fun o ->
      fst (Orchestrator.apply_rebase_result o (id 0) Worktree.Noop main)
      |> fun o ->
      Orchestrator.set_pr_body_delivered o (id 0) true |> fun o ->
      Orchestrator.set_checks_passing o (id 0) true);
  check "publication ready for review before implementation starts"
    (Patch_controller.ready_for_review (orch ()) (id 0));
  check "publication excluded from feature descendants"
    (not (Orchestrator.is_feature_descendant (orch ()) (id 0)));
  check_starts "ready publication cannot release implementation" [];
  Runtime.update_orchestrator runtime (fun o ->
      Orchestrator.set_worktree_path o (id 1) "/existing-root" |> fun o ->
      Orchestrator.fire o (Orchestrator.Start (id 1, main)));
  check "materialized root cannot bypass publication merge"
    (not (Orchestrator.agent (orch ()) (id 1)).Patch_agent.busy);
  resume ();
  check_starts "resume before publication merge remains blocked" [];
  Runtime.update_orchestrator runtime (fun o ->
      Orchestrator.mark_merged o (id 0));
  check_starts "publication merge releases original root against main"
    [ (id 1, main) ];
  resume ();
  check_starts "resume after publication merge preserves root" [ (id 1, main) ];
  Runtime.update_orchestrator runtime ready_root;
  check "implementation root remains draft during construction"
    (not (Patch_controller.ready_for_review (orch ()) (id 1)));
  check_starts "children start beneath original implementation root"
    [ (id 2, branch 1); (id 3, branch 1) ];
  Runtime.update_orchestrator runtime (fun o -> ready_branch o 2);
  let _, decisions = Patch_controller.reconcile_automerge (orch ()) ~now:0. in
  check "descendant integrates into implementation root rather than publication"
    (List.exists decisions ~f:(function
      | Patch_controller.Git_integrate { root; merge_patch_id; _ } ->
          Patch_id.equal root (id 1) && Patch_id.equal merge_patch_id (id 2)
      | Patch_controller.Github_merge _ -> false));
  Runtime.update_orchestrator runtime (fun o ->
      List.fold [ 2; 3; 4 ] ~init:o ~f:(fun o n ->
          Orchestrator.mark_merged o (id n)));
  check "implementation root waits for cumulative PR body publication"
    (not (Patch_controller.ready_for_review (orch ()) (id 1)));
  Runtime.update_orchestrator runtime (fun o ->
      Orchestrator.acknowledge_pr_body_refresh o (id 1)
        ~publication:(Orchestrator.agent o (id 1)));
  check "implementation root promotes once descendants complete"
    (Patch_controller.ready_for_review (orch ()) (id 1))

let () =
  Eio_main.run (fun _ ->
      publication_lifecycle ();
      let orch = fresh () in
      let actions = Patch_controller.plan_actions orch ~patches in
      check "children start on draft root after CI"
        (List.exists actions ~f:(function
          | Orchestrator.Start (p, b) ->
              Patch_id.equal p (id 2) && Branch.equal b (branch 1)
          | Orchestrator.Respond _ | Orchestrator.Rebase _ -> false));
      check "root waits for descendants"
        (not (Patch_controller.ready_for_review orch (id 1)));
      let orch = ready_branch orch 2 in
      check "published descendant has no PR"
        (not (Patch_agent.has_pr (Orchestrator.agent orch (id 2))));
      let counted =
        Orchestrator.increment_ci_failure_count orch (id 2) |> fun o ->
        Orchestrator.increment_ci_failure_count o (id 2)
      in
      let reset =
        Patch_controller.apply_branch_observation counted (id 2)
          ~head_sha:"head2"
          ~checks:
            [
              Ci_check.
                {
                  name = "test";
                  conclusion = "success";
                  details_url = None;
                  description = None;
                  started_at = None;
                  app_id = None;
                  check_suite_id = None;
                  id = Some 2;
                };
            ]
      in
      check "passing branch checks reset CI failure count"
        (Int.equal
           (Orchestrator.agent reset (id 2)).Patch_agent.ci_failure_count 0);
      check "feature mode rejects ad-hoc PRs"
        (Option.is_none
           (Orchestrator.find_agent
              (Orchestrator.add_agent orch ~patch_id:(id 99) ~branch:(branch 99)
                 ~base_branch:(branch 1) ~pr_number:(Pr_number.of_int 99))
              (id 99)));
      check "missing feature root keeps construction predicate total"
        (Orchestrator.construction_open (Orchestrator.remove_agent orch (id 1)));
      let awaiting_notes =
        Orchestrator.set_pr_body_delivered orch (id 2) false
      in
      check "descendant waits for implementation notes before integration"
        (not (Patch_controller.is_integration_candidate awaiting_notes (id 2)));
      let with_notes_demand, _ =
        Patch_controller.reconcile_all awaiting_notes
          ~project_name:"feature-test" ~gameplan:gp
      in
      check "PR-less descendant gets notes demand"
        (List.mem
           (Orchestrator.agent with_notes_demand (id 2)).Patch_agent.queue
           Operation_kind.Pr_body ~equal:Operation_kind.equal);
      check "PR-less descendant can dispatch notes"
        (List.exists (Patch_controller.plan_actions with_notes_demand ~patches)
           ~f:(function
          | Orchestrator.Respond (p, kind) ->
              Patch_id.equal p (id 2)
              && Operation_kind.equal kind Operation_kind.Pr_body
          | Orchestrator.Start _ | Orchestrator.Rebase _ -> false));
      let claimed, decisions =
        Patch_controller.reconcile_automerge orch ~now:0.
      in
      check "integration immediate and carries checked head"
        (List.exists decisions ~f:(function
          | Patch_controller.Git_integrate { root; head_sha; merge_patch_id } ->
              Patch_id.equal root (id 1)
              && Patch_id.equal merge_patch_id (id 2)
              && String.equal head_sha "head2"
          | Patch_controller.Github_merge _ -> false));
      check "claimed branch cannot dispatch work"
        (not
           (List.exists
              (Patch_controller.plan_actions
                 (Orchestrator.enqueue claimed (id 2) Operation_kind.Human)
                 ~patches)
              ~f:(function
                | Orchestrator.Respond (p, _) -> Patch_id.equal p (id 2)
                | Orchestrator.Start _ | Orchestrator.Rebase _ -> false)));
      let paused = Orchestrator.set_automerge_enabled orch (id 2) false in
      check "toggle pauses integration"
        (List.is_empty
           (snd (Patch_controller.reconcile_automerge paused ~now:0.)));
      let failed =
        Patch_controller.apply_automerge_failure claimed ~automerge_timeout:10.
          ~now:0. (id 2)
      in
      check "failure backoff"
        (List.is_empty
           (snd (Patch_controller.reconcile_automerge failed ~now:9.)));
      check "retry after backoff"
        (not
           (List.is_empty
              (snd (Patch_controller.reconcile_automerge failed ~now:10.))));
      let orch =
        ready_branch orch 3 |> fun o -> Orchestrator.mark_merged o (id 3)
      in
      check "grandchild stops at root"
        (Option.equal Branch.equal
           (Orchestrator.expected_base orch (id 4))
           (Some (branch 1)));
      let complete =
        List.fold [ 2; 3; 4 ] ~init:orch ~f:(fun o n ->
            Orchestrator.mark_merged o (id n))
      in
      check
        "integration demands a fresh root body while preserving delivered notes"
        ((Orchestrator.agent complete (id 1)).Patch_agent.pr_body_refresh
           .Patch_agent.pending
       && (Orchestrator.agent complete (id 1)).Patch_agent.pr_body_delivered);
      let refreshed, _ =
        Patch_controller.reconcile_all complete ~project_name:"feature-test"
          ~gameplan:gp
      in
      check "root body refresh queued after integration"
        (List.mem (Orchestrator.agent refreshed (id 1)).Patch_agent.queue
           Operation_kind.Pr_body ~equal:Operation_kind.equal);
      check "stale body prevents promotion"
        (not (Patch_controller.ready_for_review complete (id 1)));
      let dirty_rt = Runtime.create ~gameplan:gp ~main_branch:main () in
      Runtime.update_orchestrator dirty_rt (fun _ -> complete);
      let restored_dirty =
        match
          Persistence.snapshot_of_yojson
            (Runtime.read dirty_rt Persistence.snapshot_to_yojson)
        with
        | Ok snap ->
            Orchestrator.set_execution_mode snap.Runtime.orchestrator mode
        | Error e -> failwith e
      in
      check "pending body refresh survives restart"
        ((Orchestrator.agent restored_dirty (id 1)).Patch_agent.pr_body_refresh
           .Patch_agent.pending
        && not (Patch_controller.ready_for_review restored_dirty (id 1)));
      let rec legacy_snapshot = function
        | `Assoc fields ->
            `Assoc
              (List.filter_map fields ~f:(fun (key, value) ->
                   if
                     String.equal key "pr_body_refresh_pending"
                     || String.equal key "pr_body_refresh_version"
                   then None
                   else Some (key, legacy_snapshot value)))
        | `List values -> `List (List.map values ~f:legacy_snapshot)
        | value -> value
      in
      let legacy =
        match
          Persistence.snapshot_of_yojson
            (legacy_snapshot
               (Runtime.read dirty_rt Persistence.snapshot_to_yojson))
        with
        | Ok snap ->
            let mainline, _ =
              Patch_controller.reconcile_all snap.Runtime.orchestrator
                ~project_name:"feature-test" ~gameplan:gp
            in
            check "legacy migration does not refresh ordinary mainline PRs"
              (not
                 (List.mem
                    (Orchestrator.agent mainline (id 1)).Patch_agent.queue
                    Operation_kind.Pr_body ~equal:Operation_kind.equal));
            ready_root
              (Orchestrator.set_execution_mode snap.Runtime.orchestrator mode)
        | Error e -> failwith e
      in
      check "legacy merged descendants demand publication after restart"
        ((Orchestrator.agent legacy (id 1)).Patch_agent.pr_body_refresh
           .Patch_agent.pending
       && (Orchestrator.agent legacy (id 1)).Patch_agent.pr_body_delivered
        && not (Patch_controller.ready_for_review legacy (id 1)));
      let legacy_published =
        Orchestrator.acknowledge_pr_body_refresh legacy (id 1)
          ~publication:(Orchestrator.agent legacy (id 1))
      in
      check "publishing migrated cumulative body releases promotion"
        (Patch_controller.ready_for_review legacy_published (id 1));
      let failed_refresh =
        Orchestrator.apply_respond_outcome
          (Orchestrator.fire refreshed
             (Orchestrator.Respond (id 1, Operation_kind.Pr_body)))
          (id 1) Operation_kind.Pr_body Orchestrator.Respond_pr_body_miss
      in
      check "failed refresh completes its in-flight response"
        (not (Orchestrator.agent failed_refresh (id 1)).Patch_agent.busy);
      let retry, _ =
        Patch_controller.reconcile_all failed_refresh
          ~project_name:"feature-test" ~gameplan:gp
      in
      check "failed refresh retries without losing delivered notes"
        ((Orchestrator.agent retry (id 1)).Patch_agent.pr_body_delivered
        && List.mem (Orchestrator.agent retry (id 1)).Patch_agent.queue
             Operation_kind.Pr_body ~equal:Operation_kind.equal
        && not (Patch_controller.ready_for_review retry (id 1)));
      let complete =
        Orchestrator.acknowledge_pr_body_refresh complete (id 1)
          ~publication:(Orchestrator.agent complete (id 1))
      in
      check "duplicate merge observations do not demand another refresh"
        (not
           (Orchestrator.agent
              (Orchestrator.mark_merged complete (id 2))
              (id 1))
             .Patch_agent.pr_body_refresh
             .Patch_agent.pending);
      check "root ready when construction complete"
        (Patch_controller.ready_for_review complete (id 1));
      check "old root CI invalidated"
        (not
           (Patch_controller.ready_for_review
              (Orchestrator.invalidate_root_readiness complete)
              (id 1)));
      let awaiting =
        Orchestrator.invalidate_root_readiness complete |> fun o ->
        Orchestrator.set_expected_remote_head_oid o (id 1) (Some "final-head")
      in
      let observe head =
        Patch_controller.
          {
            poll_result =
              Poller.
                {
                  queue = [];
                  merged = false;
                  closed = false;
                  is_draft = true;
                  merge_state = Pr_state.Mergeable;
                  merge_ready = true;
                  head_oid = Some head;
                  review_decision = None;
                  unresolved_comment_count = 0;
                  merge_queue_required = false;
                  merge_queue_entry = None;
                  checks_passing = true;
                  ci_checks = [];
                  merge_commit_sha = None;
                };
            base_branch = Some main;
            native_stack = false;
            branch_in_root = false;
            worktree_path = None;
          }
      in
      let stale, _, _ =
        Patch_controller.apply_poll_result awaiting (id 1) (observe "root-head")
      in
      check "pre-push root CI cannot promote"
        (not (Patch_controller.ready_for_review stale (id 1)));
      check "pre-push observation retains publication marker"
        (Option.equal String.equal
           (Orchestrator.agent stale (id 1))
             .Patch_agent.expected_remote_head_oid (Some "final-head"));
      let distinct, _, _ =
        Patch_controller.apply_poll_result awaiting (id 1)
          (observe "middle-head")
      in
      check "distinct known root head settles publication"
        (Option.equal String.equal
           (Orchestrator.agent distinct (id 1)).Patch_agent.head_oid
           (Some "middle-head")
        && Option.is_none
             (Orchestrator.agent distinct (id 1))
               .Patch_agent.expected_remote_head_oid);
      let publication_state = ref stale in
      let update f = publication_state := f !publication_state in
      ignore
        (Push_publication.with_pending_push ~update ~patch_id:(id 1)
           ~local_sha:(Some "final-head") (fun () -> Worktree.Push_up_to_date)
          : Worktree.push_result);
      check "root no-op publication still waits for exact head"
        (Option.equal String.equal
           (Orchestrator.agent !publication_state (id 1))
             .Patch_agent.expected_remote_head_oid (Some "final-head"));
      ignore
        (Push_publication.with_pending_push ~update ~patch_id:(id 1)
           ~local_sha:(Some "unpublished-head") (fun () ->
             Worktree.Push_error "transport")
          : Worktree.push_result);
      check "failed root push preserves previous published head"
        (Option.equal String.equal
           (Orchestrator.agent !publication_state (id 1))
             .Patch_agent.expected_remote_head_oid (Some "final-head"));
      let observed, _, _ =
        Patch_controller.apply_poll_result stale (id 1) (observe "final-head")
      in
      check "current published root CI allows promotion"
        (Patch_controller.ready_for_review observed (id 1));
      check "feedback blocks promotion"
        (not
           (Patch_controller.ready_for_review
              (Orchestrator.enqueue complete (id 1) Operation_kind.Human)
              (id 1)));
      let claimed = Orchestrator.claim_promotion complete in
      check "promotion freezes additions"
        (not (Orchestrator.additions_allowed claimed ~dependencies:[ id 1 ]));
      check "failed claim reopens"
        (Orchestrator.additions_allowed
           (Orchestrator.release_promotion claimed)
           ~dependencies:[ id 1 ]);
      let persisted_rt = Runtime.create ~gameplan:gp ~main_branch:main () in
      Runtime.update_orchestrator persisted_rt (fun _ -> claimed);
      let json = Runtime.read persisted_rt Persistence.snapshot_to_yojson in
      let restored =
        match Persistence.snapshot_of_yojson json with
        | Ok snap -> snap.Runtime.orchestrator
        | Error e -> failwith e
      in
      check "promotion claim persisted"
        (Orchestrator.promotion_claimed restored);
      check "branch publication persisted"
        (Orchestrator.agent restored (id 2)).Patch_agent.branch_published;
      let rt = Runtime.create ~gameplan:gp ~main_branch:main () in
      Runtime.update_orchestrator rt (fun _ -> fresh ());
      check "unrooted additions refused"
        (Result.is_error
           (Runtime.add_patch rt ~title:"bad" ~description:"" ~dependencies:[]));
      check "rooted additions accepted"
        (Result.is_ok
           (Runtime.add_patch rt ~title:"good" ~description:""
              ~dependencies:[ id 1 ]));
      Runtime.update_orchestrator rt Orchestrator.claim_promotion;
      check "claimed additions refused"
        (Result.is_error
           (Runtime.add_patch rt ~title:"late" ~description:""
              ~dependencies:[ id 1 ]));
      let trace = ref [] in
      Eio.Fiber.both
        (fun () ->
          Runtime.with_patch_write rt ~patch_id:(id 1) (fun () ->
              trace := "session-start" :: !trace;
              Eio.Fiber.yield ();
              trace := "session-end" :: !trace))
        (fun () ->
          Runtime.with_root_write rt (fun () ->
              trace := "integration" :: !trace));
      check "root session and integration serialized"
        (List.equal String.equal !trace
           [ "integration"; "session-end"; "session-start" ]
        || List.equal String.equal !trace
             [ "session-end"; "session-start"; "integration" ]);
      Stdlib.print_endline "PASS feature branch decisions and lifecycle")

let () =
  let initial =
    Orchestrator.create ~patches ~main_branch:main |> fun o ->
    Orchestrator.set_execution_mode o mode |> ready_root |> fun o ->
    ready_branch o 2 |> fun o ->
    ready_branch o 3 |> fun o -> Orchestrator.mark_merged o (id 2)
  in
  let queued, _ =
    Patch_controller.reconcile_all initial ~project_name:"feature-test"
      ~gameplan:gp
  in
  let running =
    Orchestrator.fire queued
      (Orchestrator.Respond (id 1, Operation_kind.Pr_body))
  in
  check "publication is in-flight"
    (Orchestrator.agent running (id 1)).Patch_agent.busy;
  let publication = Orchestrator.agent running (id 1) in
  let raced = Orchestrator.mark_merged running (id 3) in
  let finished =
    Orchestrator.acknowledge_pr_body_refresh raced (id 1) ~publication
    |> fun o ->
    Orchestrator.apply_respond_outcome o (id 1) Operation_kind.Pr_body
      Orchestrator.Respond_ok
  in
  check "older publication preserves newer integration demand"
    ((Orchestrator.agent finished (id 1)).Patch_agent.pr_body_refresh
       .Patch_agent.pending
    && (not (Orchestrator.agent finished (id 1)).Patch_agent.busy)
    && not (Patch_controller.ready_for_review finished (id 1)));
  let queued, _ =
    Patch_controller.reconcile_all finished ~project_name:"feature-test"
      ~gameplan:gp
  in
  let running =
    Orchestrator.fire queued
      (Orchestrator.Respond (id 1, Operation_kind.Pr_body))
  in
  let current = Orchestrator.agent running (id 1) in
  let finished =
    Orchestrator.acknowledge_pr_body_refresh running (id 1) ~publication:current
    |> fun o ->
    Orchestrator.apply_respond_outcome o (id 1) Operation_kind.Pr_body
      Orchestrator.Respond_ok
  in
  check "current publication settles refresh"
    (not
       (Orchestrator.agent finished (id 1)).Patch_agent.pr_body_refresh
         .Patch_agent.pending);
  Stdlib.print_endline "feature PR publication generations: OK"

let () =
  let open Patch_agent in
  let pid = id 1 in
  let claim_rebase orch =
    Orchestrator.enqueue orch pid Operation_kind.Rebase |> fun o ->
    Orchestrator.fire o (Orchestrator.Rebase (pid, main))
  in
  let claim_conflict orch =
    Orchestrator.enqueue orch pid Operation_kind.Merge_conflict |> fun o ->
    Orchestrator.fire o
      (Orchestrator.Respond (pid, Operation_kind.Merge_conflict))
  in
  let initial = claim_rebase (fresh ()) in
  let failed, _ =
    Orchestrator.apply_rebase_result initial pid
      (Worktree.Error "transient command failure") main
  in
  check "root command error consumes failure budget"
    ((Orchestrator.agent failed pid).Patch_agent.rebase_failure_count = 1);
  let conflicted, effects =
    Orchestrator.apply_rebase_result (claim_rebase failed) pid
      (Worktree.Merge_conflict "upstream-sha") main
  in
  let a = Orchestrator.agent conflicted pid in
  check "root merge conflict queues agent repair without failure budget"
    (a.has_conflict && (not a.busy) && a.rebase_failure_count = 0
    && (not (Patch_agent.needs_intervention a))
    && List.mem a.queue Operation_kind.Merge_conflict
         ~equal:Operation_kind.equal
    && List.is_empty effects);
  (* Repeated deliveries after interrupted sessions are repairable conflicts,
     rather than two command failures that strand the root in needs-help. *)
  let repaired =
    List.fold [ 1; 2; 3 ] ~init:conflicted ~f:(fun orch _ ->
        let running = claim_conflict orch in
        let orch, decision, effects =
          Orchestrator.apply_conflict_rebase_result running pid
            (Worktree.Merge_conflict "upstream-sha") main
        in
        check "pending root merge reaches repair agent"
          (Orchestrator.equal_conflict_rebase_decision decision
             Orchestrator.Deliver_to_agent
          && List.is_empty effects);
        let orch, resolution =
          Orchestrator.apply_conflict_push_result orch pid decision None
        in
        let a = Orchestrator.agent orch pid in
        check "pending root merge retains ownership for agent"
          (Orchestrator.equal_conflict_resolution resolution
             Orchestrator.Conflict_needs_agent
          && a.busy && a.has_conflict && a.rebase_failure_count = 0
          && not (Patch_agent.needs_intervention a));
        Orchestrator.complete orch pid)
  in
  let finished, decision, effects =
    Orchestrator.apply_conflict_rebase_result (claim_conflict repaired) pid
      Worktree.Ok main
  in
  let a = Orchestrator.agent finished pid in
  check "completed root repair schedules publication and releases ownership"
    (Orchestrator.equal_conflict_rebase_decision decision
       Orchestrator.Conflict_resolved
    && List.equal Orchestrator.equal_rebase_effect effects
         [ Orchestrator.Push_branch ]
    && (not a.busy) && (not a.has_conflict)
    && not (Patch_agent.needs_intervention a))

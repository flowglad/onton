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
    }

let check label value = if not value then failwith label

module Reconciliation = Onton_test_support.Reconciliation_fixture

let ready_branch orch n =
  Orchestrator.set_pr_body_delivered orch (id n) true |> fun o ->
  Orchestrator.set_automerge_enabled o (id n) true |> fun o ->
  Orchestrator.set_base_branch o (id n) (branch 1) |> fun o ->
  Reconciliation.rebase ~noop:true o (id n) (branch 1) |> fun o ->
  Patch_controller.apply_branch_observation o (id n)
    ~head_sha:
      (Option.value (Orchestrator.agent o (id n)).Patch_agent.head_oid
         ~default:"")
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
  Reconciliation.rebase ~noop:true o (id 1) main |> fun o ->
  Orchestrator.set_pr_body_delivered o (id 1) true |> fun o ->
  Orchestrator.set_checks_passing o (id 1) true |> fun o ->
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
      | Orchestrator.Respond _ | Orchestrator.Rebase _
      | Orchestrator.Reconcile_branch _ ->
          None)
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
      Reconciliation.rebase ~noop:true o (id 0) main |> fun o ->
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
          | Orchestrator.Respond _ | Orchestrator.Rebase _
          | Orchestrator.Reconcile_branch _ ->
              false));
      check "root waits for descendants"
        (not (Patch_controller.ready_for_review orch (id 1)));
      let orch = ready_branch orch 2 in
      check "published descendant has no PR"
        (not (Patch_agent.has_pr (Orchestrator.agent orch (id 2))));
      let checked_head =
        Option.value (Orchestrator.agent orch (id 2)).Patch_agent.head_oid
          ~default:""
      in
      let counted =
        Orchestrator.increment_ci_failure_count orch (id 2) |> fun o ->
        Orchestrator.increment_ci_failure_count o (id 2)
      in
      let reset =
        Patch_controller.apply_branch_observation counted (id 2)
          ~head_sha:checked_head
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
      check "PR-less descendant notes address integration PR"
        (Option.equal Pr_number.equal
           (Orchestrator.respond_pr_number with_notes_demand (id 2)
              Operation_kind.Pr_body)
           (Patch_agent.pr_number (Orchestrator.agent with_notes_demand (id 1))));
      check "descendant CI does not address integration PR"
        (Option.is_none
           (Orchestrator.respond_pr_number with_notes_demand (id 2)
              Operation_kind.Ci));
      check "missing integration root has no notes recipient"
        (Option.is_none
           (Orchestrator.respond_pr_number
              (Orchestrator.remove_agent with_notes_demand (id 1))
              (id 2) Operation_kind.Pr_body));
      check "PR-less descendant can dispatch notes"
        (List.exists (Patch_controller.plan_actions with_notes_demand ~patches)
           ~f:(function
          | Orchestrator.Respond (p, kind) ->
              Patch_id.equal p (id 2)
              && Operation_kind.equal kind Operation_kind.Pr_body
          | Orchestrator.Start _ | Orchestrator.Rebase _
          | Orchestrator.Reconcile_branch _ ->
              false));
      let claimed, decisions =
        Patch_controller.reconcile_automerge orch ~now:0.
      in
      check "integration immediate and carries checked head"
        (List.exists decisions ~f:(function
          | Patch_controller.Git_integrate { root; head_sha; merge_patch_id } ->
              Patch_id.equal root (id 1)
              && Patch_id.equal merge_patch_id (id 2)
              && String.equal head_sha checked_head
          | Patch_controller.Github_merge _ -> false));
      check "claimed branch cannot dispatch work"
        (not
           (List.exists
              (Patch_controller.plan_actions
                 (Orchestrator.enqueue claimed (id 2) Operation_kind.Human)
                 ~patches)
              ~f:(function
                | Orchestrator.Respond (p, _) -> Patch_id.equal p (id 2)
                | Orchestrator.Start _ | Orchestrator.Rebase _
                | Orchestrator.Reconcile_branch _ ->
                    false)));
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
      let module F = Onton_core_test_support.Publication_fixture in
      let candidate = F.sha "final-head" in
      let external_head = F.sha "middle-head" in
      let awaiting =
        Orchestrator.invalidate_root_readiness complete |> fun o ->
        F.confirmed ~candidate
          ~step:(fun o event ->
            fst (Orchestrator.reconcile_branch o (id 1) event))
          ~state:(fun o ->
            (Orchestrator.agent o (id 1)).Patch_agent.branch_reconcile)
          o
      in
      let observe head =
        Patch_controller.
          {
            poll_result =
              {
                Poller.queue = [];
                merged = false;
                closed = false;
                is_draft = true;
                merge_state = Pr_state.Mergeable;
                merge_ready = true;
                base_branch = Some main;
                base_oid = Some (F.sha "root-base");
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
        Onton_test_support.Forge_poll_fixture.apply awaiting (id 1)
          (observe (F.sha "root-head"))
      in
      check "pre-push root CI cannot promote"
        (not (Patch_controller.ready_for_review stale (id 1)));
      check "pre-push observation retains publication marker"
        (Option.equal String.equal
           (Patch_agent.expected_remote_head_oid
              (Orchestrator.agent stale (id 1)))
           (Some candidate));
      let unconfirmed, _, _ =
        Onton_test_support.Forge_poll_fixture.apply awaiting (id 1)
          (observe external_head)
      in
      check "unconfirmed distinct root head cannot settle publication"
        (Option.equal String.equal
           (Patch_agent.expected_remote_head_oid
              (Orchestrator.agent unconfirmed (id 1)))
           (Some candidate)
        && not (Patch_controller.ready_for_review unconfirmed (id 1)));
      let distinct, _, _ =
        Onton_test_support.Forge_poll_fixture.apply
          ~confirmed_remote_head:external_head awaiting (id 1)
          (observe external_head)
      in
      check "directly confirmed root head settles publication"
        (Option.equal String.equal
           (Orchestrator.agent distinct (id 1)).Patch_agent.head_oid
           (Some external_head)
        && Option.is_none
             (Patch_agent.expected_remote_head_oid
                (Orchestrator.agent distinct (id 1))));
      (* Owner publication/transport interleavings are exercised by PI-6b and
         test_branch_reconcile_outbox; this fixture checks root readiness. *)
      let observed, _, _ =
        Onton_test_support.Forge_poll_fixture.apply
          ~confirmed_remote_head:candidate stale (id 1) (observe candidate)
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
        (Patch_agent.branch_published (Orchestrator.agent restored (id 2)));
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
  let module B = Branch_reconcile in
  let pid = id 1 in
  let initial, _, _, _ =
    Reconciliation.prepare ~noop:false (fresh ()) pid main
  in
  let failed =
    Reconciliation.reply initial pid
      (B.Retryable { reason = "transport"; retry_after = None })
    |> fun o -> Orchestrator.complete o pid
  in
  let a = Orchestrator.agent failed pid in
  check "root transport retry retains owner without spending repair budget"
    (B.is_pending a.Patch_agent.branch_reconcile
    && not (Patch_agent.needs_intervention a));
  let conflicted = Reconciliation.conflict (fresh ()) pid main in
  let a = Orchestrator.agent conflicted pid in
  check "root merge conflict retains durable repair after releasing capacity"
    ((not a.Patch_agent.busy)
    && (not (Patch_agent.needs_intervention a))
    &&
    match B.phase a.Patch_agent.branch_reconcile with
    | Some (B.Repairing _) -> true
    | None
    | Some
        ( B.Preparing | B.Integrating | B.Publishing | B.Confirming
        | B.Waiting _ | B.Recovering | B.Settled | B.Intervention _ ) ->
        false);
  check "root repair preserves ancestry policy"
    (match B.operation a.Patch_agent.branch_reconcile with
    | Some operation ->
        B.equal_policy operation.B.intent.B.policy B.Preserve_ancestry
    | None -> false);
  let finished = Reconciliation.resolve_conflict conflicted pid in
  let a = Orchestrator.agent finished pid in
  check "completed root repair confirms publication and releases capacity"
    ((not a.Patch_agent.busy)
    && (not (Patch_agent.has_conflict a))
    && (not (Patch_agent.needs_intervention a))
    && Option.equal B.equal_phase
         (B.phase a.Patch_agent.branch_reconcile)
         (Some B.Settled))

let () =
  let module B = Branch_reconcile in
  let open B in
  let commit digit =
    match B.Commit.make (String.make 40 digit) with
    | Some sha -> sha
    | None -> assert false
  in
  let source = commit 'a'
  and revision = commit 'b'
  and candidate = commit 'c' in
  List.iter [ false; true ] ~f:(fun changed ->
      let orch = ready_branch (ready_branch (fresh ()) 2) 3 in
      let orch =
        Orchestrator.set_head_oid orch (id 2)
          (Some (B.Commit.to_string revision))
      in
      let orch = Orchestrator.set_automerge_inflight orch (id 2) true in
      let orch, _ =
        Orchestrator.reconcile_branch orch (id 1)
          (B.Request
             {
               base = "main";
               policy = Preserve_ancestry;
               purpose = Integrate_revision { contributor = "2"; revision };
             })
      in
      let state orch =
        (Orchestrator.agent orch (id 1)).Patch_agent.branch_reconcile
      in
      let prior_publications = List.length (B.publications (state orch)) in
      let reply orch result =
        match B.pending (state orch) with
        | None -> failwith "missing integration command"
        | Some { B.kind = _; B.token; _ } ->
            fst
              (Orchestrator.reconcile_branch orch (id 1)
                 (B.Result { token; at = 100.; result }))
      in
      check "pending root reconciliation serializes sibling integration"
        (not (Patch_controller.is_integration_candidate orch (id 3)));
      let orch =
        reply orch
          (B.Observed
             {
               completed_integration = false;
               destination =
                 Branch_reconcile.Remote_id.of_destination "fixture-origin";
               B.head = source;
               B.source;
               B.target = revision;
               remote = Some source;
               B.boundary = Plain;
               topology = Equal;
               clean = true;
               B.sequencer = None;
               B.conflicts = 0;
               target_included = false;
               base_contains_source = false;
             })
      in
      let orch = reply orch B.Pinned in
      let orch = reply orch (B.Integrated candidate) in
      check "local root integration cannot complete descendant"
        (not (Orchestrator.agent orch (id 2)).Patch_agent.merged);
      let orch = reply orch B.Published in
      check
        "push acknowledgement cannot complete descendant before confirmation"
        (not (Orchestrator.agent orch (id 2)).Patch_agent.merged);
      let orch =
        if changed then
          Orchestrator.set_head_oid orch (id 2)
            (Some (B.Commit.to_string (commit 'd')))
        else orch
      in
      let orch =
        reply orch (B.Remote { sha = Some candidate; topology = B.Equal })
      in
      let child = Orchestrator.agent orch (id 2) in
      check "confirmation completes only the captured descendant revision"
        (Bool.equal child.Patch_agent.merged (not changed)
        && not child.Patch_agent.automerge_inflight);
      check "root integration retains its base lineage"
        (Option.equal Branch.equal
           (Patch_agent.branch_rebased_onto (Orchestrator.agent orch (id 1)))
           (Some main));
      check "confirmed publication remains recorded on destination root"
        (List.length (B.publications (state orch)) = prior_publications + 1))
[@@warning "-42"]

let () =
  let blocked =
    fresh () |> fun o ->
    Orchestrator.set_head_oid o (id 1) (Some "root-before") |> fun o ->
    Orchestrator.set_pr_number o (id 2) (Pr_number.of_int 2) |> fun o ->
    Orchestrator.apply_session_result o (id 2)
      (Orchestrator.Session_wontdo "Needs a shared integration API")
  in
  check "leaf WONTDO reaches integration root"
    (List.exists (Orchestrator.agent blocked (id 1)).Patch_agent.human_messages
       ~f:(fun message ->
         String.is_substring message ~substring:"Needs a shared integration API"
         && String.is_substring message ~substring:"raise WONTDO"));
  let refused_root =
    Orchestrator.apply_session_result blocked (id 1)
      (Orchestrator.Session_wontdo "Cannot safely repair the integration API")
    |> fun o ->
    Orchestrator.apply_session_result o (id 3)
      (Orchestrator.Session_wontdo "Also needs the API")
  in
  check "another leaf does not add guidance to a paused integration root"
    (List.length
       (Orchestrator.agent refused_root (id 1)).Patch_agent.human_messages
    = List.length (Orchestrator.agent blocked (id 1)).Patch_agent.human_messages
    );
  check "another leaf does not erase the integration agent's WONTDO"
    (Option.equal String.equal
       (Orchestrator.agent refused_root (id 1)).Patch_agent.wontdo_reason
       (Some "Cannot safely repair the integration API"));
  let unchanged =
    Orchestrator.apply_session_result blocked (id 1) Orchestrator.Session_ok
  in
  check "unchanged integration branch leaves WONTDO paused"
    (Option.is_some
       (Orchestrator.agent unchanged (id 2)).Patch_agent.wontdo_reason);
  let module F = Onton_core_test_support.Publication_fixture in
  let module B = Branch_reconcile in
  let candidate = F.sha "delayed-root-publication" in
  let step o event = fst (Orchestrator.reconcile_branch o (id 1) event) in
  let state o = (Orchestrator.agent o (id 1)).Patch_agent.branch_reconcile in
  let pending = F.publishing ~candidate ~step ~state unchanged in
  check "pending root publication leaves WONTDO paused"
    (Option.is_some
       (Orchestrator.agent pending (id 2)).Patch_agent.wontdo_reason);
  let acknowledged = F.reply ~step ~state pending B.Published in
  check "push acknowledgement leaves WONTDO paused"
    (Option.is_some
       (Orchestrator.agent acknowledged (id 2)).Patch_agent.wontdo_reason);
  let confirmed =
    F.reply ~step ~state acknowledged
      (B.Remote
         {
           sha = Some (Option.value_exn (B.Commit.make candidate));
           topology = B.Equal;
         })
  in
  let delayed_leaf = Orchestrator.agent confirmed (id 2) in
  check "delayed root publication schedules rebase before WONTDO retry"
    (Option.is_none delayed_leaf.Patch_agent.wontdo_reason
    && Option.value_map
         (Patch_agent.highest_priority delayed_leaf)
         ~default:false
         ~f:(Operation_kind.equal Operation_kind.Rebase));
  let advanced =
    Reconciliation.rebase unchanged (id 1) main |> fun o ->
    Orchestrator.apply_session_result o (id 1) Orchestrator.Session_ok
  in
  let leaf = Orchestrator.agent advanced (id 2) in
  check "integration advance schedules rebase before patch retry"
    (Option.is_none leaf.Patch_agent.wontdo_reason
    && Option.value_map
         (Patch_agent.highest_priority leaf)
         ~default:false
         ~f:(Operation_kind.equal Operation_kind.Rebase)
    && not (List.is_empty leaf.Patch_agent.human_messages));
  let rebasing =
    Orchestrator.fire advanced (Orchestrator.Rebase (id 2, branch 1))
  in
  let reprompted =
    Reconciliation.rebase ~noop:true rebasing (id 2) (branch 1)
  in
  check "completed rebase releases patch guidance"
    (Option.value_map
       (Patch_agent.highest_priority (Orchestrator.agent reprompted (id 2)))
       ~default:false
       ~f:(Operation_kind.equal Operation_kind.Human));
  let unpublished =
    Orchestrator.set_worktree_path (fresh ()) (id 2) "/existing-leaf"
    |> fun o ->
    Orchestrator.apply_session_result o (id 2)
      (Orchestrator.Session_wontdo "Needs a shared prerequisite")
    |> fun o ->
    Reconciliation.rebase o (id 1) main |> fun o ->
    Orchestrator.apply_session_result o (id 1) Orchestrator.Session_ok
  in
  check "unpublished leaf rebases before its next Start"
    (List.exists (Patch_controller.plan_actions unpublished ~patches)
       ~f:(function
      | Orchestrator.Rebase (pid, base) ->
          Patch_id.equal pid (id 2) && Branch.equal base (branch 1)
      | Orchestrator.Start _ | Orchestrator.Respond _
      | Orchestrator.Reconcile_branch _ ->
          false));
  Eio_main.run (fun _env ->
      let open Branch_reconcile_runner in
      let open Orchestrator in
      let in_flight =
        Orchestrator.fire unpublished (Orchestrator.Rebase (id 2, branch 1))
      in
      let runtime = Runtime.create ~gameplan:gp ~main_branch:main () in
      Runtime.update_orchestrator runtime (fun _ -> in_flight);
      let outcome =
        Branch_reconcile_runner.run ~runtime
          ~persist:(fun _ -> Error "checkpoint unavailable")
          ~patch_id:(id 2)
          ~now:(fun () -> 100.)
          ~execute:(fun ~operation:_ _ -> failwith "unexpected Git command")
          (Branch_reconcile.Request
             Branch_reconcile.
               {
                 base = Branch.to_string (branch 1);
                 policy = Rewrite;
                 purpose = Reconcile_base;
               })
      in
      check "initial rebase checkpoint failure is reported"
        (match outcome with
        | Branch_reconcile_runner.Checkpoint_failed _ -> true
        | Branch_reconcile_runner.Idle | Waiting | Repair_needed _
        | Intervention _ ->
            false);
      let recovered =
        Runtime.read runtime (fun snapshot -> snapshot.Runtime.orchestrator)
        |> fun o ->
        Orchestrator.complete_rebase_after_checkpoint_failure o (id 2)
      in
      let leaf = Orchestrator.agent recovered (id 2) in
      check
        "failed rebase checkpoint requeues Rebase and retains retry guidance"
        (Option.is_some leaf.Patch_agent.wontdo_integration_head
        && (not (List.is_empty leaf.Patch_agent.human_messages))
        && Option.value_map
             (Patch_agent.highest_priority leaf)
             ~default:false
             ~f:(Operation_kind.equal Operation_kind.Rebase)
        && List.exists (Patch_controller.plan_actions recovered ~patches)
             ~f:(function
             | Orchestrator.Rebase (pid, _) -> Patch_id.equal pid (id 2)
             | Orchestrator.Start _ | Respond _ | Reconcile_branch _ -> false));
      let with_old_receipt =
        Orchestrator.set_head_oid (fresh ()) (id 1) (Some "old-root")
        |> fun o ->
        Orchestrator.set_worktree_path o (id 2) "/old-leaf" |> fun o ->
        Reconciliation.rebase ~noop:true o (id 2) (branch 1) |> fun o ->
        Orchestrator.apply_session_result o (id 2)
          (Orchestrator.Session_wontdo "Needs updated integration")
        |> fun o ->
        Reconciliation.rebase o (id 1) main |> fun o ->
        Orchestrator.apply_session_result o (id 1) Orchestrator.Session_ok
        |> fun o -> Orchestrator.fire o (Orchestrator.Rebase (id 2, branch 1))
      in
      check "old integration receipt matches the same base"
        (Option.equal Branch.equal
           (Patch_agent.branch_rebased_onto
              (Orchestrator.agent with_old_receipt (id 2)))
           (Some (branch 1)));
      let after_failure =
        Orchestrator.complete_rebase_after_checkpoint_failure with_old_receipt
          (id 2)
      in
      let old_leaf = Orchestrator.agent after_failure (id 2) in
      check "old receipt cannot release retry after failed checkpoint"
        (Option.is_some old_leaf.Patch_agent.wontdo_integration_head
        && Option.value_map
             (Patch_agent.highest_priority old_leaf)
             ~default:false
             ~f:(Operation_kind.equal Operation_kind.Rebase));
      let pending_runtime = Runtime.create ~gameplan:gp ~main_branch:main () in
      Runtime.update_orchestrator pending_runtime (fun _ -> in_flight);
      let attempts = ref 0 in
      let pending_outcome =
        Branch_reconcile_runner.run ~runtime:pending_runtime
          ~persist:(fun _ ->
            Int.incr attempts;
            if !attempts = 1 then Ok ()
            else Error "result checkpoint unavailable")
          ~patch_id:(id 2)
          ~now:(fun () -> 100.)
          ~execute:(fun ~operation:_ _ ->
            Branch_reconcile.Needs_diagnosis "fixture")
          (Branch_reconcile.Request
             Branch_reconcile.
               {
                 base = Branch.to_string (branch 1);
                 policy = Rewrite;
                 purpose = Reconcile_base;
               })
      in
      check "result checkpoint failure is reported"
        (match pending_outcome with
        | Checkpoint_failed _ -> true
        | Idle | Waiting | Repair_needed _ | Intervention _ -> false);
      let pending =
        Runtime.read pending_runtime (fun snapshot ->
            snapshot.Runtime.orchestrator)
        |> fun o ->
        Orchestrator.complete_rebase_after_checkpoint_failure o (id 2)
      in
      let pending_leaf = Orchestrator.agent pending (id 2) in
      check "checkpointed reconciliation resumes without a duplicate rebase"
        (Option.is_some pending_leaf.Patch_agent.wontdo_integration_head
        && (not
              (List.mem pending_leaf.Patch_agent.queue Operation_kind.Rebase
                 ~equal:Operation_kind.equal))
        && List.exists (Patch_controller.plan_actions pending ~patches)
             ~f:(function
             | Orchestrator.Reconcile_branch (pid, _) ->
                 Patch_id.equal pid (id 2)
             | Start _ | Rebase _ | Respond _ -> false));
      let retry =
        Orchestrator.fire recovered (Orchestrator.Rebase (id 2, branch 1))
        |> fun o -> Reconciliation.rebase ~noop:true o (id 2) (branch 1)
      in
      check "confirmed retry releases Human guidance"
        (Option.value_map
           (Patch_agent.highest_priority (Orchestrator.agent retry (id 2)))
           ~default:false
           ~f:(Operation_kind.equal Operation_kind.Human)));
  let ready = ready_branch (fresh ()) 3 in
  let waiting =
    Orchestrator.fire
      (Orchestrator.enqueue ready (id 1) Operation_kind.Ci)
      (Orchestrator.Respond (id 1, Operation_kind.Ci))
  in
  let views orch =
    Tui.views_of_orchestrator ~orchestrator:orch ~gameplan:gp ~activity:[]
      ~resolve_routing:(fun ~complexity:_ ->
        Backend_routing.{ backend = "test"; model = None; effort = None })
      ()
  in
  let view orch n =
    List.find_exn (views orch) ~f:(fun (v : Tui.patch_view) ->
        Patch_id.equal v.Tui.patch_id (id n))
  in
  check "green child waits for busy integration root"
    (Patch_controller.is_waiting_for_integration_root waiting (id 3)
    && not (Patch_controller.is_integration_candidate waiting (id 3)));
  check "waiting status explains integration blocker"
    (Tui.equal_display_status (view waiting 3).Tui.status
       Tui.Waiting_for_integration_root);
  check "dependency badge shows the same integration blocker"
    (List.exists (view waiting 4).Tui.dep_ids ~f:(fun (dep, status) ->
         Patch_id.equal dep (id 3)
         && Tui.equal_display_status status Tui.Waiting_for_integration_root));
  check "root's active operation keeps its own status"
    (Tui.equal_display_status (view waiting 1).Tui.status Tui.Fixing_ci);
  let idle = Orchestrator.reset_busy waiting (id 1) in
  check "root becoming idle clears waiting and enables integration"
    ((not (Patch_controller.is_waiting_for_integration_root idle (id 3)))
    && Patch_controller.is_integration_candidate idle (id 3)
    && Tui.equal_display_status (view idle 3).Tui.status
         Tui.Ready_for_integration);
  let not_green = Orchestrator.set_checks_passing waiting (id 3) false in
  check "pending CI does not blame integration root"
    ((not (Patch_controller.is_waiting_for_integration_root not_green (id 3)))
    && Tui.equal_display_status (view not_green 3).Tui.status Tui.Waiting_for_ci
    );
  let queued = Orchestrator.enqueue waiting (id 3) Operation_kind.Ci in
  check "queued child work takes precedence"
    (Tui.equal_display_status (view queued 3).Tui.status Tui.Ci_queued);
  let running =
    Orchestrator.fire queued (Orchestrator.Respond (id 3, Operation_kind.Ci))
  in
  check "running child work takes precedence"
    (Tui.equal_display_status (view running 3).Tui.status Tui.Fixing_ci);
  let disabled = Orchestrator.set_automerge_enabled waiting (id 3) false in
  check "disabled integration does not blame root"
    (not (Patch_controller.is_waiting_for_integration_root disabled (id 3)));
  check "mainline patches do not wait for integration roots"
    (not
       (Patch_controller.is_waiting_for_integration_root
          (Orchestrator.create ~patches ~main_branch:main)
          (id 3)));
  check "waiting label is readable"
    (String.equal
       (Tui.label Tui.Waiting_for_integration_root)
       "waiting-for-integration-root");
  Stdlib.print_endline "PASS integration waiting status"

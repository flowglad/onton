(* @archlint.module test
   @archlint.domain orchestrator *)

open Base
open Onton
open Onton_core
open Onton_core.Types

let main = Branch.of_string "main"

let make_patch pid branch =
  Patch.
    {
      id = pid;
      title = "Test patch";
      description = "";
      branch;
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

let make_gameplan patch =
  Gameplan.
    {
      project_name = "test-project";
      repo_owner = "";
      repo_name = "";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches = [ patch ];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
      functional_changes = [];
      context_resources = [];
      reachability_traces = [];
    }

let make_orch patch agent =
  let graph = Graph.of_patches [ patch ] in
  let agents = Map.of_alist_exn (module Patch_id) [ (patch.Patch.id, agent) ] in
  Orchestrator.restore ~graph ~agents
    ~outbox:(Map.empty (module Message_id))
    ~main_branch:main ()

let make_agent ?(merge_ready = false) ?(mergeability_unknown = false)
    ?(merge_queue_required = false) ?(merge_queue_entry = None)
    ?(checks_passing = false) ?(ci_checks = []) ?(has_conflict = false)
    ?(head_oid = None) ?(review_decision = None) ?(unresolved_comment_count = 0)
    ?(review_requested_for_oid = None) ?(review_request_inflight = false)
    ?(automerge_enabled = false) ?automerge_deadline
    ?(automerge_failure_count = 0) ~patch_id ~branch ~pr_status ~merged ~queue
    ~base_branch ~is_draft ~pr_body_delivered ~start_attempts_without_pr () =
  Patch_agent.restore ~patch_id ~branch ~pr_status ~has_session:false
    ~busy:false ~merged ~queue ~satisfies:false ~changed:false ~has_conflict
    ~base_branch ~notified_base_branch:base_branch ~ci_failure_count:0
    ~session_fallback:Patch_agent.Fresh_available ~human_messages:[]
    ~inflight_human_messages:[] ~ci_checks ~merge_ready ~head_oid
    ~review_decision ~unresolved_comment_count ~mergeability_unknown
    ~merge_queue_required ~merge_queue_entry ~is_draft ~pr_body_delivered
    ~pr_body_artifact_miss_count:0 ~start_attempts_without_pr
    ~conflict_noop_count:0 ~no_commits_push_count:0 ~context_exhaustion_count:0
    ~push_failure_count:0 ~rebase_failure_count:0 ~branch_rebased_onto:None
    ~branch_rebased_onto_sha:None ~merge_commit_sha:None
    ~base_contains_merged_siblings:true
    ~anchor_history:Onton_core.Anchor_history.empty ~checks_passing
    ~current_op:None ~current_op_state:Patch_agent.Queued
    ~current_message_id:None ~generation:0 ~worktree_path:None
    ~branch_blocked:false ~llm_session_id:None ~automerge_enabled
    ~automerge_deadline ~automerge_inflight:false ~review_requested_for_oid
    ~review_request_inflight ~automerge_failure_count ~delivered_ci_run_ids:[]
    ()

let has_draft_effect effects =
  List.exists effects ~f:(function
    | Patch_controller.Set_pr_draft _ -> true
    | Patch_controller.Set_pr_base _ -> false)

let draft_effect effects =
  List.find_map effects ~f:(function
    | Patch_controller.Set_pr_draft _ as e -> Some e
    | Patch_controller.Set_pr_base _ -> None)

let apply_all_effect_successes orch effects =
  List.fold effects ~init:orch ~f:Patch_controller.apply_github_effect_success

let run_controller_cycle ~gameplan orch =
  let orch, effects, actions =
    Patch_controller.plan_tick orch ~project_name:gameplan.Gameplan.project_name
      ~gameplan
  in
  let orch = apply_all_effect_successes orch effects in
  (orch, effects, actions)

let make_poll_observation poll_result =
  Patch_controller.
    {
      poll_result;
      base_branch = None;
      branch_in_root = false;
      worktree_path = None;
    }

let () =
  let open QCheck2 in
  let open Onton_test_support.Test_generators in
  let gen_controller_case =
    Gen.(
      let* pid = gen_patch_id in
      let* branch = gen_branch in
      let* has_pr = bool in
      let* merged = bool in
      let* queue = gen_operation_kind_queue in
      let* use_main_base = bool in
      let* is_draft = bool in
      let* pr_body_delivered = bool in
      let* start_attempts_without_pr = int_range 0 3 in
      let base_branch =
        if has_pr then Some (if use_main_base then main else branch) else None
      in
      let pr_status =
        if has_pr then Patch_pr_status.Present (Pr_number.of_int 42)
        else Patch_pr_status.Absent
      in
      let patch = make_patch pid branch in
      let agent =
        make_agent ~patch_id:pid ~branch ~pr_status ~merged ~queue ~base_branch
          ~is_draft ~pr_body_delivered ~start_attempts_without_pr ()
      in
      return (patch, make_gameplan patch, make_orch patch agent))
  in

  let prop_deterministic =
    Test.make ~name:"patch_controller: reconcile_patch deterministic" ~count:300
      gen_controller_case (fun (patch, gameplan, orch) ->
        let orch1, effects1 =
          Patch_controller.reconcile_patch orch ~project_name:"test-project"
            ~gameplan ~patch
        in
        let orch2, effects2 =
          Patch_controller.reconcile_patch orch ~project_name:"test-project"
            ~gameplan ~patch
        in
        Patch_agent.equal
          (Orchestrator.agent orch1 patch.Patch.id)
          (Orchestrator.agent orch2 patch.Patch.id)
        && List.equal Patch_controller.equal_github_effect effects1 effects2)
  in

  let prop_plan_tick_deterministic =
    Test.make ~name:"patch_controller: plan_tick deterministic" ~count:300
      gen_controller_case (fun (_patch, gameplan, orch) ->
        let orch1, effects1, actions1 =
          Patch_controller.plan_tick orch ~project_name:"test-project" ~gameplan
        in
        let orch2, effects2, actions2 =
          Patch_controller.plan_tick orch ~project_name:"test-project" ~gameplan
        in
        let action_equal a b =
          match (a, b) with
          | Orchestrator.Start (p1, b1), Orchestrator.Start (p2, b2) ->
              Patch_id.equal p1 p2 && Branch.equal b1 b2
          | Orchestrator.Respond (p1, k1), Orchestrator.Respond (p2, k2) ->
              Patch_id.equal p1 p2 && Operation_kind.equal k1 k2
          | Orchestrator.Rebase (p1, b1), Orchestrator.Rebase (p2, b2) ->
              Patch_id.equal p1 p2 && Branch.equal b1 b2
          | ( Orchestrator.Start _,
              (Orchestrator.Respond _ | Orchestrator.Rebase _) )
          | ( Orchestrator.Respond _,
              (Orchestrator.Start _ | Orchestrator.Rebase _) )
          | ( Orchestrator.Rebase _,
              (Orchestrator.Start _ | Orchestrator.Respond _) ) ->
              false
        in
        Map.equal Patch_agent.equal
          (Orchestrator.agents_map orch1)
          (Orchestrator.agents_map orch2)
        && List.equal Patch_controller.equal_github_effect effects1 effects2
        && List.equal action_equal actions1 actions2)
  in

  let prop_pr_body_queue_idempotent =
    Test.make ~name:"patch_controller: pr_body queueing is idempotent"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:true
            ~pr_body_delivered:false ~start_attempts_without_pr:0 ()
        in
        let orch = make_orch patch agent in
        let orch1, _effects1 =
          Patch_controller.reconcile_patch orch ~project_name:"test-project"
            ~gameplan ~patch
        in
        let orch2, _effects2 =
          Patch_controller.reconcile_patch orch1 ~project_name:"test-project"
            ~gameplan ~patch
        in
        let a1 = Orchestrator.agent orch1 pid in
        let a2 = Orchestrator.agent orch2 pid in
        let has_pr_body a =
          List.mem a.Patch_agent.queue Operation_kind.Pr_body
            ~equal:Operation_kind.equal
        in
        has_pr_body a1 && has_pr_body a2
        && List.count a2.Patch_agent.queue
             ~f:(Operation_kind.equal Operation_kind.Pr_body)
           = 1)
  in

  let prop_draft_reemits_until_success =
    Test.make
      ~name:
        "patch_controller: ready-for-review effect re-emits until acknowledged"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        (* Agent is at the ready-for-review fixpoint except is_draft is still
           true on GitHub; reconcile must emit Set_pr_draft{draft=false} until
           the effect is acknowledged. One-way ratchet — the reverse
           direction (re-draft) is not exercised by this property. *)
        let agent =
          Patch_agent.restore ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~has_session:false ~busy:false ~merged:false ~queue:[]
            ~satisfies:false ~changed:false ~has_conflict:false
            ~base_branch:(Some main) ~notified_base_branch:(Some main)
            ~ci_failure_count:0 ~session_fallback:Patch_agent.Fresh_available
            ~human_messages:[] ~inflight_human_messages:[] ~ci_checks:[]
            ~merge_ready:false ~mergeability_unknown:false
            ~merge_queue_required:false ~merge_queue_entry:None ~is_draft:true
            ~pr_body_delivered:true ~pr_body_artifact_miss_count:0
            ~start_attempts_without_pr:0 ~conflict_noop_count:0
            ~no_commits_push_count:0 ~context_exhaustion_count:0
            ~push_failure_count:0 ~rebase_failure_count:0
            ~branch_rebased_onto:(Some main) ~branch_rebased_onto_sha:None
            ~merge_commit_sha:None ~base_contains_merged_siblings:true
            ~anchor_history:Onton_core.Anchor_history.empty ~checks_passing:true
            ~current_op:None ~current_op_state:Patch_agent.Queued
            ~current_message_id:None ~generation:0 ~worktree_path:None
            ~branch_blocked:false ~llm_session_id:None ~automerge_enabled:false
            ~automerge_deadline:None ~automerge_inflight:false
            ~automerge_failure_count:0 ~delivered_ci_run_ids:[] ()
        in
        let orch = make_orch patch agent in
        begin try
          let orch1, effects1 =
            Patch_controller.reconcile_patch orch ~project_name:"test-project"
              ~gameplan ~patch
          in
          match draft_effect effects1 with
          | None -> false
          | Some eff ->
              let orch2 =
                Patch_controller.apply_github_effect_success orch1 eff
              in
              let _orch3, effects2 =
                Patch_controller.reconcile_patch orch2
                  ~project_name:"test-project" ~gameplan ~patch
              in
              has_draft_effect effects1 && not (has_draft_effect effects2)
        with _ -> false
        end)
  in

  let prop_unknown_rebase_blocks_ready_for_review =
    Test.make
      ~name:"patch_controller: unknown rebase target blocks ready-for-review"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          Patch_agent.restore ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~has_session:false ~busy:false ~merged:false ~queue:[]
            ~satisfies:false ~changed:false ~has_conflict:false
            ~base_branch:(Some main) ~notified_base_branch:(Some main)
            ~ci_failure_count:0 ~session_fallback:Patch_agent.Fresh_available
            ~human_messages:[] ~inflight_human_messages:[] ~ci_checks:[]
            ~merge_ready:false ~mergeability_unknown:false
            ~merge_queue_required:false ~merge_queue_entry:None ~is_draft:true
            ~pr_body_delivered:true ~pr_body_artifact_miss_count:0
            ~start_attempts_without_pr:0 ~conflict_noop_count:0
            ~no_commits_push_count:0 ~context_exhaustion_count:0
            ~push_failure_count:0 ~rebase_failure_count:0
            ~branch_rebased_onto:None ~branch_rebased_onto_sha:None
            ~merge_commit_sha:None ~base_contains_merged_siblings:true
            ~anchor_history:Onton_core.Anchor_history.empty ~checks_passing:true
            ~current_op:None ~current_op_state:Patch_agent.Queued
            ~current_message_id:None ~generation:0 ~worktree_path:None
            ~branch_blocked:false ~llm_session_id:None ~automerge_enabled:false
            ~automerge_deadline:None ~automerge_inflight:false
            ~automerge_failure_count:0 ~delivered_ci_run_ids:[] ()
        in
        let orch = make_orch patch agent in
        let _orch, effects =
          Patch_controller.reconcile_patch orch ~project_name:"test-project"
            ~gameplan ~patch
        in
        not (has_draft_effect effects))
  in

  let prop_intervention_stable_after_threshold =
    Test.make
      ~name:
        "patch_controller: repeated no-PR attempts stably require intervention"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~branch ~pr_status:Patch_pr_status.Absent
            ~merged:false ~queue:[] ~base_branch:None ~is_draft:false
            ~pr_body_delivered:false ~start_attempts_without_pr:2 ()
        in
        let orch = make_orch patch agent in
        let orch1, effects1 =
          Patch_controller.reconcile_patch orch ~project_name:"test-project"
            ~gameplan ~patch
        in
        let orch2, effects2 =
          Patch_controller.reconcile_patch orch1 ~project_name:"test-project"
            ~gameplan ~patch
        in
        let a1 = Orchestrator.agent orch1 pid in
        let a2 = Orchestrator.agent orch2 pid in
        Patch_agent.needs_intervention a1
        && Patch_agent.needs_intervention a2
        && List.is_empty effects1 && List.is_empty effects2)
  in

  let prop_reconcile_all_exposes_pr_body_as_next_action =
    Test.make
      ~name:
        "patch_controller: reconcile_all exposes missing pr_body as next \
         Respond action"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:true
            ~pr_body_delivered:false ~start_attempts_without_pr:0 ()
        in
        let orch = make_orch patch agent in
        let orch, effects =
          Patch_controller.reconcile_all orch ~project_name:"test-project"
            ~gameplan
        in
        let actions =
          Patch_controller.plan_actions orch ~patches:gameplan.patches
        in
        List.is_empty effects
        && List.exists actions ~f:(function
          | Orchestrator.Respond (action_pid, kind) ->
              Patch_id.equal action_pid pid
              && Operation_kind.equal kind Operation_kind.Pr_body
          | Orchestrator.Start _ | Orchestrator.Rebase _ -> false))
  in

  let prop_reconcile_all_blocks_restart_after_intervention =
    Test.make
      ~name:
        "patch_controller: reconcile_all prevents further Start after durable \
         intervention"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~branch ~pr_status:Patch_pr_status.Absent
            ~merged:false ~queue:[] ~base_branch:None ~is_draft:false
            ~pr_body_delivered:false ~start_attempts_without_pr:2 ()
        in
        let orch = make_orch patch agent in
        let orch, effects =
          Patch_controller.reconcile_all orch ~project_name:"test-project"
            ~gameplan
        in
        let actions =
          Patch_controller.plan_actions orch ~patches:gameplan.patches
        in
        Patch_agent.needs_intervention (Orchestrator.agent orch pid)
        && List.is_empty effects
        && not
             (List.exists actions ~f:(function
               | Orchestrator.Start (action_pid, _) ->
                   Patch_id.equal action_pid pid
               | Orchestrator.Respond _ | Orchestrator.Rebase _ -> false)))
  in

  (* Dual of prop_reconcile_all_blocks_restart_after_intervention: the
     Respond/Start arms of [plan_action_for_patch] gate on
     [needs_intervention], but the Rebase arm does not — Rebase is
     orchestrator-executed and must continue tracking the base branch even
     for patches latched into needs-help. Regression guard: this held even
     before the fix if Human was in the queue (exemption inside
     [needs_intervention]), so the property explicitly keeps Human out to
     exercise the needs-intervention-true path. *)
  let prop_rebase_not_blocked_by_needs_intervention =
    Test.make ~name:"patch_controller: needs_intervention does not block Rebase"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          Patch_agent.restore ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~has_session:true ~busy:false ~merged:false
            ~queue:[ Operation_kind.Rebase ] ~satisfies:false ~changed:false
            ~has_conflict:false ~base_branch:(Some branch)
            ~notified_base_branch:(Some branch) ~ci_failure_count:3
            ~session_fallback:Patch_agent.Fresh_available ~human_messages:[]
            ~inflight_human_messages:[] ~ci_checks:[] ~merge_ready:false
            ~mergeability_unknown:false ~merge_queue_required:false
            ~merge_queue_entry:None ~is_draft:false ~pr_body_delivered:true
            ~pr_body_artifact_miss_count:0 ~start_attempts_without_pr:0
            ~conflict_noop_count:0 ~no_commits_push_count:0
            ~context_exhaustion_count:0 ~push_failure_count:0
            ~rebase_failure_count:0 ~branch_rebased_onto:None
            ~branch_rebased_onto_sha:None ~merge_commit_sha:None
            ~base_contains_merged_siblings:true
            ~anchor_history:Onton_core.Anchor_history.empty
            ~checks_passing:false ~current_op:None
            ~current_op_state:Patch_agent.Queued ~current_message_id:None
            ~generation:0 ~worktree_path:None ~branch_blocked:false
            ~llm_session_id:None ~automerge_enabled:false
            ~automerge_deadline:None ~automerge_inflight:false
            ~automerge_failure_count:0 ~delivered_ci_run_ids:[] ()
        in
        let orch = make_orch patch agent in
        let actions =
          Patch_controller.plan_actions orch ~patches:gameplan.patches
        in
        Patch_agent.needs_intervention (Orchestrator.agent orch pid)
        && List.exists actions ~f:(function
          | Orchestrator.Rebase (action_pid, _) -> Patch_id.equal action_pid pid
          | Orchestrator.Start _ | Orchestrator.Respond _ -> false))
  in

  (* Complement of the above: Respond must still be blocked under
     needs_intervention. Identical agent except the queue holds a
     non-Rebase feedback op, so the Rebase arm of [plan_action_for_patch]
     doesn't apply and the Respond arm's [needs_intervention] guard is
     what matters. *)
  let prop_respond_still_blocked_by_needs_intervention =
    Test.make ~name:"patch_controller: needs_intervention still blocks Respond"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          Patch_agent.restore ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~has_session:true ~busy:false ~merged:false
            ~queue:[ Operation_kind.Ci ] ~satisfies:false ~changed:false
            ~has_conflict:false ~base_branch:(Some branch)
            ~notified_base_branch:(Some branch) ~ci_failure_count:3
            ~session_fallback:Patch_agent.Fresh_available ~human_messages:[]
            ~inflight_human_messages:[] ~ci_checks:[] ~merge_ready:false
            ~mergeability_unknown:false ~merge_queue_required:false
            ~merge_queue_entry:None ~is_draft:false ~pr_body_delivered:true
            ~pr_body_artifact_miss_count:0 ~start_attempts_without_pr:0
            ~conflict_noop_count:0 ~no_commits_push_count:0
            ~context_exhaustion_count:0 ~push_failure_count:0
            ~rebase_failure_count:0 ~branch_rebased_onto:None
            ~branch_rebased_onto_sha:None ~merge_commit_sha:None
            ~base_contains_merged_siblings:true
            ~anchor_history:Onton_core.Anchor_history.empty
            ~checks_passing:false ~current_op:None
            ~current_op_state:Patch_agent.Queued ~current_message_id:None
            ~generation:0 ~worktree_path:None ~branch_blocked:false
            ~llm_session_id:None ~automerge_enabled:false
            ~automerge_deadline:None ~automerge_inflight:false
            ~automerge_failure_count:0 ~delivered_ci_run_ids:[] ()
        in
        let orch = make_orch patch agent in
        let actions =
          Patch_controller.plan_actions orch ~patches:gameplan.patches
        in
        Patch_agent.needs_intervention (Orchestrator.agent orch pid)
        && not
             (List.exists actions ~f:(function
               | Orchestrator.Respond (action_pid, _) ->
                   Patch_id.equal action_pid pid
               | Orchestrator.Start _ | Orchestrator.Rebase _ -> false)))
  in

  let prop_reconcile_all_converges_after_acknowledged_effects =
    Test.make
      ~name:
        "patch_controller: reconcile_all converges after acknowledging emitted \
         effects"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        (* pr_body_delivered=false so Set_pr_description fires *)
        let agent =
          Patch_agent.restore ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~has_session:false ~busy:false ~merged:false ~queue:[]
            ~satisfies:false ~changed:false ~has_conflict:false
            ~base_branch:(Some branch) ~notified_base_branch:(Some branch)
            ~ci_failure_count:0 ~session_fallback:Patch_agent.Fresh_available
            ~human_messages:[] ~inflight_human_messages:[] ~ci_checks:[]
            ~merge_ready:false ~mergeability_unknown:false
            ~merge_queue_required:false ~merge_queue_entry:None ~is_draft:true
            ~pr_body_delivered:false ~pr_body_artifact_miss_count:0
            ~start_attempts_without_pr:0 ~conflict_noop_count:0
            ~no_commits_push_count:0 ~context_exhaustion_count:0
            ~push_failure_count:0 ~rebase_failure_count:0
            ~branch_rebased_onto:None ~branch_rebased_onto_sha:None
            ~merge_commit_sha:None ~base_contains_merged_siblings:true
            ~anchor_history:Onton_core.Anchor_history.empty
            ~checks_passing:false ~current_op:None
            ~current_op_state:Patch_agent.Queued ~current_message_id:None
            ~generation:0 ~worktree_path:None ~branch_blocked:false
            ~llm_session_id:None ~automerge_enabled:false
            ~automerge_deadline:None ~automerge_inflight:false
            ~automerge_failure_count:0 ~delivered_ci_run_ids:[] ()
        in
        let orch = make_orch patch agent in
        (* Apply effects in a loop until convergence (max 5 rounds). *)
        let rec converge orch round =
          if round > 5 then false
          else
            let orch', effects =
              Patch_controller.reconcile_all orch ~project_name:"test-project"
                ~gameplan
            in
            if List.is_empty effects then true
            else converge (apply_all_effect_successes orch' effects) (round + 1)
        in
        let orch1, effects1 =
          Patch_controller.reconcile_all orch ~project_name:"test-project"
            ~gameplan
        in
        converge (apply_all_effect_successes orch1 effects1) 2)
  in

  let prop_poll_to_controller_promotes_ready_after_pr_body =
    Test.make
      ~name:
        "patch_controller: poll draft state plus delivered pr_body yields no \
         further draft effect once acknowledged"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:true
            ~pr_body_delivered:true ~start_attempts_without_pr:0 ()
          |> fun agent -> Patch_agent.set_branch_rebased_onto agent main
        in
        let orch = make_orch patch agent in
        let poll =
          Poller.
            {
              queue = [];
              merged = false;
              closed = false;
              is_draft = true;
              merge_state = Pr_state.Mergeable;
              merge_ready = false;
              head_oid = None;
              review_decision = None;
              unresolved_comment_count = 0;
              merge_queue_required = false;
              merge_queue_entry = None;
              checks_passing = true;
              ci_checks = [];
              merge_commit_sha = None;
            }
        in
        let orch, _logs, _newly_blocked =
          Patch_controller.apply_poll_result orch pid
            (make_poll_observation poll)
        in
        let orch1, effects1, _actions1 = run_controller_cycle ~gameplan orch in
        let _orch2, effects2, _actions2 =
          run_controller_cycle ~gameplan orch1
        in
        has_draft_effect effects1 && not (has_draft_effect effects2))
  in

  let prop_poll_ci_failure_never_erases_pr_body_followup =
    Test.make
      ~name:
        "patch_controller: poll-applicator CI queue does not suppress missing \
         pr_body action"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:true
            ~pr_body_delivered:false ~start_attempts_without_pr:0 ()
        in
        let orch = make_orch patch agent in
        let poll =
          Poller.
            {
              queue = [ Operation_kind.Ci ];
              merged = false;
              closed = false;
              is_draft = true;
              merge_state = Pr_state.Mergeable;
              merge_ready = false;
              head_oid = None;
              review_decision = None;
              unresolved_comment_count = 0;
              merge_queue_required = false;
              merge_queue_entry = None;
              checks_passing = false;
              ci_checks = [];
              merge_commit_sha = None;
            }
        in
        let orch, _logs, _newly_blocked =
          Patch_controller.apply_poll_result orch pid
            (make_poll_observation poll)
        in
        let orch, _effects, actions = run_controller_cycle ~gameplan orch in
        let a = Orchestrator.agent orch pid in
        (* CI is dispatched as the current action (higher priority), while
           pr_body remains queued for the next cycle. *)
        List.mem a.Patch_agent.queue Operation_kind.Pr_body
          ~equal:Operation_kind.equal
        && List.exists actions ~f:(function
          | Orchestrator.Respond (action_pid, kind) ->
              Patch_id.equal action_pid pid
              && Operation_kind.equal kind Operation_kind.Ci
          | Orchestrator.Start _ | Orchestrator.Rebase _ -> false))
  in

  let prop_idle_ci_failure_count_allows_reenqueue =
    Test.make
      ~name:
        "patch_controller: idle agent with ci_failure_count > 0 allows Ci \
         re-enqueue"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let agent =
          Patch_agent.restore ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~has_session:false ~busy:false ~merged:false ~queue:[]
            ~satisfies:false ~changed:false ~has_conflict:false
            ~base_branch:(Some main) ~notified_base_branch:(Some main)
            ~ci_failure_count:1 ~session_fallback:Patch_agent.Fresh_available
            ~human_messages:[] ~inflight_human_messages:[] ~ci_checks:[]
            ~merge_ready:false ~mergeability_unknown:false
            ~merge_queue_required:false ~merge_queue_entry:None ~is_draft:false
            ~pr_body_delivered:true ~pr_body_artifact_miss_count:0
            ~start_attempts_without_pr:0 ~conflict_noop_count:0
            ~no_commits_push_count:0 ~context_exhaustion_count:0
            ~push_failure_count:0 ~rebase_failure_count:0
            ~branch_rebased_onto:None ~branch_rebased_onto_sha:None
            ~merge_commit_sha:None ~base_contains_merged_siblings:true
            ~anchor_history:Onton_core.Anchor_history.empty
            ~checks_passing:false ~current_op:None
            ~current_op_state:Patch_agent.Queued ~current_message_id:None
            ~generation:0 ~worktree_path:None ~branch_blocked:false
            ~llm_session_id:None ~automerge_enabled:false
            ~automerge_deadline:None ~automerge_inflight:false
            ~automerge_failure_count:0 ~delivered_ci_run_ids:[] ()
        in
        let orch = make_orch patch agent in
        let poll =
          Poller.
            {
              queue = [ Operation_kind.Ci ];
              merged = false;
              closed = false;
              is_draft = false;
              merge_state = Pr_state.Mergeable;
              merge_ready = false;
              head_oid = None;
              review_decision = None;
              unresolved_comment_count = 0;
              merge_queue_required = false;
              merge_queue_entry = None;
              checks_passing = false;
              ci_checks = [];
              merge_commit_sha = None;
            }
        in
        let orch, _logs, _newly_blocked =
          Patch_controller.apply_poll_result orch pid
            (make_poll_observation poll)
        in
        let agent = Orchestrator.agent orch pid in
        List.mem agent.Patch_agent.queue Operation_kind.Ci
          ~equal:Operation_kind.equal)
  in

  let prop_delivered_ci_run_does_not_reenqueue =
    Test.make
      ~name:
        "patch_controller: delivered failing CI run is not re-enqueued from \
         poll"
      ~count:200
      Gen.(triple gen_patch_id gen_branch (int_range 1 1_000_000))
      (fun (pid, branch, run_id) ->
        let patch = make_patch pid branch in
        let check =
          Ci_check.
            {
              name = "ts2pant";
              conclusion = "failure";
              details_url = None;
              description = None;
              started_at = None;
              id = Some run_id;
            }
        in
        let agent =
          Patch_agent.restore ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~has_session:true ~busy:false ~merged:false ~queue:[]
            ~satisfies:false ~changed:true ~has_conflict:false
            ~base_branch:(Some main) ~notified_base_branch:(Some main)
            ~ci_failure_count:1 ~session_fallback:Patch_agent.Fresh_available
            ~human_messages:[] ~inflight_human_messages:[] ~ci_checks:[]
            ~merge_ready:false ~mergeability_unknown:false
            ~merge_queue_required:false ~merge_queue_entry:None ~is_draft:false
            ~pr_body_delivered:true ~pr_body_artifact_miss_count:0
            ~start_attempts_without_pr:0 ~conflict_noop_count:0
            ~no_commits_push_count:0 ~context_exhaustion_count:0
            ~push_failure_count:0 ~rebase_failure_count:0
            ~branch_rebased_onto:None ~branch_rebased_onto_sha:None
            ~merge_commit_sha:None ~base_contains_merged_siblings:true
            ~anchor_history:Onton_core.Anchor_history.empty
            ~checks_passing:false ~current_op:None
            ~current_op_state:Patch_agent.Queued ~current_message_id:None
            ~generation:0 ~worktree_path:None ~branch_blocked:false
            ~llm_session_id:None ~automerge_enabled:false
            ~automerge_deadline:None ~automerge_inflight:false
            ~automerge_failure_count:0 ~delivered_ci_run_ids:[ run_id ] ()
        in
        let orch = make_orch patch agent in
        let poll =
          Poller.
            {
              queue = [ Operation_kind.Ci ];
              merged = false;
              closed = false;
              is_draft = false;
              merge_state = Pr_state.Mergeable;
              merge_ready = false;
              head_oid = None;
              review_decision = None;
              unresolved_comment_count = 0;
              merge_queue_required = false;
              merge_queue_entry = None;
              checks_passing = false;
              ci_checks = [ check ];
              merge_commit_sha = None;
            }
        in
        let orch, _logs, _newly_blocked =
          Patch_controller.apply_poll_result orch pid
            (make_poll_observation poll)
        in
        let agent = Orchestrator.agent orch pid in
        not
          (List.mem agent.Patch_agent.queue Operation_kind.Ci
             ~equal:Operation_kind.equal))
  in

  let prop_new_ci_run_after_skip_empty_is_delivered =
    Test.make
      ~name:
        "patch_controller: new failed CI run after skip-empty delivery is \
         re-enqueued"
      ~count:200
      Gen.(
        let* pid = gen_patch_id in
        let* branch = gen_branch in
        let* old_run_id = int_range 1 1_000_000 in
        let* delta = int_range 1 1_000_000 in
        return (pid, branch, old_run_id, old_run_id + delta))
      (fun (pid, branch, old_run_id, new_run_id) ->
        let patch = make_patch pid branch in
        let agent =
          Patch_agent.restore ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~has_session:true ~busy:false ~merged:false
            ~queue:[ Operation_kind.Ci ] ~satisfies:false ~changed:true
            ~has_conflict:false ~base_branch:(Some main)
            ~notified_base_branch:(Some main) ~ci_failure_count:0
            ~session_fallback:Patch_agent.Fresh_available ~human_messages:[]
            ~inflight_human_messages:[] ~ci_checks:[] ~merge_ready:false
            ~mergeability_unknown:false ~merge_queue_required:false
            ~merge_queue_entry:None ~is_draft:false ~pr_body_delivered:true
            ~pr_body_artifact_miss_count:0 ~start_attempts_without_pr:0
            ~conflict_noop_count:0 ~no_commits_push_count:0
            ~context_exhaustion_count:0 ~push_failure_count:0
            ~rebase_failure_count:0 ~branch_rebased_onto:None
            ~branch_rebased_onto_sha:None ~merge_commit_sha:None
            ~base_contains_merged_siblings:true
            ~anchor_history:Onton_core.Anchor_history.empty
            ~checks_passing:false ~current_op:None
            ~current_op_state:Patch_agent.Queued ~current_message_id:None
            ~generation:0 ~worktree_path:None ~branch_blocked:false
            ~llm_session_id:None ~automerge_enabled:false
            ~automerge_deadline:None ~automerge_inflight:false
            ~automerge_failure_count:0 ~delivered_ci_run_ids:[ old_run_id ] ()
        in
        let orch = make_orch patch agent in
        let orch =
          Orchestrator.fire orch (Orchestrator.Respond (pid, Operation_kind.Ci))
        in
        let orch =
          Orchestrator.apply_respond_outcome orch pid Operation_kind.Ci
            Orchestrator.Respond_skip_empty
        in
        let after_skip = Orchestrator.agent orch pid in
        let check =
          Ci_check.
            {
              name = "Lint";
              conclusion = "failure";
              details_url = None;
              description = None;
              started_at = None;
              id = Some new_run_id;
            }
        in
        let poll =
          Poller.
            {
              queue = [ Operation_kind.Ci ];
              merged = false;
              closed = false;
              is_draft = false;
              merge_state = Pr_state.Mergeable;
              merge_ready = false;
              head_oid = None;
              review_decision = None;
              unresolved_comment_count = 0;
              merge_queue_required = false;
              merge_queue_entry = None;
              checks_passing = false;
              ci_checks = [ check ];
              merge_commit_sha = None;
            }
        in
        let orch, _logs, _newly_blocked =
          Patch_controller.apply_poll_result orch pid
            (make_poll_observation poll)
        in
        let agent = Orchestrator.agent orch pid in
        Int.equal after_skip.Patch_agent.ci_failure_count 0
        && (not after_skip.Patch_agent.busy)
        && List.mem agent.Patch_agent.queue Operation_kind.Ci
             ~equal:Operation_kind.equal)
  in

  let prop_poll_result_persists_world_flags =
    Test.make ~name:"patch_controller: poll result persists checks_passing flag"
      ~count:200
      Gen.(quad gen_patch_id gen_branch bool bool)
      (fun (pid, branch, checks_passing, merge_ready) ->
        let patch = make_patch pid branch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:true
            ~pr_body_delivered:false ~start_attempts_without_pr:0 ()
        in
        let orch = make_orch patch agent in
        let poll =
          Poller.
            {
              queue = [];
              merged = false;
              closed = false;
              is_draft = true;
              merge_state = Pr_state.Mergeable;
              merge_ready;
              head_oid = None;
              review_decision = None;
              unresolved_comment_count = 0;
              merge_queue_required = false;
              merge_queue_entry = None;
              checks_passing;
              ci_checks = [];
              merge_commit_sha = None;
            }
        in
        let orch, _logs, _newly_blocked =
          Patch_controller.apply_poll_result orch pid
            (make_poll_observation poll)
        in
        let a = Orchestrator.agent orch pid in
        Bool.equal a.Patch_agent.checks_passing checks_passing
        && Bool.equal a.Patch_agent.merge_ready merge_ready)
  in

  let prop_poll_observation_updates_branch_metadata =
    Test.make
      ~name:
        "patch_controller: poll observation updates base branch, \
         branch_blocked, and worktree"
      ~count:100
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let observed_base = Branch.of_string "stack/base" in
        let patch = make_patch pid branch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~merged:false ~queue:[] ~base_branch:None ~is_draft:true
            ~pr_body_delivered:false ~start_attempts_without_pr:0 ()
        in
        let orch = make_orch patch agent in
        let poll =
          Poller.
            {
              queue = [];
              merged = false;
              closed = false;
              is_draft = true;
              merge_state = Pr_state.Mergeable;
              merge_ready = false;
              head_oid = None;
              review_decision = None;
              unresolved_comment_count = 0;
              merge_queue_required = false;
              merge_queue_entry = None;
              checks_passing = false;
              ci_checks = [];
              merge_commit_sha = None;
            }
        in
        let observation =
          Patch_controller.
            {
              poll_result = poll;
              base_branch = Some observed_base;
              branch_in_root = true;
              worktree_path = Some "/tmp/custom-worktree";
            }
        in
        let orch, _logs, newly_blocked =
          Patch_controller.apply_poll_result orch pid observation
        in
        let a = Orchestrator.agent orch pid in
        Option.equal Branch.equal a.Patch_agent.base_branch (Some observed_base)
        && Option.equal String.equal a.Patch_agent.worktree_path
             (Some "/tmp/custom-worktree")
        && (not a.Patch_agent.branch_blocked)
        && newly_blocked)
  in

  let prop_mixed_cycle_converges_for_bootstrap_patch =
    Test.make
      ~name:
        "patch_controller: mixed bootstrap cycle converges after acked effects \
         and delivered notes"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        (* pr_body_delivered=false so Pr_body is enqueued in cycle 1.
           After pr_body delivery, cycle 2 should converge with only a
           draft effect and no more Respond actions. *)
        let agent =
          Patch_agent.restore ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~has_session:false ~busy:false ~merged:false ~queue:[]
            ~satisfies:false ~changed:false ~has_conflict:false
            ~base_branch:(Some main) ~notified_base_branch:(Some main)
            ~ci_failure_count:0 ~session_fallback:Patch_agent.Fresh_available
            ~human_messages:[] ~inflight_human_messages:[] ~ci_checks:[]
            ~merge_ready:false ~mergeability_unknown:false
            ~merge_queue_required:false ~merge_queue_entry:None ~is_draft:true
            ~pr_body_delivered:false ~pr_body_artifact_miss_count:0
            ~start_attempts_without_pr:0 ~conflict_noop_count:0
            ~no_commits_push_count:0 ~context_exhaustion_count:0
            ~push_failure_count:0 ~rebase_failure_count:0
            ~branch_rebased_onto:(Some main) ~branch_rebased_onto_sha:None
            ~merge_commit_sha:None ~base_contains_merged_siblings:true
            ~anchor_history:Onton_core.Anchor_history.empty
            ~checks_passing:false ~current_op:None
            ~current_op_state:Patch_agent.Queued ~current_message_id:None
            ~generation:0 ~worktree_path:None ~branch_blocked:false
            ~llm_session_id:None ~automerge_enabled:false
            ~automerge_deadline:None ~automerge_inflight:false
            ~automerge_failure_count:0 ~delivered_ci_run_ids:[] ()
        in
        let orch = make_orch patch agent in
        begin try
          (* Cycle 1: Pr_body enqueued *)
          let orch1, _effects1, actions1 =
            run_controller_cycle ~gameplan orch
          in
          let has_pr_body_action =
            List.exists actions1 ~f:(function
              | Orchestrator.Respond (action_pid, kind) ->
                  Patch_id.equal action_pid pid
                  && Operation_kind.equal kind Operation_kind.Pr_body
              | Orchestrator.Start _ | Orchestrator.Rebase _ -> false)
          in
          if not has_pr_body_action then false
          else
            (* Fire Pr_body action and simulate delivery. CI must be
               observed green before the controller will mark ready —
               that's a precondition of the fixpoint. *)
            let orch1 =
              Orchestrator.fire orch1
                (Orchestrator.Respond (pid, Operation_kind.Pr_body))
            in
            let orch1 = Orchestrator.set_pr_body_delivered orch1 pid true in
            let orch1 = Orchestrator.set_checks_passing orch1 pid true in
            let orch1 = Orchestrator.complete orch1 pid in
            (* Cycle 2: should converge — only draft effect *)
            let _orch2, effects2, actions2 =
              run_controller_cycle ~gameplan orch1
            in
            has_draft_effect effects2
            && not
                 (List.exists actions2 ~f:(function
                   | Orchestrator.Respond (action_pid, _kind) ->
                       Patch_id.equal action_pid pid
                   | Orchestrator.Start _ | Orchestrator.Rebase _ -> false))
        with _ -> false
        end)
  in

  (* -- Base branch reconciliation properties -- *)
  let has_base_effect effects =
    List.exists effects ~f:(function
      | Patch_controller.Set_pr_base _ -> true
      | Patch_controller.Set_pr_draft _ -> false)
  in

  let base_effect effects =
    List.find_map effects ~f:(function
      | Patch_controller.Set_pr_base _ as e -> Some e
      | Patch_controller.Set_pr_draft _ -> None)
  in

  let prop_set_pr_base_emitted_on_mismatch =
    Test.make
      ~name:
        "patch_controller: Set_pr_base emitted when actual base differs from \
         graph-expected base"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        (* Agent has base_branch = branch, but graph says main (no deps) *)
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~merged:false ~queue:[] ~base_branch:(Some branch) ~is_draft:true
            ~pr_body_delivered:true ~start_attempts_without_pr:0 ()
        in
        let orch = make_orch patch agent in
        let _orch', effects =
          Patch_controller.reconcile_all orch ~project_name:"test-project"
            ~gameplan
        in
        if Branch.equal branch main then not (has_base_effect effects)
        else
          match base_effect effects with
          | Some (Patch_controller.Set_pr_base { base; _ }) ->
              Branch.equal base main
          | Some (Patch_controller.Set_pr_draft _) | None -> false)
  in

  let prop_set_pr_base_not_emitted_when_correct =
    Test.make
      ~name:
        "patch_controller: Set_pr_base not emitted when base matches \
         graph-expected base"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        (* Agent has base_branch = main, graph says main (no deps) → match *)
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:true
            ~pr_body_delivered:true ~start_attempts_without_pr:0 ()
        in
        let orch = make_orch patch agent in
        let _orch', effects =
          Patch_controller.reconcile_all orch ~project_name:"test-project"
            ~gameplan
        in
        not (has_base_effect effects))
  in

  let prop_set_pr_base_converges =
    Test.make
      ~name:
        "patch_controller: Set_pr_base converges after applying effect success"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~merged:false ~queue:[] ~base_branch:(Some branch) ~is_draft:true
            ~pr_body_delivered:true ~start_attempts_without_pr:0 ()
        in
        let orch = make_orch patch agent in
        let orch1, effects1 =
          Patch_controller.reconcile_all orch ~project_name:"test-project"
            ~gameplan
        in
        let orch2 = apply_all_effect_successes orch1 effects1 in
        let _orch3, effects2 =
          Patch_controller.reconcile_all orch2 ~project_name:"test-project"
            ~gameplan
        in
        not (has_base_effect effects2))
  in

  let prop_poll_always_refreshes_base =
    Test.make
      ~name:
        "patch_controller: poll always refreshes base_branch from observation"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let new_base = Branch.of_string "new-base" in
        let patch = make_patch pid branch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~merged:false ~queue:[] ~base_branch:(Some branch) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0 ()
        in
        let orch = make_orch patch agent in
        let obs =
          Patch_controller.
            {
              poll_result =
                Poller.
                  {
                    queue = [];
                    merged = false;
                    closed = false;
                    is_draft = false;
                    merge_state = Pr_state.Mergeable;
                    merge_ready = false;
                    head_oid = None;
                    review_decision = None;
                    unresolved_comment_count = 0;
                    merge_queue_required = false;
                    merge_queue_entry = None;
                    checks_passing = false;
                    ci_checks = [];
                    merge_commit_sha = None;
                  };
              base_branch = Some new_base;
              branch_in_root = false;
              worktree_path = None;
            }
        in
        let orch', _logs, _blocked =
          Patch_controller.apply_poll_result orch pid obs
        in
        let updated = Orchestrator.agent orch' pid in
        Option.equal Branch.equal updated.Patch_agent.base_branch
          (Some new_base))
  in

  (* -- Discovery intents properties -- *)
  let prop_discovery_intents_filters_correctly =
    Test.make
      ~name:
        "patch_controller: discovery_intents returns exactly has_session && \
         !has_pr && !merged agents"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        (* Agent with session but no PR → should appear *)
        let agent_session_no_pr =
          Patch_agent.restore ~patch_id:pid ~branch
            ~pr_status:Patch_pr_status.Absent ~has_session:true ~busy:false
            ~merged:false ~queue:[] ~satisfies:false ~changed:false
            ~has_conflict:false ~base_branch:None ~notified_base_branch:None
            ~ci_failure_count:0 ~session_fallback:Patch_agent.Fresh_available
            ~human_messages:[] ~inflight_human_messages:[] ~ci_checks:[]
            ~merge_ready:false ~mergeability_unknown:false
            ~merge_queue_required:false ~merge_queue_entry:None ~is_draft:false
            ~pr_body_delivered:true ~pr_body_artifact_miss_count:0
            ~start_attempts_without_pr:0 ~conflict_noop_count:0
            ~no_commits_push_count:0 ~context_exhaustion_count:0
            ~push_failure_count:0 ~rebase_failure_count:0
            ~branch_rebased_onto:None ~branch_rebased_onto_sha:None
            ~merge_commit_sha:None ~base_contains_merged_siblings:true
            ~anchor_history:Onton_core.Anchor_history.empty
            ~checks_passing:false ~current_op:None
            ~current_op_state:Patch_agent.Queued ~current_message_id:None
            ~generation:0 ~worktree_path:None ~branch_blocked:false
            ~llm_session_id:None ~automerge_enabled:false
            ~automerge_deadline:None ~automerge_inflight:false
            ~automerge_failure_count:0 ~delivered_ci_run_ids:[] ()
        in
        let orch =
          Orchestrator.restore
            ~graph:(Graph.of_patches [ patch ])
            ~agents:
              (Map.of_alist_exn
                 (module Patch_id)
                 [ (pid, agent_session_no_pr) ])
            ~outbox:(Map.empty (module Message_id))
            ~main_branch:main ()
        in
        let intents = Patch_controller.discovery_intents orch in
        List.length intents = 1
        && List.exists intents ~f:(fun (intent_pid, _) ->
            Patch_id.equal intent_pid pid))
  in

  let prop_discovery_intents_excludes_pr_agents =
    Test.make
      ~name:"patch_controller: discovery_intents excludes agents that have a PR"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let agent =
          Patch_agent.restore ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 42))
            ~has_session:true ~busy:false ~merged:false ~queue:[]
            ~satisfies:false ~changed:false ~has_conflict:false
            ~base_branch:(Some main) ~notified_base_branch:(Some main)
            ~ci_failure_count:0 ~session_fallback:Patch_agent.Fresh_available
            ~human_messages:[] ~inflight_human_messages:[] ~ci_checks:[]
            ~merge_ready:false ~mergeability_unknown:false
            ~merge_queue_required:false ~merge_queue_entry:None ~is_draft:false
            ~pr_body_delivered:true ~pr_body_artifact_miss_count:0
            ~start_attempts_without_pr:0 ~conflict_noop_count:0
            ~no_commits_push_count:0 ~context_exhaustion_count:0
            ~push_failure_count:0 ~rebase_failure_count:0
            ~branch_rebased_onto:None ~branch_rebased_onto_sha:None
            ~merge_commit_sha:None ~base_contains_merged_siblings:true
            ~anchor_history:Onton_core.Anchor_history.empty
            ~checks_passing:false ~current_op:None
            ~current_op_state:Patch_agent.Queued ~current_message_id:None
            ~generation:0 ~worktree_path:None ~branch_blocked:false
            ~llm_session_id:None ~automerge_enabled:false
            ~automerge_deadline:None ~automerge_inflight:false
            ~automerge_failure_count:0 ~delivered_ci_run_ids:[] ()
        in
        let orch =
          Orchestrator.restore
            ~graph:(Graph.of_patches [ patch ])
            ~agents:(Map.of_alist_exn (module Patch_id) [ (pid, agent) ])
            ~outbox:(Map.empty (module Message_id))
            ~main_branch:main ()
        in
        List.is_empty (Patch_controller.discovery_intents orch))
  in

  (* Bug repro: an ad-hoc agent transitioned to Missing must not crash
     reconcile_messages, and must surface as needs_intervention with no
     Start message issued. *)
  let prop_missing_adhoc_does_not_crash_reconcile =
    Test.make ~name:"reconcile_messages tolerates Missing ad-hoc agent" ~count:1
      Gen.(return ())
      (fun () ->
        let pid = Patch_id.of_string "999" in
        let branch = Branch.of_string "feat/vanished" in
        let orch = Orchestrator.create ~patches:[] ~main_branch:main in
        let orch =
          Orchestrator.add_agent orch ~patch_id:pid ~branch ~base_branch:main
            ~pr_number:(Pr_number.of_int 999)
        in
        let orch = Orchestrator.mark_pr_missing orch pid in
        try
          let messages = Patch_controller.plan_messages orch ~patches:[] in
          let agent = Orchestrator.agent orch pid in
          let no_start_message =
            List.for_all messages
              ~f:(fun (msg : Orchestrator.patch_agent_message) ->
                match msg.action with
                | Orchestrator.Start _ -> false
                | Orchestrator.Respond _ | Orchestrator.Rebase _ -> true)
          in
          Patch_agent.needs_intervention agent
          && Patch_agent.is_pr_missing agent
          && no_start_message
        with Invalid_argument _ -> false)
  in

  (* Missing -> Present recovery via apply_poll_result. A poll that returns a
     non-merged, non-conflict state means the remote currently has the PR;
     the controller must lift the agent from Missing back to Present so
     downstream planning can proceed. *)
  let prop_apply_poll_lifts_missing_to_present =
    Test.make ~name:"apply_poll_result lifts Missing -> Present on observe"
      ~count:1
      Gen.(return ())
      (fun () ->
        let pid = Patch_id.of_string "777" in
        let branch = Branch.of_string "feat/recovered" in
        let orch = Orchestrator.create ~patches:[] ~main_branch:main in
        let orch =
          Orchestrator.add_agent orch ~patch_id:pid ~branch ~base_branch:main
            ~pr_number:(Pr_number.of_int 777)
        in
        (* Populate state that should survive the roundtrip *)
        let orch =
          Orchestrator.record_delivered_ci_run_ids orch pid [ 11; 12 ]
        in
        let orch = Orchestrator.set_pr_body_delivered orch pid true in
        let before = Orchestrator.agent orch pid in
        let orch = Orchestrator.mark_pr_missing orch pid in
        let poll_result =
          Poller.
            {
              merged = false;
              merge_state = Pr_state.Mergeable;
              ci_checks = [];
              checks_passing = true;
              merge_ready = false;
              head_oid = None;
              review_decision = None;
              unresolved_comment_count = 0;
              merge_queue_required = false;
              merge_queue_entry = None;
              queue = [];
              is_draft = false;
              closed = false;
              merge_commit_sha = None;
            }
        in
        let observation =
          Patch_controller.
            {
              poll_result;
              base_branch = None;
              branch_in_root = false;
              worktree_path = None;
            }
        in
        let orch, _logs, _newly_blocked =
          Patch_controller.apply_poll_result orch pid observation
        in
        let after = Orchestrator.agent orch pid in
        Patch_agent.is_pr_present after
        && (not (Patch_agent.is_pr_missing after))
        && List.equal Int.equal after.Patch_agent.delivered_ci_run_ids
             before.Patch_agent.delivered_ci_run_ids
        && Bool.equal after.Patch_agent.pr_body_delivered
             before.Patch_agent.pr_body_delivered)
  in

  (* Child of a Missing parent must not be Start-eligible: the parent's
     branch may not exist on the remote, so deps_satisfied gating on
     is_pr_present (not has_pr) is the correct semantic. *)
  let prop_child_of_missing_parent_not_startable =
    Test.make
      ~name:"plan_action_for_patch: child of Missing parent not Start-eligible"
      ~count:1
      Gen.(return ())
      (fun () ->
        let parent_pid = Patch_id.of_string "parent" in
        let child_pid = Patch_id.of_string "child" in
        let parent_branch = Branch.of_string "feat/parent" in
        let child_branch = Branch.of_string "feat/child" in
        let parent_patch = make_patch parent_pid parent_branch in
        let child_patch =
          {
            (make_patch child_pid child_branch) with
            dependencies = [ parent_pid ];
          }
        in
        let patches = [ parent_patch; child_patch ] in
        let gameplan = { (make_gameplan parent_patch) with patches } in
        let orch = Orchestrator.create ~patches ~main_branch:main in
        (* Bootstrap parent: Start, set_pr_number, complete, then mark Missing *)
        let orch =
          Orchestrator.fire orch (Orchestrator.Start (parent_pid, main))
        in
        let orch =
          Orchestrator.set_pr_number orch parent_pid (Pr_number.of_int 11)
        in
        let orch = Orchestrator.complete orch parent_pid in
        let orch = Orchestrator.mark_pr_missing orch parent_pid in
        (* Plan: child should NOT be Start-eligible because parent is Missing. *)
        let messages =
          Patch_controller.plan_messages orch ~patches:gameplan.Gameplan.patches
        in
        not
          (List.exists messages
             ~f:(fun (msg : Orchestrator.patch_agent_message) ->
               match msg.action with
               | Orchestrator.Start (pid, _) -> Patch_id.equal pid child_pid
               | Orchestrator.Respond _ | Orchestrator.Rebase _ -> false)))
  in

  let merge_queue_entry ?(state = Pr_state.Mq_queued) ?(position = 1) id =
    Pr_state.{ id; state; position }
  in

  let pending_patch_4_automerge_enqueue_action =
    Test.make
      ~name:
        "patch_controller: Patch 4 reconcile chooses Enqueue on queue branch"
      QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "mq-enqueue" in
        let branch = Branch.of_string "feat/mq-enqueue" in
        let patch = make_patch pid branch in
        let pr_number = Pr_number.of_int 401 in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present pr_number) ~merged:false
            ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:true ~checks_passing:true ~merge_queue_required:true
            ~automerge_enabled:true ~automerge_deadline:0.0 ()
        in
        let orch = make_orch patch agent in
        let orch, decisions =
          Patch_controller.reconcile_automerge orch ~now:1.0
        in
        match decisions with
        | [ { Patch_controller.merge_patch_id; merge_pr_number; action } ] ->
            Patch_id.equal merge_patch_id pid
            && Pr_number.equal merge_pr_number pr_number
            && Patch_controller.equal_merge_action action
                 Patch_controller.Enqueue
            && (Orchestrator.agent orch pid).Patch_agent.automerge_inflight
        | _ -> false)
  in

  let pending_patch_4_automerge_enqueued_idle =
    Test.make
      ~name:"patch_controller: Patch 4 enqueued PR yields no merge or enqueue"
      QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "mq-idle" in
        let branch = Branch.of_string "feat/mq-idle" in
        let patch = make_patch pid branch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 402))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:true ~checks_passing:true ~merge_queue_required:true
            ~merge_queue_entry:(Some (merge_queue_entry "MQE_idle"))
            ~automerge_enabled:true ~automerge_deadline:0.0 ()
        in
        let orch = make_orch patch agent in
        let orch, decisions =
          Patch_controller.reconcile_automerge orch ~now:1.0
        in
        List.is_empty decisions
        && Option.is_none
             (Orchestrator.agent orch pid).Patch_agent.automerge_deadline)
  in

  let pending_patch_4_merge_queue_entered_clears_timer =
    Test.make
      ~name:
        "patch_controller: Patch 4 entering merge queue records entry and \
         clears automerge timer" QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "mq-enqueue-success" in
        let branch = Branch.of_string "feat/mq-enqueue-success" in
        let patch = make_patch pid branch in
        let entry = merge_queue_entry "MQE_enqueue_success" in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 406))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:true ~checks_passing:true ~merge_queue_required:true
            ~automerge_enabled:true ~automerge_deadline:0.0
            ~automerge_failure_count:1 ()
        in
        let orch = make_orch patch agent in
        let orch = Orchestrator.set_automerge_inflight orch pid true in
        let orch = Patch_controller.apply_merge_queue_entered orch pid entry in
        let agent = Orchestrator.agent orch pid in
        Option.equal Pr_state.equal_merge_queue_entry agent.merge_queue_entry
          (Some entry)
        && agent.merge_queue_required
        && Option.is_none agent.automerge_deadline
        && (not agent.automerge_inflight)
        && agent.automerge_failure_count = 0)
  in

  let pending_patch_4_unapproved_not_enqueued =
    Test.make
      ~name:
        "patch_controller: Patch 4 unapproved queue PR without entry is not \
         enqueued" QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "mq-unapproved-not-enqueued" in
        let branch = Branch.of_string "feat/mq-unapproved-not-enqueued" in
        let patch = make_patch pid branch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 405))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:false ~checks_passing:true ~merge_queue_required:true
            ~automerge_enabled:true ~automerge_deadline:0.0 ()
        in
        let orch = make_orch patch agent in
        let orch, decisions =
          Patch_controller.reconcile_automerge orch ~now:1.0
        in
        let agent = Orchestrator.agent orch pid in
        List.is_empty decisions
        && Option.is_none agent.Patch_agent.automerge_deadline
        && not agent.Patch_agent.automerge_inflight)
  in

  let pending_patch_4_automerge_dequeue_on_lost_approval =
    Test.make
      ~name:
        "patch_controller: Patch 4 enqueued PR that lost approval is dequeued"
      QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "mq-dequeue" in
        let branch = Branch.of_string "feat/mq-dequeue" in
        let patch = make_patch pid branch in
        let pr_number = Pr_number.of_int 403 in
        let entry = merge_queue_entry "MQE_dequeue" in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present pr_number) ~merged:false
            ~queue:[ Operation_kind.Review_comments ]
            ~base_branch:(Some main) ~is_draft:false ~pr_body_delivered:true
            ~start_attempts_without_pr:0 ~merge_ready:false ~checks_passing:true
            ~merge_queue_required:true ~merge_queue_entry:(Some entry)
            ~automerge_enabled:true ~automerge_deadline:0.0 ()
        in
        let orch = make_orch patch agent in
        let orch, decisions =
          Patch_controller.reconcile_automerge orch ~now:1.0
        in
        match decisions with
        | [ { Patch_controller.merge_patch_id; merge_pr_number; action } ] ->
            Patch_id.equal merge_patch_id pid
            && Pr_number.equal merge_pr_number pr_number
            && Patch_controller.equal_merge_action action
                 (Patch_controller.Dequeue entry.Pr_state.id)
            && (Orchestrator.agent orch pid).Patch_agent.automerge_inflight
        | _ -> false)
  in

  let pending_patch_4_unmergeable_dequeues =
    Test.make
      ~name:
        "patch_controller: Patch 4 UNMERGEABLE entry dequeues from merge queue"
      QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "mq-unmergeable" in
        let branch = Branch.of_string "feat/mq-unmergeable" in
        let patch = make_patch pid branch in
        let pr_number = Pr_number.of_int 404 in
        let entry =
          merge_queue_entry "MQE_unmergeable" ~state:Pr_state.Mq_unmergeable
        in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present pr_number) ~merged:false
            ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:true ~checks_passing:true ~merge_queue_required:true
            ~merge_queue_entry:(Some entry) ~automerge_enabled:true
            ~automerge_deadline:0.0 ()
        in
        let orch = make_orch patch agent in
        let orch, decisions =
          Patch_controller.reconcile_automerge orch ~now:1.0
        in
        match decisions with
        | [ { Patch_controller.merge_patch_id; merge_pr_number; action } ] ->
            Patch_id.equal merge_patch_id pid
            && Pr_number.equal merge_pr_number pr_number
            && Patch_controller.equal_merge_action action
                 (Patch_controller.Dequeue entry.Pr_state.id)
            && (Orchestrator.agent orch pid).Patch_agent.automerge_inflight
        | _ -> false)
  in

  let pending_patch_4_visible_ci_failure_dequeues =
    Test.make
      ~name:
        "patch_controller: queued PR with visible failing check dequeues from \
         merge queue" QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "mq-ci-failure" in
        let branch = Branch.of_string "feat/mq-ci-failure" in
        let patch = make_patch pid branch in
        let pr_number = Pr_number.of_int 407 in
        let entry = merge_queue_entry "MQE_ci_failure" in
        let failing_check =
          Ci_check.
            {
              name = "ci";
              conclusion = "failure";
              details_url = None;
              description = None;
              started_at = None;
              id = Some 407;
            }
        in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present pr_number) ~merged:false
            ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:false ~checks_passing:false
            ~ci_checks:[ failing_check ] ~merge_queue_required:true
            ~merge_queue_entry:(Some entry) ~automerge_enabled:true ()
        in
        let orch = make_orch patch agent in
        let orch, decisions =
          Patch_controller.reconcile_automerge orch ~now:1.0
        in
        match decisions with
        | [ { Patch_controller.merge_patch_id; merge_pr_number; action } ] ->
            Patch_id.equal merge_patch_id pid
            && Pr_number.equal merge_pr_number pr_number
            && Patch_controller.equal_merge_action action
                 (Patch_controller.Dequeue entry.Pr_state.id)
            && (Orchestrator.agent orch pid).Patch_agent.automerge_inflight
        | _ -> false)
  in

  let pending_patch_4_conflict_alarm_dequeues =
    Test.make
      ~name:
        "patch_controller: queued PR with conflict alarm dequeues from merge \
         queue"
      QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "mq-conflict" in
        let branch = Branch.of_string "feat/mq-conflict" in
        let patch = make_patch pid branch in
        let pr_number = Pr_number.of_int 408 in
        let entry = merge_queue_entry "MQE_conflict" in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present pr_number) ~merged:false
            ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:true ~checks_passing:true ~has_conflict:true
            ~merge_queue_required:true ~merge_queue_entry:(Some entry)
            ~automerge_enabled:true ()
        in
        let orch = make_orch patch agent in
        let orch, decisions =
          Patch_controller.reconcile_automerge orch ~now:1.0
        in
        match decisions with
        | [ { Patch_controller.merge_patch_id; merge_pr_number; action } ] ->
            Patch_id.equal merge_patch_id pid
            && Pr_number.equal merge_pr_number pr_number
            && Patch_controller.equal_merge_action action
                 (Patch_controller.Dequeue entry.Pr_state.id)
            && (Orchestrator.agent orch pid).Patch_agent.automerge_inflight
        | _ -> false)
  in

  (* A direct-merge patch that is approval-ready in every respect but reads
     merge_ready=false solely because GitHub is recomputing mergeability
     (mergeStateStatus=UNKNOWN) after the base advanced. The existing armed
     deadline must be PRESERVED (not cleared and re-armed), so the idle window
     measures real elapsed time across sibling merges. Regression test for the
     "every merge resets all the other automerge counters" bug. *)
  let pending_automerge_transient_unknown_holds_deadline =
    Test.make
      ~name:
        "patch_controller: transient mergeability Unknown holds the automerge \
         deadline" QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "am-unknown-hold" in
        let branch = Branch.of_string "feat/am-unknown-hold" in
        let patch = make_patch pid branch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 910))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:false ~mergeability_unknown:true ~checks_passing:true
            ~automerge_enabled:true ~automerge_deadline:100.0 ()
        in
        let orch = make_orch patch agent in
        let orch, decisions =
          Patch_controller.reconcile_automerge orch ~now:1.0
        in
        let agent = Orchestrator.agent orch pid in
        List.is_empty decisions
        && Option.equal Float.equal agent.Patch_agent.automerge_deadline
             (Some 100.0)
        && not agent.Patch_agent.automerge_inflight)
  in

  (* Even when the held deadline has already elapsed, we must NOT fire a merge
     while mergeability is Unknown — firing requires merge_ready. The deadline is
     held (not cleared), so the next ready poll fires immediately. *)
  let pending_automerge_unknown_never_fires_while_indeterminate =
    Test.make
      ~name:
        "patch_controller: elapsed deadline does not fire while mergeability \
         Unknown" QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "am-unknown-nofire" in
        let branch = Branch.of_string "feat/am-unknown-nofire" in
        let patch = make_patch pid branch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 912))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:false ~mergeability_unknown:true ~checks_passing:true
            ~automerge_enabled:true ~automerge_deadline:0.0 ()
        in
        let orch = make_orch patch agent in
        let orch, decisions =
          Patch_controller.reconcile_automerge orch ~now:1.0
        in
        let agent = Orchestrator.agent orch pid in
        List.is_empty decisions
        && Option.equal Float.equal agent.Patch_agent.automerge_deadline
             (Some 0.0)
        && not agent.Patch_agent.automerge_inflight)
  in

  (* A genuine not-ready that is NOT the transient mergeability-Unknown blip
     (here: merge_ready false with mergeability known, e.g. a stale BLOCKED that
     leaves merge_ready false) must still clear the deadline — the hold is scoped
     to mergeability Unknown only. *)
  let pending_automerge_blocked_clears_deadline =
    Test.make
      ~name:
        "patch_controller: not-ready with mergeability known clears the \
         automerge deadline" QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "am-blocked-clear" in
        let branch = Branch.of_string "feat/am-blocked-clear" in
        let patch = make_patch pid branch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 911))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:false ~mergeability_unknown:false ~checks_passing:true
            ~automerge_enabled:true ~automerge_deadline:100.0 ()
        in
        let orch = make_orch patch agent in
        let orch, decisions =
          Patch_controller.reconcile_automerge orch ~now:1.0
        in
        let agent = Orchestrator.agent orch pid in
        List.is_empty decisions
        && Option.is_none agent.Patch_agent.automerge_deadline)
  in

  let review_request_claims_ready_required_pr =
    Test.make ~name:"patch_controller: review request reconcile claims ready PR"
      QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "review-request-ready" in
        let branch = Branch.of_string "feat/review-request-ready" in
        let patch = make_patch pid branch in
        let pr_number = Pr_number.of_int 377 in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present pr_number) ~merged:false
            ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:false ~checks_passing:true
            ~head_oid:(Some "head-ready")
            ~review_decision:(Some "REVIEW_REQUIRED") ()
        in
        let orch = make_orch patch agent in
        let orch, decisions = Patch_controller.reconcile_review_requests orch in
        match decisions with
        | [ { Patch_controller.review_patch_id; review_pr_number } ] ->
            Patch_id.equal review_patch_id pid
            && Pr_number.equal review_pr_number pr_number
            && (Orchestrator.agent orch pid).Patch_agent.review_request_inflight
        | _ -> false)
  in

  let review_request_inflight_is_not_reclaimed =
    Test.make
      ~name:"patch_controller: review request reconcile skips inflight PR"
      QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "review-request-inflight" in
        let branch = Branch.of_string "feat/review-request-inflight" in
        let patch = make_patch pid branch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 378))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:false ~checks_passing:true
            ~head_oid:(Some "head-inflight")
            ~review_decision:(Some "REVIEW_REQUIRED")
            ~review_request_inflight:true ()
        in
        let orch = make_orch patch agent in
        let orch, decisions = Patch_controller.reconcile_review_requests orch in
        let agent = Orchestrator.agent orch pid in
        List.is_empty decisions && agent.Patch_agent.review_request_inflight)
  in

  let review_request_current_head_is_not_reclaimed =
    Test.make
      ~name:
        "patch_controller: review request reconcile skips already-requested \
         head"
      QCheck2.Gen.unit (fun () ->
        let pid = Patch_id.of_string "review-request-current-head" in
        let branch = Branch.of_string "feat/review-request-current-head" in
        let patch = make_patch pid branch in
        let agent =
          make_agent ~patch_id:pid ~branch
            ~pr_status:(Patch_pr_status.Present (Pr_number.of_int 379))
            ~merged:false ~queue:[] ~base_branch:(Some main) ~is_draft:false
            ~pr_body_delivered:true ~start_attempts_without_pr:0
            ~merge_ready:false ~checks_passing:true
            ~head_oid:(Some "head-requested")
            ~review_decision:(Some "REVIEW_REQUIRED")
            ~review_requested_for_oid:(Some "head-requested") ()
        in
        let orch = make_orch patch agent in
        let orch, decisions = Patch_controller.reconcile_review_requests orch in
        let agent = Orchestrator.agent orch pid in
        List.is_empty decisions && not agent.Patch_agent.review_request_inflight)
  in

  let suite =
    [
      review_request_claims_ready_required_pr;
      review_request_inflight_is_not_reclaimed;
      review_request_current_head_is_not_reclaimed;
      pending_automerge_transient_unknown_holds_deadline;
      pending_automerge_unknown_never_fires_while_indeterminate;
      pending_automerge_blocked_clears_deadline;
      pending_patch_4_automerge_enqueue_action;
      pending_patch_4_automerge_enqueued_idle;
      pending_patch_4_merge_queue_entered_clears_timer;
      pending_patch_4_unapproved_not_enqueued;
      pending_patch_4_automerge_dequeue_on_lost_approval;
      pending_patch_4_unmergeable_dequeues;
      pending_patch_4_visible_ci_failure_dequeues;
      pending_patch_4_conflict_alarm_dequeues;
      prop_missing_adhoc_does_not_crash_reconcile;
      prop_apply_poll_lifts_missing_to_present;
      prop_child_of_missing_parent_not_startable;
      prop_deterministic;
      prop_plan_tick_deterministic;
      prop_pr_body_queue_idempotent;
      prop_draft_reemits_until_success;
      prop_unknown_rebase_blocks_ready_for_review;
      prop_intervention_stable_after_threshold;
      prop_reconcile_all_exposes_pr_body_as_next_action;
      prop_reconcile_all_blocks_restart_after_intervention;
      prop_rebase_not_blocked_by_needs_intervention;
      prop_respond_still_blocked_by_needs_intervention;
      prop_reconcile_all_converges_after_acknowledged_effects;
      prop_poll_to_controller_promotes_ready_after_pr_body;
      prop_poll_ci_failure_never_erases_pr_body_followup;
      prop_idle_ci_failure_count_allows_reenqueue;
      prop_delivered_ci_run_does_not_reenqueue;
      prop_new_ci_run_after_skip_empty_is_delivered;
      prop_poll_result_persists_world_flags;
      prop_poll_observation_updates_branch_metadata;
      prop_mixed_cycle_converges_for_bootstrap_patch;
      prop_set_pr_base_emitted_on_mismatch;
      prop_set_pr_base_not_emitted_when_correct;
      prop_set_pr_base_converges;
      prop_poll_always_refreshes_base;
      prop_discovery_intents_filters_correctly;
      prop_discovery_intents_excludes_pr_agents;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

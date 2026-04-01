open Base
open Onton
open Onton.Types

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
    }

let make_gameplan patch =
  Gameplan.
    {
      project_name = "test-project";
      problem_statement = "";
      solution_summary = "";
      design_decisions = "";
      patches = [ patch ];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
    }

let make_orch patch agent =
  let graph = Graph.of_patches [ patch ] in
  let agents = Map.of_alist_exn (module Patch_id) [ (patch.Patch.id, agent) ] in
  Orchestrator.restore ~graph ~agents
    ~outbox:(Map.empty (module Message_id))
    ~main_branch:main

let make_agent ~patch_id ~has_pr ~pr_number ~merged ~needs_intervention ~queue
    ~base_branch ~is_draft ~pr_description_applied
    ~implementation_notes_delivered ~start_attempts_without_pr =
  Patch_agent.restore ~patch_id ~has_pr ~pr_number ~has_session:false
    ~busy:false ~merged ~needs_intervention ~queue ~satisfies:false
    ~changed:false ~has_conflict:false ~base_branch ~ci_failure_count:0
    ~ci_fix_running:false ~session_fallback:Patch_agent.Fresh_available
    ~human_messages:[] ~ci_checks:[] ~mergeable:false ~merge_ready:false
    ~is_draft ~pr_description_applied ~implementation_notes_delivered
    ~start_attempts_without_pr ~checks_passing:false
    ~no_unresolved_comments:false ~current_op:None ~current_message_id:None
    ~generation:0 ~worktree_path:None ~head_branch:None ~branch_blocked:false

let has_notes_queued agent =
  List.mem agent.Patch_agent.queue Operation_kind.Implementation_notes
    ~equal:Operation_kind.equal

let has_description_effect effects =
  List.exists effects ~f:(function
    | Patch_controller.Set_pr_description _ -> true
    | Patch_controller.Set_pr_draft _ -> false)

let has_draft_effect effects =
  List.exists effects ~f:(function
    | Patch_controller.Set_pr_draft _ -> true
    | Patch_controller.Set_pr_description _ -> false)

let description_effect effects =
  List.find_map effects ~f:(function
    | Patch_controller.Set_pr_description _ as e -> Some e
    | Patch_controller.Set_pr_draft _ -> None)

let draft_effect effects =
  List.find_map effects ~f:(function
    | Patch_controller.Set_pr_draft _ as e -> Some e
    | Patch_controller.Set_pr_description _ -> None)

let apply_all_effect_successes orch effects =
  List.fold effects ~init:orch ~f:Patch_controller.apply_github_effect_success

let implementation_notes_action actions pid =
  List.find actions ~f:(function
    | Orchestrator.Respond (action_pid, kind) ->
        Patch_id.equal action_pid pid
        && Operation_kind.equal kind Operation_kind.Implementation_notes
    | Orchestrator.Start _ | Orchestrator.Rebase _ -> false)

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
      head_branch = None;
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
      let* needs_intervention = bool in
      let* queue = gen_operation_kind_queue in
      let* use_main_base = bool in
      let* is_draft = bool in
      let* pr_description_applied = bool in
      let* implementation_notes_delivered = bool in
      let* start_attempts_without_pr = int_range 0 3 in
      let base_branch =
        if has_pr then Some (if use_main_base then main else branch) else None
      in
      let pr_number = if has_pr then Some (Pr_number.of_int 42) else None in
      let patch = make_patch pid branch in
      let agent =
        make_agent ~patch_id:pid ~has_pr ~pr_number ~merged ~needs_intervention
          ~queue ~base_branch ~is_draft ~pr_description_applied
          ~implementation_notes_delivered ~start_attempts_without_pr
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

  let prop_notes_queue_idempotent =
    Test.make
      ~name:"patch_controller: implementation notes queueing is idempotent"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~has_pr:true
            ~pr_number:(Some (Pr_number.of_int 42))
            ~merged:false ~needs_intervention:false ~queue:[]
            ~base_branch:(Some main) ~is_draft:true ~pr_description_applied:true
            ~implementation_notes_delivered:false ~start_attempts_without_pr:0
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
        has_notes_queued a1 && has_notes_queued a2
        && List.count a2.Patch_agent.queue
             ~f:(Operation_kind.equal Operation_kind.Implementation_notes)
           = 1
        && List.is_empty effects1 && List.is_empty effects2)
  in

  let prop_description_reemits_until_success =
    Test.make
      ~name:"patch_controller: description effect re-emits until acknowledged"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~has_pr:true
            ~pr_number:(Some (Pr_number.of_int 42))
            ~merged:false ~needs_intervention:false ~queue:[]
            ~base_branch:(Some branch) ~is_draft:true
            ~pr_description_applied:false ~implementation_notes_delivered:false
            ~start_attempts_without_pr:0
        in
        let orch = make_orch patch agent in
        begin try
          let orch1, effects1 =
            Patch_controller.reconcile_patch orch ~project_name:"test-project"
              ~gameplan ~patch
          in
          match description_effect effects1 with
          | None -> false
          | Some eff ->
              let orch2 =
                Patch_controller.apply_github_effect_success orch1 eff
              in
              let _orch3, effects2 =
                Patch_controller.reconcile_patch orch2
                  ~project_name:"test-project" ~gameplan ~patch
              in
              has_description_effect effects1
              && not (has_description_effect effects2)
        with _ -> false
        end)
  in

  let prop_draft_reemits_until_success =
    Test.make ~name:"patch_controller: draft effect re-emits until acknowledged"
      ~count:200
      Gen.(triple gen_patch_id gen_branch bool)
      (fun (pid, branch, notes_delivered) ->
        let desired_draft = not notes_delivered in
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~has_pr:true
            ~pr_number:(Some (Pr_number.of_int 42))
            ~merged:false ~needs_intervention:false ~queue:[]
            ~base_branch:(Some main) ~is_draft:(not desired_draft)
            ~pr_description_applied:true
            ~implementation_notes_delivered:notes_delivered
            ~start_attempts_without_pr:0
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
          make_agent ~patch_id:pid ~has_pr:false ~pr_number:None ~merged:false
            ~needs_intervention:false ~queue:[] ~base_branch:None
            ~is_draft:false ~pr_description_applied:false
            ~implementation_notes_delivered:false ~start_attempts_without_pr:2
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
        a1.Patch_agent.needs_intervention && a2.Patch_agent.needs_intervention
        && List.is_empty effects1 && List.is_empty effects2)
  in

  let prop_reconcile_all_exposes_notes_as_next_action =
    Test.make
      ~name:
        "patch_controller: reconcile_all exposes missing implementation notes \
         as next Respond action"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~has_pr:true
            ~pr_number:(Some (Pr_number.of_int 42))
            ~merged:false ~needs_intervention:false ~queue:[]
            ~base_branch:(Some main) ~is_draft:true ~pr_description_applied:true
            ~implementation_notes_delivered:false ~start_attempts_without_pr:0
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
              && Operation_kind.equal kind Operation_kind.Implementation_notes
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
          make_agent ~patch_id:pid ~has_pr:false ~pr_number:None ~merged:false
            ~needs_intervention:false ~queue:[] ~base_branch:None
            ~is_draft:false ~pr_description_applied:false
            ~implementation_notes_delivered:false ~start_attempts_without_pr:2
        in
        let orch = make_orch patch agent in
        let orch, effects =
          Patch_controller.reconcile_all orch ~project_name:"test-project"
            ~gameplan
        in
        let actions =
          Patch_controller.plan_actions orch ~patches:gameplan.patches
        in
        (Orchestrator.agent orch pid).Patch_agent.needs_intervention
        && List.is_empty effects
        && not
             (List.exists actions ~f:(function
               | Orchestrator.Start (action_pid, _) ->
                   Patch_id.equal action_pid pid
               | Orchestrator.Respond _ | Orchestrator.Rebase _ -> false)))
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
        let agent =
          make_agent ~patch_id:pid ~has_pr:true
            ~pr_number:(Some (Pr_number.of_int 42))
            ~merged:false ~needs_intervention:false ~queue:[]
            ~base_branch:(Some branch) ~is_draft:true
            ~pr_description_applied:false ~implementation_notes_delivered:true
            ~start_attempts_without_pr:0
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
        has_description_effect effects1
        && (not (has_description_effect effects2))
        && not (has_draft_effect effects2))
  in

  let prop_poll_to_controller_promotes_ready_after_notes =
    Test.make
      ~name:
        "patch_controller: poll draft state plus delivered notes yields no \
         further draft effect once acknowledged"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~has_pr:true
            ~pr_number:(Some (Pr_number.of_int 42))
            ~merged:false ~needs_intervention:false ~queue:[]
            ~base_branch:(Some main) ~is_draft:true ~pr_description_applied:true
            ~implementation_notes_delivered:true ~start_attempts_without_pr:0
        in
        let orch = make_orch patch agent in
        let poll =
          Poller.
            {
              queue = [];
              merged = false;
              closed = false;
              is_draft = true;
              has_conflict = false;
              mergeable = false;
              merge_ready = false;
              checks_passing = false;
              ci_checks = [];
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

  let prop_poll_ci_failure_never_erases_notes_followup =
    Test.make
      ~name:
        "patch_controller: poll-applicator CI queue does not suppress missing \
         implementation notes action"
      ~count:200
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let patch = make_patch pid branch in
        let gameplan = make_gameplan patch in
        let agent =
          make_agent ~patch_id:pid ~has_pr:true
            ~pr_number:(Some (Pr_number.of_int 42))
            ~merged:false ~needs_intervention:false ~queue:[]
            ~base_branch:(Some main) ~is_draft:true ~pr_description_applied:true
            ~implementation_notes_delivered:false ~start_attempts_without_pr:0
        in
        let orch = make_orch patch agent in
        let poll =
          Poller.
            {
              queue = [ Operation_kind.Ci ];
              merged = false;
              closed = false;
              is_draft = true;
              has_conflict = false;
              mergeable = false;
              merge_ready = false;
              checks_passing = false;
              ci_checks = [];
            }
        in
        let orch, _logs, _newly_blocked =
          Patch_controller.apply_poll_result orch pid
            (make_poll_observation poll)
        in
        let orch, _effects, actions = run_controller_cycle ~gameplan orch in
        let a = Orchestrator.agent orch pid in
        (* CI is dispatched as the current action (higher priority), while
           implementation notes remain queued for the next cycle. *)
        has_notes_queued a
        && List.exists actions ~f:(function
          | Orchestrator.Respond (action_pid, kind) ->
              Patch_id.equal action_pid pid
              && Operation_kind.equal kind Operation_kind.Ci
          | Orchestrator.Start _ | Orchestrator.Rebase _ -> false))
  in

  let prop_poll_result_persists_world_flags =
    Test.make
      ~name:
        "patch_controller: poll result persists mergeable and checks_passing \
         flags"
      ~count:200
      Gen.(quad gen_patch_id gen_branch bool bool)
      (fun (pid, branch, mergeable, checks_passing) ->
        let patch = make_patch pid branch in
        let agent =
          make_agent ~patch_id:pid ~has_pr:true
            ~pr_number:(Some (Pr_number.of_int 42))
            ~merged:false ~needs_intervention:false ~queue:[]
            ~base_branch:(Some main) ~is_draft:true ~pr_description_applied:true
            ~implementation_notes_delivered:false ~start_attempts_without_pr:0
        in
        let orch = make_orch patch agent in
        let poll =
          Poller.
            {
              queue = [];
              merged = false;
              closed = false;
              is_draft = true;
              has_conflict = false;
              mergeable;
              merge_ready = false;
              checks_passing;
              ci_checks = [];
            }
        in
        let orch, _logs, _newly_blocked =
          Patch_controller.apply_poll_result orch pid
            (make_poll_observation poll)
        in
        let a = Orchestrator.agent orch pid in
        Bool.equal a.Patch_agent.mergeable mergeable
        && Bool.equal a.Patch_agent.checks_passing checks_passing)
  in

  let prop_poll_observation_updates_branch_metadata =
    Test.make
      ~name:
        "patch_controller: poll observation updates head branch, base branch, \
         branch_blocked, and worktree"
      ~count:100
      Gen.(pair gen_patch_id gen_branch)
      (fun (pid, branch) ->
        let head_branch = Branch.of_string "feature/head" in
        let observed_base = Branch.of_string "stack/base" in
        let patch = make_patch pid branch in
        let agent =
          make_agent ~patch_id:pid ~has_pr:true
            ~pr_number:(Some (Pr_number.of_int 42))
            ~merged:false ~needs_intervention:false ~queue:[] ~base_branch:None
            ~is_draft:true ~pr_description_applied:true
            ~implementation_notes_delivered:false ~start_attempts_without_pr:0
        in
        let orch = make_orch patch agent in
        let poll =
          Poller.
            {
              queue = [];
              merged = false;
              closed = false;
              is_draft = true;
              has_conflict = false;
              mergeable = false;
              merge_ready = false;
              checks_passing = false;
              ci_checks = [];
            }
        in
        let observation =
          Patch_controller.
            {
              poll_result = poll;
              head_branch = Some head_branch;
              base_branch = Some observed_base;
              branch_in_root = true;
              worktree_path = Some "/tmp/custom-worktree";
            }
        in
        let orch, _logs, newly_blocked =
          Patch_controller.apply_poll_result orch pid observation
        in
        let a = Orchestrator.agent orch pid in
        Option.equal Branch.equal a.Patch_agent.head_branch (Some head_branch)
        && Option.equal Branch.equal a.Patch_agent.base_branch
             (Some observed_base)
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
        let agent =
          make_agent ~patch_id:pid ~has_pr:true
            ~pr_number:(Some (Pr_number.of_int 42))
            ~merged:false ~needs_intervention:false ~queue:[]
            ~base_branch:(Some main) ~is_draft:true
            ~pr_description_applied:false ~implementation_notes_delivered:false
            ~start_attempts_without_pr:0
        in
        let orch = make_orch patch agent in
        begin try
          let orch1, effects1, actions1 = run_controller_cycle ~gameplan orch in
          match implementation_notes_action actions1 pid with
          | None -> false
          | Some notes_action ->
              let orch1 = Orchestrator.fire orch1 notes_action in
              let orch1 =
                let orch1 =
                  Orchestrator.set_implementation_notes_delivered orch1 pid true
                in
                Orchestrator.complete orch1 pid
              in
              let _orch2, effects2, actions2 =
                run_controller_cycle ~gameplan orch1
              in
              has_description_effect effects1
              && List.exists actions1 ~f:(function
                | Orchestrator.Respond (action_pid, kind) ->
                    Patch_id.equal action_pid pid
                    && Operation_kind.equal kind
                         Operation_kind.Implementation_notes
                | Orchestrator.Start _ | Orchestrator.Rebase _ -> false)
              && has_draft_effect effects2
              && not
                   (List.exists actions2 ~f:(function
                     | Orchestrator.Respond (action_pid, kind) ->
                         Patch_id.equal action_pid pid
                         && Operation_kind.equal kind
                              Operation_kind.Implementation_notes
                     | Orchestrator.Start _ | Orchestrator.Rebase _ -> false))
        with _ -> false
        end)
  in

  let suite =
    [
      prop_deterministic;
      prop_plan_tick_deterministic;
      prop_notes_queue_idempotent;
      prop_description_reemits_until_success;
      prop_draft_reemits_until_success;
      prop_intervention_stable_after_threshold;
      prop_reconcile_all_exposes_notes_as_next_action;
      prop_reconcile_all_blocks_restart_after_intervention;
      prop_reconcile_all_converges_after_acknowledged_effects;
      prop_poll_to_controller_promotes_ready_after_notes;
      prop_poll_ci_failure_never_erases_notes_followup;
      prop_poll_result_persists_world_flags;
      prop_poll_observation_updates_branch_metadata;
      prop_mixed_cycle_converges_for_bootstrap_patch;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

(* @archlint.module test
   @archlint.domain orchestrator *)

open Base
open Onton
open Onton_core
open Onton_core.Types

(** QCheck2 property-based tests for orchestrator tick and spawn logic.

    These properties verify the spec's liveness guarantees, action
    preconditions, dependency ordering, and invariant preservation under
    arbitrary command sequences. *)

let main = Branch.of_string "main"
let merge_queue_entry = Pr_state.{ id = "mq"; state = Mq_queued; position = 1 }

let patch ?(dependencies = []) id =
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

let make_gameplan patches =
  Gameplan.
    {
      project_name = "test-project";
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
    }

let tick orch ~patches =
  Patch_controller.tick orch ~project_name:"test-project"
    ~gameplan:(make_gameplan patches)

let pending_actions orch ~patches =
  let _orch, _effects, actions =
    Patch_controller.plan_tick orch ~project_name:"test-project"
      ~gameplan:(make_gameplan patches)
  in
  actions

(* ========== Tick action precondition properties ========== *)

let () =
  let open QCheck2 in
  let open Onton_test_support.Test_generators in
  (* Every Start action targets a patch that does not yet have a PR *)
  let prop_start_targets_no_pr =
    Test.make ~name:"tick: Start only for patches without PR"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let _orch, _effects, actions = tick orch ~patches in
          List.for_all actions ~f:(function
            | Orchestrator.Start (pid, _) ->
                not (Patch_agent.has_pr (Orchestrator.agent orch pid))
            | Orchestrator.Respond (_, _)
            | Orchestrator.Rebase (_, _)
            | Orchestrator.Reconcile_branch _ ->
                true)
        with _ -> false)
  in

  (* Every Respond action targets a patch that has_pr, not merged, not busy,
     not needs_intervention *)
  let prop_respond_preconditions =
    Test.make ~name:"tick: Respond respects preconditions" gen_patch_list_unique
      (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          (* Tick once to start patches, then complete + enqueue to get responds *)
          let orch, _effects, _actions = tick orch ~patches in
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                let a = Orchestrator.agent o p.Patch.id in
                if a.Patch_agent.busy then
                  let o =
                    Orchestrator.set_pr_number o p.Patch.id (Pr_number.of_int 1)
                  in
                  let o = Orchestrator.complete o p.Patch.id in
                  Orchestrator.enqueue o p.Patch.id Operation_kind.Ci
                else o)
          in
          let _orch, _effects, actions = tick orch ~patches in
          List.for_all actions ~f:(function
            | Orchestrator.Respond (pid, _) ->
                let a = Orchestrator.agent orch pid in
                Patch_agent.has_pr a && (not a.Patch_agent.merged)
                && (not a.Patch_agent.busy)
                && not (Patch_agent.needs_intervention a)
            | Orchestrator.Start (_, _)
            | Orchestrator.Rebase (_, _)
            | Orchestrator.Reconcile_branch _ ->
                true)
        with _ -> false)
  in

  (* Respond always picks the highest-priority operation from the queue *)
  let prop_respond_highest_priority =
    Test.make ~name:"tick: Respond picks highest priority" gen_patch_list_unique
      (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch, _effects, _actions = tick orch ~patches in
          (* Enqueue multiple operations *)
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                let a = Orchestrator.agent o p.Patch.id in
                if a.Patch_agent.busy then
                  let o =
                    Orchestrator.set_pr_number o p.Patch.id (Pr_number.of_int 1)
                  in
                  let o = Orchestrator.complete o p.Patch.id in
                  let o = Orchestrator.enqueue o p.Patch.id Operation_kind.Ci in
                  Orchestrator.enqueue o p.Patch.id Operation_kind.Human
                else o)
          in
          let _orch, _effects, actions = tick orch ~patches in
          List.for_all actions ~f:(function
            | Orchestrator.Respond (pid, k) -> (
                let a = Orchestrator.agent orch pid in
                let highest = Patch_agent.highest_priority a in
                match highest with
                | Some expected -> Operation_kind.equal k expected
                | None -> false)
            | Orchestrator.Start (_, _)
            | Orchestrator.Rebase (_, _)
            | Orchestrator.Reconcile_branch _ ->
                true)
        with _ -> false)
  in

  (* ========== Tick idempotence / convergence ========== *)

  (* A patch that already has_pr never gets a Start action *)
  let prop_tick_no_double_start =
    Test.make ~name:"tick: no Start for patches that already have PR"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch, _effects, _actions = tick orch ~patches in
          let _orch, _effects, actions2 = tick orch ~patches in
          List.for_all actions2 ~f:(function
            | Orchestrator.Start (pid, _) ->
                not (Patch_agent.has_pr (Orchestrator.agent orch pid))
            | Orchestrator.Respond (_, _)
            | Orchestrator.Rebase (_, _)
            | Orchestrator.Reconcile_branch _ ->
                true)
        with _ -> false)
  in

  (* pending_actions returns the same actions that tick would fire *)
  let prop_pending_matches_tick =
    Test.make ~name:"tick: pending_actions = tick actions" gen_patch_list_unique
      (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let pending = pending_actions orch ~patches in
          let _orch, _effects, actions = tick orch ~patches in
          let action_equal a b =
            match (a, b) with
            | ( Orchestrator.Reconcile_branch (p1, o1),
                Orchestrator.Reconcile_branch (p2, o2) ) ->
                Patch_id.equal p1 p2 && Int.equal o1 o2
            | ( Orchestrator.Reconcile_branch _,
                ( Orchestrator.Start _ | Orchestrator.Respond _
                | Orchestrator.Rebase _ ) ) ->
                false
            | Orchestrator.Start (p1, b1), Orchestrator.Start (p2, b2) ->
                Patch_id.equal p1 p2 && Branch.equal b1 b2
            | Orchestrator.Respond (p1, k1), Orchestrator.Respond (p2, k2) ->
                Patch_id.equal p1 p2 && Operation_kind.equal k1 k2
            | Orchestrator.Rebase (p1, b1), Orchestrator.Rebase (p2, b2) ->
                Patch_id.equal p1 p2 && Branch.equal b1 b2
            | ( Orchestrator.Start _,
                ( Orchestrator.Respond _ | Orchestrator.Rebase _
                | Orchestrator.Reconcile_branch _ ) )
            | ( Orchestrator.Respond _,
                ( Orchestrator.Start _ | Orchestrator.Rebase _
                | Orchestrator.Reconcile_branch _ ) )
            | ( Orchestrator.Rebase _,
                ( Orchestrator.Start _ | Orchestrator.Respond _
                | Orchestrator.Reconcile_branch _ ) ) ->
                false
          in
          List.length pending = List.length actions
          && List.for_all2_exn pending actions ~f:action_equal
        with _ -> false)
  in

  (* ========== Dependency ordering ========== *)

  (* If B depends on A, B does not get Start before A has a PR *)
  let prop_spawn_respects_deps =
    Test.make ~name:"tick: spawn respects dependency order"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let _orch, _effects, actions = tick orch ~patches in
          let started_ids =
            List.filter_map actions ~f:(function
              | Orchestrator.Start (pid, _) -> Some pid
              | Orchestrator.Respond (_, _)
              | Orchestrator.Rebase (_, _)
              | Orchestrator.Reconcile_branch _ ->
                  None)
          in
          let graph = Orchestrator.graph orch in
          List.for_all started_ids ~f:(fun pid ->
              let deps = Graph.deps graph pid in
              (* Each dep must either be merged or have a PR or also be started *)
              List.for_all deps ~f:(fun dep ->
                  let a = Orchestrator.agent orch dep in
                  Patch_agent.has_pr a || a.Patch_agent.merged
                  || List.mem started_ids dep ~equal:Patch_id.equal))
        with _ -> false)
  in

  (* ========== No duplicate actions per patch ========== *)
  let prop_no_duplicate_patch_actions =
    Test.make ~name:"tick: at most one action per patch" gen_patch_list_unique
      (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let _orch, _effects, actions = tick orch ~patches in
          let pids =
            List.map actions ~f:(function
              | Orchestrator.Start (pid, _) -> pid
              | Orchestrator.Respond (pid, _) -> pid
              | Orchestrator.Rebase (pid, _)
              | Orchestrator.Reconcile_branch (pid, _) ->
                  pid)
          in
          let deduped = List.dedup_and_sort pids ~compare:Patch_id.compare in
          List.length pids = List.length deduped
        with _ -> false)
  in

  (* ========== fire preserves agent count ========== *)
  let prop_fire_preserves_agents =
    Test.make ~name:"fire: preserves agent count" gen_patch_list_unique
      (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let count_before = List.length (Orchestrator.all_agents orch) in
          let _orch, _effects, actions = tick orch ~patches in
          let orch_after =
            List.fold actions ~init:orch ~f:(fun o a -> Orchestrator.fire o a)
          in
          List.length (Orchestrator.all_agents orch_after) = count_before
        with _ -> false)
  in

  (* ========== Merged patches never get actions ========== *)
  let prop_merged_no_actions =
    Test.make ~name:"tick: merged patches get no actions" gen_patch_list_unique
      (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          (* Tick, complete, and merge everything *)
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                let o, _effects, _actions = tick o ~patches in
                let a = Orchestrator.agent o p.Patch.id in
                if a.Patch_agent.busy then
                  let o =
                    Orchestrator.set_pr_number o p.Patch.id (Pr_number.of_int 1)
                  in
                  let o = Orchestrator.complete o p.Patch.id in
                  Orchestrator.mark_merged o p.Patch.id
                else o)
          in
          let _orch, _effects, actions = tick orch ~patches in
          let merged_ids =
            List.filter_map patches ~f:(fun (p : Patch.t) ->
                if (Orchestrator.agent orch p.Patch.id).Patch_agent.merged then
                  Some p.Patch.id
                else None)
          in
          not
            (List.exists actions ~f:(function
                | Orchestrator.Start (pid, _)
                | Orchestrator.Respond (pid, _)
                | Orchestrator.Rebase (pid, _)
                | Orchestrator.Reconcile_branch (pid, _)
                -> List.mem merged_ids pid ~equal:Patch_id.equal))
        with _ -> false)
  in

  (* ========== needs_intervention blocks Respond ========== *)
  let prop_intervention_blocks_respond =
    Test.make ~name:"tick: needs_intervention blocks Respond"
      gen_patch_list_unique (fun patches ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch =
                Orchestrator.set_pr_number orch pid (Pr_number.of_int 1)
              in
              (* Exhaust fallback chain, then complete — this triggers
                 needs_intervention since session_fallback=Given_up *)
              let orch = Orchestrator.set_session_failed orch pid in
              let orch = Orchestrator.set_tried_fresh orch pid in
              let orch = Orchestrator.complete orch pid in
              let a = Orchestrator.agent orch pid in
              assert (Patch_agent.needs_intervention a);
              (* Enqueue work — should be blocked by needs_intervention *)
              let orch = Orchestrator.enqueue orch pid Operation_kind.Ci in
              let _orch, _effects, actions = tick orch ~patches in
              not
                (List.exists actions ~f:(function
                  | Orchestrator.Respond (p, _) -> Patch_id.equal p pid
                  | Orchestrator.Start (_, _)
                  | Orchestrator.Rebase (_, _)
                  | Orchestrator.Reconcile_branch _ ->
                      false))
        with _ -> false)
  in

  (* ========== Command sequence: complete/enqueue cycle ========== *)
  let prop_complete_enqueue_cycle =
    Test.make ~name:"tick: complete+enqueue produces Respond or Rebase"
      (Gen.pair gen_patch_list_unique gen_operation_kind)
      (fun (patches, kind) ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch =
                Orchestrator.set_pr_number orch first.Patch.id
                  (Pr_number.of_int 1)
              in
              (* Mark post-PR phase done so the controller doesn't auto-
                 enqueue Pr_body alongside [kind]. *)
              let orch =
                Orchestrator.set_pr_body_delivered orch first.Patch.id true
              in
              let orch = Orchestrator.complete orch first.Patch.id in
              let orch = Orchestrator.enqueue orch first.Patch.id kind in
              let _orch, _effects, actions = tick orch ~patches in
              List.exists actions ~f:(function
                | Orchestrator.Respond (pid, k) ->
                    Patch_id.equal pid first.Patch.id
                    && Operation_kind.equal k kind
                | Orchestrator.Rebase (pid, _) ->
                    Patch_id.equal pid first.Patch.id
                    && Operation_kind.equal kind Operation_kind.Rebase
                | Orchestrator.Start (_, _) | Orchestrator.Reconcile_branch _ ->
                    false)
        with _ -> false)
  in

  (* ========== All startable patches get started ========== *)
  let prop_all_startable_fired =
    Test.make ~name:"tick: all startable patches get Start"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch_after, _effects, actions = tick orch ~patches in
          let started_ids =
            List.filter_map actions ~f:(function
              | Orchestrator.Start (pid, _) -> Some pid
              | Orchestrator.Respond (_, _)
              | Orchestrator.Rebase (_, _)
              | Orchestrator.Reconcile_branch _ ->
                  None)
          in
          (* After tick, every agent that had startable preconditions should
             now have has_pr = true *)
          let graph = Orchestrator.graph orch_after in
          List.for_all (Graph.all_patch_ids graph) ~f:(fun pid ->
              let a_before = Orchestrator.agent orch pid in
              let a_after = Orchestrator.agent orch_after pid in
              if
                (not (Patch_agent.has_pr a_before))
                && Graph.deps_satisfied graph pid
                     ~has_merged:(fun p ->
                       (Orchestrator.agent orch p).Patch_agent.merged)
                     ~has_pr:(fun p ->
                       Patch_agent.has_pr (Orchestrator.agent orch p))
              then
                a_after.Patch_agent.busy
                && List.mem started_ids pid ~equal:Patch_id.equal
              else true)
        with _ -> false)
  in

  (* Rebase action only fires for patches with Rebase queued as highest
     priority *)
  let prop_rebase_only_for_rebase_queued =
    Test.make ~name:"tick: Rebase only for patches with Rebase queued"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch, _effects, _actions = tick orch ~patches in
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                let a = Orchestrator.agent o p.Patch.id in
                if a.Patch_agent.busy then
                  let o =
                    Orchestrator.set_pr_number o p.Patch.id (Pr_number.of_int 1)
                  in
                  let o = Orchestrator.complete o p.Patch.id in
                  Orchestrator.enqueue o p.Patch.id Operation_kind.Rebase
                else o)
          in
          let _orch, _effects, actions = tick orch ~patches in
          List.for_all actions ~f:(function
            | Orchestrator.Rebase (pid, _) ->
                let a = Orchestrator.agent orch pid in
                List.mem a.Patch_agent.queue Operation_kind.Rebase
                  ~equal:Operation_kind.equal
            | Orchestrator.Start _ | Orchestrator.Respond _
            | Orchestrator.Reconcile_branch _ ->
                true)
        with _ -> false)
  in

  (* Respond never fires for Rebase operation *)
  let prop_respond_never_fires_rebase =
    Test.make ~name:"tick: Respond never fires for Rebase" gen_patch_list_unique
      (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch, _effects, _actions = tick orch ~patches in
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                let a = Orchestrator.agent o p.Patch.id in
                if a.Patch_agent.busy then
                  let o =
                    Orchestrator.set_pr_number o p.Patch.id (Pr_number.of_int 1)
                  in
                  let o = Orchestrator.complete o p.Patch.id in
                  Orchestrator.enqueue o p.Patch.id Operation_kind.Rebase
                else o)
          in
          let _orch, _effects, actions = tick orch ~patches in
          not
            (List.exists actions ~f:(function
              | Orchestrator.Respond (_, k) ->
                  Operation_kind.equal k Operation_kind.Rebase
              | Orchestrator.Start _ | Orchestrator.Rebase _
              | Orchestrator.Reconcile_branch _ ->
                  false))
        with _ -> false)
  in

  (* ========== send_human_message properties ========== *)

  (* send_human_message adds message, clears intervention, enqueues Human *)
  let prop_send_human_message =
    Test.make
      ~name:"send_human_message: adds msg + clears intervention + enqueues"
      gen_patch_list_unique (fun patches ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch =
                Orchestrator.set_pr_number orch pid (Pr_number.of_int 1)
              in
              (* Drive to needs_intervention *)
              let orch = Orchestrator.set_session_failed orch pid in
              let orch = Orchestrator.set_tried_fresh orch pid in
              let orch = Orchestrator.complete orch pid in
              let a = Orchestrator.agent orch pid in
              let was_intervening = Patch_agent.needs_intervention a in
              let orch = Orchestrator.send_human_message orch pid "fix this" in
              let a = Orchestrator.agent orch pid in
              was_intervening
              && (not (Patch_agent.needs_intervention a))
              && List.length a.Patch_agent.human_messages = 1
              && List.mem a.Patch_agent.queue Operation_kind.Human
                   ~equal:Operation_kind.equal
        with _ -> false)
  in

  let prop_adhoc_draft_preserved =
    Test.make ~name:"ready ad-hoc PR retains draft state"
      Gen.(return ())
      (fun () ->
        try
          let pid = Patch_id.of_string "adhoc" in
          let t = Orchestrator.create ~patches:[] ~main_branch:main in
          let t =
            Orchestrator.add_agent t ~patch_id:pid
              ~branch:(Branch.of_string "adhoc") ~base_branch:main
              ~pr_number:(Pr_number.of_int 47)
          in
          let t = Orchestrator.set_base_branch t pid main in
          let t = Orchestrator.set_is_draft t pid true in
          let t = Orchestrator.set_checks_passing t pid true in
          let t =
            Onton_test_support.Reconciliation_fixture.rebase ~noop:true t pid
              main
          in
          let _, effects =
            Patch_controller.reconcile_all t ~project_name:"test-project"
              ~gameplan:(make_gameplan [])
          in
          List.is_empty effects && Patch_controller.ready_for_review t pid
        with _ -> false)
  in

  let prop_owned_rebase_completion =
    Test.make
      ~name:"owner completion records base and preserves session intervention"
      Gen.(triple gen_patch_list_unique gen_branch bool)
      (fun (patches, new_base, given_up) ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let module F = Onton_test_support.Reconciliation_fixture in
              let module B = Branch_reconcile in
              let pid = first.Patch.id in
              let initial = Orchestrator.create ~patches ~main_branch:main in
              let initial =
                if given_up then
                  Orchestrator.set_session_failed initial pid |> fun t ->
                  Orchestrator.set_tried_fresh t pid
                else initial
              in
              List.for_all [ false; true ] ~f:(fun noop ->
                  let t = F.rebase ~noop initial pid new_base in
                  let agent = Orchestrator.agent t pid in
                  (not agent.Patch_agent.busy)
                  && Option.equal Branch.equal
                       (Patch_agent.branch_rebased_onto agent)
                       (Some new_base)
                  && Option.equal B.equal_phase
                       (B.phase (F.state t pid))
                       (Some B.Settled)
                  && Bool.equal (Patch_agent.needs_intervention agent) given_up
                  && List.length (B.integrations (F.state t pid)) = 1)
        with _ -> false)
  in

  let prop_owned_conflict_completion =
    Test.make
      ~name:"owner conflict retains repair without charging legacy budgets"
      (Gen.pair gen_patch_list_unique gen_branch) (fun (patches, new_base) ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let module F = Onton_test_support.Reconciliation_fixture in
              let module B = Branch_reconcile in
              let pid = first.Patch.id in
              let initial = Orchestrator.create ~patches ~main_branch:main in
              let conflicted = F.conflict initial pid new_base in
              let agent = Orchestrator.agent conflicted pid in
              let retained =
                B.is_pending (F.state conflicted pid)
                && List.is_empty (B.integrations (F.state conflicted pid))
                && (not agent.Patch_agent.busy)
                && not (Patch_agent.needs_intervention agent)
              in
              let finished = F.resolve_conflict conflicted pid in
              retained
              && Option.equal B.equal_phase
                   (B.phase (F.state finished pid))
                   (Some B.Settled)
              && Option.equal Branch.equal
                   (Patch_agent.branch_rebased_onto
                      (Orchestrator.agent finished pid))
                   (Some new_base)
              && List.length (B.integrations (F.state finished pid)) = 1
        with _ -> false)
  in

  let prop_mark_merged_skips_merge_queue_dependents =
    Test.make
      ~name:
        "MQ-CASCADE-1: mark_merged does not enqueue Rebase for dependent in \
         merge queue"
      Gen.(return ())
      (fun () ->
        try
          let parent = patch "parent" in
          let child = patch "child" ~dependencies:[ parent.Patch.id ] in
          let patches = [ parent; child ] in
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch =
            Orchestrator.set_pr_number orch child.Patch.id (Pr_number.of_int 2)
          in
          let orch =
            Orchestrator.set_merge_queue_entry orch child.Patch.id
              (Some merge_queue_entry)
          in
          let orch = Orchestrator.mark_merged orch parent.Patch.id in
          let child_agent = Orchestrator.agent orch child.Patch.id in
          not
            (List.mem child_agent.Patch_agent.queue Operation_kind.Rebase
               ~equal:Operation_kind.equal)
        with _ -> false)
  in

  let prop_rebase_cascade_skips_merge_queue_dependents =
    Test.make
      ~name:
        "MQ-CASCADE-2: completed rebase does not strand-enqueue queued \
         dependent"
      Gen.(return ())
      (fun () ->
        try
          let parent = patch "parent" in
          let child = patch "child" ~dependencies:[ parent.Patch.id ] in
          let patches = [ parent; child ] in
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch =
            Orchestrator.set_pr_number orch parent.Patch.id (Pr_number.of_int 1)
          in
          let orch =
            Orchestrator.fire orch
              (Orchestrator.Start (child.Patch.id, parent.Patch.branch))
          in
          let orch =
            Orchestrator.set_pr_number orch child.Patch.id (Pr_number.of_int 2)
          in
          let orch = Orchestrator.complete orch child.Patch.id in
          let orch =
            Orchestrator.set_merge_queue_entry orch child.Patch.id
              (Some merge_queue_entry)
          in
          let orch =
            Onton_test_support.Reconciliation_fixture.rebase orch
              parent.Patch.id main
          in
          let child_agent = Orchestrator.agent orch child.Patch.id in
          not
            (List.mem child_agent.Patch_agent.queue Operation_kind.Rebase
               ~equal:Operation_kind.equal)
        with _ -> false)
  in

  (* Publication results are owned, revision-bound events. Transient failures
     retain the candidate and back off; they do not manufacture forge conflicts. *)
  let prop_owned_publication_result =
    Test.make
      ~name:"owned publication results preserve budgets and isolate duplicates"
      Gen.(pair gen_patch_list_unique (int_range 0 5))
      (fun (patches, outcome) ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let module B = Branch_reconcile in
              let module F = Onton_core_test_support.Publication_fixture in
              let pid = first.Patch.id in
              let state t =
                (Orchestrator.agent t pid).Patch_agent.branch_reconcile
              in
              let step t event =
                fst (Orchestrator.reconcile_branch t pid event)
              in
              let initial = Orchestrator.create ~patches ~main_branch:main in
              let publishing =
                F.publishing ~candidate:(F.sha "rewritten") ~step ~state initial
              in
              let result =
                match outcome with
                | 0 | 1 -> B.Published
                | 2 ->
                    B.Retryable
                      { reason = "lease_violation"; retry_after = None }
                | 3 ->
                    B.Retryable
                      { reason = "merge_queue_locked"; retry_after = None }
                | 4 -> B.Retryable { reason = "transport"; retry_after = None }
                | _ -> B.Needs_diagnosis "permission_denied"
              in
              let event = F.completion ~state publishing result in
              let after = step publishing event in
              let duplicate = step after event in
              let agent = Orchestrator.agent after pid in
              let expected_outcome =
                if outcome <= 1 then
                  Option.equal B.equal_phase
                    (B.phase (state after))
                    (Some B.Confirming)
                else if outcome = 5 then F.is_diagnosis (state after)
                else
                  Option.is_none (B.pending (state after))
                  && (not (B.can_execute_git ~at:100. (state after)))
                  && B.can_execute_git ~at:10000. (state after)
              in
              expected_outcome
              && B.equal (state after) (state duplicate)
              && Option.equal B.equal_publication_identity
                   (B.publication_observation_target (state publishing))
                   (B.publication_observation_target (state after))
              && (not (Patch_agent.has_conflict agent))
              && List.is_empty agent.Patch_agent.queue
              && agent.Patch_agent.start_attempts_without_pr = 0
              && not (Patch_agent.needs_intervention agent)
        with _ -> false)
  in

  let prop_owned_recovery_rejects_old_push =
    Test.make ~name:"history recovery rejects prior publication completions"
      gen_patch_list_unique (fun patches ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let module B = Branch_reconcile in
              let module F = Onton_core_test_support.Publication_fixture in
              let pid = first.Patch.id in
              let state t =
                (Orchestrator.agent t pid).Patch_agent.branch_reconcile
              in
              let step t event =
                fst (Orchestrator.reconcile_branch t pid event)
              in
              let initial = Orchestrator.create ~patches ~main_branch:main in
              let t =
                F.publishing ~candidate:(F.sha "repair") ~step ~state initial
              in
              let stale = F.completion ~state t B.Published in
              let recovering =
                F.reply ~step ~state t (B.Recovery_required "uncertain_history")
              in
              let after = step recovering stale in
              B.equal (state recovering) (state after)
              && B.is_pending (state after)
              && Option.is_some (B.pending (state after))
        with _ -> false)
  in

  (* ========== apply_poll_result (Poll_applicator) properties ========== *)

  (* Merged poll result -> agent marked merged *)
  let prop_poll_merged =
    Test.make ~name:"apply_poll_result: merged -> mark_merged"
      gen_patch_list_unique (fun patches ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch = Orchestrator.complete orch pid in
              let poll =
                Poller.
                  {
                    queue = [];
                    merged = true;
                    closed = false;
                    is_draft = false;
                    merge_state = Pr_state.Mergeable;
                    merge_ready = false;
                    base_branch = None;
                    base_oid = None;
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
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      base_branch = None;
                      native_stack = false;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              (Orchestrator.agent orch' pid).Patch_agent.merged
        with _ -> false)
  in

  (* Identified forge conflict becomes owner evidence *)
  let prop_poll_conflict_set =
    Test.make ~name:"apply_poll_result: identified conflict -> owner conflict"
      gen_patch_list_unique (fun patches ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch = Orchestrator.complete orch pid in
              let orch =
                Orchestrator.set_pr_number orch pid (Pr_number.of_int 7)
              in
              let poll =
                Poller.
                  {
                    queue = [];
                    merged = false;
                    closed = false;
                    is_draft = false;
                    merge_state = Pr_state.Conflicting;
                    merge_ready = false;
                    base_branch = Some main;
                    base_oid = Some (String.make 40 'b');
                    head_oid = Some (String.make 40 'a');
                    review_decision = None;
                    unresolved_comment_count = 0;
                    merge_queue_required = false;
                    merge_queue_entry = None;
                    checks_passing = true;
                    ci_checks = [];
                    merge_commit_sha = None;
                  }
              in
              let orch', _logs, _newly_blocked =
                Onton_test_support.Forge_poll_fixture.apply
                  ~confirmed_remote_head:(String.make 40 'a') orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      base_branch = Some main;
                      native_stack = false;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              Patch_agent.has_conflict (Orchestrator.agent orch' pid)
        with _ -> false)
  in

  (* A mergeable forge response cannot clear a local sequencer conflict *)
  let prop_poll_conflict_cleared =
    Test.make
      ~name:"apply_poll_result: mergeable report preserves local conflict"
      gen_patch_list_unique (fun patches ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch =
                Onton_test_support.Conflict_fixture.conflict orch pid
              in
              let orch = Orchestrator.complete orch pid in
              let poll =
                Poller.
                  {
                    queue = [];
                    merged = false;
                    closed = false;
                    is_draft = false;
                    merge_state = Pr_state.Mergeable;
                    merge_ready = false;
                    base_branch = None;
                    base_oid = None;
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
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      base_branch = None;
                      native_stack = false;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              Patch_agent.has_conflict (Orchestrator.agent orch' pid)
        with _ -> false)
  in

  (* mergeability_unknown is a poll-mirror: after apply_poll_result it equals
     [poll merge_state = Unknown], unconditionally (no hysteresis, unlike
     has_conflict). This is what [automerge_transient_hold] reads. *)
  let prop_poll_mergeability_unknown_mirror =
    Test.make
      ~name:
        "apply_poll_result: mergeability_unknown mirrors merge_state = Unknown"
      Gen.(pair gen_patch_list_unique gen_merge_state)
      (fun (patches, merge_state) ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch = Orchestrator.complete orch pid in
              let poll =
                Poller.
                  {
                    queue = [];
                    merged = false;
                    closed = false;
                    is_draft = false;
                    merge_state;
                    merge_ready = false;
                    base_branch = None;
                    base_oid = None;
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
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      base_branch = None;
                      native_stack = false;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              Bool.equal
                (Orchestrator.agent orch' pid).Patch_agent.mergeability_unknown
                (Pr_state.equal_merge_state merge_state Pr_state.Unknown)
        with _ -> false)
  in

  (* No-conflict poll preserves local Merge_conflict state *)
  let prop_poll_conflict_not_cleared_with_local_merge_conflict =
    Test.make
      ~name:"apply_poll_result: no conflict keeps local Merge_conflict state"
      gen_patch_list_unique (fun patches ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch =
                Onton_test_support.Conflict_fixture.conflict orch pid
              in
              let orch = Orchestrator.complete orch pid in
              let orch =
                Orchestrator.enqueue orch pid Operation_kind.Merge_conflict
              in
              let poll =
                Poller.
                  {
                    queue = [];
                    merged = false;
                    closed = false;
                    is_draft = false;
                    merge_state = Pr_state.Mergeable;
                    merge_ready = false;
                    base_branch = None;
                    base_oid = None;
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
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      base_branch = None;
                      native_stack = false;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              Patch_agent.has_conflict (Orchestrator.agent orch' pid)
        with _ -> false)
  in

  (* New comments enqueue Review_comments
     (comments are fetched lazily at delivery time) *)
  let prop_poll_new_comments =
    Test.make ~name:"apply_poll_result: new comments added"
      (Gen.pair gen_patch_list_unique gen_comment) (fun (patches, _comment) ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch = Orchestrator.complete orch pid in
              let poll =
                Poller.
                  {
                    queue = [ Operation_kind.Review_comments ];
                    merged = false;
                    closed = false;
                    is_draft = false;
                    merge_state = Pr_state.Mergeable;
                    merge_ready = false;
                    base_branch = None;
                    base_oid = None;
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
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      base_branch = None;
                      native_stack = false;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              let a = Orchestrator.agent orch' pid in
              List.mem a.Patch_agent.queue Operation_kind.Review_comments
                ~equal:Operation_kind.equal
        with _ -> false)
  in

  (* Active CI work suppresses duplicate CI re-enqueue. *)
  let prop_poll_active_ci_suppresses =
    Test.make ~name:"apply_poll_result: active Ci suppresses duplicate enqueue"
      gen_patch_list_unique (fun patches ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch =
                Orchestrator.set_pr_number orch pid (Pr_number.of_int 1)
              in
              let orch = Orchestrator.complete orch pid in
              let orch = Orchestrator.enqueue orch pid Operation_kind.Ci in
              let orch =
                Orchestrator.fire orch
                  (Orchestrator.Respond (pid, Operation_kind.Ci))
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
                    base_branch = None;
                    base_oid = None;
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
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      base_branch = None;
                      native_stack = false;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              let a = Orchestrator.agent orch' pid in
              (* CI should NOT be enqueued *)
              not
                (List.mem a.Patch_agent.queue Operation_kind.Ci
                   ~equal:Operation_kind.equal)
        with _ -> false)
  in

  let prop_poll_completed_ci_reenqueues =
    Test.make
      ~name:"apply_poll_result: completed failed Ci re-enqueues on next poll"
      gen_patch_list_unique (fun patches ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch =
                Orchestrator.set_pr_number orch pid (Pr_number.of_int 1)
              in
              let orch = Orchestrator.complete orch pid in
              let orch = Orchestrator.enqueue orch pid Operation_kind.Ci in
              let orch =
                Orchestrator.fire orch
                  (Orchestrator.Respond (pid, Operation_kind.Ci))
              in
              let orch = Orchestrator.complete orch pid in
              let poll =
                Poller.
                  {
                    queue = [ Operation_kind.Ci ];
                    merged = false;
                    closed = false;
                    is_draft = false;
                    merge_state = Pr_state.Mergeable;
                    merge_ready = false;
                    base_branch = None;
                    base_oid = None;
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
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      base_branch = None;
                      native_stack = false;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              let a = Orchestrator.agent orch' pid in
              List.mem a.Patch_agent.queue Operation_kind.Ci
                ~equal:Operation_kind.equal
        with _ -> false)
  in

  (* CI passing resets ci_failure_count *)
  let prop_poll_ci_pass_resets_failure_count =
    Test.make ~name:"apply_poll_result: checks_passing resets ci_failure_count"
      gen_patch_list_unique (fun patches ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch = Orchestrator.complete orch pid in
              let orch = Orchestrator.increment_ci_failure_count orch pid in
              let poll =
                Poller.
                  {
                    queue = [];
                    merged = false;
                    closed = false;
                    is_draft = false;
                    merge_state = Pr_state.Mergeable;
                    merge_ready = false;
                    base_branch = None;
                    base_oid = None;
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
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      base_branch = None;
                      native_stack = false;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              let a = Orchestrator.agent orch' pid in
              a.Patch_agent.ci_failure_count = 0
        with _ -> false)
  in

  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_start_targets_no_pr;
      prop_respond_preconditions;
      prop_respond_highest_priority;
      prop_tick_no_double_start;
      prop_pending_matches_tick;
      prop_spawn_respects_deps;
      prop_no_duplicate_patch_actions;
      prop_fire_preserves_agents;
      prop_merged_no_actions;
      prop_intervention_blocks_respond;
      prop_complete_enqueue_cycle;
      prop_rebase_only_for_rebase_queued;
      prop_respond_never_fires_rebase;
      prop_all_startable_fired;
      prop_adhoc_draft_preserved;
      prop_owned_rebase_completion;
      prop_owned_conflict_completion;
      prop_mark_merged_skips_merge_queue_dependents;
      prop_rebase_cascade_skips_merge_queue_dependents;
      prop_owned_publication_result;
      prop_owned_recovery_rejects_old_push;
      prop_poll_merged;
      prop_poll_conflict_set;
      prop_poll_conflict_cleared;
      prop_poll_mergeability_unknown_mirror;
      prop_poll_conflict_not_cleared_with_local_merge_conflict;
      prop_poll_new_comments;
      prop_send_human_message;
      prop_poll_active_ci_suppresses;
      prop_poll_completed_ci_reenqueues;
      prop_poll_ci_pass_resets_failure_count;
    ];
  Stdlib.print_endline "orchestrator tick/spawn: all properties passed"

let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:"orchestrator forge request checkpoint controls accepted evidence"
       ~count:100 QCheck2.Gen.bool (fun replace ->
         try
           let module X = Onton_core_test_support.Forge_fixture in
           let module F = Forge_observation in
           let pid = Patch_id.of_string "forge" in
           let initial =
             Orchestrator.create ~patches:[ patch "forge" ] ~main_branch:main
           in
           let initial =
             Orchestrator.set_pr_number initial pid (Pr_number.of_int 7)
           in
           let requested, ticket =
             Orchestrator.begin_forge_observation initial pid
               ~request:(X.request "orchestrator")
           in
           match ticket with
           | None -> false
           | Some ticket ->
               let current =
                 if replace then
                   Orchestrator.set_pr_number requested pid (Pr_number.of_int 8)
                 else requested
               in
               let finished, result =
                 Orchestrator.accept_forge_observation current pid ~ticket
                   ~confirmed_head:None
                   (X.observe ticket.F.request Pr_state.Conflicting)
               in
               Bool.equal (Result.is_ok result) (not replace)
               && Int.equal
                    (Orchestrator.agent requested pid).Patch_agent.generation
                    (Orchestrator.agent initial pid).Patch_agent.generation
               && Bool.equal
                    (Option.is_some
                       (F.latest_fact
                          (Branch_reconcile.forge_observations
                             (Orchestrator.agent finished pid)
                               .Patch_agent.branch_reconcile)))
                    (not replace)
         with _ -> false))

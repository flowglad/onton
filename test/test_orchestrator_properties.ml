open Base
open Onton
open Onton.Types

(** QCheck2 property-based tests for orchestrator tick and spawn logic.

    These properties verify the spec's liveness guarantees, action
    preconditions, dependency ordering, and invariant preservation under
    arbitrary command sequences. *)

let main = Branch.of_string "main"

let make_gameplan patches =
  Gameplan.
    {
      project_name = "test-project";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches;
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
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
            | Orchestrator.Respond (_, _) | Orchestrator.Rebase (_, _) -> true)
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
            | Orchestrator.Start (_, _) | Orchestrator.Rebase (_, _) -> true)
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
            | Orchestrator.Start (_, _) | Orchestrator.Rebase (_, _) -> true)
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
            | Orchestrator.Respond (_, _) | Orchestrator.Rebase (_, _) -> true)
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
              | Orchestrator.Respond (_, _) | Orchestrator.Rebase (_, _) -> None)
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
              | Orchestrator.Rebase (pid, _) -> pid)
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
                  | Orchestrator.Start (_, _) | Orchestrator.Rebase (_, _) ->
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
                | Orchestrator.Start (_, _) -> false)
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
              | Orchestrator.Respond (_, _) | Orchestrator.Rebase (_, _) -> None)
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
            | Orchestrator.Start _ | Orchestrator.Respond _ -> true)
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
              | Orchestrator.Start _ | Orchestrator.Rebase _ -> false))
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

  (* ========== apply_rebase_result properties ========== *)

  (* All outcomes set base_branch — complete refreshes it from initial_base,
     so the final value may differ from new_base if a dependency merged. For
     a single-patch orchestrator (no deps), initial_base = main = new_base
     only when new_base happens to equal main. We just check it's Some. *)
  let prop_rebase_sets_base =
    Test.make ~name:"apply_rebase_result: always sets base_branch"
      (Gen.pair gen_patch_list_unique gen_branch) (fun (patches, new_base) ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let results = [ Worktree.Ok; Worktree.Noop; Worktree.Conflict ] in
              List.for_all results ~f:(fun r ->
                  let orch' =
                    Orchestrator.apply_rebase_result orch pid r new_base
                  in
                  let a = Orchestrator.agent orch' pid in
                  Option.is_some a.Patch_agent.base_branch)
        with _ -> false)
  in

  (* Ok/Noop -> agent not busy after *)
  let prop_rebase_ok_noop_complete =
    Test.make ~name:"apply_rebase_result: Ok/Noop -> not busy"
      (Gen.pair gen_patch_list_unique gen_branch) (fun (patches, new_base) ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let check r =
                let orch' =
                  Orchestrator.apply_rebase_result orch pid r new_base
                in
                not (Orchestrator.agent orch' pid).Patch_agent.busy
              in
              check Worktree.Ok && check Worktree.Noop
        with _ -> false)
  in

  (* Conflict -> Merge_conflict in queue *)
  let prop_rebase_conflict_enqueues =
    Test.make ~name:"apply_rebase_result: Conflict -> Merge_conflict queued"
      (Gen.pair gen_patch_list_unique gen_branch) (fun (patches, new_base) ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch' =
                Orchestrator.apply_rebase_result orch pid Worktree.Conflict
                  new_base
              in
              let a = Orchestrator.agent orch' pid in
              a.Patch_agent.has_conflict
              && List.mem a.Patch_agent.queue Operation_kind.Merge_conflict
                   ~equal:Operation_kind.equal
        with _ -> false)
  in

  (* Ok/Noop -> conflict cleared *)
  let prop_rebase_ok_noop_clears_conflict =
    Test.make ~name:"apply_rebase_result: Ok/Noop -> clears has_conflict"
      (Gen.pair gen_patch_list_unique gen_branch) (fun (patches, new_base) ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch = Orchestrator.set_has_conflict orch pid in
              let check outcome =
                let orch' =
                  Orchestrator.apply_rebase_result orch pid outcome new_base
                in
                not (Orchestrator.agent orch' pid).Patch_agent.has_conflict
              in
              check Worktree.Ok && check Worktree.Noop
        with _ -> false)
  in

  (* Error -> session failed + tried_fresh *)
  let prop_rebase_error_fails =
    Test.make ~name:"apply_rebase_result: Error -> session failed"
      (Gen.pair gen_patch_list_unique gen_branch) (fun (patches, new_base) ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch' =
                Orchestrator.apply_rebase_result orch pid
                  (Worktree.Error "test error") new_base
              in
              let a = Orchestrator.agent orch' pid in
              (not a.Patch_agent.busy)
              && Patch_agent.equal_session_fallback
                   a.Patch_agent.session_fallback Patch_agent.Given_up
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
                    has_conflict = false;
                    merge_ready = false;
                    checks_passing = true;
                    ci_checks = [];
                  }
              in
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      head_branch = None;
                      base_branch = None;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              (Orchestrator.agent orch' pid).Patch_agent.merged
        with _ -> false)
  in

  (* Conflict detected -> has_conflict set *)
  let prop_poll_conflict_set =
    Test.make ~name:"apply_poll_result: conflict -> has_conflict"
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
                    merged = false;
                    closed = false;
                    is_draft = false;
                    has_conflict = true;
                    merge_ready = false;
                    checks_passing = true;
                    ci_checks = [];
                  }
              in
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      head_branch = None;
                      base_branch = None;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              (Orchestrator.agent orch' pid).Patch_agent.has_conflict
        with _ -> false)
  in

  (* Conflict cleared -> has_conflict cleared *)
  let prop_poll_conflict_cleared =
    Test.make ~name:"apply_poll_result: no conflict -> clears has_conflict"
      gen_patch_list_unique (fun patches ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _effects, _actions = tick orch ~patches in
              let orch = Orchestrator.set_has_conflict orch pid in
              let orch = Orchestrator.complete orch pid in
              let poll =
                Poller.
                  {
                    queue = [];
                    merged = false;
                    closed = false;
                    is_draft = false;
                    has_conflict = false;
                    merge_ready = false;
                    checks_passing = true;
                    ci_checks = [];
                  }
              in
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      head_branch = None;
                      base_branch = None;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              not (Orchestrator.agent orch' pid).Patch_agent.has_conflict
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
              let orch = Orchestrator.set_has_conflict orch pid in
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
                    has_conflict = false;
                    merge_ready = false;
                    checks_passing = true;
                    ci_checks = [];
                  }
              in
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      head_branch = None;
                      base_branch = None;
                      branch_in_root = false;
                      worktree_path = None;
                    }
              in
              (Orchestrator.agent orch' pid).Patch_agent.has_conflict
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
                    has_conflict = false;
                    merge_ready = false;
                    checks_passing = true;
                    ci_checks = [];
                  }
              in
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      head_branch = None;
                      base_branch = None;
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
                    has_conflict = false;
                    merge_ready = false;
                    checks_passing = false;
                    ci_checks = [];
                  }
              in
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      head_branch = None;
                      base_branch = None;
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
                    has_conflict = false;
                    merge_ready = false;
                    checks_passing = false;
                    ci_checks = [];
                  }
              in
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      head_branch = None;
                      base_branch = None;
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
                    has_conflict = false;
                    merge_ready = false;
                    checks_passing = true;
                    ci_checks = [];
                  }
              in
              let orch', _logs, _newly_blocked =
                Patch_controller.apply_poll_result orch pid
                  Patch_controller.
                    {
                      poll_result = poll;
                      head_branch = None;
                      base_branch = None;
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
      prop_rebase_sets_base;
      prop_rebase_ok_noop_complete;
      prop_rebase_conflict_enqueues;
      prop_rebase_ok_noop_clears_conflict;
      prop_rebase_error_fails;
      prop_poll_merged;
      prop_poll_conflict_set;
      prop_poll_conflict_cleared;
      prop_poll_conflict_not_cleared_with_local_merge_conflict;
      prop_poll_new_comments;
      prop_send_human_message;
      prop_poll_active_ci_suppresses;
      prop_poll_completed_ci_reenqueues;
      prop_poll_ci_pass_resets_failure_count;
    ];
  Stdlib.print_endline "orchestrator tick/spawn: all properties passed"

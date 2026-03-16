open Base
open Onton
open Onton.Types

(** QCheck2 property-based tests for orchestrator tick and spawn logic.

    These properties verify the spec's liveness guarantees, action
    preconditions, dependency ordering, and invariant preservation under
    arbitrary command sequences. *)

let main = Branch.of_string "main"

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
          let _orch, actions = Orchestrator.tick orch ~patches in
          List.for_all actions ~f:(function
            | Orchestrator.Start (pid, _) ->
                not (Orchestrator.agent orch pid).Patch_agent.has_pr
            | Orchestrator.Respond (_, _) -> true)
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
          let orch, _ = Orchestrator.tick orch ~patches in
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                let a = Orchestrator.agent o p.Patch.id in
                if a.Patch_agent.busy then
                  let o = Orchestrator.complete o p.Patch.id in
                  Orchestrator.enqueue o p.Patch.id Operation_kind.Ci
                else o)
          in
          let _orch, actions = Orchestrator.tick orch ~patches in
          List.for_all actions ~f:(function
            | Orchestrator.Respond (pid, _) ->
                let a = Orchestrator.agent orch pid in
                a.Patch_agent.has_pr && (not a.Patch_agent.merged)
                && (not a.Patch_agent.busy)
                && not a.Patch_agent.needs_intervention
            | Orchestrator.Start (_, _) -> true)
        with _ -> false)
  in

  (* Respond always picks the highest-priority operation from the queue *)
  let prop_respond_highest_priority =
    Test.make ~name:"tick: Respond picks highest priority" gen_patch_list_unique
      (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch, _ = Orchestrator.tick orch ~patches in
          (* Enqueue multiple operations *)
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                let a = Orchestrator.agent o p.Patch.id in
                if a.Patch_agent.busy then
                  let o = Orchestrator.complete o p.Patch.id in
                  let o = Orchestrator.enqueue o p.Patch.id Operation_kind.Ci in
                  Orchestrator.enqueue o p.Patch.id Operation_kind.Human
                else o)
          in
          let _orch, actions = Orchestrator.tick orch ~patches in
          List.for_all actions ~f:(function
            | Orchestrator.Respond (pid, k) -> (
                let a = Orchestrator.agent orch pid in
                let highest = Patch_agent.highest_priority a in
                match highest with
                | Some expected -> Operation_kind.equal k expected
                | None -> false)
            | Orchestrator.Start (_, _) -> true)
        with _ -> false)
  in

  (* ========== Tick idempotence / convergence ========== *)

  (* A patch that already has_pr never gets a Start action *)
  let prop_tick_no_double_start =
    Test.make ~name:"tick: no Start for patches that already have PR"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch, _ = Orchestrator.tick orch ~patches in
          let _orch, actions2 = Orchestrator.tick orch ~patches in
          List.for_all actions2 ~f:(function
            | Orchestrator.Start (pid, _) ->
                not (Orchestrator.agent orch pid).Patch_agent.has_pr
            | Orchestrator.Respond (_, _) -> true)
        with _ -> false)
  in

  (* pending_actions returns the same actions that tick would fire *)
  let prop_pending_matches_tick =
    Test.make ~name:"tick: pending_actions = tick actions" gen_patch_list_unique
      (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let pending = Orchestrator.pending_actions orch ~patches in
          let _orch, actions = Orchestrator.tick orch ~patches in
          let action_equal a b =
            match (a, b) with
            | Orchestrator.Start (p1, b1), Orchestrator.Start (p2, b2) ->
                Patch_id.equal p1 p2 && Branch.equal b1 b2
            | Orchestrator.Respond (p1, k1), Orchestrator.Respond (p2, k2) ->
                Patch_id.equal p1 p2 && Operation_kind.equal k1 k2
            | Orchestrator.Start _, Orchestrator.Respond _
            | Orchestrator.Respond _, Orchestrator.Start _ ->
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
          let _orch, actions = Orchestrator.tick orch ~patches in
          let started_ids =
            List.filter_map actions ~f:(function
              | Orchestrator.Start (pid, _) -> Some pid
              | Orchestrator.Respond (_, _) -> None)
          in
          let graph = Orchestrator.graph orch in
          List.for_all started_ids ~f:(fun pid ->
              let deps = Graph.deps graph pid in
              (* Each dep must either be merged or have a PR or also be started *)
              List.for_all deps ~f:(fun dep ->
                  let a = Orchestrator.agent orch dep in
                  a.Patch_agent.has_pr || a.Patch_agent.merged
                  || List.mem started_ids dep ~equal:Patch_id.equal))
        with _ -> false)
  in

  (* ========== No duplicate actions per patch ========== *)
  let prop_no_duplicate_patch_actions =
    Test.make ~name:"tick: at most one action per patch" gen_patch_list_unique
      (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let _orch, actions = Orchestrator.tick orch ~patches in
          let pids =
            List.map actions ~f:(function
              | Orchestrator.Start (pid, _) -> pid
              | Orchestrator.Respond (pid, _) -> pid)
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
          let _orch, actions = Orchestrator.tick orch ~patches in
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
                let o, _ = Orchestrator.tick o ~patches in
                let a = Orchestrator.agent o p.Patch.id in
                if a.Patch_agent.busy then
                  let o = Orchestrator.complete o p.Patch.id in
                  Orchestrator.mark_merged o p.Patch.id
                else o)
          in
          let _orch, actions = Orchestrator.tick orch ~patches in
          let merged_ids =
            List.filter_map patches ~f:(fun (p : Patch.t) ->
                if (Orchestrator.agent orch p.Patch.id).Patch_agent.merged then
                  Some p.Patch.id
                else None)
          in
          not
            (List.exists actions ~f:(function
                | Orchestrator.Start (pid, _) | Orchestrator.Respond (pid, _) ->
                List.mem merged_ids pid ~equal:Patch_id.equal))
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
              let orch, _ = Orchestrator.tick orch ~patches in
              (* Set session_failed, then complete — this triggers
                 needs_intervention since session_failed=true *)
              let orch = Orchestrator.set_session_failed orch pid in
              let orch = Orchestrator.complete orch pid in
              let a = Orchestrator.agent orch pid in
              assert a.Patch_agent.needs_intervention;
              (* Enqueue work — should be blocked by needs_intervention *)
              let orch = Orchestrator.enqueue orch pid Operation_kind.Ci in
              let _orch, actions = Orchestrator.tick orch ~patches in
              not
                (List.exists actions ~f:(function
                  | Orchestrator.Respond (p, _) -> Patch_id.equal p pid
                  | Orchestrator.Start (_, _) -> false))
        with _ -> false)
  in

  (* ========== Command sequence: complete/enqueue cycle ========== *)
  let prop_complete_enqueue_cycle =
    Test.make ~name:"tick: complete+enqueue produces Respond"
      (Gen.pair gen_patch_list_unique gen_operation_kind)
      (fun (patches, kind) ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _ = Orchestrator.tick orch ~patches in
              let orch = Orchestrator.complete orch first.Patch.id in
              let orch = Orchestrator.enqueue orch first.Patch.id kind in
              let _orch, actions = Orchestrator.tick orch ~patches in
              List.exists actions ~f:(function
                | Orchestrator.Respond (pid, k) ->
                    Patch_id.equal pid first.Patch.id
                    && Operation_kind.equal k kind
                | Orchestrator.Start (_, _) -> false)
        with _ -> false)
  in

  (* ========== All startable patches get started ========== *)
  let prop_all_startable_fired =
    Test.make ~name:"tick: all startable patches get Start"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch_after, actions = Orchestrator.tick orch ~patches in
          let started_ids =
            List.filter_map actions ~f:(function
              | Orchestrator.Start (pid, _) -> Some pid
              | Orchestrator.Respond (_, _) -> None)
          in
          (* After tick, every agent that had startable preconditions should
             now have has_pr = true *)
          let graph = Orchestrator.graph orch_after in
          List.for_all (Graph.all_patch_ids graph) ~f:(fun pid ->
              let a_before = Orchestrator.agent orch pid in
              let a_after = Orchestrator.agent orch_after pid in
              if
                (not a_before.Patch_agent.has_pr)
                && Graph.deps_satisfied graph pid
                     ~has_merged:(fun p ->
                       (Orchestrator.agent orch p).Patch_agent.merged)
                     ~has_pr:(fun p ->
                       (Orchestrator.agent orch p).Patch_agent.has_pr)
              then
                a_after.Patch_agent.has_pr
                && List.mem started_ids pid ~equal:Patch_id.equal
              else true)
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
      prop_all_startable_fired;
    ];
  Stdlib.print_endline "orchestrator tick/spawn: all properties passed"

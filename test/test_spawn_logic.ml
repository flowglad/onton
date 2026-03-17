open Base
open Onton
open Onton.Types

(** QCheck2 property-based tests for Onton.Spawn_logic.plan_spawns.

    These properties verify from the spec that: 1. Only non-busy, non-merged,
    non-intervention agents with queued operations produce actions. 2. Start
    only for patches without PRs where deps are satisfied. 3. Respond only for
    patches with PRs, respecting priority. *)

let main = Branch.of_string "main"

(* ========== Helper: prepare orchestrator with PRs and queued ops ========== *)

(** Tick once to start eligible patches, then complete them and assign PR
    numbers so they become eligible for Respond/Rebase actions. *)
let prepare_with_prs orch patches =
  let orch, _ = Orchestrator.tick orch ~patches in
  List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
      let a = Orchestrator.agent o p.Patch.id in
      if a.Patch_agent.busy then
        let o = Orchestrator.set_pr_number o p.Patch.id (Pr_number.of_int 1) in
        Orchestrator.complete o p.Patch.id
      else o)

(* ========== Property 1: only eligible agents produce spawns ========== *)

let () =
  let open QCheck2 in
  let open Onton_test_support.Test_generators in
  (* No action targets a busy, merged, or removed agent *)
  let prop_only_eligible_agents =
    Test.make ~name:"plan_spawns: only non-busy, non-merged, non-removed agents"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          List.for_all spawns ~f:(fun s ->
              let pid = Onton.Spawn_logic.patch_id_of s in
              let a = Orchestrator.agent orch pid in
              (not a.Patch_agent.busy) && (not a.Patch_agent.merged)
              && not a.Patch_agent.removed)
        with _ -> false)
  in

  (* After marking first patch merged, it gets no spawns *)
  let prop_merged_excluded =
    Test.make ~name:"plan_spawns: merged agents never spawned"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch = prepare_with_prs orch patches in
          (* Merge all patches *)
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                Orchestrator.mark_merged o p.Patch.id)
          in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          let merged_ids =
            List.filter_map patches ~f:(fun (p : Patch.t) ->
                if (Orchestrator.agent orch p.Patch.id).Patch_agent.merged then
                  Some p.Patch.id
                else None)
          in
          not
            (List.exists spawns ~f:(fun s ->
                 List.mem merged_ids
                   (Onton.Spawn_logic.patch_id_of s)
                   ~equal:Patch_id.equal))
        with _ -> false)
  in

  (* needs_intervention blocks Respond spawns *)
  let prop_intervention_blocks =
    Test.make ~name:"plan_spawns: needs_intervention blocks Respond"
      gen_patch_list_unique (fun patches ->
        try
          match patches with
          | [] -> true
          | first :: _ ->
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch, _ = Orchestrator.tick orch ~patches in
              let orch =
                Orchestrator.set_pr_number orch pid (Pr_number.of_int 1)
              in
              (* Exhaust fallback to trigger needs_intervention *)
              let orch = Orchestrator.set_session_failed orch pid in
              let orch = Orchestrator.set_tried_fresh orch pid in
              let orch = Orchestrator.complete orch pid in
              let orch = Orchestrator.enqueue orch pid Operation_kind.Ci in
              let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
              not
                (List.exists spawns ~f:(fun s ->
                     match Onton.Spawn_logic.classify s with
                     | `Respond p -> Patch_id.equal p pid
                     | `Start _ | `Rebase _ -> false))
        with _ -> false)
  in

  (* ========== Property 2: Start only for no-PR patches with deps satisfied ========== *)

  (* Every Start targets a patch without a PR *)
  let prop_start_no_pr =
    Test.make ~name:"plan_spawns: Start only for patches without PR"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          List.for_all spawns ~f:(fun s ->
              match Onton.Spawn_logic.classify s with
              | `Start pid ->
                  not (Orchestrator.agent orch pid).Patch_agent.has_pr
              | `Respond _ | `Rebase _ -> true)
        with _ -> false)
  in

  (* Every Start target has deps_satisfied *)
  let prop_start_deps_satisfied =
    Test.make ~name:"plan_spawns: Start only when deps satisfied"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let graph = Orchestrator.graph orch in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          List.for_all spawns ~f:(fun s ->
              match Onton.Spawn_logic.classify s with
              | `Start pid ->
                  Graph.deps_satisfied graph pid
                    ~has_merged:(fun p ->
                      (Orchestrator.agent orch p).Patch_agent.merged
                      && not (Orchestrator.agent orch p).Patch_agent.removed)
                    ~has_pr:(fun p ->
                      (Orchestrator.agent orch p).Patch_agent.has_pr
                      && not (Orchestrator.agent orch p).Patch_agent.removed)
              | `Respond _ | `Rebase _ -> true)
        with _ -> false)
  in

  (* All patches whose Start preconditions hold do get started (liveness) *)
  let prop_all_startable_started =
    Test.make ~name:"plan_spawns: all startable patches get Start"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          let started_ids =
            List.filter_map spawns ~f:(fun s ->
                match Onton.Spawn_logic.classify s with
                | `Start pid -> Some pid
                | `Respond _ | `Rebase _ -> None)
          in
          let graph = Orchestrator.graph orch in
          List.for_all (Graph.all_patch_ids graph) ~f:(fun pid ->
              let a = Orchestrator.agent orch pid in
              if
                (not a.Patch_agent.has_pr) && (not a.Patch_agent.busy)
                && (not a.Patch_agent.merged)
                && (not a.Patch_agent.removed)
                && Graph.deps_satisfied graph pid
                     ~has_merged:(fun p ->
                       (Orchestrator.agent orch p).Patch_agent.merged
                       && not (Orchestrator.agent orch p).Patch_agent.removed)
                     ~has_pr:(fun p ->
                       (Orchestrator.agent orch p).Patch_agent.has_pr
                       && not (Orchestrator.agent orch p).Patch_agent.removed)
              then List.mem started_ids pid ~equal:Patch_id.equal
              else true)
        with _ -> false)
  in

  (* ========== Property 3: Respond only with PR, highest priority ========== *)

  (* Every Respond targets a patch with a PR *)
  let prop_respond_has_pr =
    Test.make ~name:"plan_spawns: Respond only for patches with PR"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch = prepare_with_prs orch patches in
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                Orchestrator.enqueue o p.Patch.id Operation_kind.Ci)
          in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          List.for_all spawns ~f:(fun s ->
              match Onton.Spawn_logic.classify s with
              | `Respond pid -> (Orchestrator.agent orch pid).Patch_agent.has_pr
              | `Start _ | `Rebase _ -> true)
        with _ -> false)
  in

  (* Respond always picks highest-priority feedback operation *)
  let prop_respond_priority =
    Test.make ~name:"plan_spawns: Respond picks highest priority"
      (QCheck2.Gen.pair gen_patch_list_unique gen_feedback_kind)
      (fun (patches, kind) ->
        try
          match patches with
          | [] -> true
          | first :: _ -> (
              let pid = first.Patch.id in
              let orch = Orchestrator.create ~patches ~main_branch:main in
              let orch = prepare_with_prs orch patches in
              (* Enqueue two feedback ops to test priority *)
              let orch = Orchestrator.enqueue orch pid Operation_kind.Ci in
              let orch = Orchestrator.enqueue orch pid kind in
              let a = Orchestrator.agent orch pid in
              let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
              let respond_for_pid =
                List.find spawns ~f:(fun s ->
                    match s with
                    | Orchestrator.Respond (p, _) -> Patch_id.equal p pid
                    | Orchestrator.Start _ | Orchestrator.Rebase _ -> false)
              in
              match respond_for_pid with
              | Some (Orchestrator.Respond (_, k)) -> (
                  let expected = Patch_agent.highest_priority a in
                  match expected with
                  | Some e -> Operation_kind.equal k e
                  | None -> false)
              | Some (Orchestrator.Start _ | Orchestrator.Rebase _) -> false
              | None ->
                  (* No respond is ok if agent is ineligible *)
                  a.Patch_agent.needs_intervention || a.Patch_agent.busy
                  || a.Patch_agent.merged)
        with _ -> false)
  in

  (* Respond never fires for Rebase kind *)
  let prop_respond_not_rebase =
    Test.make ~name:"plan_spawns: Respond never fires Rebase"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch = prepare_with_prs orch patches in
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                Orchestrator.enqueue o p.Patch.id Operation_kind.Rebase)
          in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          not
            (List.exists spawns ~f:(fun s ->
                 match s with
                 | Orchestrator.Respond (_, k) ->
                     Operation_kind.equal k Operation_kind.Rebase
                 | Orchestrator.Start _ | Orchestrator.Rebase _ -> false))
        with _ -> false)
  in

  (* At most one spawn per patch *)
  let prop_one_action_per_patch =
    Test.make ~name:"plan_spawns: at most one action per patch"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          let pids = List.map spawns ~f:Onton.Spawn_logic.patch_id_of in
          let deduped = List.dedup_and_sort pids ~compare:Patch_id.compare in
          List.length pids = List.length deduped
        with _ -> false)
  in

  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_only_eligible_agents;
      prop_merged_excluded;
      prop_intervention_blocks;
      prop_start_no_pr;
      prop_start_deps_satisfied;
      prop_all_startable_started;
      prop_respond_has_pr;
      prop_respond_priority;
      prop_respond_not_rebase;
      prop_one_action_per_patch;
    ];
  Stdlib.print_endline "spawn_logic: all properties passed"

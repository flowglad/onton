(* @archlint.module test
   @archlint.domain spawn-logic *)

open Base
open Onton
open Onton_core
open Onton_core.Types

(** QCheck2 property-based tests for Onton.Spawn_logic.plan_spawns.

    These properties verify from the spec that: 1. Only non-busy, non-merged,
    non-intervention agents with queued operations produce actions. 2. Start
    only for patches without PRs where deps are satisfied and every unmerged dep
    is itself review-ready — notes delivered, no conflict, and CI green
    ([open_dep_review_ready]). 3. Respond only for patches with PRs, respecting
    priority. *)

let main = Branch.of_string "main"

let make_gameplan patches =
  Gameplan.
    {
      project_name = "test-project";
      repo_owner = "";
      repo_name = "";
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
    }

(* ========== Helper: prepare orchestrator with PRs and queued ops ========== *)

(** Tick once to start eligible patches, then complete them and assign PR
    numbers so they become eligible for Respond/Rebase actions. *)
let prepare_with_prs orch patches =
  let orch, _effects, _actions =
    Patch_controller.tick orch ~project_name:"test-project"
      ~gameplan:(make_gameplan patches)
  in
  List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
      let a = Orchestrator.agent o p.Patch.id in
      if a.Patch_agent.busy then
        let o = Orchestrator.set_pr_number o p.Patch.id (Pr_number.of_int 1) in
        Orchestrator.complete o p.Patch.id
      else o)

(* Spec spelling-out of the gate a child's Start now waits on for its open-PR
   dependency — written independently of [Patch_controller] so the properties
   test the spec, not the implementation. A dep is review-ready when its PR body
   is delivered, CI is green, and no conflict is active. Deliberately does NOT
   require the dep to be on [main] / rebased: a child may start stacked on a
   mid-chain dep, so the gate omits [ready_for_review]'s base conjuncts. *)
let open_dep_review_ready orch d =
  let a = Orchestrator.agent orch d in
  a.Patch_agent.pr_body_delivered && a.Patch_agent.checks_passing
  && not a.Patch_agent.has_conflict

(* ========== Property 1: only eligible agents produce spawns ========== *)

let () =
  let open QCheck2 in
  let open Onton_test_support.Test_generators in
  (* No action targets a busy or merged agent *)
  let prop_only_eligible_agents =
    Test.make ~name:"plan_spawns: only non-busy, non-merged agents"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          (* Prepare PRs so agents have varied state *)
          let orch = prepare_with_prs orch patches in
          (* Mark every 4th as merged *)
          let orch =
            List.foldi patches ~init:orch ~f:(fun i o (p : Patch.t) ->
                if i % 4 = 0 && i > 0 then Orchestrator.mark_merged o p.Patch.id
                else o)
          in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          List.for_all spawns ~f:(fun s ->
              let pid = Onton.Spawn_logic.patch_id_of s in
              let a = Orchestrator.agent orch pid in
              (not a.Patch_agent.busy) && not a.Patch_agent.merged)
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
              let orch, _effects, _actions =
                Patch_controller.tick orch ~project_name:"test-project"
                  ~gameplan:(make_gameplan patches)
              in
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
          (* Give some agents PRs so we can verify Start excludes them *)
          let orch = prepare_with_prs orch patches in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          List.for_all spawns ~f:(fun s ->
              match Onton.Spawn_logic.classify s with
              | `Start pid ->
                  not (Patch_agent.has_pr (Orchestrator.agent orch pid))
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
                      (Orchestrator.agent orch p).Patch_agent.merged)
                    ~has_pr:(fun p ->
                      Patch_agent.has_pr (Orchestrator.agent orch p))
              | `Respond _ | `Rebase _ -> true)
        with _ -> false)
  in

  (* deps-notes-ready (spec): every Start target's unmerged deps have
     delivered their implementation notes. Half the patches get delivered
     notes so the generator exercises both gate outcomes. *)
  let prop_start_deps_notes_ready =
    Test.make
      ~name:"plan_spawns: Start only when unmerged deps' notes delivered"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch = prepare_with_prs orch patches in
          let orch =
            List.foldi patches ~init:orch ~f:(fun i o (p : Patch.t) ->
                if i % 2 = 0 then
                  let o =
                    Orchestrator.set_pr_body_delivered o p.Patch.id true
                  in
                  Orchestrator.set_checks_passing o p.Patch.id true
                else o)
          in
          let graph = Orchestrator.graph orch in
          let has_merged p = (Orchestrator.agent orch p).Patch_agent.merged in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          List.for_all spawns ~f:(fun s ->
              match Onton.Spawn_logic.classify s with
              | `Start pid ->
                  List.for_all (Graph.open_pr_deps graph pid ~has_merged)
                    ~f:(fun d ->
                      (Orchestrator.agent orch d).Patch_agent.pr_body_delivered)
              | `Respond _ | `Rebase _ -> true)
        with _ -> false)
  in

  (* All patches whose Start preconditions hold do get started (liveness) *)
  let prop_all_startable_started =
    Test.make ~name:"plan_spawns: all startable patches get Start"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch = prepare_with_prs orch patches in
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                let o = Orchestrator.set_pr_body_delivered o p.Patch.id true in
                Orchestrator.set_checks_passing o p.Patch.id true)
          in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          let started_ids =
            List.filter_map spawns ~f:(fun s ->
                match Onton.Spawn_logic.classify s with
                | `Start pid -> Some pid
                | `Respond _ | `Rebase _ -> None)
          in
          let graph = Orchestrator.graph orch in
          let has_merged p = (Orchestrator.agent orch p).Patch_agent.merged in
          List.for_all (Graph.all_patch_ids graph) ~f:(fun pid ->
              let a = Orchestrator.agent orch pid in
              if
                (not (Patch_agent.has_pr a))
                && (not a.Patch_agent.busy) && (not a.Patch_agent.merged)
                && Graph.deps_satisfied graph pid ~has_merged ~has_pr:(fun p ->
                    Patch_agent.has_pr (Orchestrator.agent orch p))
                && List.for_all
                     (Graph.open_pr_deps graph pid ~has_merged)
                     ~f:(open_dep_review_ready orch)
              then List.mem started_ids pid ~equal:Patch_id.equal
              else true)
        with _ -> false)
  in

  (* Safety: every Start target's open-PR deps have reached the ready-for-review
     fixpoint — the new restriction. Drive both notes and CI-green so the
     frontier deps actually satisfy it and the property bites. *)
  let prop_start_deps_ready_for_review =
    Test.make
      ~name:"plan_spawns: Start only when open deps are ready-for-review"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch = prepare_with_prs orch patches in
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                let o = Orchestrator.set_pr_body_delivered o p.Patch.id true in
                Orchestrator.set_checks_passing o p.Patch.id true)
          in
          let graph = Orchestrator.graph orch in
          let has_merged p = (Orchestrator.agent orch p).Patch_agent.merged in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          List.for_all spawns ~f:(fun s ->
              match Onton.Spawn_logic.classify s with
              | `Start pid ->
                  List.for_all
                    (Graph.open_pr_deps graph pid ~has_merged)
                    ~f:(open_dep_review_ready orch)
              | `Respond _ | `Rebase _ -> true)
        with _ -> false)
  in

  (* Liveness through the new gate: once every frontier dep is both
     notes-delivered and CI-green (so it has reached the ready-for-review
     fixpoint), every patch whose open deps are all ready gets its Start. *)
  let prop_startable_with_ready_deps_started =
    Test.make
      ~name:
        "plan_spawns: startable patches start once deps reach ready-for-review"
      gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch = prepare_with_prs orch patches in
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Patch.t) ->
                let o = Orchestrator.set_pr_body_delivered o p.Patch.id true in
                Orchestrator.set_checks_passing o p.Patch.id true)
          in
          let graph = Orchestrator.graph orch in
          let has_merged p = (Orchestrator.agent orch p).Patch_agent.merged in
          let spawns = Onton.Spawn_logic.plan_spawns orch ~patches in
          let started_ids =
            List.filter_map spawns ~f:(fun s ->
                match Onton.Spawn_logic.classify s with
                | `Start pid -> Some pid
                | `Respond _ | `Rebase _ -> None)
          in
          List.for_all (Graph.all_patch_ids graph) ~f:(fun pid ->
              let a = Orchestrator.agent orch pid in
              if
                (not (Patch_agent.has_pr a))
                && (not a.Patch_agent.busy) && (not a.Patch_agent.merged)
                && Graph.deps_satisfied graph pid ~has_merged ~has_pr:(fun p ->
                    Patch_agent.has_pr (Orchestrator.agent orch p))
                && List.for_all
                     (Graph.open_pr_deps graph pid ~has_merged)
                     ~f:(open_dep_review_ready orch)
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
              | `Respond pid -> Patch_agent.has_pr (Orchestrator.agent orch pid)
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
                  (* No respond is ok whenever Respond preconditions fail *)
                  (not (Patch_agent.has_pr a))
                  || Patch_agent.needs_intervention a
                  || a.Patch_agent.busy || a.Patch_agent.merged)
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
      prop_start_deps_notes_ready;
      prop_all_startable_started;
      prop_start_deps_ready_for_review;
      prop_startable_with_ready_deps_started;
      prop_respond_has_pr;
      prop_respond_priority;
      prop_respond_not_rebase;
      prop_one_action_per_patch;
    ];
  Stdlib.print_endline "spawn_logic: all properties passed"

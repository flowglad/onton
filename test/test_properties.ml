(* @archlint.module test
   @archlint.domain orchestrator *)

open Base
open Onton
open Onton_core

let make_gameplan patches =
  Types.Gameplan.
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

(* ========== Graph properties ========== *)

let () =
  let open QCheck2 in
  let prop_all_ids_preserved =
    Test.make ~name:"graph: all ids preserved"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let g = Graph.of_patches patches in
          let ids = Graph.all_patch_ids g in
          List.length ids = List.length patches
          && List.for_all patches ~f:(fun (p : Types.Patch.t) ->
              List.mem ids p.id ~equal:Types.Patch_id.equal)
        with _ -> false)
  in
  let prop_deps_match =
    Test.make ~name:"graph: deps match declared"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let g = Graph.of_patches patches in
          List.for_all patches ~f:(fun (p : Types.Patch.t) ->
              let d = Graph.deps g p.id in
              List.for_all p.dependencies ~f:(fun dep ->
                  List.mem d dep ~equal:Types.Patch_id.equal))
        with _ -> false)
  in
  let prop_dependents_inverse =
    Test.make ~name:"graph: dependents inverse of deps"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let g = Graph.of_patches patches in
          List.for_all patches ~f:(fun (p : Types.Patch.t) ->
              List.for_all (Graph.deps g p.id) ~f:(fun dep_id ->
                  List.mem
                    (Graph.dependents g dep_id)
                    p.id ~equal:Types.Patch_id.equal))
        with _ -> false)
  in
  let prop_no_dep_satisfiable =
    Test.make ~name:"graph: no-dep patch always satisfiable"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let g = Graph.of_patches patches in
          List.for_all patches ~f:(fun (p : Types.Patch.t) ->
              if List.is_empty p.dependencies then
                Graph.deps_satisfied g p.id
                  ~has_merged:(fun _ -> false)
                  ~has_pr:(fun _ -> false)
              else true)
        with _ -> false)
  in
  let prop_depends_on_consistent =
    Test.make ~name:"graph: depends_on consistent with deps"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let g = Graph.of_patches patches in
          List.for_all patches ~f:(fun (p : Types.Patch.t) ->
              List.for_all (Graph.deps g p.id) ~f:(fun dep ->
                  Graph.depends_on g p.id ~dep))
        with _ -> false)
  in
  let prop_transitive_ancestors_excludes_self =
    Test.make ~name:"graph: transitive_ancestors excludes the patch itself"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let g = Graph.of_patches patches in
          List.for_all patches ~f:(fun (p : Types.Patch.t) ->
              not
                (List.mem
                   (Graph.transitive_ancestors g p.id)
                   p.id ~equal:Types.Patch_id.equal))
        with _ -> false)
  in
  let prop_transitive_ancestors_includes_direct_deps =
    Test.make
      ~name:"graph: transitive_ancestors includes every direct dependency"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let g = Graph.of_patches patches in
          List.for_all patches ~f:(fun (p : Types.Patch.t) ->
              let ancestors = Graph.transitive_ancestors g p.id in
              List.for_all (Graph.deps g p.id) ~f:(fun dep ->
                  List.mem ancestors dep ~equal:Types.Patch_id.equal))
        with _ -> false)
  in
  let prop_transitive_ancestors_transitive =
    Test.make
      ~name:
        "graph: transitive_ancestors is closed under deps (grand-deps included)"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let g = Graph.of_patches patches in
          List.for_all patches ~f:(fun (p : Types.Patch.t) ->
              let ancestors = Graph.transitive_ancestors g p.id in
              List.for_all ancestors ~f:(fun anc ->
                  List.for_all (Graph.deps g anc) ~f:(fun grand ->
                      (* A back-edge to the root is permitted: the root
                         is deliberately excluded from its own ancestors. *)
                      Types.Patch_id.equal grand p.id
                      || List.mem ancestors grand ~equal:Types.Patch_id.equal)))
        with _ -> false)
  in
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_all_ids_preserved;
      prop_deps_match;
      prop_dependents_inverse;
      prop_no_dep_satisfiable;
      prop_depends_on_consistent;
      prop_transitive_ancestors_excludes_self;
      prop_transitive_ancestors_includes_direct_deps;
      prop_transitive_ancestors_transitive;
    ];

  (* Hand-crafted cycle test: gen_patch_dag only produces DAGs, so the
     [Set.mem seen] termination guard in [transitive_ancestors] is never
     exercised by the property tests above. Build A ↔ B directly and
     confirm the walk terminates and excludes the root. *)
  (let make_patch pid deps =
     Types.Patch.
       {
         id = pid;
         title = "";
         description = "";
         branch = Types.Branch.of_string ("b-" ^ Types.Patch_id.to_string pid);
         dependencies = deps;
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
   in
   let a = Types.Patch_id.of_string "A" in
   let b = Types.Patch_id.of_string "B" in
   let g = Graph.of_patches [ make_patch a [ b ]; make_patch b [ a ] ] in
   let sort = List.sort ~compare:Types.Patch_id.compare in
   let anc_a = sort (Graph.transitive_ancestors g a) in
   let anc_b = sort (Graph.transitive_ancestors g b) in
   assert (List.equal Types.Patch_id.equal anc_a (sort [ b ]));
   assert (List.equal Types.Patch_id.equal anc_b (sort [ a ]));
   (* Three-node cycle A→B→C→A: exercises the non-adjacent back-edge path
      through the [seen]-set guard. When visiting from B we reach C, then
      C's dep A triggers the termination guard ([A] is already in [seen]). *)
   let c = Types.Patch_id.of_string "C" in
   let g3 =
     Graph.of_patches
       [ make_patch a [ b ]; make_patch b [ c ]; make_patch c [ a ] ]
   in
   let anc3_a = sort (Graph.transitive_ancestors g3 a) in
   assert (List.equal Types.Patch_id.equal anc3_a (sort [ b; c ])));

  let prop_initial_base_all_merged_returns_main =
    Test.make ~name:"graph: initial_base returns main when all deps merged"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let main = Types.Branch.of_string "main" in
          let g = Graph.of_patches patches in
          List.for_all patches ~f:(fun (p : Types.Patch.t) ->
              let base =
                Graph.initial_base g p.id
                  ~has_merged:(fun _ -> true)
                  ~branch_of:(fun _ -> Types.Branch.of_string "x")
                  ~main
              in
              Types.Branch.equal base main)
        with _ -> false)
  in
  let prop_initial_base_open_dep_returns_dep_branch =
    Test.make ~name:"graph: initial_base returns dep branch when dep is open"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let main = Types.Branch.of_string "main" in
          let g = Graph.of_patches patches in
          let branch_of pid =
            match
              List.find patches ~f:(fun (p : Types.Patch.t) ->
                  Types.Patch_id.equal p.id pid)
            with
            | Some p -> p.Types.Patch.branch
            | None -> main
          in
          List.for_all patches ~f:(fun (p : Types.Patch.t) ->
              match p.dependencies with
              | [ dep_id ] ->
                  let base =
                    Graph.initial_base g p.id
                      ~has_merged:(fun _ -> false)
                      ~branch_of ~main
                  in
                  Types.Branch.equal base (branch_of dep_id)
              | _ -> true)
        with _ -> false)
  in
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_initial_base_all_merged_returns_main;
      prop_initial_base_open_dep_returns_dep_branch;
    ];
  Stdlib.print_endline "graph: all properties passed"

(* ========== Poller properties ========== *)

let () =
  let open QCheck2 in
  let prop_merged_sticky =
    Test.make ~name:"poller: merged is sticky"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result = Poller.poll ~was_merged:true pr in
        result.merged)
  in
  let prop_merged_from_pr =
    Test.make ~name:"poller: merged reflects pr state"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result = Poller.poll ~was_merged:false pr in
        Bool.equal result.merged (Pr_state.merged pr))
  in
  let prop_conflict_queue =
    Test.make ~name:"poller: conflict in queue iff has_conflict"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result = Poller.poll ~was_merged:false pr in
        let in_queue =
          List.mem result.queue Types.Operation_kind.Merge_conflict
            ~equal:Types.Operation_kind.equal
        in
        Bool.equal in_queue (Poller.has_conflict result))
  in
  let prop_ci_queue =
    Test.make ~name:"poller: ci in queue iff checks failing"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result = Poller.poll ~was_merged:false pr in
        let in_queue =
          List.mem result.queue Types.Operation_kind.Ci
            ~equal:Types.Operation_kind.equal
        in
        Bool.equal in_queue (Pr_state.ci_failed pr))
  in
  let prop_review_queue =
    Test.make ~name:"poller: review in queue iff unresolved comments"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result = Poller.poll ~was_merged:false pr in
        let in_queue =
          List.mem result.queue Types.Operation_kind.Review_comments
            ~equal:Types.Operation_kind.equal
        in
        Bool.equal in_queue (not (List.is_empty pr.Pr_state.comments)))
  in
  let prop_no_rebase =
    Test.make ~name:"poller: rebase never in poll queue"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result = Poller.poll ~was_merged:false pr in
        not
          (List.mem result.queue Types.Operation_kind.Rebase
             ~equal:Types.Operation_kind.equal))
  in
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_merged_sticky;
      prop_merged_from_pr;
      prop_conflict_queue;
      prop_ci_queue;
      prop_review_queue;
      prop_no_rebase;
    ];
  Stdlib.print_endline "poller: all properties passed"

(* ========== Orchestrator properties ========== *)

let () =
  let open QCheck2 in
  let main = Types.Branch.of_string "main" in
  let prop_agent_count =
    Test.make ~name:"orchestrator: one agent per patch"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          List.length (Orchestrator.all_agents orch) = List.length patches
        with _ -> false)
  in
  let prop_root_starts =
    Test.make ~name:"orchestrator: tick fires Start for root"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let _orch, _effects, actions = tick orch ~patches in
          match patches with
          | [] -> true
          | first :: _ ->
              List.exists actions ~f:(function
                | Orchestrator.Start (pid, _) ->
                    Types.Patch_id.equal pid first.Types.Patch.id
                | Orchestrator.Respond (_, _)
                | Orchestrator.Rebase (_, _)
                | Orchestrator.Reconcile_branch _ ->
                    false)
        with _ -> false)
  in
  let prop_tick_convergence =
    Test.make ~name:"orchestrator: repeated tick converges"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let rec loop o n =
            if n = 0 then o
            else
              let o, _effects, _actions = tick o ~patches in
              loop o (n - 1)
          in
          let orch_stable = loop orch (List.length patches + 1) in
          let _orch_final, _effects, actions = tick orch_stable ~patches in
          List.is_empty actions
        with _ -> false)
  in
  let prop_no_merged_actions =
    Test.make ~name:"orchestrator: no actions for all started+merged"
      Onton_test_support.Test_generators.gen_patch_list_unique (fun patches ->
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let rec tick_all o n =
            if n = 0 then o
            else
              let o, _effects, _actions = tick o ~patches in
              tick_all o (n - 1)
          in
          let orch = tick_all orch (List.length patches + 1) in
          let orch =
            List.fold patches ~init:orch ~f:(fun o (p : Types.Patch.t) ->
                let a = Orchestrator.agent o p.id in
                let o =
                  if a.Patch_agent.busy then
                    let o =
                      Orchestrator.set_pr_number o p.id
                        (Types.Pr_number.of_int 1)
                    in
                    Orchestrator.complete o p.id
                  else o
                in
                Orchestrator.mark_merged o p.id)
          in
          let _, _effects, actions = tick orch ~patches in
          List.is_empty actions
        with _ -> false)
  in
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_agent_count;
      prop_root_starts;
      prop_tick_convergence;
      prop_no_merged_actions;
    ];

  (* orchestrator: complete then enqueue produces Respond *)
  let pid = Types.Patch_id.of_string "p1" in
  let patches =
    [
      Types.Patch.
        {
          id = pid;
          title = "P1";
          description = "";
          branch = Types.Branch.of_string "b1";
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
        };
    ]
  in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  let orch, _effects, _actions = tick orch ~patches in
  let orch = Orchestrator.set_pr_number orch pid (Types.Pr_number.of_int 1) in
  let orch = Orchestrator.complete orch pid in
  let orch = Orchestrator.enqueue orch pid Types.Operation_kind.Ci in
  let _orch, _effects, actions = tick orch ~patches in
  assert (Int.equal (List.length actions) 1);
  assert (
    List.exists actions ~f:(function
      | Orchestrator.Respond (p, k) ->
          Types.Patch_id.equal p pid
          && Types.Operation_kind.equal k Types.Operation_kind.Ci
      | Orchestrator.Start _ | Orchestrator.Rebase _
      | Orchestrator.Reconcile_branch _ ->
          false));

  (* orchestrator: mark_merged makes has_merged true *)
  let orch2 = Orchestrator.create ~patches ~main_branch:main in
  let orch2 = Orchestrator.mark_merged orch2 pid in
  assert (Orchestrator.agent orch2 pid).Patch_agent.merged;

  Stdlib.print_endline "orchestrator: all properties passed"

(* ========== Reconciler properties ========== *)

let () =
  let pid = Types.Patch_id.of_string "p1" in
  let main = Types.Branch.of_string "main" in
  let mk_view ?(has_pr = true) ?(merged = false) ?(busy = false)
      ?(needs_intervention = false) ?(branch_blocked = false) ?(queue = [])
      ?(in_merge_queue = false) ?(base_contains_merged_siblings = true)
      ?(sibling_rebase_target = None) id =
    Reconciler.
      {
        id;
        has_pr;
        merged;
        busy;
        needs_intervention;
        branch_blocked;
        in_merge_queue;
        queue;
        base_branch = main;
        branch_rebased_onto = Some main;
        base_contains_merged_siblings;
        sibling_rebase_target;
      }
  in
  let mk_patch id =
    Types.Patch.
      {
        id;
        title = Types.Patch_id.to_string id;
        description = "";
        branch = Types.Branch.of_string "b1";
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
  in

  (* detect_merges ignores already-merged *)
  let actions =
    Reconciler.detect_merges
      [ mk_view ~merged:true pid ]
      ~merged_pr_patches:[ pid ]
  in
  assert (List.is_empty actions);

  (* detect_merges marks unmerged *)
  let actions =
    Reconciler.detect_merges [ mk_view pid ] ~merged_pr_patches:[ pid ]
  in
  assert (
    List.exists actions ~f:(function
      | Reconciler.Mark_merged p -> Types.Patch_id.equal p pid
      | Reconciler.Enqueue_rebase _ -> false
      | Reconciler.Start_operation _ -> false));

  (* plan_operations skips busy *)
  let g = Graph.of_patches [ mk_patch pid ] in
  let actions =
    Reconciler.plan_operations
      [ mk_view ~busy:true ~queue:[ Types.Operation_kind.Ci ] pid ]
      ~has_merged:(fun _ -> false)
      ~branch_of:(fun _ -> Types.Branch.of_string "b1")
      ~graph:g ~main
  in
  assert (List.is_empty actions);

  (* plan_operations skips needs_intervention *)
  let actions =
    Reconciler.plan_operations
      [
        mk_view ~needs_intervention:true ~queue:[ Types.Operation_kind.Ci ] pid;
      ]
      ~has_merged:(fun _ -> false)
      ~branch_of:(fun _ -> Types.Branch.of_string "b1")
      ~graph:g ~main
  in
  assert (List.is_empty actions);

  (* plan_operations picks highest priority (Rebase < Ci) *)
  let actions =
    Reconciler.plan_operations
      [
        mk_view
          ~queue:[ Types.Operation_kind.Ci; Types.Operation_kind.Rebase ]
          pid;
      ]
      ~has_merged:(fun _ -> false)
      ~branch_of:(fun _ -> Types.Branch.of_string "b1")
      ~graph:g ~main
  in
  assert (List.length actions = 1);
  (match List.hd_exn actions with
  | Reconciler.Start_operation { kind; _ } ->
      assert (Types.Operation_kind.equal kind Types.Operation_kind.Rebase)
  | Reconciler.Mark_merged _ -> assert false
  | Reconciler.Enqueue_rebase _ -> assert false);

  (* non-rebase ops get new_base = None *)
  let actions =
    Reconciler.plan_operations
      [ mk_view ~queue:[ Types.Operation_kind.Ci ] pid ]
      ~has_merged:(fun _ -> false)
      ~branch_of:(fun _ -> Types.Branch.of_string "b1")
      ~graph:g ~main
  in
  assert (List.length actions = 1);
  (match List.hd_exn actions with
  | Reconciler.Start_operation { kind; new_base; _ } ->
      assert (Types.Operation_kind.equal kind Types.Operation_kind.Ci);
      assert (Option.is_none new_base)
  | Reconciler.Mark_merged _ -> assert false
  | Reconciler.Enqueue_rebase _ -> assert false);

  (* reconcile: merged patch produces Mark_merged *)
  let actions =
    Reconciler.reconcile ~graph:g ~main ~merged_pr_patches:[ pid ]
      ~branch_of:(fun _ -> Types.Branch.of_string "b1")
      [ mk_view pid ]
  in
  assert (
    List.exists actions ~f:(function
      | Reconciler.Mark_merged p -> Types.Patch_id.equal p pid
      | Reconciler.Enqueue_rebase _ -> false
      | Reconciler.Start_operation _ -> false));

  (* reconcile: merge triggers rebase for dependents *)
  let p1 = Types.Patch_id.of_string "p1" in
  let p2 = Types.Patch_id.of_string "p2" in
  let patches2 =
    [
      Types.Patch.
        {
          id = p1;
          title = "P1";
          description = "";
          branch = Types.Branch.of_string "b1";
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
        };
      Types.Patch.
        {
          id = p2;
          title = "P2";
          description = "";
          branch = Types.Branch.of_string "b2";
          dependencies = [ p1 ];
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
        };
    ]
  in
  let g2 = Graph.of_patches patches2 in
  let views = [ mk_view p1; mk_view p2 ] in
  let actions =
    Reconciler.reconcile ~graph:g2 ~main ~merged_pr_patches:[ p1 ]
      ~branch_of:(fun pid ->
        if Types.Patch_id.equal pid p1 then Types.Branch.of_string "b1"
        else Types.Branch.of_string "b2")
      views
  in
  assert (
    List.exists actions ~f:(function
      | Reconciler.Enqueue_rebase p -> Types.Patch_id.equal p p2
      | Reconciler.Mark_merged _ -> false
      | Reconciler.Start_operation _ -> false));

  Stdlib.print_endline "reconciler: all properties passed"

(* ========== Session result properties ========== *)

let () =
  let open QCheck2 in
  let open Onton in
  let module G = Onton_test_support.Test_generators in
  (* Helper: create a 1-patch orchestrator and fire the Start action so the
     patch becomes busy. *)
  let mk_busy_orch () =
    let pid = Types.Patch_id.of_string "p1" in
    let main = Types.Branch.of_string "main" in
    let patches =
      [
        Types.Patch.
          {
            id = pid;
            title = "P";
            description = "";
            branch = Types.Branch.of_string "b1";
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
          };
      ]
    in
    let orch = Orchestrator.create ~patches ~main_branch:main in
    let orch, _effects, _actions = tick orch ~patches in
    (orch, pid)
  in
  (* Property: complete is idempotent (no crash on already-not-busy) *)
  let prop_complete_idempotent =
    Test.make ~name:"session: complete is idempotent" G.gen_patch_id (fun pid ->
        let main = Types.Branch.of_string "main" in
        let patches =
          [
            Types.Patch.
              {
                id = pid;
                title = "P";
                description = "";
                branch = Types.Branch.of_string "b1";
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
              };
          ]
        in
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch, _effects, _actions = tick orch ~patches in
          (* First complete: busy → not-busy *)
          let orch = Orchestrator.complete orch pid in
          (* Second complete: should be a no-op, not crash *)
          let orch = Orchestrator.complete orch pid in
          not (Orchestrator.agent orch pid).Patch_agent.busy
        with Invalid_argument _ -> false)
  in
  (* Property: apply_session_result never crashes on busy agent *)
  let prop_session_result_no_crash =
    Test.make ~name:"session: apply_session_result on busy agent never crashes"
      ~count:500 (Gen.pair G.gen_session_result Gen.bool) (fun (result, _) ->
        try
          let orch, pid = mk_busy_orch () in
          let _orch = Orchestrator.apply_session_result orch pid result in
          true
        with _ -> false)
  in
  (* Property: apply_session_result twice never crashes (double-complete bug) *)
  let prop_session_result_double_apply =
    Test.make
      ~name:
        "session: apply_session_result twice never crashes (double-complete)"
      ~count:500 G.gen_session_result (fun result ->
        try
          let orch, pid = mk_busy_orch () in
          let orch = Orchestrator.apply_session_result orch pid result in
          let _orch = Orchestrator.apply_session_result orch pid result in
          true
        with _ -> false)
  in
  (* Property: session results either complete (failure) or defer (success).
     Session_ok and Session_no_commits all represent a
     healthy LLM session; completion is deferred to apply_respond_outcome.
     All other failure results complete the agent immediately. *)
  let prop_failure_results_complete =
    Test.make ~name:"session: failure results leave agent not-busy" ~count:500
      G.gen_session_result (fun result ->
        let orch, pid = mk_busy_orch () in
        let orch = Orchestrator.apply_session_result orch pid result in
        let a = Orchestrator.agent orch pid in
        match result with
        | Orchestrator.Session_ok | Orchestrator.Session_no_commits ->
            (* LLM session succeeded — completion deferred to
               apply_respond_outcome, so agent stays busy here. *)
            a.Patch_agent.busy
        | Orchestrator.Session_process_error _ | Orchestrator.Session_no_resume
        | Orchestrator.Session_timed_out _ | Orchestrator.Session_failed _
        | Orchestrator.Session_wontdo _ | Orchestrator.Session_give_up
        | Orchestrator.Session_context_exhausted ->
            not a.Patch_agent.busy)
  in
  let prop_timeouts_preserve_session =
    Test.make ~name:"session: repeated timeouts preserve id and retry budget"
      ~count:300
      (Gen.pair (Gen.option Gen.string) (Gen.int_range 1 12))
      (fun (session_id, count) ->
        try
          let orch, pid = mk_busy_orch () in
          let orch = Orchestrator.set_llm_session_id orch pid session_id in
          let orch = Orchestrator.set_session_failed orch pid in
          let rec loop orch remaining =
            if remaining = 0 then true
            else
              let orch =
                Orchestrator.apply_session_result orch pid
                  (Orchestrator.Session_timed_out
                     { session_id; detail = Some "deadline" })
              in
              let a = Orchestrator.agent orch pid in
              Option.equal String.equal a.Patch_agent.llm_session_id session_id
              && Patch_agent.equal_session_fallback a.session_fallback
                   Fresh_available
              && (not a.busy)
              && a.no_commits_push_count = 0
              && a.context_exhaustion_count = 0
              && a.start_attempts_without_pr = 0
              && loop
                   (Orchestrator.fire orch
                      (Orchestrator.Start (pid, Types.Branch.of_string "main")))
                   (remaining - 1)
          in
          loop orch count
        with _ -> false)
  in
  (* Property: Session_give_up sets needs_intervention *)
  let prop_give_up_intervention =
    Test.make ~name:"session: give_up sets needs_intervention" (Gen.return ())
      (fun () ->
        let orch, pid = mk_busy_orch () in
        let orch =
          Orchestrator.apply_session_result orch pid
            Orchestrator.Session_give_up
        in
        let a = Orchestrator.agent orch pid in
        Patch_agent.needs_intervention a)
  in
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_complete_idempotent;
      prop_session_result_no_crash;
      prop_session_result_double_apply;
      prop_failure_results_complete;
      prop_timeouts_preserve_session;
      prop_give_up_intervention;
    ];
  Stdlib.print_endline "session result: all properties passed"

(* ========== llm_session_id properties (A1-A6) ========== *)

let () =
  let open QCheck2 in
  let open Onton in
  let module G = Onton_test_support.Test_generators in
  let mk_busy_orch () =
    let pid = Types.Patch_id.of_string "p1" in
    let main = Types.Branch.of_string "main" in
    let patches =
      [
        Types.Patch.
          {
            id = pid;
            title = "P";
            description = "";
            branch = Types.Branch.of_string "b1";
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
          };
      ]
    in
    let orch = Orchestrator.create ~patches ~main_branch:main in
    let orch, _effects, _actions = tick orch ~patches in
    (orch, pid)
  in
  (* A1: Session_ok preserves llm_session_id *)
  let prop_a1_session_ok_preserves_session_id =
    Test.make ~name:"A1: Session_ok preserves llm_session_id" (Gen.return ())
      (fun () ->
        let orch, pid = mk_busy_orch () in
        let orch = Orchestrator.set_llm_session_id orch pid (Some "test-id") in
        let orch = Orchestrator.apply_session_result orch pid Session_ok in
        let orch = Orchestrator.complete orch pid in
        let a = Orchestrator.agent orch pid in
        Option.equal String.equal a.Patch_agent.llm_session_id (Some "test-id"))
  in
  (* A2: Resume failure preserves llm_session_id (session may still be valid) *)
  let prop_a2_resume_failure_preserves_session_id =
    Test.make ~name:"A2: resume failure preserves llm_session_id"
      (Gen.return ()) (fun () ->
        let orch, pid = mk_busy_orch () in
        let orch =
          Orchestrator.set_pr_number orch pid (Types.Pr_number.of_int 1)
        in
        let orch = Orchestrator.set_llm_session_id orch pid (Some "test-id") in
        let orch = Orchestrator.on_session_failure orch pid ~is_fresh:false in
        let a = Orchestrator.agent orch pid in
        Option.equal String.equal a.Patch_agent.llm_session_id (Some "test-id"))
  in
  (* A3: Session_give_up clears llm_session_id *)
  let prop_a3_give_up_clears_session_id =
    Test.make ~name:"A3: Session_give_up clears llm_session_id" (Gen.return ())
      (fun () ->
        let orch, pid = mk_busy_orch () in
        let orch = Orchestrator.set_llm_session_id orch pid (Some "test-id") in
        let orch = Orchestrator.apply_session_result orch pid Session_give_up in
        let a = Orchestrator.agent orch pid in
        Option.is_none a.Patch_agent.llm_session_id)
  in
  (* A4: set_llm_session_id is idempotent *)
  let prop_a4_set_session_id_idempotent =
    Test.make ~name:"A4: set_llm_session_id is idempotent" ~count:200
      (Gen.pair G.gen_patch_id (Gen.option Gen.string_small))
      (fun (pid, v) ->
        try
          let main = Types.Branch.of_string "main" in
          let patches =
            [
              Types.Patch.
                {
                  id = pid;
                  title = "P";
                  description = "";
                  branch = Types.Branch.of_string "b1";
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
                };
            ]
          in
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let once = Orchestrator.set_llm_session_id orch pid v in
          let twice = Orchestrator.set_llm_session_id once pid v in
          let a1 = Orchestrator.agent once pid in
          let a2 = Orchestrator.agent twice pid in
          Option.equal String.equal a1.Patch_agent.llm_session_id
            a2.Patch_agent.llm_session_id
        with Invalid_argument _ -> false)
  in
  (* A5: Fresh failure on respond path preserves llm_session_id *)
  let prop_a5_fresh_failure_preserves_session_id =
    Test.make ~name:"A5: fresh failure on respond path preserves llm_session_id"
      (Gen.return ()) (fun () ->
        let orch, pid = mk_busy_orch () in
        let orch =
          Orchestrator.set_pr_number orch pid (Types.Pr_number.of_int 1)
        in
        let orch = Orchestrator.set_llm_session_id orch pid (Some "test-id") in
        let orch = Orchestrator.on_session_failure orch pid ~is_fresh:true in
        let a = Orchestrator.agent orch pid in
        Option.equal String.equal a.Patch_agent.llm_session_id (Some "test-id"))
  in
  (* A6: Session_no_resume clears llm_session_id *)
  let prop_a6_no_resume_clears_session_id =
    Test.make ~name:"A6: Session_no_resume clears llm_session_id"
      (Gen.return ()) (fun () ->
        let orch, pid = mk_busy_orch () in
        let orch = Orchestrator.set_llm_session_id orch pid (Some "test-id") in
        let orch =
          Orchestrator.apply_session_result orch pid Session_no_resume
        in
        let a = Orchestrator.agent orch pid in
        Option.is_none a.Patch_agent.llm_session_id)
  in
  (* A7: Session_no_resume from Fresh_available stays Fresh_available.
     A stub-resume never spoke to the LLM — it must not consume retry budget. *)
  let prop_a7_no_resume_does_not_escalate_from_fresh =
    Test.make ~name:"A7: Session_no_resume preserves Fresh_available"
      (Gen.return ()) (fun () ->
        let orch, pid = mk_busy_orch () in
        let orch = Orchestrator.set_llm_session_id orch pid (Some "stub-id") in
        let orch =
          Orchestrator.apply_session_result orch pid Session_no_resume
        in
        let a = Orchestrator.agent orch pid in
        Patch_agent.equal_session_fallback a.Patch_agent.session_fallback
          Patch_agent.Fresh_available)
  in
  (* A8: Session_no_resume from Tried_fresh stays Tried_fresh (no escalation
     to Given_up). *)
  let prop_a8_no_resume_does_not_escalate_from_tried_fresh =
    Test.make ~name:"A8: Session_no_resume preserves Tried_fresh"
      (Gen.return ()) (fun () ->
        let orch, pid = mk_busy_orch () in
        let orch = Orchestrator.set_tried_fresh orch pid in
        let orch = Orchestrator.set_llm_session_id orch pid (Some "stub-id") in
        let orch =
          Orchestrator.apply_session_result orch pid Session_no_resume
        in
        let a = Orchestrator.agent orch pid in
        Patch_agent.equal_session_fallback a.Patch_agent.session_fallback
          Patch_agent.Tried_fresh)
  in
  (* A9: two consecutive Session_no_resume outcomes never reach Given_up.
     Pin down the regression scenario: a poisoned sidecar pointing at a stub
     would otherwise burn the budget on every retry. *)
  let prop_a9_repeated_no_resume_never_gives_up =
    Test.make ~name:"A9: repeated Session_no_resume does not reach Given_up"
      (Gen.return ()) (fun () ->
        let orch, pid = mk_busy_orch () in
        let orch = Orchestrator.set_llm_session_id orch pid (Some "stub-a") in
        let orch =
          Orchestrator.apply_session_result orch pid Session_no_resume
        in
        let orch = Orchestrator.set_llm_session_id orch pid (Some "stub-b") in
        let orch =
          Orchestrator.apply_session_result orch pid Session_no_resume
        in
        let a = Orchestrator.agent orch pid in
        not
          (Patch_agent.equal_session_fallback a.Patch_agent.session_fallback
             Patch_agent.Given_up))
  in
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_a1_session_ok_preserves_session_id;
      prop_a2_resume_failure_preserves_session_id;
      prop_a3_give_up_clears_session_id;
      prop_a4_set_session_id_idempotent;
      prop_a5_fresh_failure_preserves_session_id;
      prop_a6_no_resume_clears_session_id;
      prop_a7_no_resume_does_not_escalate_from_fresh;
      prop_a8_no_resume_does_not_escalate_from_tried_fresh;
      prop_a9_repeated_no_resume_never_gives_up;
    ];
  Stdlib.print_endline "llm_session_id: all properties passed (A1-A9)"

(* Session and publication outcomes have independent owners and budgets. *)
let () =
  let open QCheck2 in
  let main = Types.Branch.of_string "main" in
  let mk_busy_orch () =
    let pid = Types.Patch_id.of_string "psf-pid" in
    let patches =
      [
        Types.Patch.
          {
            id = pid;
            title = "P";
            description = "";
            branch = Types.Branch.of_string "psf";
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
          };
      ]
    in
    let orch = Orchestrator.create ~patches ~main_branch:main in
    let orch, _, _ = tick orch ~patches in
    (orch, pid)
  in
  let module B = Branch_reconcile in
  let module F = Onton_core_test_support.Publication_fixture in
  let publication orch pid =
    let state t = (Orchestrator.agent t pid).Patch_agent.branch_reconcile in
    let step t event = fst (Orchestrator.reconcile_branch t pid event) in
    ( F.publishing ~candidate:(F.sha "session-work") ~state ~step orch,
      state,
      step )
  in
  let same_budgets (before : Patch_agent.t) (after : Patch_agent.t) =
    Patch_agent.equal_session_fallback before.session_fallback
      after.session_fallback
    && before.start_attempts_without_pr = after.start_attempts_without_pr
    && before.ci_failure_count = after.ci_failure_count
    && before.context_exhaustion_count = after.context_exhaustion_count
    && before.no_commits_push_count = after.no_commits_push_count
    && Option.equal String.equal before.llm_session_id after.llm_session_id
  in
  let prop_publication_budget_isolation =
    Test.make
      ~name:"publication failure preserves implementation budgets and candidate"
      ~count:500
      Gen.(triple (int_range 0 4) bool string)
      (fun (failures, permanent, reason) ->
        try
          let orch, pid = mk_busy_orch () in
          let orch =
            List.fold (List.range 0 failures) ~init:orch ~f:(fun t _ ->
                Orchestrator.increment_start_attempts_without_pr t pid
                |> fun t -> Orchestrator.increment_ci_failure_count t pid)
          in
          let orch = Orchestrator.set_tried_fresh orch pid in
          let orch =
            Orchestrator.set_llm_session_id orch pid (Some "retained-session")
          in
          let orch, state, step = publication orch pid in
          let before = Orchestrator.agent orch pid in
          let identity = B.publication_observation_target (state orch) in
          let result =
            if permanent then B.Permanent reason
            else B.Retryable { reason; retry_after = None }
          in
          let event = F.completion ~state orch result in
          let failed = step orch event in
          let duplicate = step failed event in
          same_budgets before (Orchestrator.agent failed pid)
          && B.equal (state failed) (state duplicate)
          && Option.equal B.equal_publication_identity identity
               (B.publication_observation_target (state failed))
          && List.is_empty (B.publications (state failed))
        with _ -> false)
  in
  let prop_publication_retry_has_no_session_cap =
    Test.make
      ~name:
        "publication infrastructure retries retain work beyond old session cap"
      ~count:300
      Gen.(int_range 3 20)
      (fun retries ->
        try
          let orch, pid = mk_busy_orch () in
          let orch, state, step = publication orch pid in
          let before = Orchestrator.agent orch pid in
          let identity = B.publication_observation_target (state orch) in
          let failed =
            List.fold (List.range 0 retries) ~init:orch ~f:(fun t _ ->
                let t =
                  F.reply ~state ~step t
                    (B.Retryable
                       { reason = "transport offline"; retry_after = None })
                in
                match B.phase (state t) with
                | Some (B.Waiting { until; _ }) -> step t (B.Tick until)
                | Some
                    ( Preparing | Integrating | Repairing _ | Publishing
                    | Confirming | Recovering | Settled | Intervention _ )
                | None ->
                    failwith "retry failed to retain owner backoff")
          in
          let after = Orchestrator.agent failed pid in
          same_budgets before after
          && (not (Patch_agent.needs_intervention after))
          && B.is_pending (state failed)
          && Option.equal B.equal_publication_identity identity
               (B.publication_observation_target (state failed))
        with _ -> false)
  in
  let prop_session_success_keeps_owner_intervention =
    Test.make
      ~name:"successful implementation cannot clear publication intervention"
      ~count:200 Gen.string (fun suffix ->
        try
          let orch, pid = mk_busy_orch () in
          let orch, state, step = publication orch pid in
          let reason = "permission_denied:" ^ suffix in
          let orch = F.reply ~state ~step orch (B.Permanent reason) in
          let failed = state orch in
          let orch =
            Orchestrator.apply_session_result orch pid Orchestrator.Session_ok
          in
          B.equal failed (state orch)
          && Option.equal String.equal
               (Patch_agent.intervention_reason (Orchestrator.agent orch pid))
               (Some reason)
        with _ -> false)
  in
  (* Session_no_commits property tests (PNC-N): clear fallback,
     increment counter, reset on Session_ok, trigger needs_intervention at
     the threshold, and reset_intervention_state clears the counter. PNC-2
     through PNC-5 test the pure [Patch_agent] functions directly (no
     orchestrator noise); PNC-1 and PNC-6 through PNC-8 go through
     [apply_session_result]. *)
  let fresh_agent () =
    Patch_agent.create
      ~branch:(Types.Branch.of_string "nc")
      (Types.Patch_id.of_string "nc-pid")
  in
  let prop_pnc1_clears_session_fallback =
    Test.make ~name:"PNC-1: Session_no_commits clears session_fallback"
      (Gen.return ()) (fun () ->
        let orch, pid = mk_busy_orch () in
        let orch = Orchestrator.set_tried_fresh orch pid in
        let orch =
          Orchestrator.apply_session_result orch pid Session_no_commits
        in
        let a = Orchestrator.agent orch pid in
        Patch_agent.equal_session_fallback a.Patch_agent.session_fallback
          Patch_agent.Fresh_available)
  in
  let prop_pnc2_increments_counter =
    Test.make
      ~name:
        "PNC-2: increment_no_commits_push_count bumps the counter by exactly 1"
      (Gen.int_range 0 5) (fun n ->
        let a =
          List.fold (List.range 0 n) ~init:(fresh_agent ()) ~f:(fun a _ ->
              Patch_agent.increment_no_commits_push_count a)
        in
        Int.equal a.Patch_agent.no_commits_push_count n)
  in
  let prop_pnc3_intervention_threshold =
    Test.make ~name:"PNC-3: no_commits_push_count >= 2 flips needs_intervention"
      (Gen.int_range 0 4) (fun n ->
        let a =
          List.fold (List.range 0 n) ~init:(fresh_agent ()) ~f:(fun a _ ->
              Patch_agent.increment_no_commits_push_count a)
        in
        Bool.equal (Patch_agent.needs_intervention a) (n >= 2))
  in
  let prop_pnc4_reset_counter =
    Test.make
      ~name:"PNC-4: reset_no_commits_push_count zeros the counter (idempotent)"
      (Gen.int_range 0 5) (fun n ->
        let a =
          List.fold (List.range 0 n) ~init:(fresh_agent ()) ~f:(fun a _ ->
              Patch_agent.increment_no_commits_push_count a)
        in
        let a = Patch_agent.reset_no_commits_push_count a in
        let a = Patch_agent.reset_no_commits_push_count a in
        Int.equal a.Patch_agent.no_commits_push_count 0)
  in
  let prop_pnc5_reset_intervention_clears_counter =
    Test.make
      ~name:"PNC-5: reset_intervention_state zeros no_commits_push_count"
      (Gen.int_range 0 5) (fun n ->
        let a =
          List.fold (List.range 0 n) ~init:(fresh_agent ()) ~f:(fun a _ ->
              Patch_agent.increment_no_commits_push_count a)
        in
        let a = Patch_agent.reset_intervention_state a in
        Int.equal a.Patch_agent.no_commits_push_count 0
        && not (Patch_agent.needs_intervention a))
  in
  let prop_pnc6_apply_bumps_counter =
    Test.make
      ~name:"PNC-6: apply_session_result Session_no_commits bumps counter by 1"
      (Gen.return ()) (fun () ->
        let orch, pid = mk_busy_orch () in
        let before =
          (Orchestrator.agent orch pid).Patch_agent.no_commits_push_count
        in
        let orch =
          Orchestrator.apply_session_result orch pid Session_no_commits
        in
        let after =
          (Orchestrator.agent orch pid).Patch_agent.no_commits_push_count
        in
        Int.equal before 0 && Int.equal after 1)
  in
  let prop_pnc7_apply_completes_failed =
    Test.make
      ~name:
        "PNC-7: apply_session_result Session_no_commits defers completion \
         (agent stays busy)" (Gen.return ()) (fun () ->
        let orch, pid = mk_busy_orch () in
        let orch =
          Orchestrator.apply_session_result orch pid Session_no_commits
        in
        let a = Orchestrator.agent orch pid in
        (* Session_no_commits defers completion to apply_respond_outcome
           via Respond_no_commits, so agent stays busy after
           apply_session_result alone. *)
        a.Patch_agent.busy)
  in
  let prop_pnc8_apply_preserves_other_counters =
    Test.make
      ~name:
        "PNC-8: apply_session_result Session_no_commits leaves \
         start_attempts_without_pr and ci_failure_count untouched"
      (Gen.pair (Gen.int_range 0 3) (Gen.int_range 0 2))
      (fun (start_n, ci_n) ->
        let orch, pid = mk_busy_orch () in
        let orch =
          List.fold (List.range 0 start_n) ~init:orch ~f:(fun o _ ->
              Orchestrator.increment_start_attempts_without_pr o pid)
        in
        let orch =
          List.fold (List.range 0 ci_n) ~init:orch ~f:(fun o _ ->
              Orchestrator.increment_ci_failure_count o pid)
        in
        let a_before = Orchestrator.agent orch pid in
        let orch =
          Orchestrator.apply_session_result orch pid Session_no_commits
        in
        let a_after = Orchestrator.agent orch pid in
        Int.equal a_before.Patch_agent.start_attempts_without_pr
          a_after.Patch_agent.start_attempts_without_pr
        && Int.equal a_before.Patch_agent.ci_failure_count
             a_after.Patch_agent.ci_failure_count)
  in
  (* context_exhaustion_count property tests (PCE-N) mirror PNC-N: bump the
     counter, clear llm_session_id (so the next session starts fresh rather
     than resuming the overflowed thread), trigger needs_intervention at >= 2,
     and reset on Session_ok / reset_intervention_state. *)
  let prop_pce1_increments_counter =
    Test.make ~name:"PCE-1: on_context_exhausted bumps the counter by exactly 1"
      (Gen.int_range 0 5) (fun n ->
        let a =
          List.fold (List.range 0 n) ~init:(fresh_agent ()) ~f:(fun a _ ->
              Patch_agent.on_context_exhausted a)
        in
        Int.equal a.Patch_agent.context_exhaustion_count n)
  in
  let prop_pce2_intervention_threshold =
    Test.make
      ~name:"PCE-2: context_exhaustion_count >= 2 flips needs_intervention"
      (Gen.int_range 0 4) (fun n ->
        let a =
          List.fold (List.range 0 n) ~init:(fresh_agent ()) ~f:(fun a _ ->
              Patch_agent.on_context_exhausted a)
        in
        Bool.equal (Patch_agent.needs_intervention a) (n >= 2))
  in
  let prop_pce3_clears_session_id =
    Test.make ~name:"PCE-3: on_context_exhausted clears llm_session_id"
      (Gen.return ()) (fun () ->
        let a =
          Patch_agent.set_llm_session_id (fresh_agent ()) (Some "overflowed")
        in
        let a = Patch_agent.on_context_exhausted a in
        Option.is_none a.Patch_agent.llm_session_id)
  in
  let prop_pce4_reset_intervention_clears_counter =
    Test.make ~name:"PCE-4: reset_intervention_state zeros context_exhaustion"
      (Gen.int_range 0 5) (fun n ->
        let a =
          List.fold (List.range 0 n) ~init:(fresh_agent ()) ~f:(fun a _ ->
              Patch_agent.on_context_exhausted a)
        in
        let a = Patch_agent.reset_intervention_state a in
        Int.equal a.Patch_agent.context_exhaustion_count 0
        && not (Patch_agent.needs_intervention a))
  in
  let prop_pce5_apply_bumps_and_clears =
    Test.make
      ~name:
        "PCE-5: apply_session_result Session_context_exhausted bumps counter, \
         clears session, completes" (Gen.return ()) (fun () ->
        let orch, pid = mk_busy_orch () in
        let orch =
          Orchestrator.set_llm_session_id orch pid (Some "overflowed")
        in
        let orch =
          Orchestrator.apply_session_result orch pid Session_context_exhausted
        in
        let a = Orchestrator.agent orch pid in
        Int.equal a.Patch_agent.context_exhaustion_count 1
        && Option.is_none a.Patch_agent.llm_session_id
        && not a.Patch_agent.busy)
  in
  let prop_pce6_session_ok_resets =
    Test.make ~name:"PCE-6: Session_ok resets context_exhaustion_count"
      (Gen.int_range 1 4) (fun n ->
        let orch, pid = mk_busy_orch () in
        let orch =
          List.fold (List.range 0 n) ~init:orch ~f:(fun o _ ->
              Orchestrator.apply_session_result o pid Session_context_exhausted)
        in
        let orch = Orchestrator.apply_session_result orch pid Session_ok in
        let a = Orchestrator.agent orch pid in
        Int.equal a.Patch_agent.context_exhaustion_count 0)
  in
  (* Pure push-gate tests — verify the two pure decision helpers in
     [Worktree] that drive [force_push_with_lease]. *)
  let prop_gate_zero_skips =
    Test.make ~name:"PG-1: push_gate_from_count (Some 0) = Skip_no_commits"
      (Gen.return ()) (fun () ->
        Worktree.equal_push_gate
          (Worktree.push_gate_from_count (Some 0))
          Worktree.Skip_no_commits)
  in
  let prop_gate_positive_proceeds =
    Test.make ~name:"PG-2: push_gate_from_count (Some n>0) = Proceed"
      (Gen.int_range 1 1000) (fun n ->
        Worktree.equal_push_gate
          (Worktree.push_gate_from_count (Some n))
          Worktree.Proceed)
  in
  let prop_gate_unknown_proceeds =
    Test.make
      ~name:
        "PG-3: push_gate_from_count None = Proceed (unknown defaults to push, \
         so real failures surface via push)" (Gen.return ()) (fun () ->
        Worktree.equal_push_gate
          (Worktree.push_gate_from_count None)
          Worktree.Proceed)
  in
  let prop_commit_count_nonzero_exit_none =
    Test.make
      ~name:
        "PG-4: parse_commit_count code<>0 = None (git error -> unknown, not \
         zero)"
      (Gen.pair (Gen.int_range 1 255) Gen.string_small)
      (fun (code, stdout) ->
        Option.is_none (Worktree.parse_commit_count ~code ~stdout))
  in
  let prop_commit_count_valid_int =
    Test.make ~name:"PG-5: parse_commit_count code=0 of int string = Some n"
      (Gen.int_range 0 10000) (fun n ->
        Option.equal Int.equal
          (Worktree.parse_commit_count ~code:0 ~stdout:(Int.to_string n))
          (Some n))
  in
  let prop_commit_count_trailing_newline =
    Test.make
      ~name:
        "PG-6: parse_commit_count code=0 strips surrounding whitespace (git \
         appends \\n)" (Gen.int_range 0 10000) (fun n ->
        Option.equal Int.equal
          (Worktree.parse_commit_count ~code:0
             ~stdout:(Printf.sprintf "  %d\n" n))
          (Some n))
  in
  let prop_commit_count_garbage_none =
    Test.make
      ~name:"PG-7: parse_commit_count code=0 of non-int = None (defensive)"
      (Gen.oneof_list [ "hello"; "12abc"; ""; "  "; "-"; "1.5" ])
      (fun s -> Option.is_none (Worktree.parse_commit_count ~code:0 ~stdout:s))
  in
  let prop_classify_ok_zero_porcelain_equal_up_to_date =
    Test.make
      ~name:
        "PG-8: classify_push_result code=0 with '=' porcelain = Push_up_to_date"
      (Gen.return ()) (fun () ->
        Worktree.equal_push_result
          (Worktree.classify_push_result ~code:0
             ~stdout:"To origin\n= refs/heads/x:refs/heads/x [up to date]\nDone"
             ~stderr:"")
          Worktree.Push_up_to_date)
  in
  let prop_classify_ok_forced_equal_push_ok =
    Test.make
      ~name:"PG-9: classify_push_result code=0 with '+' porcelain = Push_ok"
      (Gen.return ()) (fun () ->
        Worktree.equal_push_result
          (Worktree.classify_push_result ~code:0
             ~stdout:"To origin\n+ refs/heads/x:refs/heads/x [forced]\nDone"
             ~stderr:"")
          Worktree.Push_ok)
  in
  let prop_classify_nonzero_bang_equal_rejected =
    Test.make
      ~name:
        "PG-10: classify_push_result code<>0 with '!' porcelain = Push_rejected"
      (Gen.int_range 1 255) (fun code ->
        match
          Worktree.classify_push_result ~code
            ~stdout:
              "To origin\n\
              \ ! [rejected] refs/heads/x -> refs/heads/x (stale info)"
            ~stderr:""
        with
        | Worktree.Push_rejected _ -> true
        | Worktree.Push_ok | Worktree.Push_up_to_date | Worktree.Push_error _ ->
            false)
  in
  let prop_classify_nonzero_no_porcelain_equal_error =
    Test.make
      ~name:
        "PG-11: classify_push_result code<>0 with no recognizable porcelain = \
         Push_error"
      (Gen.pair (Gen.int_range 1 255) Gen.string_small)
      (fun (code, stderr) ->
        match Worktree.classify_push_result ~code ~stdout:"" ~stderr with
        | Worktree.Push_error _ -> true
        | Worktree.Push_ok | Worktree.Push_up_to_date | Worktree.Push_rejected _
          ->
            false)
  in
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_publication_budget_isolation;
      prop_publication_retry_has_no_session_cap;
      prop_session_success_keeps_owner_intervention;
      prop_pnc1_clears_session_fallback;
      prop_pnc2_increments_counter;
      prop_pnc3_intervention_threshold;
      prop_pnc4_reset_counter;
      prop_pnc5_reset_intervention_clears_counter;
      prop_pnc6_apply_bumps_counter;
      prop_pnc7_apply_completes_failed;
      prop_pnc8_apply_preserves_other_counters;
      prop_pce1_increments_counter;
      prop_pce2_intervention_threshold;
      prop_pce3_clears_session_id;
      prop_pce4_reset_intervention_clears_counter;
      prop_pce5_apply_bumps_and_clears;
      prop_pce6_session_ok_resets;
      prop_gate_zero_skips;
      prop_gate_positive_proceeds;
      prop_gate_unknown_proceeds;
      prop_commit_count_nonzero_exit_none;
      prop_commit_count_valid_int;
      prop_commit_count_trailing_newline;
      prop_commit_count_garbage_none;
      prop_classify_ok_zero_porcelain_equal_up_to_date;
      prop_classify_ok_forced_equal_push_ok;
      prop_classify_nonzero_bang_equal_rejected;
      prop_classify_nonzero_no_porcelain_equal_error;
    ];
  Stdlib.print_endline
    "Session outcomes and owned publication properties passed"

let () = Stdlib.print_endline "all property tests passed"

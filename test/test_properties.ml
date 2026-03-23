open Base
open Onton

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
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_all_ids_preserved;
      prop_deps_match;
      prop_dependents_inverse;
      prop_no_dep_satisfiable;
      prop_depends_on_consistent;
    ];

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
        Bool.equal result.merged (Github.merged pr))
  in
  let prop_conflict_queue =
    Test.make ~name:"poller: conflict in queue iff has_conflict"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result = Poller.poll ~was_merged:false pr in
        let in_queue =
          List.mem result.queue Types.Operation_kind.Merge_conflict
            ~equal:Types.Operation_kind.equal
        in
        Bool.equal in_queue result.has_conflict)
  in
  let prop_ci_queue =
    Test.make ~name:"poller: ci in queue iff checks failing"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result = Poller.poll ~was_merged:false pr in
        let in_queue =
          List.mem result.queue Types.Operation_kind.Ci
            ~equal:Types.Operation_kind.equal
        in
        Bool.equal in_queue (Github.ci_failed pr))
  in
  let prop_review_queue =
    Test.make ~name:"poller: review in queue iff unresolved comments"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result = Poller.poll ~was_merged:false pr in
        let in_queue =
          List.mem result.queue Types.Operation_kind.Review_comments
            ~equal:Types.Operation_kind.equal
        in
        Bool.equal in_queue (not (List.is_empty pr.Github.Pr_state.comments)))
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
          let _orch, actions = Orchestrator.tick orch ~patches in
          match patches with
          | [] -> true
          | first :: _ ->
              List.exists actions ~f:(function
                | Orchestrator.Start (pid, _) ->
                    Types.Patch_id.equal pid first.Types.Patch.id
                | Orchestrator.Respond (_, _) | Orchestrator.Rebase (_, _) ->
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
              let o, _ = Orchestrator.tick o ~patches in
              loop o (n - 1)
          in
          let orch_stable = loop orch (List.length patches + 1) in
          let _orch_final, actions = Orchestrator.tick orch_stable ~patches in
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
              let o, _ = Orchestrator.tick o ~patches in
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
          let _, actions = Orchestrator.tick orch ~patches in
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
        };
    ]
  in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  let orch, _ = Orchestrator.tick orch ~patches in
  let orch = Orchestrator.set_pr_number orch pid (Types.Pr_number.of_int 1) in
  let orch = Orchestrator.complete orch pid in
  let orch = Orchestrator.enqueue orch pid Types.Operation_kind.Ci in
  let _orch, actions = Orchestrator.tick orch ~patches in
  assert (Int.equal (List.length actions) 1);
  assert (
    List.exists actions ~f:(function
      | Orchestrator.Respond (p, k) ->
          Types.Patch_id.equal p pid
          && Types.Operation_kind.equal k Types.Operation_kind.Ci
      | Orchestrator.Start _ | Orchestrator.Rebase _ -> false));

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
      ?(needs_intervention = false) ?(branch_blocked = false) ?(queue = []) id =
    Reconciler.
      {
        id;
        has_pr;
        merged;
        busy;
        needs_intervention;
        branch_blocked;
        queue;
        base_branch = main;
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
          };
      ]
    in
    let orch = Orchestrator.create ~patches ~main_branch:main in
    let orch, _ = Orchestrator.tick orch ~patches in
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
              };
          ]
        in
        try
          let orch = Orchestrator.create ~patches ~main_branch:main in
          let orch, _ = Orchestrator.tick orch ~patches in
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
  (* Property: all failure results leave agent not-busy *)
  let prop_failure_results_complete =
    Test.make ~name:"session: failure results leave agent not-busy" ~count:500
      G.gen_session_result (fun result ->
        let orch, pid = mk_busy_orch () in
        let orch = Orchestrator.apply_session_result orch pid result in
        let a = Orchestrator.agent orch pid in
        match result with
        | Orchestrator.Session_ok ->
            (* Session_ok does NOT complete — agent stays busy *)
            true
        | Orchestrator.Session_process_error _ | Orchestrator.Session_no_resume
        | Orchestrator.Session_failed _ | Orchestrator.Session_give_up
        | Orchestrator.Session_worktree_missing ->
            not a.Patch_agent.busy)
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
        a.Patch_agent.needs_intervention)
  in
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_complete_idempotent;
      prop_session_result_no_crash;
      prop_session_result_double_apply;
      prop_failure_results_complete;
      prop_give_up_intervention;
    ];
  Stdlib.print_endline "session result: all properties passed"

let () = Stdlib.print_endline "all property tests passed"

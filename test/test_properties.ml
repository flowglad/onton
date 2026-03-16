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
  let no_addressed = Set.empty (module Types.Comment_id) in
  let prop_merged_sticky =
    Test.make ~name:"poller: merged is sticky"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result =
          Poller.poll ~was_merged:true ~addressed_ids:no_addressed pr
        in
        result.merged)
  in
  let prop_merged_from_pr =
    Test.make ~name:"poller: merged reflects pr state"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:no_addressed pr
        in
        Bool.equal result.merged pr.Github.Pr_state.merged)
  in
  let prop_conflict_queue =
    Test.make ~name:"poller: conflict in queue iff has_conflict"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:no_addressed pr
        in
        let in_queue =
          List.mem result.queue Types.Operation_kind.Merge_conflict
            ~equal:Types.Operation_kind.equal
        in
        Bool.equal in_queue result.has_conflict)
  in
  let prop_ci_queue =
    Test.make ~name:"poller: ci in queue iff checks failing"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:no_addressed pr
        in
        let in_queue =
          List.mem result.queue Types.Operation_kind.Ci
            ~equal:Types.Operation_kind.equal
        in
        Bool.equal in_queue (Github.ci_failed pr))
  in
  let prop_review_queue =
    Test.make ~name:"poller: review in queue iff new comments"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:no_addressed pr
        in
        let all_ids =
          Set.of_list
            (module Types.Comment_id)
            (List.map pr.Github.Pr_state.comments ~f:(fun c ->
                 c.Types.Comment.id))
        in
        let result_all_addressed =
          Poller.poll ~was_merged:false ~addressed_ids:all_ids pr
        in
        let in_queue =
          List.mem result.queue Types.Operation_kind.Review_comments
            ~equal:Types.Operation_kind.equal
        in
        (* when addressed_ids is empty: in_queue iff there are new comments *)
        (* when addressed_ids covers all comments: never in_queue *)
        Bool.equal in_queue (not (List.is_empty result.new_comments))
        && not
             (List.mem result_all_addressed.queue
                Types.Operation_kind.Review_comments
                ~equal:Types.Operation_kind.equal))
  in
  let prop_no_rebase =
    Test.make ~name:"poller: rebase never in poll queue"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:no_addressed pr
        in
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
                | Orchestrator.Respond (_, _) -> false)
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
          not
            (List.exists actions ~f:(function
              | Orchestrator.Start (_, _) -> true
              | Orchestrator.Respond (_, _) -> false))
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
                let o = Orchestrator.complete o p.id in
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
          branch = Types.Branch.of_string "b1";
          dependencies = [];
        };
    ]
  in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  let orch, _ = Orchestrator.tick orch ~patches in
  let orch = Orchestrator.complete orch pid in
  let orch = Orchestrator.enqueue orch pid Types.Operation_kind.Ci in
  let _orch, actions = Orchestrator.tick orch ~patches in
  assert (
    List.exists actions ~f:(function
      | Orchestrator.Respond (p, k) ->
          Types.Patch_id.equal p pid
          && Types.Operation_kind.equal k Types.Operation_kind.Ci
      | Orchestrator.Start (_, _) -> false));

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
      ?(needs_intervention = false) ?(queue = []) id =
    Reconciler.
      {
        id;
        has_pr;
        merged;
        busy;
        needs_intervention;
        queue;
        base_branch = main;
      }
  in
  let mk_patch id =
    Types.Patch.
      {
        id;
        title = Types.Patch_id.to_string id;
        branch = Types.Branch.of_string "b1";
        dependencies = [];
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
          branch = Types.Branch.of_string "b1";
          dependencies = [];
        };
      Types.Patch.
        {
          id = p2;
          title = "P2";
          branch = Types.Branch.of_string "b2";
          dependencies = [ p1 ];
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

(* ========== Comment dedup properties ========== *)

let () =
  let open QCheck2 in
  let open Onton in
  (* Poller: addressed comments are excluded from new_comments *)
  let prop_addressed_filtered =
    Test.make ~name:"dedup: addressed ids filtered from new_comments"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let all_ids =
          List.map pr.Github.Pr_state.comments ~f:(fun (c : Types.Comment.t) ->
              c.id)
        in
        let addressed = Set.of_list (module Types.Comment_id) all_ids in
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:addressed pr
        in
        List.is_empty result.new_comments)
  in
  (* Poller: all-addressed means no Review_comments in queue *)
  let prop_addressed_suppresses_review =
    Test.make ~name:"dedup: all addressed suppresses review queue"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let all_ids =
          List.map pr.Github.Pr_state.comments ~f:(fun (c : Types.Comment.t) ->
              c.id)
        in
        let addressed = Set.of_list (module Types.Comment_id) all_ids in
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:addressed pr
        in
        not
          (List.mem result.queue Types.Operation_kind.Review_comments
             ~equal:Types.Operation_kind.equal))
  in
  (* Poller: unaddressed comments appear in new_comments *)
  let prop_unaddressed_preserved =
    Test.make ~name:"dedup: unaddressed comments preserved"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        let no_addressed = Set.empty (module Types.Comment_id) in
        let result =
          Poller.poll ~was_merged:false ~addressed_ids:no_addressed pr
        in
        List.length result.new_comments
        = List.length pr.Github.Pr_state.comments)
  in
  (* add_pending_comment is idempotent: adding same comment twice = once *)
  let prop_add_pending_idempotent =
    Test.make ~name:"dedup: add_pending_comment idempotent"
      (Gen.pair Onton_test_support.Test_generators.gen_patch_id
         Onton_test_support.Test_generators.gen_comment) (fun (pid, comment) ->
        let a = Patch_agent.create pid in
        let a = Patch_agent.add_pending_comment a comment ~valid:true in
        let a = Patch_agent.add_pending_comment a comment ~valid:true in
        List.length a.Patch_agent.pending_comments = 1)
  in
  (* Synthetic-to-real migration: content-matching synthetic ID gets upgraded *)
  let prop_synthetic_migration =
    Test.make ~name:"dedup: synthetic to real id migration"
      (Gen.pair Onton_test_support.Test_generators.gen_patch_id
         (Gen.triple
            (Gen.string_size ~gen:Gen.printable (Gen.int_range 1 40))
            (Gen.option
               (Gen.string_size
                  ~gen:Gen.(char_range 'a' 'z')
                  (Gen.int_range 3 20)))
            (Gen.option (Gen.int_range 1 500))))
      (fun (pid, (body, path, line)) ->
        let synthetic_id = Types.Comment_id.of_int (-999_999) in
        let real_id = Types.Comment_id.of_int 42 in
        let synthetic_comment =
          Types.Comment.{ id = synthetic_id; body; path; line }
        in
        let real_comment = Types.Comment.{ id = real_id; body; path; line } in
        let a = Patch_agent.create pid in
        let a =
          Patch_agent.add_pending_comment a synthetic_comment ~valid:true
        in
        let a = Patch_agent.add_pending_comment a real_comment ~valid:true in
        (* Should still be 1 entry, with the real ID *)
        List.length a.Patch_agent.pending_comments = 1
        && Types.Comment_id.equal
             (List.hd_exn a.Patch_agent.pending_comments).comment
               .Types.Comment.id real_id)
  in
  (* Partial addressed: only non-addressed comments pass through *)
  let prop_partial_addressed =
    Test.make ~name:"dedup: partial addressed filters correctly"
      Onton_test_support.Test_generators.gen_pr_state (fun pr ->
        match pr.Github.Pr_state.comments with
        | [] -> true
        | first :: _ ->
            let addressed =
              Set.singleton (module Types.Comment_id) first.Types.Comment.id
            in
            let result =
              Poller.poll ~was_merged:false ~addressed_ids:addressed pr
            in
            (not
               (List.exists result.new_comments ~f:(fun (c : Types.Comment.t) ->
                    Types.Comment_id.equal c.id first.id)))
            && List.length result.new_comments
               >= List.length pr.Github.Pr_state.comments - 1)
  in
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_addressed_filtered;
      prop_addressed_suppresses_review;
      prop_unaddressed_preserved;
      prop_add_pending_idempotent;
      prop_synthetic_migration;
      prop_partial_addressed;
    ];
  Stdlib.print_endline "comment dedup: all properties passed"

let () = Stdlib.print_endline "all property tests passed"

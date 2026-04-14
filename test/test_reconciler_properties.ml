open Base
open Onton
open Onton_test_support.Test_generators

let is_mark_merged = function
  | Reconciler.Mark_merged _ -> true
  | Reconciler.Enqueue_rebase _ | Reconciler.Start_operation _ -> false

let is_start_operation = function
  | Reconciler.Start_operation _ -> true
  | Reconciler.Mark_merged _ | Reconciler.Enqueue_rebase _ -> false

let action_patch_id = function
  | Reconciler.Mark_merged pid -> pid
  | Reconciler.Enqueue_rebase pid -> pid
  | Reconciler.Start_operation { patch_id; _ } -> patch_id

let gen_bool_list n =
  QCheck2.Gen.list_size (QCheck2.Gen.return n) QCheck2.Gen.bool

(* ========== detect_merges properties ========== *)

let prop_detect_merges_only_mark_merged =
  QCheck2.Test.make ~name:"detect_merges: only produces Mark_merged" ~count:500
    (QCheck2.Gen.pair
       (QCheck2.Gen.list_small gen_patch_view)
       (QCheck2.Gen.list_small gen_patch_id))
    (fun (views, merged_prs) ->
      let actions =
        Reconciler.detect_merges views ~merged_pr_patches:merged_prs
      in
      List.for_all actions ~f:is_mark_merged)

let prop_detect_merges_subset_of_merged_prs =
  QCheck2.Test.make ~name:"detect_merges: marks subset of merged_pr_patches"
    ~count:500
    (QCheck2.Gen.pair
       (QCheck2.Gen.list_small gen_patch_view)
       (QCheck2.Gen.list_small gen_patch_id))
    (fun (views, merged_prs) ->
      let actions =
        Reconciler.detect_merges views ~merged_pr_patches:merged_prs
      in
      let merged_set = Set.of_list (module Types.Patch_id) merged_prs in
      List.for_all actions ~f:(fun a -> Set.mem merged_set (action_patch_id a)))

let prop_detect_merges_skips_already_merged =
  QCheck2.Test.make ~name:"detect_merges: skips already-merged views" ~count:500
    (QCheck2.Gen.pair
       (QCheck2.Gen.list_small gen_patch_view)
       (QCheck2.Gen.list_small gen_patch_id))
    (fun (views, merged_prs) ->
      let actions =
        Reconciler.detect_merges views ~merged_pr_patches:merged_prs
      in
      let already_merged =
        List.filter_map views ~f:(fun v ->
            if v.Reconciler.merged then Some v.Reconciler.id else None)
        |> Set.of_list (module Types.Patch_id)
      in
      List.for_all actions ~f:(fun a ->
          not (Set.mem already_merged (action_patch_id a))))

(* ========== detect_rebases properties ========== *)

let gen_rebase_scenario =
  QCheck2.Gen.(
    gen_patch_list_unique >>= fun patches ->
    let graph = Graph.of_patches patches in
    let ids = List.map patches ~f:(fun (p : Types.Patch.t) -> p.id) in
    let main = Types.Branch.of_string "main" in
    let gen_subset =
      map
        (fun bools ->
          List.filter_mapi ids ~f:(fun i id ->
              match List.nth bools i with
              | Some true -> Some id
              | Some false | None -> None))
        (gen_bool_list (List.length ids))
    in
    map
      (fun newly_merged ->
        let views =
          List.map patches ~f:(fun (p : Types.Patch.t) ->
              Reconciler.
                {
                  id = p.id;
                  has_pr = true;
                  merged = false;
                  busy = false;
                  needs_intervention = false;
                  branch_blocked = false;
                  queue = [];
                  base_branch = main;
                  branch_rebased_onto = Some main;
                })
        in
        (graph, views, newly_merged))
      gen_subset)

let prop_detect_rebases_only_enqueue_rebase =
  QCheck2.Test.make ~name:"detect_rebases: only produces Enqueue_rebase"
    ~count:500 gen_rebase_scenario (fun (graph, views, newly_merged) ->
      try
        let actions = Reconciler.detect_rebases graph views ~newly_merged in
        List.for_all actions ~f:(function
          | Reconciler.Enqueue_rebase _ -> true
          | Reconciler.Mark_merged _ | Reconciler.Start_operation _ -> false)
      with _ -> false)

let prop_detect_rebases_never_targets_merged =
  QCheck2.Test.make
    ~name:"detect_rebases: never enqueues rebase for newly-merged patches"
    ~count:500 gen_rebase_scenario (fun (graph, views, newly_merged) ->
      try
        let actions = Reconciler.detect_rebases graph views ~newly_merged in
        let merged_set = Set.of_list (module Types.Patch_id) newly_merged in
        List.for_all actions ~f:(fun a ->
            not (Set.mem merged_set (action_patch_id a)))
      with _ -> false)

let prop_detect_rebases_targets_are_dependents =
  QCheck2.Test.make
    ~name:"detect_rebases: targets are dependents of newly-merged" ~count:500
    gen_rebase_scenario (fun (graph, views, newly_merged) ->
      try
        let actions = Reconciler.detect_rebases graph views ~newly_merged in
        let all_dependents =
          List.concat_map newly_merged ~f:(Graph.dependents graph)
          |> Set.of_list (module Types.Patch_id)
        in
        List.for_all actions ~f:(fun a ->
            Set.mem all_dependents (action_patch_id a))
      with _ -> false)

let prop_detect_rebases_skips_already_queued =
  QCheck2.Test.make
    ~name:"detect_rebases: skips patches with Rebase already in queue"
    ~count:500
    QCheck2.Gen.(
      gen_patch_list_unique >>= fun patches ->
      let graph = Graph.of_patches patches in
      let ids = List.map patches ~f:(fun (p : Types.Patch.t) -> p.id) in
      let main = Types.Branch.of_string "main" in
      let views =
        List.map patches ~f:(fun (p : Types.Patch.t) ->
            Reconciler.
              {
                id = p.id;
                has_pr = true;
                merged = false;
                busy = false;
                needs_intervention = false;
                branch_blocked = false;
                queue = [ Types.Operation_kind.Rebase ];
                base_branch = main;
                branch_rebased_onto = Some main;
              })
      in
      return (graph, views, ids))
    (fun (graph, views, newly_merged) ->
      try
        let actions = Reconciler.detect_rebases graph views ~newly_merged in
        List.is_empty actions
      with _ -> false)

(* ========== plan_operations properties ========== *)

let gen_plan_scenario =
  QCheck2.Gen.(
    gen_patch_list_unique >>= fun patches ->
    let graph = Graph.of_patches patches in
    let main = Types.Branch.of_string "main" in
    let has_merged _ = false in
    let branch_of _ = Types.Branch.of_string "b" in
    map
      (fun views -> (graph, main, has_merged, branch_of, views))
      (list_size
         (return (List.length patches))
         (quad bool bool bool gen_operation_kind_queue)
      |> map (fun props ->
          List.map2_exn patches props
            ~f:(fun (p : Types.Patch.t) (busy, intervention, has_pr, queue) ->
              Reconciler.
                {
                  id = p.id;
                  has_pr;
                  merged = false;
                  busy;
                  needs_intervention = intervention;
                  branch_blocked = false;
                  queue;
                  base_branch = main;
                  branch_rebased_onto = Some main;
                }))))

let prop_plan_skips_busy =
  QCheck2.Test.make ~name:"plan_operations: never starts op on busy patch"
    ~count:500 gen_plan_scenario
    (fun (graph, main, has_merged, branch_of, views) ->
      let actions =
        Reconciler.plan_operations views ~has_merged ~branch_of ~graph ~main
      in
      let busy_ids =
        List.filter_map views ~f:(fun v ->
            if v.Reconciler.busy then Some v.Reconciler.id else None)
        |> Set.of_list (module Types.Patch_id)
      in
      List.for_all actions ~f:(fun a ->
          not (Set.mem busy_ids (action_patch_id a))))

let prop_plan_skips_intervention =
  QCheck2.Test.make
    ~name:"plan_operations: never starts op on needs_intervention patch"
    ~count:500 gen_plan_scenario
    (fun (graph, main, has_merged, branch_of, views) ->
      let actions =
        Reconciler.plan_operations views ~has_merged ~branch_of ~graph ~main
      in
      let intervention_ids =
        List.filter_map views ~f:(fun v ->
            if v.Reconciler.needs_intervention then Some v.Reconciler.id
            else None)
        |> Set.of_list (module Types.Patch_id)
      in
      List.for_all actions ~f:(fun a ->
          not (Set.mem intervention_ids (action_patch_id a))))

let prop_plan_skips_no_pr =
  QCheck2.Test.make ~name:"plan_operations: never starts op on patch without PR"
    ~count:500 gen_plan_scenario
    (fun (graph, main, has_merged, branch_of, views) ->
      let actions =
        Reconciler.plan_operations views ~has_merged ~branch_of ~graph ~main
      in
      let no_pr_ids =
        List.filter_map views ~f:(fun v ->
            if not v.Reconciler.has_pr then Some v.Reconciler.id else None)
        |> Set.of_list (module Types.Patch_id)
      in
      List.for_all actions ~f:(fun a ->
          not (Set.mem no_pr_ids (action_patch_id a))))

let prop_plan_picks_highest_priority =
  QCheck2.Test.make
    ~name:"plan_operations: picks highest priority op from queue" ~count:500
    gen_plan_scenario (fun (graph, main, has_merged, branch_of, views) ->
      try
        let actions =
          Reconciler.plan_operations views ~has_merged ~branch_of ~graph ~main
        in
        let view_by_id =
          Map.of_alist_exn
            (module Types.Patch_id)
            (List.map views ~f:(fun v -> (v.Reconciler.id, v)))
        in
        List.for_all actions ~f:(function
          | Reconciler.Start_operation { patch_id; kind; _ } -> (
              let v = Map.find_exn view_by_id patch_id in
              let min_priority =
                List.map v.Reconciler.queue ~f:Priority.priority
                |> List.min_elt ~compare:Int.compare
              in
              match min_priority with
              | Some p -> Priority.priority kind = p
              | None -> false)
          | Reconciler.Mark_merged _ | Reconciler.Enqueue_rebase _ -> true)
      with _ -> false)

let prop_plan_new_base_only_for_rebase =
  QCheck2.Test.make ~name:"plan_operations: new_base is Some only for Rebase"
    ~count:500 gen_plan_scenario
    (fun (graph, main, has_merged, branch_of, views) ->
      let actions =
        Reconciler.plan_operations views ~has_merged ~branch_of ~graph ~main
      in
      List.for_all actions ~f:(function
        | Reconciler.Start_operation { kind; new_base; _ } -> (
            match kind with
            | Types.Operation_kind.Rebase -> Option.is_some new_base
            | Types.Operation_kind.Human | Types.Operation_kind.Merge_conflict
            | Types.Operation_kind.Ci | Types.Operation_kind.Review_comments
            | Types.Operation_kind.Pr_body ->
                Option.is_none new_base)
        | Reconciler.Mark_merged _ | Reconciler.Enqueue_rebase _ -> true))

let prop_plan_suppresses_rebase_multi_dep =
  QCheck2.Test.make
    ~name:"plan_operations: suppresses Rebase when open_deps > 1" ~count:500
    gen_plan_scenario (fun (graph, main, has_merged, branch_of, views) ->
      let actions =
        Reconciler.plan_operations views ~has_merged ~branch_of ~graph ~main
      in
      (* No Rebase should be emitted for a patch with multiple open deps. *)
      List.for_all actions ~f:(function
        | Reconciler.Start_operation { patch_id; kind; _ } -> (
            match kind with
            | Types.Operation_kind.Rebase ->
                List.length (Graph.open_pr_deps graph patch_id ~has_merged) <= 1
            | Types.Operation_kind.Human | Types.Operation_kind.Merge_conflict
            | Types.Operation_kind.Ci | Types.Operation_kind.Review_comments
            | Types.Operation_kind.Pr_body ->
                true)
        | Reconciler.Mark_merged _ | Reconciler.Enqueue_rebase _ -> true))

let prop_plan_only_start_operation =
  QCheck2.Test.make ~name:"plan_operations: only produces Start_operation"
    ~count:500 gen_plan_scenario
    (fun (graph, main, has_merged, branch_of, views) ->
      let actions =
        Reconciler.plan_operations views ~has_merged ~branch_of ~graph ~main
      in
      List.for_all actions ~f:is_start_operation)

let prop_plan_empty_queue_no_action =
  QCheck2.Test.make ~name:"plan_operations: empty queue produces no action"
    ~count:500 gen_plan_scenario
    (fun (graph, main, has_merged, branch_of, views) ->
      let empty_queue_views =
        List.map views ~f:(fun v -> { v with Reconciler.queue = [] })
      in
      let actions =
        Reconciler.plan_operations empty_queue_views ~has_merged ~branch_of
          ~graph ~main
      in
      List.is_empty actions)

(* ========== reconcile properties ========== *)

let gen_reconcile_scenario =
  QCheck2.Gen.(
    gen_patch_list_unique >>= fun patches ->
    let graph = Graph.of_patches patches in
    let ids = List.map patches ~f:(fun (p : Types.Patch.t) -> p.id) in
    let main = Types.Branch.of_string "main" in
    let branch_of _ = Types.Branch.of_string "b" in
    let gen_subset =
      map
        (fun bools ->
          List.filter_mapi ids ~f:(fun i id ->
              match List.nth bools i with
              | Some true -> Some id
              | Some false | None -> None))
        (gen_bool_list (List.length ids))
    in
    map2
      (fun merged_prs views -> (graph, main, branch_of, merged_prs, views))
      gen_subset
      (return
         (List.map patches ~f:(fun (p : Types.Patch.t) ->
              Reconciler.
                {
                  id = p.id;
                  has_pr = true;
                  merged = false;
                  busy = false;
                  needs_intervention = false;
                  branch_blocked = false;
                  queue = [];
                  base_branch = main;
                  branch_rebased_onto = Some main;
                }))))

let prop_reconcile_merges_subset =
  QCheck2.Test.make
    ~name:"reconcile: Mark_merged targets are subset of merged_pr_patches"
    ~count:500 gen_reconcile_scenario
    (fun (graph, main, branch_of, merged_prs, views) ->
      let actions =
        Reconciler.reconcile ~graph ~main ~merged_pr_patches:merged_prs
          ~branch_of views
      in
      let merged_set = Set.of_list (module Types.Patch_id) merged_prs in
      List.for_all actions ~f:(function
        | Reconciler.Mark_merged pid -> Set.mem merged_set pid
        | Reconciler.Enqueue_rebase _ | Reconciler.Start_operation _ -> true))

let prop_reconcile_no_dup_action_types_per_patch =
  QCheck2.Test.make ~name:"reconcile: at most one Start_operation per patch"
    ~count:500 gen_reconcile_scenario
    (fun (graph, main, branch_of, merged_prs, views) ->
      let actions =
        Reconciler.reconcile ~graph ~main ~merged_pr_patches:merged_prs
          ~branch_of views
      in
      let started =
        List.filter_map actions ~f:(function
          | Reconciler.Start_operation { patch_id; _ } -> Some patch_id
          | Reconciler.Mark_merged _ | Reconciler.Enqueue_rebase _ -> None)
      in
      let deduped =
        List.dedup_and_sort started ~compare:Types.Patch_id.compare
      in
      List.length started = List.length deduped)

let prop_reconcile_no_action_on_merged =
  QCheck2.Test.make
    ~name:"reconcile: no Start_operation on already-merged views" ~count:500
    gen_reconcile_scenario (fun (graph, main, branch_of, merged_prs, views) ->
      let merged_views =
        List.map views ~f:(fun v -> { v with Reconciler.merged = true })
      in
      let actions =
        Reconciler.reconcile ~graph ~main ~merged_pr_patches:merged_prs
          ~branch_of merged_views
      in
      not (List.exists actions ~f:is_start_operation))

(* ========== detect_notified_base_drift properties ========== *)

(* Tests specifically for the drift detector that catches the
   GitHub-auto-retargeted-after-merge case. Each property constructs views
   explicitly (not through the randomized gen_patch_view which defaults to no
   drift). *)

let main_br = Types.Branch.of_string "main"
let dep_br = Types.Branch.of_string "dep"
let other_br = Types.Branch.of_string "other"

let mk_view ~id ?(has_pr = true) ?(merged = false) ?(queue = [])
    ?(base_branch = main_br) ?(branch_rebased_onto = Some main_br) () =
  Reconciler.
    {
      id;
      has_pr;
      merged;
      busy = false;
      needs_intervention = false;
      branch_blocked = false;
      queue;
      base_branch;
      branch_rebased_onto;
    }

let pid s = Types.Patch_id.of_string s

let prop_drift_fires_on_divergence =
  QCheck2.Test.make ~name:"drift: fires when rebased_onto != base_branch"
    ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      let v =
        mk_view ~id:(pid "p1") ~base_branch:main_br
          ~branch_rebased_onto:(Some dep_br) ()
      in
      match Reconciler.detect_notified_base_drift [ v ] with
      | [ Reconciler.Enqueue_rebase p ] -> Types.Patch_id.equal p (pid "p1")
      | [] -> false
      | [ Reconciler.Mark_merged _ ] | [ Reconciler.Start_operation _ ] -> false
      | _ :: _ :: _ -> false)

let prop_drift_silent_on_match =
  QCheck2.Test.make ~name:"drift: silent when rebased_onto == base_branch"
    ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      let v =
        mk_view ~id:(pid "p1") ~base_branch:main_br
          ~branch_rebased_onto:(Some main_br) ()
      in
      List.is_empty (Reconciler.detect_notified_base_drift [ v ]))

let prop_drift_silent_on_none =
  QCheck2.Test.make
    ~name:"drift: silent when branch_rebased_onto = None (pre-start)" ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      let v =
        mk_view ~id:(pid "p1") ~base_branch:main_br ~branch_rebased_onto:None ()
      in
      List.is_empty (Reconciler.detect_notified_base_drift [ v ]))

let prop_drift_silent_on_merged =
  QCheck2.Test.make ~name:"drift: silent on merged patches" ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      let v =
        mk_view ~id:(pid "p1") ~merged:true ~base_branch:main_br
          ~branch_rebased_onto:(Some dep_br) ()
      in
      List.is_empty (Reconciler.detect_notified_base_drift [ v ]))

let prop_drift_silent_on_no_pr =
  QCheck2.Test.make ~name:"drift: silent when has_pr = false" ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      let v =
        mk_view ~id:(pid "p1") ~has_pr:false ~base_branch:main_br
          ~branch_rebased_onto:(Some dep_br) ()
      in
      List.is_empty (Reconciler.detect_notified_base_drift [ v ]))

let prop_drift_silent_when_rebase_queued =
  QCheck2.Test.make
    ~name:"drift: silent when Rebase is already in the queue (idempotent)"
    ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      let v =
        mk_view ~id:(pid "p1")
          ~queue:[ Types.Operation_kind.Rebase ]
          ~base_branch:main_br ~branch_rebased_onto:(Some dep_br) ()
      in
      List.is_empty (Reconciler.detect_notified_base_drift [ v ]))

let prop_drift_always_produces_enqueue_rebase =
  QCheck2.Test.make ~name:"drift: every emitted action is Enqueue_rebase"
    ~count:200
    (QCheck2.Gen.list_size
       (QCheck2.Gen.int_range 1 6)
       Onton_test_support.Test_generators.gen_patch_view)
    (fun views ->
      (* Force some drift by setting rebased_onto to a branch different from
         base_branch on a random subset. *)
      let drifted_views =
        List.mapi views ~f:(fun i v ->
            if i % 2 = 0 then
              { v with Reconciler.branch_rebased_onto = Some other_br }
            else v)
      in
      Reconciler.detect_notified_base_drift drifted_views
      |> List.for_all ~f:(function
        | Reconciler.Enqueue_rebase _ -> true
        | Reconciler.Mark_merged _ | Reconciler.Start_operation _ -> false))

(* End-to-end: reconcile emits Enqueue_rebase on a drifted view when no
   other detector would have caught it (base_branch already matches
   initial_base, merged list is empty, no newly_merged). This is the exact
   patch-2 scenario from retire-blue-green. *)
let prop_reconcile_e2e_catches_drift =
  QCheck2.Test.make
    ~name:"reconcile: drift detector catches GitHub-auto-retargeted case"
    ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      (* Single patch with no deps; base_branch = main, rebased_onto = dep
         (stale). No merged_prs. *)
      let patch : Types.Patch.t =
        {
          id = pid "p1";
          title = "t";
          description = "";
          branch = Types.Branch.of_string "p1";
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
      let graph = Graph.of_patches [ patch ] in
      let views =
        [
          mk_view ~id:(pid "p1") ~base_branch:main_br
            ~branch_rebased_onto:(Some dep_br) ();
        ]
      in
      let actions =
        Reconciler.reconcile ~graph ~main:main_br ~merged_pr_patches:[]
          ~branch_of:(fun _ -> main_br)
          views
      in
      List.exists actions ~f:(function
        | Reconciler.Enqueue_rebase p -> Types.Patch_id.equal p (pid "p1")
        | Reconciler.Mark_merged _ | Reconciler.Start_operation _ -> false))

(* Dedup: when both stale-base and drift detectors would fire for the same
   patch, reconcile emits exactly one Enqueue_rebase. *)
let prop_reconcile_dedup_rebase =
  QCheck2.Test.make
    ~name:"reconcile: dedup — at most one Enqueue_rebase per patch" ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      let patch : Types.Patch.t =
        {
          id = pid "p1";
          title = "t";
          description = "";
          branch = Types.Branch.of_string "p1";
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
      let graph = Graph.of_patches [ patch ] in
      (* Construct a view that triggers BOTH detect_stale_bases and
         detect_notified_base_drift: base_branch=dep (stale vs initial_base
         which is main), rebased_onto=other (stale vs base). *)
      let views =
        [
          mk_view ~id:(pid "p1") ~base_branch:dep_br
            ~branch_rebased_onto:(Some other_br) ();
        ]
      in
      let actions =
        Reconciler.reconcile ~graph ~main:main_br ~merged_pr_patches:[]
          ~branch_of:(fun _ -> main_br)
          views
      in
      let rebases =
        List.filter actions ~f:(function
          | Reconciler.Enqueue_rebase _ -> true
          | Reconciler.Mark_merged _ | Reconciler.Start_operation _ -> false)
      in
      List.length rebases = 1)

let () =
  let tests =
    [
      prop_detect_merges_only_mark_merged;
      prop_detect_merges_subset_of_merged_prs;
      prop_detect_merges_skips_already_merged;
      prop_detect_rebases_only_enqueue_rebase;
      prop_detect_rebases_never_targets_merged;
      prop_detect_rebases_targets_are_dependents;
      prop_detect_rebases_skips_already_queued;
      prop_plan_skips_busy;
      prop_plan_skips_intervention;
      prop_plan_skips_no_pr;
      prop_plan_picks_highest_priority;
      prop_plan_new_base_only_for_rebase;
      prop_plan_suppresses_rebase_multi_dep;
      prop_plan_only_start_operation;
      prop_plan_empty_queue_no_action;
      prop_reconcile_merges_subset;
      prop_reconcile_no_dup_action_types_per_patch;
      prop_reconcile_no_action_on_merged;
      prop_drift_fires_on_divergence;
      prop_drift_silent_on_match;
      prop_drift_silent_on_none;
      prop_drift_silent_on_merged;
      prop_drift_silent_on_no_pr;
      prop_drift_silent_when_rebase_queued;
      prop_drift_always_produces_enqueue_rebase;
      prop_reconcile_e2e_catches_drift;
      prop_reconcile_dedup_rebase;
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t);
  Stdlib.print_endline
    (Printf.sprintf "reconciler properties: all %d tests passed"
       (List.length tests))

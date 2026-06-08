(* @archlint.module test
   @archlint.domain reconciler *)

open Base
open Onton_core
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
                  base_contains_merged_siblings = true;
                  sibling_rebase_target = None;
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
                base_contains_merged_siblings = true;
                sibling_rebase_target = None;
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
                  base_contains_merged_siblings = true;
                  sibling_rebase_target = None;
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
            | Types.Operation_kind.Findings | Types.Operation_kind.Pr_body ->
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
            | Types.Operation_kind.Findings | Types.Operation_kind.Pr_body ->
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
                  base_contains_merged_siblings = true;
                  sibling_rebase_target = None;
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
    ?(base_branch = main_br) ?(branch_rebased_onto = Some main_br)
    ?(base_contains_merged_siblings = true) ?(sibling_rebase_target = None) () =
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
      base_contains_merged_siblings;
      sibling_rebase_target;
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
          complexity = None;
          precedents = [];
          required_context = [];
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
          complexity = None;
          precedents = [];
          required_context = [];
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

(* ========== detect_sibling_stale_bases (fan-in) ========== *)

let mk_patch ~id ~deps : Types.Patch.t =
  {
    id = pid id;
    title = "";
    description = "";
    branch = Types.Branch.of_string id;
    dependencies = List.map deps ~f:pid;
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

(* P -> {d1, d2, d3}, with d2 a stacked child of d1 and d3 an independent
   sibling — the exact fan-in shape from the bug report. *)
let fanin_graph () =
  Graph.of_patches
    [
      mk_patch ~id:"p" ~deps:[ "d1"; "d2"; "d3" ];
      mk_patch ~id:"d1" ~deps:[];
      mk_patch ~id:"d2" ~deps:[ "d1" ];
      mk_patch ~id:"d3" ~deps:[];
    ]

let br s = Types.Branch.of_string s
let branch_of p = br (Types.Patch_id.to_string p)
let merged_in ids p = List.mem ids p ~equal:Types.Patch_id.equal

(* Build the fan-in view set for P given which deps are merged and whether P's
   base (the sole open dep) is believed to contain P's merged siblings. *)
let fanin_views ~merged ~contains =
  let p_view =
    let open_deps =
      List.filter
        [ pid "d1"; pid "d2"; pid "d3" ]
        ~f:(fun d -> not (merged_in merged d))
    in
    let base = match open_deps with [ d ] -> branch_of d | _ -> main_br in
    (* The frontier the poller would compute: in this flat fan-in every open
       dep's own deps are merged or main-based (its structural base already
       holds the content), so the frontier is the sole open dep itself when
       the base is stale. *)
    let sibling_rebase_target =
      match (contains, open_deps) with false, [ d ] -> Some d | _ -> None
    in
    mk_view ~id:(pid "p") ~base_branch:base ~branch_rebased_onto:(Some base)
      ~base_contains_merged_siblings:contains ~sibling_rebase_target ()
  in
  [
    p_view;
    mk_view ~id:(pid "d1") ();
    mk_view ~id:(pid "d2") ();
    mk_view ~id:(pid "d3") ();
  ]

let sibling_rebase_targets ~merged ~contains =
  let graph = fanin_graph () in
  let views = fanin_views ~merged ~contains in
  Reconciler.detect_sibling_stale_bases graph views
    ~has_merged:(merged_in merged)
  |> List.filter_map ~f:(function
    | Reconciler.Enqueue_rebase p -> Some p
    | Reconciler.Mark_merged _ | Reconciler.Start_operation _ -> None)

(* REG-perm-{1,2,3}: for each "last-but-one merged" permutation, the detector
   enqueues a rebase of the *sole open dep* (the base B), so B absorbs P's
   merged siblings before P is cut from it. *)
let prop_fanin_perm_open_d2 =
  QCheck2.Test.make
    ~name:"sibling: {d1,d3 merged, d2 open}, base stale → rebase d2" ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      List.equal Types.Patch_id.equal
        (sibling_rebase_targets ~merged:[ pid "d1"; pid "d3" ] ~contains:false)
        [ pid "d2" ])

let prop_fanin_perm_open_d1 =
  QCheck2.Test.make
    ~name:"sibling: {d2,d3 merged, d1 open}, base stale → rebase d1" ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      List.equal Types.Patch_id.equal
        (sibling_rebase_targets ~merged:[ pid "d2"; pid "d3" ] ~contains:false)
        [ pid "d1" ])

let prop_fanin_perm_open_d3 =
  QCheck2.Test.make
    ~name:"sibling: {d1,d2 merged, d3 open}, base stale → rebase d3" ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      List.equal Types.Patch_id.equal
        (sibling_rebase_targets ~merged:[ pid "d1"; pid "d2" ] ~contains:false)
        [ pid "d3" ])

(* Positive: once the base contains the merged siblings, the detector is
   silent — no over-rebasing (and the eligibility gate would Allow P). *)
let prop_fanin_contains_silent =
  QCheck2.Test.make
    ~name:"sibling: base already contains merged siblings → no rebase" ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      List.is_empty
        (sibling_rebase_targets ~merged:[ pid "d1"; pid "d3" ] ~contains:true))

(* Invariant (ii): a merged dep that the base already contains never triggers a
   sibling rebase — i.e. unrelated/already-absorbed merges do not cause churn. *)
let prop_fanin_no_rebase_when_contained =
  QCheck2.Test.make
    ~name:"sibling: no rebase from already-absorbed merges (invariant ii)"
    ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      (* d1 merged, d2 open; base (d2) already contains d1. No rebase. *)
      List.is_empty (sibling_rebase_targets ~merged:[ pid "d1" ] ~contains:true))

(* Idempotent: if the base B already has a Rebase queued, no new emission. *)
let prop_fanin_idempotent_when_queued =
  QCheck2.Test.make ~name:"sibling: silent when base already has Rebase queued"
    ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      let graph = fanin_graph () in
      let merged = [ pid "d1"; pid "d3" ] in
      let views =
        [
          mk_view ~id:(pid "p")
            ~base_branch:(branch_of (pid "d2"))
            ~branch_rebased_onto:(Some (branch_of (pid "d2")))
            ~base_contains_merged_siblings:false ();
          (* d2 (the base B) already has a Rebase queued *)
          mk_view ~id:(pid "d2") ~queue:[ Types.Operation_kind.Rebase ] ();
        ]
      in
      List.is_empty
        (Reconciler.detect_sibling_stale_bases graph views
           ~has_merged:(merged_in merged)))

(* A queued/unstarted sole-open dep has no PR/worktree yet. The containment
   oracle may therefore fail-closed for P's base branch, but the sibling detector
   must not enqueue a Rebase for B until B itself has a PR. Once B starts and the
   branch exists locally, a later tick can recompute containment or enqueue the
   rebase normally. *)
let prop_fanin_silent_when_base_has_no_pr =
  QCheck2.Test.make ~name:"sibling: silent when sole open dep has no PR"
    ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      let graph = fanin_graph () in
      let merged = [ pid "d1"; pid "d3" ] in
      let views =
        [
          mk_view ~id:(pid "p")
            ~base_branch:(branch_of (pid "d2"))
            ~branch_rebased_onto:(Some (branch_of (pid "d2")))
            ~base_contains_merged_siblings:false ();
          mk_view ~id:(pid "d2") ~has_pr:false ();
        ]
      in
      List.is_empty
        (Reconciler.detect_sibling_stale_bases graph views
           ~has_merged:(merged_in merged)))

(* The fan-in patch P most in need of this detector is an *unstarted* one: its
   pending Start(P, base=B) is deferred by Start_eligibility
   ([Base_missing_merged_sibling]) until B absorbs the merged sibling, and an
   unstarted P has no PR of its own for any other detector to act on. The
   detector must therefore fire on P's behalf regardless of P's own PR state —
   only B's rebasability (PR present, no Rebase queued) gates the emission. *)
let prop_fanin_unstarted_p_enqueues_base_rebase =
  QCheck2.Test.make
    ~name:"sibling: unstarted P (no PR), base stale → rebase sole open dep"
    ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      let graph = fanin_graph () in
      let merged = [ pid "d1"; pid "d3" ] in
      let views =
        [
          (* P is unstarted: no PR, no base recorded yet. The frontier the
             poller would compute is the sole open dep d2 itself (d2's own dep
             d1 is merged, so d2's structural base is main — the content
             source). *)
          mk_view ~id:(pid "p") ~has_pr:false ~branch_rebased_onto:None
            ~base_contains_merged_siblings:false
            ~sibling_rebase_target:(Some (pid "d2"))
            ();
          mk_view ~id:(pid "d2") ();
        ]
      in
      let targets =
        Reconciler.detect_sibling_stale_bases graph views
          ~has_merged:(merged_in merged)
        |> List.filter_map ~f:(function
          | Reconciler.Enqueue_rebase p -> Some p
          | Reconciler.Mark_merged _ | Reconciler.Start_operation _ -> None)
      in
      List.equal Types.Patch_id.equal targets [ pid "d2" ])

(* Only Enqueue_rebase, never other action kinds; total over which-dep-open. *)
let prop_fanin_only_enqueue_rebase =
  QCheck2.Test.make
    ~name:"sibling: only emits Enqueue_rebase, over all open-dep choices"
    ~count:200
    QCheck2.Gen.(oneof_list [ "d1"; "d2"; "d3" ])
    (fun open_dep ->
      let merged =
        List.filter
          [ pid "d1"; pid "d2"; pid "d3" ]
          ~f:(fun d -> not (Types.Patch_id.equal d (pid open_dep)))
      in
      let graph = fanin_graph () in
      let views = fanin_views ~merged ~contains:false in
      let actions =
        Reconciler.detect_sibling_stale_bases graph views
          ~has_merged:(merged_in merged)
      in
      List.for_all actions ~f:(function
        | Reconciler.Enqueue_rebase _ -> true
        | Reconciler.Mark_merged _ | Reconciler.Start_operation _ -> false))

(* No raise / no emission when P is not at the last-but-one edge (>1 open dep)
   or when all deps merged (0 open deps, base = main). *)
let prop_fanin_silent_off_edge =
  QCheck2.Test.make ~name:"sibling: silent at 0 or >1 open deps (totality)"
    ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      (* >1 open dep: nothing merged → 3 open deps. *)
      let none_merged =
        List.is_empty (sibling_rebase_targets ~merged:[] ~contains:false)
      in
      (* 0 open deps: all merged → base is main, no sole open dep. *)
      let all_merged =
        List.is_empty
          (sibling_rebase_targets
             ~merged:[ pid "d1"; pid "d2"; pid "d3" ]
             ~contains:true)
      in
      none_merged && all_merged)

(* reconcile-level: the sibling-stale scenario surfaces Enqueue_rebase d2 (the
   demand for freshening the base B before P is cut from it). P's own deferral
   is enforced separately by the orchestrator's runnable_messages gate
   (Start_eligibility), exercised by the SEP-3f property; at the pure reconcile
   layer P legitimately also gets a dependent rebase from detect_rebases. *)
let prop_fanin_reconcile_enqueues_base_rebase =
  QCheck2.Test.make ~name:"sibling: reconcile enqueues base (d2) rebase"
    ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      let graph = fanin_graph () in
      let merged = [ pid "d1"; pid "d3" ] in
      let views = fanin_views ~merged ~contains:false in
      let actions =
        Reconciler.reconcile ~graph ~main:main_br ~merged_pr_patches:merged
          ~branch_of views
      in
      List.exists actions ~f:(function
        | Reconciler.Enqueue_rebase p -> Types.Patch_id.equal p (pid "d2")
        | Reconciler.Mark_merged _ | Reconciler.Start_operation _ -> false))

(* merge_target agrees with Graph.initial_base over the fan-in graph for any
   subset of merged deps that leaves at most one open dep (its precondition),
   and yields main when every dep is merged. *)
let prop_merge_target_matches_initial_base =
  QCheck2.Test.make ~name:"merge_target = initial_base (<=1 open dep)"
    ~count:300
    QCheck2.Gen.(oneof_list [ "d1"; "d2"; "d3" ])
    (fun open_dep ->
      let graph = fanin_graph () in
      (* Mark every dep except [open_dep] as merged, so P has exactly one open
         dep — the precondition for both merge_target and initial_base. *)
      let merged =
        List.filter
          [ pid "d1"; pid "d2"; pid "d3" ]
          ~f:(fun d -> not (Types.Patch_id.equal d (pid open_dep)))
      in
      let has_merged = merged_in merged in
      let mt =
        Reconciler.merge_target graph (pid "p") ~has_merged ~branch_of
          ~main:main_br
      in
      let ib =
        Graph.initial_base graph (pid "p") ~has_merged ~branch_of ~main:main_br
      in
      Types.Branch.equal mt ib
      && Types.Branch.equal mt (branch_of (pid open_dep)))

(* When all deps are merged, P has no open deps and merge_target is main. *)
let prop_merge_target_all_merged_is_main =
  QCheck2.Test.make ~name:"merge_target = main when all deps merged" ~count:1
    QCheck2.Gen.(return ())
    (fun () ->
      let graph = fanin_graph () in
      let merged = [ pid "d1"; pid "d2"; pid "d3" ] in
      Types.Branch.equal
        (Reconciler.merge_target graph (pid "p") ~has_merged:(merged_in merged)
           ~branch_of ~main:main_br)
        main_br)

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
      prop_fanin_perm_open_d2;
      prop_fanin_perm_open_d1;
      prop_fanin_perm_open_d3;
      prop_fanin_contains_silent;
      prop_fanin_no_rebase_when_contained;
      prop_fanin_idempotent_when_queued;
      prop_fanin_silent_when_base_has_no_pr;
      prop_fanin_unstarted_p_enqueues_base_rebase;
      prop_fanin_only_enqueue_rebase;
      prop_fanin_silent_off_edge;
      prop_fanin_reconcile_enqueues_base_rebase;
      prop_merge_target_matches_initial_base;
      prop_merge_target_all_merged_is_main;
    ]
  in
  List.iter tests ~f:(fun t -> QCheck2.Test.check_exn t);
  Stdlib.print_endline
    (Printf.sprintf "reconciler properties: all %d tests passed"
       (List.length tests))

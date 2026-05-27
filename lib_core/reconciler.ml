open Base
open Types

type patch_view = {
  id : Patch_id.t;
  has_pr : bool;
  merged : bool;
  busy : bool;
  needs_intervention : bool;
  branch_blocked : bool;
  queue : Operation_kind.t list;
  base_branch : Branch.t;
  branch_rebased_onto : Branch.t option;
  base_contains_merged_siblings : bool;
      (** Whether this patch's resolved base branch already contains the squash
          commit of every *merged* dependency of this patch. Computed
          effectfully by the caller ([poller_fiber]) via [Worktree.is_ancestor]
          over each merged dep's recorded merge-commit SHA, fail-closed to
          [false] when a SHA is not yet known. [true] when the base is main (all
          deps merged into main) or the base branch already carries the merged
          siblings. Drives [detect_sibling_stale_bases] and the Start/Rebase
          gate. *)
}
[@@deriving sexp_of]

type action =
  | Mark_merged of Patch_id.t
  | Enqueue_rebase of Patch_id.t
  | Start_operation of {
      patch_id : Patch_id.t;
      kind : Operation_kind.t;
      new_base : Branch.t option;
    }
[@@deriving sexp_of]

let highest_priority_op queue =
  List.min_elt queue ~compare:(fun a b ->
      Int.compare (Priority.priority a) (Priority.priority b))

let merge_target graph patch_id ~has_merged ~branch_of ~main =
  Graph.initial_base graph patch_id ~has_merged ~branch_of ~main

let detect_merges views ~merged_pr_patches =
  let merged_set = Set.of_list (module Patch_id) merged_pr_patches in
  List.filter_map views ~f:(fun v ->
      if Set.mem merged_set v.id && not v.merged then Some (Mark_merged v.id)
      else None)

let detect_rebases graph views ~newly_merged =
  let newly_merged_set = Set.of_list (module Patch_id) newly_merged in
  let view_by_id =
    match
      Map.of_alist (module Patch_id) (List.map views ~f:(fun v -> (v.id, v)))
    with
    | `Ok m -> m
    | `Duplicate_key pid ->
        invalid_arg
          (Printf.sprintf "Reconciler.detect_rebases: duplicate patch view %s"
             (Patch_id.to_string pid))
  in
  newly_merged
  |> List.concat_map ~f:(Graph.dependents graph)
  |> Set.of_list (module Patch_id)
  |> Set.to_list
  |> List.filter_map ~f:(fun dep_id ->
      match Map.find view_by_id dep_id with
      | Some v
        when (not (Set.mem newly_merged_set dep_id))
             && v.has_pr && (not v.merged)
             && not
                  (List.mem v.queue Operation_kind.Rebase
                     ~equal:Operation_kind.equal) ->
          Some (Enqueue_rebase dep_id)
      | _ -> None)

(** Detect agents whose local branch is rebased onto something other than
    [base_branch]. [branch_rebased_onto] is updated by Start (to the initial
    base) and by successful Rebase (to the rebase target). If [base_branch] has
    since moved — typically because a dep branch was merged and deleted on
    GitHub, causing GitHub to auto-retarget the PR to [main], and the poller
    then refreshed [base_branch] to match — the local branch still carries the
    old dep's commits in its history and needs a rebase even though
    [base_branch] already equals the structurally-correct base.

    This is the case [detect_stale_bases] misses: there, the structural check
    [base_branch vs initial_base] both read [main] and agreed, so no rebase was
    enqueued. *)

let detect_notified_base_drift views =
  List.filter_map views ~f:(fun v ->
      if
        v.has_pr && (not v.merged)
        && not
             (List.mem v.queue Operation_kind.Rebase ~equal:Operation_kind.equal)
      then
        match v.branch_rebased_onto with
        | Some rebased_onto when not (Branch.equal rebased_onto v.base_branch)
          ->
            Some (Enqueue_rebase v.id)
        | Some _ | None -> None
      else None)

(** Detect agents whose base_branch still points at a merged dependency's
    branch. This catches cases where the event-driven detect_rebases missed the
    rebase (e.g. agent had needs_intervention at the time). *)
let detect_stale_bases graph views ~has_merged ~branch_of ~main =
  List.filter_map views ~f:(fun v ->
      if
        v.has_pr && (not v.merged)
        && (not
              (List.mem v.queue Operation_kind.Rebase
                 ~equal:Operation_kind.equal))
        && List.length (Graph.open_pr_deps graph v.id ~has_merged) <= 1
      then
        let correct_base =
          Graph.initial_base graph v.id ~has_merged ~branch_of ~main
        in
        if not (Branch.equal v.base_branch correct_base) then
          Some (Enqueue_rebase v.id)
        else None
      else None)

(** Detect fan-in patches whose resolved base branch does not yet contain a
    merged *sibling* dependency. When [P] depends on several patches and exactly
    one is still open, [P]'s base is that sole open dep [B]; [B]'s branch is a
    sibling of [P]'s already-merged deps and does not carry their squash
    commits. The other rebase detectors never fix this: [B] is not a dependent
    of the merged sibling, so none of [detect_rebases] / [detect_stale_bases] /
    [detect_notified_base_drift] enqueues [B]'s rebase. This detector creates
    that demand — it enqueues a [Rebase] of [B] (the dependency), which on its
    next run rebases onto [Graph.initial_base B] (= main once [B]'s own deps are
    merged) and thereby absorbs [P]'s merged siblings. [P]'s own
    [Start]/[Rebase] stays deferred by the eligibility gate
    ([Base_missing_merged_sibling]) until [B] is fresh.

    Guards mirror the other detectors (has_pr, ~merged, and skip when [B]
    already has a [Rebase] queued). The [open_pr_deps = 1] guard keeps
    [sole_open_dep] total and pins this to the "last-but-one dependency merged"
    edge, where a single rebase of [B] realizes containment for [P]. *)
let detect_sibling_stale_bases graph views ~has_merged =
  let view_by_id =
    List.fold views
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc v ->
        match Map.add acc ~key:v.id ~data:v with
        | `Ok m -> m
        | `Duplicate ->
            invalid_arg
              (Printf.sprintf
                 "Reconciler.detect_sibling_stale_bases: duplicate patch view \
                  %s"
                 (Patch_id.to_string v.id)))
  in
  (* [b] is rebasable only once it has a PR and does not already have a Rebase
     queued. Guarding on [b]'s own view (not the fan-in patch [v]'s) keeps this
     detector as disciplined as the other three, which only ever enqueue a
     rebase for a patch they have validated. In particular, an unstarted/queued
     dep with no worktree leaves the fan-in patch fail-closed for now; once [b]
     starts and obtains a PR, the next reconcile tick can enqueue its rebase. *)
  let base_rebasable b =
    match Map.find view_by_id b with
    | Some bv ->
        bv.has_pr
        && not
             (List.mem bv.queue Operation_kind.Rebase
                ~equal:Operation_kind.equal)
    | None -> false
  in
  List.filter_map views ~f:(fun v ->
      if
        v.has_pr && (not v.merged)
        && List.length (Graph.open_pr_deps graph v.id ~has_merged) = 1
        && not v.base_contains_merged_siblings
      then
        let b = Graph.sole_open_dep graph v.id ~has_merged in
        if base_rebasable b then Some (Enqueue_rebase b) else None
      else None)

let plan_operations views ~has_merged ~branch_of ~graph ~main =
  List.filter_map views ~f:(fun v ->
      if
        v.has_pr && (not v.merged) && (not v.busy) && (not v.needs_intervention)
        && not v.branch_blocked
      then
        match highest_priority_op v.queue with
        | Some kind ->
            let new_base =
              match kind with
              | Operation_kind.Rebase ->
                  if List.length (Graph.open_pr_deps graph v.id ~has_merged) > 1
                  then None
                  else
                    Some (merge_target graph v.id ~has_merged ~branch_of ~main)
              | Operation_kind.Human | Operation_kind.Merge_conflict
              | Operation_kind.Ci | Operation_kind.Review_comments
              | Operation_kind.Findings | Operation_kind.Pr_body ->
                  None
            in
            if
              Operation_kind.equal kind Operation_kind.Rebase
              && Option.is_none new_base
            then None
            else Some (Start_operation { patch_id = v.id; kind; new_base })
        | None -> None
      else None)

let reconcile ~graph ~main ~merged_pr_patches ~branch_of views =
  let merges = detect_merges views ~merged_pr_patches in
  let newly_merged =
    List.filter_map merges ~f:(function
      | Mark_merged pid -> Some pid
      | Enqueue_rebase _ | Start_operation _ -> None)
  in
  let has_merged pid =
    List.exists views ~f:(fun v -> Patch_id.equal v.id pid && v.merged)
    || List.exists merged_pr_patches ~f:(Patch_id.equal pid)
  in
  let event_rebases = detect_rebases graph views ~newly_merged in
  let stale_rebases =
    detect_stale_bases graph views ~has_merged ~branch_of ~main
  in
  let drift_rebases = detect_notified_base_drift views in
  let sibling_rebases = detect_sibling_stale_bases graph views ~has_merged in
  (* Deduplicate across the four rebase detectors. Priority is arbitrary —
     they all emit the same [Enqueue_rebase] with the same patch_id — but we
     must not emit duplicates, since the orchestrator's [enqueue] is
     idempotent but tests assert on action counts. *)
  let rebases =
    let pid_of = function
      | Enqueue_rebase pid -> Some pid
      | Mark_merged _ | Start_operation _ -> None
    in
    let add_unseen seen actions =
      List.fold actions ~init:(seen, []) ~f:(fun (seen, acc) a ->
          match pid_of a with
          | Some pid when not (Set.mem seen pid) -> (Set.add seen pid, a :: acc)
          | Some _ -> (seen, acc)
          | None -> (seen, a :: acc))
    in
    let seen, kept1 = add_unseen (Set.empty (module Patch_id)) event_rebases in
    let seen, kept2 = add_unseen seen stale_rebases in
    let seen, kept3 = add_unseen seen drift_rebases in
    let _seen, kept4 = add_unseen seen sibling_rebases in
    List.rev kept1 @ List.rev kept2 @ List.rev kept3 @ List.rev kept4
  in
  let rebase_set =
    List.filter_map rebases ~f:(function
      | Enqueue_rebase pid -> Some pid
      | Mark_merged _ | Start_operation _ -> None)
    |> Set.of_list (module Patch_id)
  in
  let effective_views =
    List.map views ~f:(fun v ->
        {
          v with
          merged = has_merged v.id;
          queue =
            (if Set.mem rebase_set v.id then Operation_kind.Rebase :: v.queue
             else v.queue);
        })
  in
  let operations =
    plan_operations effective_views ~has_merged ~branch_of ~graph ~main
  in
  merges @ rebases @ operations

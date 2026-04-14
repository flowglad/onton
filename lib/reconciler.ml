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
              | Operation_kind.Pr_body ->
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
  (* Deduplicate across the three rebase detectors. Priority is arbitrary —
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
    let _seen, kept3 = add_unseen seen drift_rebases in
    List.rev kept1 @ List.rev kept2 @ List.rev kept3
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

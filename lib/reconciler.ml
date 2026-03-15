open Base
open Types

type patch_view = {
  id : Patch_id.t;
  has_pr : bool;
  merged : bool;
  busy : bool;
  needs_intervention : bool;
  queue : Operation_kind.t list;
  base_branch : Branch.t;
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

(** Operation_kind priority: lower number = higher priority. rebase=0, human=1,
    merge_conflict=2, ci=3, review_comments=4. *)
let operation_priority = function
  | Operation_kind.Rebase -> 0
  | Operation_kind.Human -> 1
  | Operation_kind.Merge_conflict -> 2
  | Operation_kind.Ci -> 3
  | Operation_kind.Review_comments -> 4

let highest_priority_op queue =
  List.min_elt queue ~compare:(fun a b ->
      Int.compare (operation_priority a) (operation_priority b))

let merge_target graph patch_id ~has_merged ~branch_of ~main =
  Graph.initial_base graph patch_id ~has_merged ~branch_of ~main

let detect_merges views ~merged_pr_patches =
  let merged_set = Set.of_list (module Patch_id) merged_pr_patches in
  List.filter_map views ~f:(fun v ->
      if Set.mem merged_set v.id && not v.merged then Some (Mark_merged v.id)
      else None)

let detect_rebases graph views ~newly_merged =
  let view_by_id =
    Map.of_alist_exn (module Patch_id) (List.map views ~f:(fun v -> (v.id, v)))
  in
  List.concat_map newly_merged ~f:(fun merged_id ->
      Graph.dependents graph merged_id
      |> List.filter_map ~f:(fun dep_id ->
          match Map.find view_by_id dep_id with
          | Some v
            when v.has_pr && (not v.merged)
                 && not
                      (List.mem v.queue Operation_kind.Rebase
                         ~equal:Operation_kind.equal) ->
              Some (Enqueue_rebase dep_id)
          | _ -> None))

let plan_operations views ~has_merged ~branch_of ~graph ~main =
  List.filter_map views ~f:(fun v ->
      if v.has_pr && (not v.merged) && (not v.busy) && not v.needs_intervention
      then
        match highest_priority_op v.queue with
        | Some kind ->
            let new_base =
              match kind with
              | Operation_kind.Rebase ->
                  Some (merge_target graph v.id ~has_merged ~branch_of ~main)
              | Operation_kind.Human | Operation_kind.Merge_conflict
              | Operation_kind.Ci | Operation_kind.Review_comments ->
                  None
            in
            Some (Start_operation { patch_id = v.id; kind; new_base })
        | None -> None
      else None)

let reconcile ~graph ~main ~merged_pr_patches ~branch_of views =
  let merges = detect_merges views ~merged_pr_patches in
  (* After merges are applied, compute the new merged set for rebase detection *)
  let has_merged pid =
    List.exists views ~f:(fun v -> Patch_id.equal v.id pid && v.merged)
    || List.exists merged_pr_patches ~f:(Patch_id.equal pid)
  in
  let rebases = detect_rebases graph views ~newly_merged:merged_pr_patches in
  let operations = plan_operations views ~has_merged ~branch_of ~graph ~main in
  merges @ rebases @ operations

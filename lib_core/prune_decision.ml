(* @archlint.module core
   @archlint.domain prune-decision *)

open Base
open Types

type project_status = All_terminal | Not_terminal | No_patches
[@@deriving show, eq, sexp_of, compare]

type recovery_ref = { name : string; revision : Branch_reconcile.Commit.t }
[@@deriving eq, compare, sexp_of]

type reclamation =
  | Retain of Branch_reconcile.Commit.t list
  | Reclaim of recovery_ref list
[@@deriving eq, compare, sexp_of]

let parse_recovery_refs text =
  List.fold_result (String.split_lines text) ~init:[] ~f:(fun refs line ->
      match String.split line ~on:'\000' with
      | [ name; value; ""; "commit" ]
        when String.is_prefix name ~prefix:"refs/onton/reconcile/"
             && not (String.exists name ~f:Char.is_whitespace) -> (
          match Branch_reconcile.Commit.make value with
          | Some revision
            when not (List.exists refs ~f:(fun r -> String.equal r.name name))
            ->
              Ok ({ name; revision } :: refs)
          | Some _ | None -> Error "invalid_recovery_ref_inventory")
      | _ -> Error "invalid_recovery_ref_inventory")
  |> Result.map ~f:(List.sort ~compare:compare_recovery_ref)

let repository_within_project ~project_dir ~git_dir =
  String.equal project_dir git_dir
  || String.is_prefix git_dir
       ~prefix:(String.rstrip project_dir ~drop:(Char.equal '/') ^ "/")

let recovery_dependencies agents =
  Map.data agents
  |> List.concat_map ~f:(fun (agent : Patch_agent.t) ->
      Branch_reconcile.required_revisions agent.branch_reconcile)
  |> List.dedup_and_sort ~compare:Branch_reconcile.Commit.compare

let plan_reclamation ~project ~protected_projects ~inventory ~required
    ~reachability =
  let owned r =
    String.is_prefix r.name
      ~prefix:(Branch_reconcile.recovery_project_prefix ~project)
  in
  let candidates = List.filter inventory ~f:owned in
  let independent r =
    (not (owned r))
    && List.exists protected_projects ~f:(fun project ->
        String.is_prefix r.name
          ~prefix:(Branch_reconcile.recovery_project_prefix ~project))
  in
  let required =
    List.dedup_and_sort required ~compare:Branch_reconcile.Commit.compare
  in
  List.fold_result required ~init:[] ~f:(fun retained revision ->
      match
        List.filter reachability ~f:(fun (r, _) ->
            Branch_reconcile.Commit.equal r revision)
      with
      | [ (_, refs) ]
        when List.for_all refs ~f:(fun r ->
                 List.mem inventory r ~equal:equal_recovery_ref) ->
          if List.exists refs ~f:owned && not (List.exists refs ~f:independent)
          then Ok (revision :: retained)
          else Ok retained
      | _ -> Error "recovery_ref_reachability_unproven")
  |> Result.map ~f:(function
    | [] -> Reclaim candidates
    | retained -> Retain (List.rev retained))

let classify_snapshot ~(patches : Patch.t list)
    ~(agents : Patch_agent.t Map.M(Patch_id).t) ~closed_patch_ids =
  if List.is_empty patches then No_patches
  else
    let closed_patch_ids = Set.of_list (module Patch_id) closed_patch_ids in
    let all_terminal =
      List.for_all patches ~f:(fun (p : Patch.t) ->
          match Map.find agents p.Patch.id with
          | Some agent ->
              (agent.Patch_agent.merged || Set.mem closed_patch_ids p.Patch.id)
              && not (Branch_reconcile.is_unsettled agent.branch_reconcile)
          | None -> false)
    in
    if all_terminal then All_terminal else Not_terminal

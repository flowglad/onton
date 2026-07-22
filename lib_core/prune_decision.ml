(* @archlint.module core
   @archlint.domain prune-decision *)

open Base
open Types

type project_status = All_terminal | Not_terminal | No_patches
[@@deriving show, eq, sexp_of, compare]

let classify_snapshot ~(patches : Patch.t list)
    ~(agents : Patch_agent.t Map.M(Patch_id).t) ~closed_patch_ids =
  if List.is_empty patches then No_patches
  else
    let closed_patch_ids = Set.of_list (module Patch_id) closed_patch_ids in
    let all_terminal =
      List.for_all patches ~f:(fun (p : Patch.t) ->
          match Map.find agents p.Patch.id with
          | Some agent ->
              agent.Patch_agent.merged || Set.mem closed_patch_ids p.Patch.id
          | None -> false)
    in
    if all_terminal then All_terminal else Not_terminal

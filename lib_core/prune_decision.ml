(* @archlint.module core
   @archlint.domain prune-decision *)

open Base
open Types

type project_status = All_merged | Not_merged | No_patches
[@@deriving show, eq, sexp_of, compare]

let classify_snapshot ~(patches : Patch.t list)
    ~(agents : Patch_agent.t Map.M(Patch_id).t) =
  if List.is_empty patches then No_patches
  else
    let all_merged =
      List.for_all patches ~f:(fun (p : Patch.t) ->
          match Map.find agents p.Patch.id with
          | Some agent -> agent.Patch_agent.merged
          | None -> false)
    in
    if all_merged then All_merged else Not_merged

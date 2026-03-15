open Base
open Types

type t = {
  deps_map : Patch_id.t list Map.M(Patch_id).t;
  dependents_map : Patch_id.t list Map.M(Patch_id).t;
  all_ids : Patch_id.t list;
}
[@@deriving sexp_of]

let of_patches (patches : Patch.t list) =
  let deps_map =
    List.fold patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc { Patch.id; dependencies; _ } ->
        Map.set acc ~key:id ~data:dependencies)
  in
  let dependents_map =
    List.fold patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc { Patch.id; dependencies; _ } ->
        List.fold dependencies ~init:acc ~f:(fun acc dep_id ->
            Map.update acc dep_id ~f:(fun existing ->
                id :: Option.value existing ~default:[])))
  in
  let all_ids = List.map patches ~f:(fun { Patch.id; _ } -> id) in
  { deps_map; dependents_map; all_ids }

let deps t patch_id = Map.find t.deps_map patch_id |> Option.value ~default:[]

let depends_on t patch_id ~dep =
  List.mem (deps t patch_id) dep ~equal:Patch_id.equal

let open_pr_deps t patch_id ~has_merged =
  deps t patch_id |> List.filter ~f:(fun d -> not (has_merged d))

let deps_satisfied t patch_id ~has_merged ~has_pr =
  let open_deps = open_pr_deps t patch_id ~has_merged in
  List.length open_deps <= 1
  && List.for_all (deps t patch_id) ~f:(fun d -> has_merged d || has_pr d)

let sole_open_dep t patch_id ~has_merged =
  match open_pr_deps t patch_id ~has_merged with
  | [ d ] -> d
  | _ -> invalid_arg "sole_open_dep: expected exactly 1 open dep"

let initial_base t patch_id ~has_merged ~branch_of ~main =
  match open_pr_deps t patch_id ~has_merged with
  | [] -> main
  | [ d ] -> branch_of d
  | _ -> invalid_arg "initial_base: more than 1 open dep"

let dependents t patch_id =
  Map.find t.dependents_map patch_id |> Option.value ~default:[]

let all_patch_ids t = t.all_ids

open Base
open Types

type t = {
  deps_map : Patch_id.t list Map.M(Patch_id).t;
  dependents_map : Patch_id.t list Map.M(Patch_id).t;
  all_ids : Patch_id.t list;
}
[@@deriving sexp_of]

let dedup_deps deps = List.dedup_and_sort deps ~compare:Patch_id.compare

let of_patches (patches : Patch.t list) =
  let deps_map =
    List.fold patches
      ~init:(Map.empty (module Patch_id))
      ~f:(fun acc { Patch.id; dependencies; _ } ->
        let deps = dedup_deps dependencies in
        match Map.add acc ~key:id ~data:deps with
        | `Ok m -> m
        | `Duplicate -> invalid_arg "of_patches: duplicate patch id")
  in
  let dependents_map =
    Map.fold deps_map
      ~init:(Map.empty (module Patch_id))
      ~f:(fun ~key:patch_id ~data:deps acc ->
        List.fold deps ~init:acc ~f:(fun acc dep_id ->
            Map.update acc dep_id ~f:(fun existing ->
                patch_id :: Option.value existing ~default:[])))
  in
  let all_ids = Map.keys deps_map in
  { deps_map; dependents_map; all_ids }

let deps t patch_id = Map.find t.deps_map patch_id |> Option.value ~default:[]

let depends_on t patch_id ~dep =
  List.mem (deps t patch_id) dep ~equal:Patch_id.equal

(** All patches reachable by walking [deps] transitively from [patch_id],
    excluding [patch_id] itself. Missing IDs in [deps_map] act as leaves.
    Dependency cycles that point back to [patch_id] do not re-add it. *)
let transitive_ancestors t patch_id =
  let rec visit seen pid =
    List.fold (deps t pid) ~init:seen ~f:(fun seen dep ->
        if Set.mem seen dep then seen else visit (Set.add seen dep) dep)
  in
  visit (Set.singleton (module Patch_id) patch_id) patch_id
  |> Fn.flip Set.remove patch_id
  |> Set.to_list

let open_pr_deps t patch_id ~has_merged =
  deps t patch_id |> List.filter ~f:(fun d -> not (has_merged d))

let deps_satisfied t patch_id ~has_merged ~has_pr =
  let all_deps = deps t patch_id in
  let open_deps = List.filter all_deps ~f:(fun d -> not (has_merged d)) in
  List.length open_deps <= 1
  && List.for_all all_deps ~f:(fun d -> has_merged d || has_pr d)

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

let add_patch t patch_id =
  if Map.mem t.deps_map patch_id then t
  else
    {
      deps_map = Map.set t.deps_map ~key:patch_id ~data:[];
      dependents_map = t.dependents_map;
      all_ids = t.all_ids @ [ patch_id ];
    }

let remove_patch t patch_id =
  {
    deps_map = Map.remove t.deps_map patch_id;
    dependents_map = Map.remove t.dependents_map patch_id;
    all_ids =
      List.filter t.all_ids ~f:(fun id -> not (Patch_id.equal id patch_id));
  }

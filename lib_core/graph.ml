(* @archlint.module core
   @archlint.domain graph *)

open Base
open Types

type requirement = Stackable | Merged [@@deriving sexp_of]

type dependency = { patch_id : Patch_id.t; requirement : requirement }
[@@deriving sexp_of]

let dependency_id d = d.patch_id

type t = {
  deps_map : dependency list Map.M(Patch_id).t;
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
        let deps =
          dedup_deps dependencies
          |> List.map ~f:(fun patch_id -> { patch_id; requirement = Stackable })
        in
        match Map.add acc ~key:id ~data:deps with
        | `Ok m -> m
        | `Duplicate -> invalid_arg "of_patches: duplicate patch id")
  in
  let dependents_map =
    Map.fold deps_map
      ~init:(Map.empty (module Patch_id))
      ~f:(fun ~key:patch_id ~data:deps acc ->
        List.fold deps ~init:acc ~f:(fun acc dependency ->
            let dep_id = dependency_id dependency in
            Map.update acc dep_id ~f:(fun existing ->
                patch_id :: Option.value existing ~default:[])))
  in
  let all_ids = Map.keys deps_map in
  { deps_map; dependents_map; all_ids }

let dependency_edges t patch_id =
  Map.find t.deps_map patch_id |> Option.value ~default:[]

let deps t patch_id = dependency_edges t patch_id |> List.map ~f:dependency_id

let merge_deps_satisfied t patch_id ~has_merged =
  List.for_all (dependency_edges t patch_id) ~f:(fun d ->
      match d.requirement with
      | Stackable -> true
      | Merged -> has_merged d.patch_id)

let merge_required_deps t patch_id =
  List.filter_map (dependency_edges t patch_id) ~f:(fun d ->
      match d.requirement with Stackable -> None | Merged -> Some d.patch_id)

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
  merge_deps_satisfied t patch_id ~has_merged
  && List.length open_deps <= 1
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

let add_patch_with_deps t patch_id ~deps =
  if Map.mem t.deps_map patch_id then t
  else
    let deps =
      List.filter deps ~f:(Map.mem t.deps_map)
      |> dedup_deps
      |> List.filter ~f:(fun d -> not (Patch_id.equal d patch_id))
    in
    let dependents_map =
      List.fold deps ~init:t.dependents_map ~f:(fun acc dep_id ->
          Map.update acc dep_id ~f:(fun existing ->
              patch_id :: Option.value existing ~default:[]))
    in
    {
      deps_map =
        Map.set t.deps_map ~key:patch_id
          ~data:
            (List.map deps ~f:(fun patch_id ->
                 { patch_id; requirement = Stackable }));
      dependents_map;
      all_ids = t.all_ids @ [ patch_id ];
    }

let add_patch t patch_id = add_patch_with_deps t patch_id ~deps:[]

let add_dependency ?(requirement = Stackable) t patch_id ~dep =
  if
    Patch_id.equal patch_id dep
    || (not (Map.mem t.deps_map patch_id))
    || not (Map.mem t.deps_map dep)
  then t
  else
    let existing = dependency_edges t patch_id in
    if List.exists existing ~f:(fun d -> Patch_id.equal d.patch_id dep) then
      let edges =
        List.map existing ~f:(fun d ->
            if Patch_id.equal d.patch_id dep && Poly.equal requirement Merged
            then { d with requirement = Merged }
            else d)
      in
      { t with deps_map = Map.set t.deps_map ~key:patch_id ~data:edges }
    else
      {
        t with
        deps_map =
          Map.set t.deps_map ~key:patch_id
            ~data:({ patch_id = dep; requirement } :: existing);
        dependents_map =
          Map.update t.dependents_map dep ~f:(fun existing ->
              patch_id :: Option.value existing ~default:[]);
      }

let of_gameplan gameplan =
  let graph = of_patches gameplan.Gameplan.patches in
  match gameplan.publication with
  | None -> graph
  | Some _ ->
      List.fold gameplan.patches ~init:graph ~f:(fun graph p ->
          if Gameplan.is_publication_patch gameplan p.Patch.id then graph
          else
            add_dependency ~requirement:Merged graph p.id
              ~dep:Gameplan.publication_patch_id)

let remove_patch t patch_id =
  let deps = deps t patch_id in
  let dependents =
    Map.find t.dependents_map patch_id |> Option.value ~default:[]
  in
  let dependents_map =
    List.fold deps ~init:t.dependents_map ~f:(fun acc dep_id ->
        Map.change acc dep_id ~f:(function
          | None -> None
          | Some lst ->
              let lst' =
                List.filter lst ~f:(fun d -> not (Patch_id.equal d patch_id))
              in
              if List.is_empty lst' then None else Some lst'))
  in
  let deps_map =
    List.fold dependents ~init:t.deps_map ~f:(fun acc dependent_id ->
        Map.change acc dependent_id ~f:(function
          | None -> None
          | Some lst ->
              let lst' =
                List.filter lst ~f:(fun d ->
                    not (Patch_id.equal d.patch_id patch_id))
              in
              Some lst'))
  in
  {
    deps_map = Map.remove deps_map patch_id;
    dependents_map = Map.remove dependents_map patch_id;
    all_ids =
      List.filter t.all_ids ~f:(fun id -> not (Patch_id.equal id patch_id));
  }

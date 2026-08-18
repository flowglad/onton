(* @archlint.module core
   @archlint.domain orchestrator *)

open Base
open Types

type missing_patch = {
  patch_id : Patch_id.t;
  branch : Branch.t;
  dependencies : Patch_id.t list;
}
[@@deriving show, eq]

type repair =
  | Preserved_snapshot_patch of Patch_id.t
  | Reconstructed_missing_patch of Patch_id.t
[@@deriving show, eq]

type result = { gameplan : Gameplan.t; repairs : repair list }
[@@deriving show, eq]

let runtime_patch_number patch_id =
  let raw = Patch_id.to_string patch_id in
  match String.chop_prefix raw ~prefix:"add" with
  | None -> None
  | Some suffix -> (
      match Int.of_string_opt suffix with
      | Some n when n > 0 && String.equal suffix (Int.to_string n) -> Some n
      | Some _ | None -> None)

let is_runtime_patch_id patch_id =
  Option.is_some (runtime_patch_number patch_id)

let ids_of_patches patches =
  List.map patches ~f:(fun patch -> patch.Patch.id)
  |> Set.of_list (module Patch_id)

let dedup_known_dependencies ~known dependencies =
  List.fold dependencies
    ~init:([], Set.empty (module Patch_id))
    ~f:(fun (acc, seen) dependency ->
      if Set.mem known dependency && not (Set.mem seen dependency) then
        (dependency :: acc, Set.add seen dependency)
      else (acc, seen))
  |> fst |> List.rev

let parse_added_event ~patch_id message =
  let prefix =
    Printf.sprintf "Added patch %s (" (Patch_id.to_string patch_id)
  in
  match String.chop_prefix message ~prefix with
  | None -> None
  | Some rest -> (
      match String.substr_index rest ~pattern:") — " with
      | None -> None
      | Some separator_index ->
          let deps_text = String.prefix rest separator_index in
          let title =
            String.drop_prefix rest (separator_index + String.length ") — ")
            |> String.strip
          in
          let dependencies =
            if String.equal deps_text "no dependencies" then Some []
            else
              String.chop_prefix deps_text ~prefix:"depends on "
              |> Option.map ~f:(fun raw ->
                  String.split raw ~on:','
                  |> List.map ~f:(fun dependency ->
                      Patch_id.of_string (String.strip dependency)))
          in
          Option.map dependencies ~f:(fun dependencies -> (title, dependencies))
      )

let activity_hint activity_log patch_id =
  Activity_log.recent_events activity_log ~limit:Int.max_value
  |> List.find_map ~f:(fun event ->
      match event.Activity_log.Event.patch_id with
      | Some event_patch_id when Patch_id.equal event_patch_id patch_id ->
          parse_added_event ~patch_id event.message
      | Some _ | None -> None)

let reconstructed_patch ~gameplan ~known activity_log
    ({ patch_id; branch; dependencies } : missing_patch) =
  if
    (not (is_runtime_patch_id patch_id))
    || not (Branch.equal branch (Gameplan.branch_of_id gameplan patch_id))
  then None
  else
    let title, dependencies =
      match activity_hint activity_log patch_id with
      | Some (title, logged_dependencies) when not (String.is_empty title) ->
          (title, logged_dependencies)
      | Some (_, logged_dependencies) ->
          ( "Recovered runtime patch " ^ Patch_id.to_string patch_id,
            logged_dependencies )
      | None ->
          ( "Recovered runtime patch " ^ Patch_id.to_string patch_id,
            dependencies )
    in
    let dependencies = dedup_known_dependencies ~known dependencies in
    Some
      ({
         Patch.id = patch_id;
         title;
         description = title;
         branch;
         dependencies;
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
        : Patch.t)

let reconcile ~loaded ~persisted ~missing_patches ~activity_log =
  let loaded_ids = ids_of_patches loaded.Gameplan.patches in
  let _, preserved =
    List.fold persisted.Gameplan.patches ~init:(loaded_ids, [])
      ~f:(fun (seen, acc) patch ->
        if
          is_runtime_patch_id patch.Patch.id
          && not (Set.mem seen patch.Patch.id)
        then (Set.add seen patch.id, patch :: acc)
        else (seen, acc))
  in
  let preserved = List.rev preserved in
  let gameplan =
    { loaded with Gameplan.patches = loaded.patches @ preserved }
  in
  let known = ids_of_patches gameplan.Gameplan.patches in
  let reconstructable, reconstructable_ids =
    List.fold missing_patches ~init:([], known) ~f:(fun (acc, seen) missing ->
        if
          Set.mem seen missing.patch_id
          || (not (is_runtime_patch_id missing.patch_id))
          || not
               (Branch.equal missing.branch
                  (Gameplan.branch_of_id gameplan missing.patch_id))
        then (acc, seen)
        else (missing :: acc, Set.add seen missing.patch_id))
  in
  let reconstructed =
    List.rev reconstructable
    |> List.filter_map
         ~f:
           (reconstructed_patch ~gameplan ~known:reconstructable_ids
              activity_log)
  in
  let gameplan =
    { gameplan with Gameplan.patches = gameplan.patches @ reconstructed }
  in
  let repairs =
    List.map preserved ~f:(fun patch -> Preserved_snapshot_patch patch.Patch.id)
    @ List.map reconstructed ~f:(fun patch ->
        Reconstructed_missing_patch patch.Patch.id)
  in
  { gameplan; repairs }

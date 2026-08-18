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

let canonical_runtime_branch gameplan patch_id =
  Option.map (runtime_patch_number patch_id) ~f:(fun _ ->
      Gameplan.branch_of_id gameplan patch_id)

let ids_of_patches patches =
  List.map patches ~f:(fun patch -> patch.Patch.id)
  |> Set.of_list (module Patch_id)

let dedup_known_dependencies ~known ~patch_id dependencies =
  List.fold dependencies
    ~init:([], Set.empty (module Patch_id))
    ~f:(fun (acc, seen) dependency ->
      if
        Set.mem known dependency
        && (not (Patch_id.equal dependency patch_id))
        && not (Set.mem seen dependency)
      then (dependency :: acc, Set.add seen dependency)
      else (acc, seen))
  |> fst |> List.rev

let dependency_map patches =
  List.fold patches
    ~init:(Map.empty (module Patch_id))
    ~f:(fun acc patch ->
      match Map.add acc ~key:patch.Patch.id ~data:patch.dependencies with
      | `Ok map -> map
      | `Duplicate -> acc)

let has_dependency_path dependencies ~source ~target =
  let rec loop seen = function
    | [] -> false
    | patch_id :: rest ->
        if Patch_id.equal patch_id target then true
        else if Set.mem seen patch_id then loop seen rest
        else
          let seen = Set.add seen patch_id in
          let next =
            Option.value (Map.find dependencies patch_id) ~default:[]
          in
          loop seen (next @ rest)
  in
  loop (Set.empty (module Patch_id)) [ source ]

let sanitize_dependencies ~known ~dependencies patch =
  let dependencies =
    dedup_known_dependencies ~known ~patch_id:patch.Patch.id patch.dependencies
    |> List.filter ~f:(fun dependency ->
        not
          (has_dependency_path dependencies ~source:dependency
             ~target:patch.Patch.id))
  in
  { patch with Patch.dependencies }

let added_patch_event_message (patch : Patch.t) =
  let dependencies =
    match patch.dependencies with
    | [] -> "no dependencies"
    | dependencies ->
        "depends on "
        ^ (List.map dependencies ~f:Patch_id.to_string
          |> String.concat ~sep:", ")
  in
  Printf.sprintf "Added patch %s (%s) — %s"
    (Patch_id.to_string patch.id)
    dependencies patch.title

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
  match canonical_runtime_branch gameplan patch_id with
  | None -> None
  | Some canonical_branch when not (Branch.equal branch canonical_branch) ->
      None
  | Some _ ->
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
      let dependencies =
        dedup_known_dependencies ~known ~patch_id dependencies
      in
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
  let loaded_branches =
    List.map loaded.Gameplan.patches ~f:(fun patch ->
        Branch.to_string patch.Patch.branch)
    |> Set.of_list (module String)
  in
  let _, _, preserved =
    List.fold persisted.Gameplan.patches ~init:(loaded_ids, loaded_branches, [])
      ~f:(fun (seen_ids, seen_branches, acc) patch ->
        if
          is_runtime_patch_id patch.Patch.id
          && (not (Set.mem seen_ids patch.Patch.id))
          && not (Set.mem seen_branches (Branch.to_string patch.Patch.branch))
        then
          ( Set.add seen_ids patch.id,
            Set.add seen_branches (Branch.to_string patch.branch),
            patch :: acc )
        else (seen_ids, seen_branches, acc))
  in
  let preserved = List.rev preserved in
  let gameplan =
    { loaded with Gameplan.patches = loaded.patches @ preserved }
  in
  let known = ids_of_patches gameplan.Gameplan.patches in
  let known_branches =
    List.map gameplan.Gameplan.patches ~f:(fun patch ->
        Branch.to_string patch.Patch.branch)
    |> Set.of_list (module String)
  in
  let reconstructable, reconstructable_ids, _ =
    List.fold missing_patches ~init:([], known, known_branches)
      ~f:(fun (acc, seen_ids, seen_branches) missing ->
        match canonical_runtime_branch gameplan missing.patch_id with
        | Some canonical_branch
          when (not (Set.mem seen_ids missing.patch_id))
               && (not
                     (Set.mem seen_branches (Branch.to_string missing.branch)))
               && Branch.equal missing.branch canonical_branch ->
            ( missing :: acc,
              Set.add seen_ids missing.patch_id,
              Set.add seen_branches (Branch.to_string missing.branch) )
        | Some _ | None -> (acc, seen_ids, seen_branches))
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
  let known = ids_of_patches gameplan.Gameplan.patches in
  let dependencies = dependency_map gameplan.patches in
  let gameplan =
    {
      gameplan with
      Gameplan.patches =
        List.map gameplan.patches
          ~f:(sanitize_dependencies ~known ~dependencies);
    }
  in
  let repairs =
    List.map preserved ~f:(fun patch -> Preserved_snapshot_patch patch.Patch.id)
    @ List.map reconstructed ~f:(fun patch ->
        Reconstructed_missing_patch patch.Patch.id)
  in
  { gameplan; repairs }

open Base

type t = {
  project_name : string;
  problem_statement : string;
  solution_summary : string;
  patches : Types.Patch.t list;
  dependency_graph : Types.Patch_id.t list Map.M(Types.Patch_id).t;
}

let extract_section ~header lines =
  let header_prefix = "## " ^ header in
  let rec find_start = function
    | [] -> None
    | line :: rest ->
        if String.is_prefix line ~prefix:header_prefix then collect_body rest []
        else find_start rest
  and collect_body lines acc =
    match lines with
    | [] -> Some (String.concat ~sep:"\n" (List.rev acc) |> String.strip)
    | line :: rest ->
        if String.is_prefix line ~prefix:"## " then
          Some (String.concat ~sep:"\n" (List.rev acc) |> String.strip)
        else collect_body rest (line :: acc)
  in
  find_start lines

let parse_project_name lines =
  let rec find = function
    | [] ->
        Error
          "No project name found (expected '# Gameplan: {name}' or '# {name}')"
    | line :: rest -> (
        match String.chop_prefix line ~prefix:"# Gameplan: " with
        | Some name -> Ok (String.strip name)
        | None -> (
            match String.chop_prefix line ~prefix:"# " with
            | Some name when not (String.is_prefix name ~prefix:"#") ->
                Ok (String.strip name)
            | _ -> find rest))
  in
  find lines

let slugify name =
  String.lowercase name
  |> String.map ~f:(fun c ->
      if Char.is_alphanum c || Char.equal c '-' || Char.equal c '_' then c
      else '-')
  |> String.split ~on:'-'
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> String.concat ~sep:"-"

let is_valid_patch_id s =
  (not (String.is_empty s))
  && String.for_all s ~f:(fun c -> Char.is_alphanum c)
  && not (String.is_substring s ~substring:"..")

let parse_dep_graph_line line =
  (* Format: "- Patch ID [CLASS] -> [deps]" *)
  let line = String.strip line in
  match String.chop_prefix line ~prefix:"- Patch " with
  | None -> None
  | Some rest -> (
      let parts = String.lsplit2 rest ~on:' ' in
      match parts with
      | None -> None
      | Some (patch_id, after_id) -> (
          (* Find the -> *)
          match String.substr_index after_id ~pattern:"->" with
          | None -> None
          | Some arrow_idx ->
              let deps_str =
                String.drop_prefix after_id (arrow_idx + 2) |> String.strip
              in
              let deps_str =
                deps_str
                |> String.chop_prefix_if_exists ~prefix:"["
                |> String.chop_suffix_if_exists ~suffix:"]"
                |> String.strip
              in
              let deps =
                if String.is_empty deps_str then []
                else
                  String.split deps_str ~on:','
                  |> List.map ~f:(fun s ->
                      Types.Patch_id.of_string (String.strip s))
              in
              Some (Types.Patch_id.of_string patch_id, deps)))

let parse_dep_graph lines =
  let _in_graph, result =
    List.fold lines
      ~init:(false, Map.empty (module Types.Patch_id))
      ~f:(fun (in_graph, acc) line ->
        let stripped = String.strip line in
        if String.is_prefix stripped ~prefix:"## Dependency Graph" then
          (true, acc)
        else if in_graph && String.is_prefix stripped ~prefix:"## " then
          (false, acc)
        else if in_graph then
          match parse_dep_graph_line line with
          | Some (id, deps) -> (true, Map.set acc ~key:id ~data:deps)
          | None -> (in_graph, acc)
        else (in_graph, acc))
  in
  result

let parse_patch_header line =
  (* Format: "### Patch N [CLASS]: Title" *)
  let line = String.strip line in
  match String.chop_prefix line ~prefix:"### Patch " with
  | None -> None
  | Some rest -> (
      match String.lsplit2 rest ~on:' ' with
      | None -> None
      | Some (patch_id, after_id) -> (
          (* Skip [CLASS]: and get title *)
          match String.substr_index after_id ~pattern:"]: " with
          | None -> None
          | Some idx ->
              let title =
                String.drop_prefix after_id (idx + 3) |> String.strip
              in
              Some (patch_id, title)))

let parse_patches lines dep_graph ~project_name =
  let slug = slugify project_name in
  List.filter_map lines ~f:(fun line ->
      match parse_patch_header line with
      | None -> None
      | Some (patch_id_str, title) ->
          let id = Types.Patch_id.of_string patch_id_str in
          let dependencies =
            Map.find dep_graph id |> Option.value ~default:[]
          in
          let branch =
            Types.Branch.of_string (slug ^ "/patch-" ^ patch_id_str)
          in
          Some { Types.Patch.id; title; branch; dependencies })

let detect_cycle dep_graph =
  (* DFS cycle detection *)
  let visited = Hashtbl.create (module Types.Patch_id) in
  let in_stack = Hashtbl.create (module Types.Patch_id) in
  let found_cycle = ref None in
  let rec visit node =
    if Option.is_some !found_cycle then ()
    else if Hashtbl.mem in_stack node then
      found_cycle :=
        Some
          (Printf.sprintf "Cycle detected involving patch %s"
             (Types.Patch_id.to_string node))
    else if not (Hashtbl.mem visited node) then (
      Hashtbl.set visited ~key:node ~data:();
      Hashtbl.set in_stack ~key:node ~data:();
      let deps = Map.find dep_graph node |> Option.value ~default:[] in
      List.iter deps ~f:visit;
      Hashtbl.remove in_stack node)
  in
  Map.iter_keys dep_graph ~f:visit;
  !found_cycle

let validate ~patches ~dep_graph =
  (* Check for invalid patch IDs *)
  let invalid_ids =
    List.filter_map patches ~f:(fun p ->
        let id_str = Types.Patch_id.to_string p.Types.Patch.id in
        if is_valid_patch_id id_str then None
        else Some (Printf.sprintf "Invalid patch ID: %S" id_str))
  in
  if not (List.is_empty invalid_ids) then Error (List.hd_exn invalid_ids)
  else
    (* Check for duplicate patch IDs *)
    let all_ids = List.map patches ~f:(fun p -> p.Types.Patch.id) in
    let unique_ids = Set.of_list (module Types.Patch_id) all_ids in
    if Set.length unique_ids <> List.length all_ids then
      let dup =
        List.find_map_exn all_ids ~f:(fun id ->
            let count = List.count all_ids ~f:(Types.Patch_id.equal id) in
            if count > 1 then
              Some
                (Printf.sprintf "Duplicate patch ID: %s"
                   (Types.Patch_id.to_string id))
            else None)
      in
      Error dup
    else
      let patch_ids = unique_ids in
      (* Check all dep graph keys (source patches) exist *)
      let orphan_sources =
        Map.fold dep_graph ~init:[] ~f:(fun ~key:from ~data:_ acc ->
            if Set.mem patch_ids from then acc
            else
              Printf.sprintf "Dependency graph references nonexistent patch %s"
                (Types.Patch_id.to_string from)
              :: acc)
      in
      if not (List.is_empty orphan_sources) then
        Error (List.hd_exn orphan_sources)
      else
        (* Check all dep targets exist *)
        let missing =
          Map.fold dep_graph ~init:[] ~f:(fun ~key:from ~data:deps acc ->
              List.fold deps ~init:acc ~f:(fun acc dep ->
                  if Set.mem patch_ids dep then acc
                  else
                    Printf.sprintf "Patch %s depends on nonexistent patch %s"
                      (Types.Patch_id.to_string from)
                      (Types.Patch_id.to_string dep)
                    :: acc))
        in
        if not (List.is_empty missing) then Error (List.hd_exn missing)
        else
          (* Check no self-deps *)
          let self_deps =
            Map.fold dep_graph ~init:[] ~f:(fun ~key:from ~data:deps acc ->
                if List.mem deps from ~equal:Types.Patch_id.equal then
                  Printf.sprintf "Patch %s depends on itself"
                    (Types.Patch_id.to_string from)
                  :: acc
                else acc)
          in
          if not (List.is_empty self_deps) then Error (List.hd_exn self_deps)
          else
            match detect_cycle dep_graph with
            | Some msg -> Error msg
            | None -> Ok ()

let parse_string input =
  let lines = String.split_lines input in
  match parse_project_name lines with
  | Error e -> Error e
  | Ok project_name -> (
      let problem_statement =
        extract_section ~header:"Problem Statement" lines
        |> Option.value ~default:""
      in
      let solution_summary =
        extract_section ~header:"Solution Summary" lines
        |> Option.value ~default:""
      in
      let dep_graph = parse_dep_graph lines in
      let patches = parse_patches lines dep_graph ~project_name in
      match validate ~patches ~dep_graph with
      | Error e -> Error e
      | Ok () ->
          Ok
            {
              project_name;
              problem_statement;
              solution_summary;
              patches;
              dependency_graph = dep_graph;
            })

let parse_file path =
  try
    let ic = Stdlib.In_channel.open_text path in
    let contents =
      Exn.protect
        ~finally:(fun () -> Stdlib.In_channel.close ic)
        ~f:(fun () -> Stdlib.In_channel.input_all ic)
    in
    parse_string contents
  with Sys_error msg -> Error (Printf.sprintf "Cannot read file: %s" msg)

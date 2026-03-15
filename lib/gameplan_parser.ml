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

let parse_dep_graph_line line =
  (* Format: "- Patch N [CLASS] -> [deps]" *)
  let line = String.strip line in
  match String.chop_prefix line ~prefix:"- Patch " with
  | None -> None
  | Some rest -> (
      (* Extract patch number *)
      let parts = String.lsplit2 rest ~on:' ' in
      match parts with
      | None -> None
      | Some (num_str, after_num) -> (
          match Int.of_string_opt num_str with
          | None -> None
          | Some patch_num -> (
              (* Find the -> *)
              match String.substr_index after_num ~pattern:"->" with
              | None -> None
              | Some arrow_idx ->
                  let deps_str =
                    String.drop_prefix after_num (arrow_idx + 2) |> String.strip
                  in
                  (* Parse [1, 2, 3] or [] *)
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
                      |> List.filter_map ~f:(fun s ->
                          Int.of_string_opt (String.strip s))
                      |> List.map ~f:Types.Patch_id.of_int
                  in
                  Some (Types.Patch_id.of_int patch_num, deps))))

let parse_dep_graph lines =
  let in_graph = ref false in
  List.fold lines
    ~init:(Map.empty (module Types.Patch_id))
    ~f:(fun acc line ->
      let stripped = String.strip line in
      if String.is_prefix stripped ~prefix:"## Dependency Graph" then (
        in_graph := true;
        acc)
      else if !in_graph && String.is_prefix stripped ~prefix:"## " then (
        in_graph := false;
        acc)
      else if !in_graph then
        match parse_dep_graph_line line with
        | Some (id, deps) -> Map.set acc ~key:id ~data:deps
        | None -> acc
      else acc)

let parse_patch_header line =
  (* Format: "### Patch N [CLASS]: Title" *)
  let line = String.strip line in
  match String.chop_prefix line ~prefix:"### Patch " with
  | None -> None
  | Some rest -> (
      match String.lsplit2 rest ~on:' ' with
      | None -> None
      | Some (num_str, after_num) -> (
          match Int.of_string_opt num_str with
          | None -> None
          | Some patch_num -> (
              (* Skip [CLASS]: and get title *)
              match String.substr_index after_num ~pattern:"]: " with
              | None -> None
              | Some idx ->
                  let title =
                    String.drop_prefix after_num (idx + 3) |> String.strip
                  in
                  Some (patch_num, title))))

let parse_patches lines dep_graph ~project_name =
  List.filter_map lines ~f:(fun line ->
      match parse_patch_header line with
      | None -> None
      | Some (num, title) ->
          let id = Types.Patch_id.of_int num in
          let dependencies =
            Map.find dep_graph id |> Option.value ~default:[]
          in
          let branch =
            Types.Branch.of_string (project_name ^ "/patch-" ^ Int.to_string num)
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
          (Printf.sprintf "Cycle detected involving patch %d"
             (Types.Patch_id.to_int node))
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
  let patch_ids =
    List.map patches ~f:(fun p -> p.Types.Patch.id)
    |> Set.of_list (module Types.Patch_id)
  in
  (* Check all dep targets exist *)
  let missing =
    Map.fold dep_graph ~init:[] ~f:(fun ~key:from ~data:deps acc ->
        List.fold deps ~init:acc ~f:(fun acc dep ->
            if Set.mem patch_ids dep then acc
            else
              Printf.sprintf "Patch %d depends on nonexistent patch %d"
                (Types.Patch_id.to_int from)
                (Types.Patch_id.to_int dep)
              :: acc))
  in
  if not (List.is_empty missing) then Error (List.hd_exn missing)
  else
    (* Check no self-deps *)
    let self_deps =
      Map.fold dep_graph ~init:[] ~f:(fun ~key:from ~data:deps acc ->
          if List.mem deps from ~equal:Types.Patch_id.equal then
            Printf.sprintf "Patch %d depends on itself"
              (Types.Patch_id.to_int from)
            :: acc
          else acc)
    in
    if not (List.is_empty self_deps) then Error (List.hd_exn self_deps)
    else
      match detect_cycle dep_graph with Some msg -> Error msg | None -> Ok ()

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
    let contents = Stdio.In_channel.read_all path in
    parse_string contents
  with Sys_error msg -> Error (Printf.sprintf "Cannot read file: %s" msg)

open Base

type t = {
  gameplan : Types.Gameplan.t;
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
  match invalid_ids with
  | msg :: _ -> Error msg
  | [] -> (
      (* Check for duplicate patch IDs *)
      let all_ids = List.map patches ~f:(fun p -> p.Types.Patch.id) in
      let unique_ids = Set.of_list (module Types.Patch_id) all_ids in
      if Set.length unique_ids <> List.length all_ids then
        let dup =
          let seen = Hashtbl.create (module Types.Patch_id) in
          List.find_map all_ids ~f:(fun id ->
              if Hashtbl.mem seen id then
                Some
                  (Printf.sprintf "Duplicate patch ID: %s"
                     (Types.Patch_id.to_string id))
              else (
                Hashtbl.set seen ~key:id ~data:();
                None))
        in
        match dup with
        | Some msg -> Error msg
        | None -> Error "Duplicate patch ID detected but could not identify it"
      else
        let patch_ids = unique_ids in
        (* Check all dep graph keys (source patches) exist *)
        let orphan_sources =
          Map.fold dep_graph ~init:[] ~f:(fun ~key:from ~data:_ acc ->
              if Set.mem patch_ids from then acc
              else
                Printf.sprintf
                  "Dependency graph references nonexistent patch %s"
                  (Types.Patch_id.to_string from)
                :: acc)
        in
        match orphan_sources with
        | msg :: _ -> Error msg
        | [] -> (
            (* Check all dep targets exist *)
            let missing =
              Map.fold dep_graph ~init:[] ~f:(fun ~key:from ~data:deps acc ->
                  List.fold deps ~init:acc ~f:(fun acc dep ->
                      if Set.mem patch_ids dep then acc
                      else
                        Printf.sprintf
                          "Patch %s depends on nonexistent patch %s"
                          (Types.Patch_id.to_string from)
                          (Types.Patch_id.to_string dep)
                        :: acc))
            in
            match missing with
            | msg :: _ -> Error msg
            | [] -> (
                (* Check no self-deps *)
                let self_deps =
                  Map.fold dep_graph ~init:[]
                    ~f:(fun ~key:from ~data:deps acc ->
                      if List.mem deps from ~equal:Types.Patch_id.equal then
                        Printf.sprintf "Patch %s depends on itself"
                          (Types.Patch_id.to_string from)
                        :: acc
                      else acc)
                in
                match self_deps with
                | msg :: _ -> Error msg
                | [] -> (
                    match detect_cycle dep_graph with
                    | Some msg -> Error msg
                    | None -> Ok ()))))

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
              gameplan =
                {
                  Types.Gameplan.project_name;
                  problem_statement;
                  solution_summary;
                  patches;
                };
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

let%test_module "Gameplan_parser" =
  (module struct
    let%test "slugify lowercase and strip" =
      String.equal (slugify "My Project") "my-project"

    let%test "slugify special chars" =
      String.equal (slugify "foo@bar!baz") "foo-bar-baz"

    let%test "slugify already clean" =
      String.equal (slugify "clean-name") "clean-name"

    let%test "parse_project_name from gameplan header" =
      let lines = [ "# Gameplan: My Project"; "## Problem" ] in
      match parse_project_name lines with
      | Ok name -> String.equal name "My Project"
      | Error _ -> false

    let%test "parse_project_name from h1" =
      let lines = [ "# Simple Name" ] in
      match parse_project_name lines with
      | Ok name -> String.equal name "Simple Name"
      | Error _ -> false

    let%test "parse_project_name missing" =
      let lines = [ "no heading here" ] in
      match parse_project_name lines with Ok _ -> false | Error _ -> true

    let%test "extract_section finds section" =
      let lines =
        [
          "## Problem Statement";
          "The problem is X.";
          "";
          "## Solution Summary";
          "We fix it.";
        ]
      in
      match extract_section ~header:"Problem Statement" lines with
      | Some s -> String.is_substring s ~substring:"problem is X"
      | None -> false

    let%test "extract_section missing" =
      let lines = [ "## Other"; "stuff" ] in
      Option.is_none (extract_section ~header:"Problem Statement" lines)

    let%test "parse_dep_graph_line valid" =
      match parse_dep_graph_line "- Patch 1 [CORE] -> [2, 3]" with
      | Some (id, deps) ->
          String.equal (Types.Patch_id.to_string id) "1" && List.length deps = 2
      | None -> false

    let%test "parse_dep_graph_line no deps" =
      match parse_dep_graph_line "- Patch 1 [CORE] -> []" with
      | Some (_, deps) -> List.is_empty deps
      | None -> false

    let%test "detect_cycle finds cycle" =
      let dep_graph =
        Map.of_alist_exn
          (module Types.Patch_id)
          [
            (Types.Patch_id.of_string "a", [ Types.Patch_id.of_string "b" ]);
            (Types.Patch_id.of_string "b", [ Types.Patch_id.of_string "a" ]);
          ]
      in
      Option.is_some (detect_cycle dep_graph)

    let%test "detect_cycle no cycle" =
      let dep_graph =
        Map.of_alist_exn
          (module Types.Patch_id)
          [
            (Types.Patch_id.of_string "a", []);
            (Types.Patch_id.of_string "b", [ Types.Patch_id.of_string "a" ]);
          ]
      in
      Option.is_none (detect_cycle dep_graph)

    let%test "validate rejects self-dep" =
      let pid = Types.Patch_id.of_string "a" in
      let patches =
        [
          Types.Patch.
            {
              id = pid;
              title = "A";
              branch = Types.Branch.of_string "ba";
              dependencies = [ pid ];
            };
        ]
      in
      let dep_graph =
        Map.of_alist_exn (module Types.Patch_id) [ (pid, [ pid ]) ]
      in
      match validate ~patches ~dep_graph with Ok () -> false | Error _ -> true

    let%test "validate rejects duplicate ids" =
      let pid = Types.Patch_id.of_string "a" in
      let patch =
        Types.Patch.
          {
            id = pid;
            title = "A";
            branch = Types.Branch.of_string "ba";
            dependencies = [];
          }
      in
      let dep_graph = Map.of_alist_exn (module Types.Patch_id) [ (pid, []) ] in
      match validate ~patches:[ patch; patch ] ~dep_graph with
      | Ok () -> false
      | Error _ -> true

    let%test "parse_string roundtrip" =
      let input =
        {|# Gameplan: Test Project

## Problem Statement
A problem.

## Solution Summary
A solution.

## Dependency Graph
- Patch 1 [CORE] -> []
- Patch 2 [CORE] -> [1]

## Patches

### Patch 1 [CORE]: First patch
Do thing 1.

### Patch 2 [CORE]: Second patch
Do thing 2.
|}
      in
      match parse_string input with
      | Ok result ->
          List.length result.gameplan.patches = 2
          && String.equal result.gameplan.project_name "Test Project"
      | Error _ -> false
  end)

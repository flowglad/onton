open Base

type t = {
  gameplan : Types.Gameplan.t;
  dependency_graph : Types.Patch_id.t list Map.M(Types.Patch_id).t;
}

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

let json_string_list json key =
  let open Yojson.Safe.Util in
  match json |> member key with
  | `List items ->
      List.filter_map items ~f:(function `String s -> Some s | _ -> None)
  | _ -> []

let parse_json_string input =
  match
    try Ok (Yojson.Safe.from_string input)
    with Yojson.Json_error msg ->
      Error (Printf.sprintf "JSON parse error: %s" msg)
  with
  | Error msg -> Error msg
  | Ok json -> (
      let open Yojson.Safe.Util in
      try
        let project_name = json |> member "projectName" |> to_string in
        let problem_statement =
          match json |> member "problemStatement" with
          | `String s -> s
          | _ -> ""
        in
        let solution_summary =
          match json |> member "solutionSummary" with `String s -> s | _ -> ""
        in
        let design_decisions =
          match json |> member "finalStateSpec" with `String s -> s | _ -> ""
        in
        let current_state_analysis =
          match json |> member "currentStateAnalysis" with
          | `String s -> s
          | _ -> ""
        in
        let explicit_opinions =
          match json |> member "explicitOpinions" with
          | `List items ->
              List.filter_map items ~f:(fun obj ->
                  let opinion =
                    match obj |> member "opinion" with
                    | `String s -> s
                    | _ -> ""
                  in
                  let rationale =
                    match obj |> member "rationale" with
                    | `String s -> s
                    | _ -> ""
                  in
                  if String.is_empty opinion then None
                  else
                    Some
                      (Printf.sprintf "- OPINION: %s\n  RATIONALE: %s" opinion
                         rationale))
              |> String.concat ~sep:"\n"
          | _ -> ""
        in
        let acceptance_criteria = json_string_list json "acceptanceCriteria" in
        let open_questions = json_string_list json "openQuestions" in
        (* Dependency graph: {patch, classification, dependsOn[]} format *)
        let dep_graph =
          match json |> member "dependencyGraph" with
          | `List items ->
              List.fold items
                ~init:(Map.empty (module Types.Patch_id))
                ~f:(fun acc entry ->
                  let patch_num = entry |> member "patch" |> to_int in
                  let patch_id =
                    Types.Patch_id.of_string (Int.to_string patch_num)
                  in
                  let depends_on =
                    match entry |> member "dependsOn" with
                    | `List deps ->
                        List.filter_map deps ~f:(fun d ->
                            match d with
                            | `Int n ->
                                Some
                                  (Types.Patch_id.of_string (Int.to_string n))
                            | _ -> None)
                    | _ -> []
                  in
                  Map.set acc ~key:patch_id ~data:depends_on)
          | _ -> Map.empty (module Types.Patch_id)
        in
        let slug = slugify project_name in
        let patches =
          json |> member "patches" |> to_list
          |> List.map ~f:(fun p ->
              let id_num = p |> member "number" |> to_int in
              let id_str = Int.to_string id_num in
              let id = Types.Patch_id.of_string id_str in
              let title = p |> member "title" |> to_string in
              let changes = json_string_list p "changes" in
              let description =
                match changes with
                | [] -> ""
                | items ->
                    List.map items ~f:(fun s -> "- " ^ s)
                    |> String.concat ~sep:"\n"
              in
              let branch = Types.Branch.of_string (slug ^ "/patch-" ^ id_str) in
              let dependencies =
                Map.find dep_graph id |> Option.value ~default:[]
              in
              let spec =
                match p |> member "spec" with `String s -> s | _ -> ""
              in
              let classification =
                match p |> member "classification" with
                | `String s -> s
                | _ -> ""
              in
              let test_stubs_introduced =
                json_string_list p "testStubsIntroduced"
              in
              let test_stubs_implemented =
                json_string_list p "testStubsImplemented"
              in
              let files =
                match p |> member "files" with
                | `List items ->
                    List.filter_map items ~f:(fun obj ->
                        match obj |> member "path" with
                        | `String path ->
                            let action =
                              match obj |> member "action" with
                              | `String s -> s
                              | _ -> "modify"
                            in
                            let desc =
                              match obj |> member "description" with
                              | `String s -> s
                              | _ -> ""
                            in
                            Some (Printf.sprintf "%s (%s): %s" path action desc)
                        | _ -> None)
                | _ -> []
              in
              {
                Types.Patch.id;
                title;
                description;
                branch;
                dependencies;
                spec;
                acceptance_criteria = [];
                files;
                classification;
                changes;
                test_stubs_introduced;
                test_stubs_implemented;
              })
        in
        match open_questions with
        | _ :: _ ->
            let bullet_list =
              List.map open_questions ~f:(fun q -> "  - " ^ q)
              |> String.concat ~sep:"\n"
            in
            Error
              (Printf.sprintf
                 "Gameplan has %d open question(s) that must be resolved \
                  before orchestration:\n\
                  %s"
                 (List.length open_questions)
                 bullet_list)
        | [] -> (
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
                        design_decisions;
                        patches;
                        current_state_analysis;
                        explicit_opinions;
                        acceptance_criteria;
                        open_questions;
                      };
                    dependency_graph = dep_graph;
                  })
      with Type_error (msg, _) ->
        Error (Printf.sprintf "JSON structure error: %s" msg))

let read_file path =
  try
    let ic = Stdlib.In_channel.open_text path in
    let contents =
      Exn.protect
        ~finally:(fun () -> Stdlib.In_channel.close ic)
        ~f:(fun () -> Stdlib.In_channel.input_all ic)
    in
    Ok contents
  with Sys_error msg -> Error (Printf.sprintf "Cannot read file: %s" msg)

let parse_json_file path =
  match read_file path with
  | Error e -> Error e
  | Ok contents -> parse_json_string contents

let parse_file path =
  match read_file path with
  | Error e -> Error e
  | Ok contents -> parse_json_string contents

let%test_module "Gameplan_parser" =
  (module struct
    let%test "slugify lowercase and strip" =
      String.equal (slugify "My Project") "my-project"

    let%test "slugify special chars" =
      String.equal (slugify "foo@bar!baz") "foo-bar-baz"

    let%test "slugify already clean" =
      String.equal (slugify "clean-name") "clean-name"

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
              description = "";
              branch = Types.Branch.of_string "ba";
              dependencies = [ pid ];
              spec = "";
              acceptance_criteria = [];
              files = [];
              classification = "";
              changes = [];
              test_stubs_introduced = [];
              test_stubs_implemented = [];
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
            description = "";
            branch = Types.Branch.of_string "ba";
            dependencies = [];
            spec = "";
            acceptance_criteria = [];
            files = [];
            classification = "";
            changes = [];
            test_stubs_introduced = [];
            test_stubs_implemented = [];
          }
      in
      let dep_graph = Map.of_alist_exn (module Types.Patch_id) [ (pid, []) ] in
      match validate ~patches:[ patch; patch ] ~dep_graph with
      | Ok () -> false
      | Error _ -> true

    let%test "parse_json_string: real format fields" =
      let input =
        {|{
          "projectName": "my-proj",
          "problemStatement": "Something is broken.",
          "solutionSummary": "Do things fast.",
          "finalStateSpec": "must be correct",
          "currentStateAnalysis": "current state is bad",
          "explicitOpinions": [
            {"opinion": "Use tests", "rationale": "Catches bugs"}
          ],
          "acceptanceCriteria": ["builds", "tests pass"],
          "patches": [
            {
              "number": 1,
              "title": "First",
              "classification": "CORE",
              "changes": ["Add module X", "Wire up Y"],
              "spec": "module X must ...",
              "testStubsIntroduced": ["test_x"],
              "testStubsImplemented": [],
              "files": [
                {"path": "lib/x.ml", "action": "create", "description": "New module"}
              ]
            }
          ],
          "dependencyGraph": [
            {"patch": 1, "classification": "CORE", "dependsOn": []}
          ]
        }|}
      in
      match parse_json_string input with
      | Ok result ->
          String.equal result.gameplan.project_name "my-proj"
          && String.equal result.gameplan.problem_statement
               "Something is broken."
          && String.equal result.gameplan.solution_summary "Do things fast."
          && String.equal result.gameplan.design_decisions "must be correct"
          && String.equal result.gameplan.current_state_analysis
               "current state is bad"
          && String.is_substring result.gameplan.explicit_opinions
               ~substring:"Use tests"
          && List.equal String.equal result.gameplan.acceptance_criteria
               [ "builds"; "tests pass" ]
          && List.length result.gameplan.patches = 1
          &&
          let p = List.hd_exn result.gameplan.patches in
          String.equal (Types.Patch_id.to_string p.Types.Patch.id) "1"
          && String.equal p.classification "CORE"
          && List.length p.changes = 2
          && String.is_substring p.description ~substring:"Add module X"
          && String.equal p.spec "module X must ..."
          && List.equal String.equal p.test_stubs_introduced [ "test_x" ]
          && List.is_empty p.test_stubs_implemented
          && String.is_substring (List.hd_exn p.files)
               ~substring:"lib/x.ml (create)"
      | Error msg ->
          Stdlib.Printf.eprintf "parse error: %s\n" msg;
          false

    let%test "parse_json_string: branch derived from project slug and number" =
      let input =
        {|{
          "projectName": "My Project",
          "solutionSummary": "S",
          "patches": [{"number": 1, "title": "T", "changes": []}],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Ok result ->
          let patch = List.hd_exn result.gameplan.patches in
          String.equal
            (Types.Branch.to_string patch.Types.Patch.branch)
            "my-project/patch-1"
      | Error _ -> false

    let%test "parse_json_string: dependency graph with dependsOn" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [
            {"number": 1, "title": "A", "changes": []},
            {"number": 2, "title": "B", "changes": []}
          ],
          "dependencyGraph": [
            {"patch": 1, "dependsOn": []},
            {"patch": 2, "dependsOn": [1]}
          ]
        }|}
      in
      match parse_json_string input with
      | Ok result ->
          let p2 =
            List.find_exn result.gameplan.patches ~f:(fun p ->
                String.equal (Types.Patch_id.to_string p.Types.Patch.id) "2")
          in
          List.equal Types.Patch_id.equal p2.Types.Patch.dependencies
            [ Types.Patch_id.of_string "1" ]
      | Error _ -> false

    let%test "parse_json_string: invalid JSON returns Error" =
      match parse_json_string "{not valid json}" with
      | Ok _ -> false
      | Error _ -> true

    let%test "parse_json_string: cycle returns Error" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [
            {"number": 1, "title": "A", "changes": []},
            {"number": 2, "title": "B", "changes": []}
          ],
          "dependencyGraph": [
            {"patch": 1, "dependsOn": [2]},
            {"patch": 2, "dependsOn": [1]}
          ]
        }|}
      in
      match parse_json_string input with
      | Ok _ -> false
      | Error msg -> String.is_substring msg ~substring:"Cycle"

    let%test "parse_json_string: open questions returns Error" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "openQuestions": ["Which database?", "Auth strategy?"],
          "patches": [
            {"number": 1, "title": "A", "changes": []}
          ],
          "dependencyGraph": [
            {"patch": 1, "dependsOn": []}
          ]
        }|}
      in
      match parse_json_string input with
      | Ok _ -> false
      | Error msg ->
          String.is_substring msg ~substring:"open question"
          && String.is_substring msg ~substring:"Which database?"
          && String.is_substring msg ~substring:"Auth strategy?"

    let%test "parse_json_string: empty open questions succeeds" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "openQuestions": [],
          "patches": [
            {"number": 1, "title": "A", "changes": []}
          ],
          "dependencyGraph": [
            {"patch": 1, "dependsOn": []}
          ]
        }|}
      in
      match parse_json_string input with Ok _ -> true | Error _ -> false
  end)

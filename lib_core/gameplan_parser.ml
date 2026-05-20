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

let validate_functional_changes ~patches ~functional_changes =
  let patch_ids =
    List.map patches ~f:(fun p -> p.Types.Patch.id)
    |> Set.of_list (module Types.Patch_id)
  in
  let rec loop seen_ids = function
    | [] -> Ok ()
    | (fc : Types.Functional_change.t) :: rest ->
        if String.is_empty (String.strip fc.id) then
          Error "functionalChanges[].id must be a non-empty string"
        else if Set.mem seen_ids fc.id then
          Error (Printf.sprintf "Duplicate functionalChange id: %s" fc.id)
        else if not (Set.mem patch_ids fc.owned_by) then
          Error
            (Printf.sprintf
               "functionalChange %s is ownedBy nonexistent patch %s" fc.id
               (Types.Patch_id.to_string fc.owned_by))
        else loop (Set.add seen_ids fc.id) rest
  in
  loop (Set.empty (module String)) functional_changes

let valid_context_resource_kinds =
  [
    "existing-implementation";
    "contract";
    "predecessor";
    "reference-doc";
    "test-or-static-check";
    "external-reference";
  ]

let validate_context_resources ~patches ~context_resources =
  let patch_ids =
    List.map patches ~f:(fun p -> p.Types.Patch.id)
    |> Set.of_list (module Types.Patch_id)
  in
  let resource_ids =
    List.map context_resources ~f:(fun r -> r.Types.Context_resource.id)
  in
  let rec check_ids seen = function
    | [] -> Ok ()
    | id :: rest ->
        let stripped = String.strip id in
        if String.is_empty stripped then
          Error "contextResources[].id must be a non-empty string"
        else if Set.mem seen stripped then
          Error (Printf.sprintf "Duplicate contextResource id: %s" stripped)
        else check_ids (Set.add seen stripped) rest
  in
  match check_ids (Set.empty (module String)) resource_ids with
  | Error e -> Error e
  | Ok () -> (
      let resource_id_set = Set.of_list (module String) resource_ids in
      match
        List.find_map context_resources ~f:(fun r ->
            if
              not
                (List.mem valid_context_resource_kinds r.kind
                   ~equal:String.equal)
            then
              Some
                (Printf.sprintf "contextResource %s has invalid kind %S" r.id
                   r.kind)
            else
              List.find_map r.consumed_by ~f:(fun patch_id ->
                  if Set.mem patch_ids patch_id then None
                  else
                    Some
                      (Printf.sprintf
                         "contextResource %s consumedBy nonexistent patch %s"
                         r.id
                         (Types.Patch_id.to_string patch_id))))
      with
      | Some e -> Error e
      | None -> (
          match
            List.find_map patches ~f:(fun p ->
                List.find_map p.Types.Patch.required_context ~f:(fun id ->
                    if Set.mem resource_id_set id then None
                    else
                      Some
                        (Printf.sprintf
                           "Patch %s requiredContext references nonexistent \
                            contextResource %s"
                           (Types.Patch_id.to_string p.id)
                           id)))
          with
          | Some e -> Error e
          | None -> (
              match
                List.find_map patches ~f:(fun p ->
                    List.find_map context_resources ~f:(fun r ->
                        let patch_id = p.Types.Patch.id in
                        let listed_in_patch =
                          List.mem p.required_context r.id ~equal:String.equal
                        in
                        let listed_in_resource =
                          List.mem r.consumed_by patch_id
                            ~equal:Types.Patch_id.equal
                        in
                        if Bool.equal listed_in_patch listed_in_resource then
                          None
                        else
                          Some
                            (Printf.sprintf
                               "Patch %s requiredContext must match \
                                contextResource %s consumedBy"
                               (Types.Patch_id.to_string patch_id)
                               r.id)))
              with
              | Some e -> Error e
              | None -> Ok ())))

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

let patch_id_of_json = function
  | `Int n -> Some (Types.Patch_id.of_string (Int.to_string n))
  | `String s when is_valid_patch_id s -> Some (Types.Patch_id.of_string s)
  | _ -> None

let json_string_list json key =
  let open Yojson.Safe.Util in
  match json |> member key with
  | `List items ->
      List.filter_map items ~f:(function `String s -> Some s | _ -> None)
  | _ -> []

let json_required_string_list json key =
  let open Yojson.Safe.Util in
  match json |> member key with
  | `Null -> []
  | `List items ->
      List.map items ~f:(fun item ->
          match item with
          | `String s -> s
          | _ ->
              raise
                (Type_error
                   (Printf.sprintf "%s[] must contain only strings" key, item)))
  | value ->
      raise
        (Type_error (Printf.sprintf "%s must be an array of strings" key, value))

let json_patch_id_list json key =
  let open Yojson.Safe.Util in
  match json |> member key with
  | `Null -> []
  | `List items ->
      List.map items ~f:(fun item ->
          match patch_id_of_json item with
          | Some id -> id
          | None ->
              raise
                (Type_error
                   (Printf.sprintf "%s[] must contain only patch IDs" key, item)))
  | value ->
      raise
        (Type_error
           (Printf.sprintf "%s must be an array of patch IDs" key, value))

let json_precedents json =
  let open Yojson.Safe.Util in
  match json |> member "precedents" with
  | `List items ->
      List.filter_map items ~f:(fun obj ->
          match obj |> member "kind" with
          | `String kind when not (String.is_empty kind) ->
              let name =
                match obj |> member "name" with `String s -> s | _ -> ""
              in
              let url =
                match obj |> member "url" with
                | `String s when not (String.is_empty s) -> Some s
                | _ -> None
              in
              let why_applicable =
                match obj |> member "whyApplicable" with
                | `String s -> s
                | _ -> ""
              in
              if String.is_empty name then None
              else Some { Types.Precedent.kind; name; url; why_applicable }
          | _ -> None)
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
        let optional_string key =
          match json |> member key with
          | `String s -> Base.String.strip s
          | `Null -> ""
          | _ -> raise (Type_error (key ^ " must be a string", json))
        in
        (* [repo_owner] and [repo_name] are forge-agnostic at this layer —
           we only enforce non-empty when present (empty strings are normalised
           to [""] so legacy gameplans without the keys keep loading; the
           [--gameplan] CLI path enforces the non-empty constraint at session
           start, and forge-specific format validation lives in the forge
           backend, e.g. {!Onton_core.Github_target.validate_target}). *)
        let repo_owner = optional_string "owner" in
        let repo_name = optional_string "repo" in
        let problem_statement =
          match json |> member "problemStatement" with
          | `String s -> s
          | _ -> ""
        in
        let solution_summary =
          match json |> member "solutionSummary" with `String s -> s | _ -> ""
        in
        let final_state_spec =
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
        let functional_changes =
          match json |> member "functionalChanges" with
          | `Null -> []
          | `List items ->
              List.map items ~f:(fun obj ->
                  let id =
                    match obj |> member "id" with
                    | `String s -> s
                    | _ ->
                        raise
                          (Type_error
                             ("functionalChanges[].id must be a string", obj))
                  in
                  let description =
                    match obj |> member "description" with
                    | `String s -> s
                    | _ ->
                        raise
                          (Type_error
                             ( "functionalChanges[].description must be a string",
                               obj ))
                  in
                  let owned_by =
                    match patch_id_of_json (obj |> member "ownedBy") with
                    | Some id -> id
                    | None ->
                        raise
                          (Type_error
                             ( "functionalChanges[].ownedBy must be an integer \
                                or alphanumeric patch id",
                               obj ))
                  in
                  { Types.Functional_change.id; description; owned_by })
          | _ ->
              raise
                (Type_error
                   ("functionalChanges must be an array of objects", json))
        in
        let context_resources =
          match json |> member "contextResources" with
          | `Null -> []
          | `List items ->
              List.map items ~f:(fun obj ->
                  let id =
                    match obj |> member "id" with
                    | `String s -> String.strip s
                    | _ ->
                        raise
                          (Type_error
                             ("contextResources[].id must be a string", obj))
                  in
                  let kind =
                    match obj |> member "kind" with
                    | `String s -> s
                    | _ ->
                        raise
                          (Type_error
                             ("contextResources[].kind must be a string", obj))
                  in
                  let paths = json_string_list obj "paths" in
                  let why =
                    match obj |> member "why" with `String s -> s | _ -> ""
                  in
                  let consumed_by = json_patch_id_list obj "consumedBy" in
                  { Types.Context_resource.id; kind; paths; why; consumed_by })
          | _ ->
              raise
                (Type_error
                   ("contextResources must be an array of objects", json))
        in
        let open_questions =
          match json |> member "openQuestions" with
          | `Null -> []
          | `List items ->
              List.mapi items ~f:(fun i item ->
                  match item with
                  | `String s -> s
                  | _ ->
                      raise
                        (Type_error
                           ( Printf.sprintf "openQuestions[%d] must be a string"
                               i,
                             json )))
          | _ ->
              raise
                (Type_error ("openQuestions must be an array of strings", json))
        in
        (* Dependency graph: {patch, classification, dependsOn[]} format *)
        let dep_graph =
          match json |> member "dependencyGraph" with
          | `List items ->
              List.fold items
                ~init:(Map.empty (module Types.Patch_id))
                ~f:(fun acc entry ->
                  let patch_id =
                    match patch_id_of_json (entry |> member "patch") with
                    | Some id -> id
                    | None ->
                        raise
                          (Type_error
                             ( "dependencyGraph[].patch must be an integer or \
                                alphanumeric string",
                               entry ))
                  in
                  let depends_on =
                    match entry |> member "dependsOn" with
                    | `List deps -> List.filter_map deps ~f:patch_id_of_json
                    | _ -> []
                  in
                  Map.set acc ~key:patch_id ~data:depends_on)
          | _ -> Map.empty (module Types.Patch_id)
        in
        let slug = slugify project_name in
        let patches =
          json |> member "patches" |> to_list
          |> List.map ~f:(fun p ->
              let id, id_str =
                match patch_id_of_json (p |> member "number") with
                | Some id -> (id, Types.Patch_id.to_string id)
                | None ->
                    raise
                      (Type_error
                         ( "patches[].number must be an integer or \
                            alphanumeric string",
                           p ))
              in
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
              let complexity =
                match p |> member "complexity" with
                | `Int n when n >= 1 && n <= 3 -> Some n
                | _ -> None
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
              let precedents = json_precedents p in
              let required_context =
                json_required_string_list p "requiredContext"
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
                complexity;
                precedents;
                required_context;
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
            | Ok () -> (
                match
                  validate_functional_changes ~patches ~functional_changes
                with
                | Error e -> Error e
                | Ok () -> (
                    match
                      validate_context_resources ~patches ~context_resources
                    with
                    | Error e -> Error e
                    | Ok () ->
                        Ok
                          {
                            gameplan =
                              {
                                Types.Gameplan.project_name;
                                repo_owner;
                                repo_name;
                                problem_statement;
                                solution_summary;
                                final_state_spec;
                                patches;
                                functional_changes;
                                context_resources;
                                current_state_analysis;
                                explicit_opinions;
                                acceptance_criteria;
                                open_questions;
                              };
                            dependency_graph = dep_graph;
                          })))
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
              complexity = None;
              precedents = [];
              required_context = [];
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
            complexity = None;
            precedents = [];
            required_context = [];
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
          && String.equal result.gameplan.final_state_spec "must be correct"
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

    let%test "parse_json_string: string patch IDs" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [
            {"number": "auth", "title": "Auth module", "changes": []},
            {"number": "db", "title": "DB layer", "changes": []}
          ],
          "dependencyGraph": [
            {"patch": "auth", "dependsOn": []},
            {"patch": "db", "dependsOn": ["auth"]}
          ]
        }|}
      in
      match parse_json_string input with
      | Ok result ->
          let p1 = List.hd_exn result.gameplan.patches in
          let p2 = List.nth_exn result.gameplan.patches 1 in
          String.equal (Types.Patch_id.to_string p1.Types.Patch.id) "auth"
          && String.equal (Types.Patch_id.to_string p2.Types.Patch.id) "db"
          && String.equal
               (Types.Branch.to_string p1.Types.Patch.branch)
               "p/patch-auth"
          && List.equal Types.Patch_id.equal p2.Types.Patch.dependencies
               [ Types.Patch_id.of_string "auth" ]
      | Error msg ->
          Stdlib.Printf.eprintf "parse error: %s\n" msg;
          false

    let%test "parse_json_string: invalid JSON returns Error" =
      match parse_json_string "{not valid json}" with
      | Ok _ -> false
      | Error _ -> true

    let%test "parse_json_string: precedents are extracted with all four fields"
        =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [{
            "number": 1,
            "title": "A",
            "changes": [],
            "precedents": [
              {"kind":"library","name":"Bindlib","url":"https://example.com","whyApplicable":"capture-avoiding subst"},
              {"kind":"pattern","name":"Locally nameless","whyApplicable":"fallback design"}
            ]
          }],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Error _ -> false
      | Ok result -> (
          let p = List.hd_exn result.gameplan.patches in
          match p.Types.Patch.precedents with
          | [ a; b ] ->
              String.equal a.Types.Precedent.kind "library"
              && String.equal a.name "Bindlib"
              && (match a.url with
                | Some "https://example.com" -> true
                | _ -> false)
              && String.equal a.why_applicable "capture-avoiding subst"
              && String.equal b.kind "pattern"
              && String.equal b.name "Locally nameless"
              && Option.is_none b.url
          | _ -> false)

    let%test "parse_json_string: missing precedents defaults to empty list" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Error _ -> false
      | Ok result ->
          let p = List.hd_exn result.gameplan.patches in
          List.is_empty p.Types.Patch.precedents

    let%test
        "parse_json_string: contextResources and requiredContext round-trip" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "contextResources": [
            {
              "id": "parser-contract",
              "kind": "contract",
              "paths": ["lib_core/gameplan_parser.mli", "docs/gameplan.md"],
              "why": "Defines parser compatibility guarantees.",
              "consumedBy": [1]
            }
          ],
          "patches": [
            {
              "number": 1,
              "title": "A",
              "changes": [],
              "requiredContext": ["parser-contract"]
            }
          ],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Error msg ->
          Stdlib.Printf.eprintf "parse error: %s\n" msg;
          false
      | Ok result -> (
          match
            (result.gameplan.context_resources, result.gameplan.patches)
          with
          | [ resource ], [ patch ] ->
              String.equal resource.Types.Context_resource.id "parser-contract"
              && String.equal resource.kind "contract"
              && List.equal String.equal resource.paths
                   [ "lib_core/gameplan_parser.mli"; "docs/gameplan.md" ]
              && String.equal resource.why
                   "Defines parser compatibility guarantees."
              && List.equal Types.Patch_id.equal resource.consumed_by
                   [ Types.Patch_id.of_string "1" ]
              && List.equal String.equal patch.Types.Patch.required_context
                   [ "parser-contract" ]
          | _ -> false)

    let%test
        "parse_json_string: missing context fields default to empty for legacy"
        =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Error _ -> false
      | Ok result -> (
          match result.gameplan.patches with
          | [ patch ] ->
              List.is_empty result.gameplan.context_resources
              && List.is_empty patch.Types.Patch.required_context
          | _ -> false)

    let%test "parse_json_string: duplicate contextResource id returns Error" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "contextResources": [
            {"id": "ctx", "kind": "contract", "paths": [], "why": "", "consumedBy": []},
            {"id": "ctx", "kind": "contract", "paths": [], "why": "", "consumedBy": []}
          ],
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Ok _ -> false
      | Error msg ->
          String.is_substring msg ~substring:"Duplicate contextResource"

    let%test
        "parse_json_string: missing requiredContext reference returns Error" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "contextResources": [],
          "patches": [
            {"number": 1, "title": "A", "changes": [], "requiredContext": ["missing"]}
          ],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Ok _ -> false
      | Error msg ->
          String.is_substring msg
            ~substring:"requiredContext references nonexistent"

    let%test "parse_json_string: nonexistent consumedBy patch returns Error" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "contextResources": [
            {"id": "ctx", "kind": "contract", "paths": [], "why": "", "consumedBy": [2]}
          ],
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Ok _ -> false
      | Error msg ->
          String.is_substring msg ~substring:"consumedBy nonexistent patch"

    let%test
        "parse_json_string: requiredContext and consumedBy mismatch returns \
         Error" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "contextResources": [
            {"id": "ctx", "kind": "contract", "paths": [], "why": "", "consumedBy": [1]}
          ],
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Ok _ -> false
      | Error msg ->
          String.is_substring msg
            ~substring:"requiredContext must match contextResource"

    let%test "parse_json_string: precedent without kind or name is skipped" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [{
            "number": 1,
            "title": "A",
            "changes": [],
            "precedents": [
              {"name": "no kind", "whyApplicable": "x"},
              {"kind": "library", "whyApplicable": "no name"},
              {"kind": "library", "name": "OK", "whyApplicable": "ok"}
            ]
          }],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Error _ -> false
      | Ok result -> (
          let p = List.hd_exn result.gameplan.patches in
          match p.Types.Patch.precedents with
          | [ only ] -> String.equal only.Types.Precedent.name "OK"
          | _ -> false)

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

    let%test "parse_json_string: functionalChanges round-trip" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [
            {"number": 1, "title": "A", "changes": []},
            {"number": 2, "title": "B", "changes": []}
          ],
          "functionalChanges": [
            {"id": "FC-1", "description": "Behavior X is added", "ownedBy": 1},
            {"id": "FC-2", "description": "Behavior Y is added", "ownedBy": 2}
          ],
          "dependencyGraph": [
            {"patch": 1, "dependsOn": []},
            {"patch": 2, "dependsOn": [1]}
          ]
        }|}
      in
      match parse_json_string input with
      | Error _ -> false
      | Ok result -> (
          match result.gameplan.functional_changes with
          | [ a; b ] ->
              String.equal a.Types.Functional_change.id "FC-1"
              && String.equal a.description "Behavior X is added"
              && String.equal (Types.Patch_id.to_string a.owned_by) "1"
              && String.equal b.Types.Functional_change.id "FC-2"
              && String.equal (Types.Patch_id.to_string b.owned_by) "2"
          | _ -> false)

    let%test "parse_json_string: functionalChanges missing defaults to empty" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Error _ -> false
      | Ok result -> List.is_empty result.gameplan.functional_changes

    let%test
        "parse_json_string: functionalChange owned by nonexistent patch \
         returns Error" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "functionalChanges": [
            {"id": "FC-1", "description": "x", "ownedBy": 99}
          ],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Ok _ -> false
      | Error msg ->
          String.is_substring msg ~substring:"FC-1"
          && String.is_substring msg ~substring:"nonexistent patch"

    let%test "parse_json_string: duplicate functionalChange id returns Error" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "functionalChanges": [
            {"id": "FC-1", "description": "x", "ownedBy": 1},
            {"id": "FC-1", "description": "y", "ownedBy": 1}
          ],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Ok _ -> false
      | Error msg ->
          String.is_substring msg ~substring:"Duplicate functionalChange"

    let%test
        "parse_json_string: whitespace-only functionalChange id returns Error" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "functionalChanges": [
            {"id": "  ", "description": "x", "ownedBy": 1}
          ],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Ok _ -> false
      | Error msg ->
          String.is_substring msg
            ~substring:"functionalChanges[].id must be a non-empty string"

    let%test "parse_json_string: owner/repo round-trip when present" =
      let input =
        {|{
          "projectName": "p",
          "owner": "flowglad",
          "repo": "onton",
          "solutionSummary": "s",
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Error _ -> false
      | Ok result ->
          String.equal result.gameplan.repo_owner "flowglad"
          && String.equal result.gameplan.repo_name "onton"

    let%test "parse_json_string: missing owner/repo defaults to empty (legacy)"
        =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Error _ -> false
      | Ok result ->
          String.is_empty result.gameplan.repo_owner
          && String.is_empty result.gameplan.repo_name

    let%test
        "parse_json_string: owner accepts any non-empty string (forge-specific \
         validation lives in the forge backend)" =
      let input =
        {|{
          "projectName": "p",
          "owner": "group/subgroup",
          "repo": "thing.repo",
          "solutionSummary": "s",
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Error _ -> false
      | Ok result ->
          String.equal result.gameplan.repo_owner "group/subgroup"
          && String.equal result.gameplan.repo_name "thing.repo"

    let%test "parse_json_string: owner non-string returns Error" =
      let input =
        {|{
          "projectName": "p",
          "owner": 42,
          "solutionSummary": "s",
          "patches": [{"number": 1, "title": "A", "changes": []}],
          "dependencyGraph": [{"patch": 1, "dependsOn": []}]
        }|}
      in
      match parse_json_string input with
      | Ok _ -> false
      | Error msg -> String.is_substring msg ~substring:"owner"

    let%test "parse_json_string: non-string open question returns Error" =
      let input =
        {|{
          "projectName": "p",
          "solutionSummary": "s",
          "openQuestions": [123],
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
      | Error msg -> String.is_substring msg ~substring:"openQuestions[0]"
  end)

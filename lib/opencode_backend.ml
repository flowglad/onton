open Base

let build_args ~cwd_path ~prompt ~resume_session =
  let base = [ "opencode"; "run"; "--format"; "json"; "--dir"; cwd_path ] in
  let resume_args =
    match resume_session with
    | Some session_id -> [ "--session"; session_id ]
    | None -> []
  in
  base @ resume_args @ [ prompt ]

(* OpenCode emits lowercase tool names and camelCase input keys, while the
   downstream summary extractor in [bin/main.ml] expects the PascalCase names
   and snake_case keys used by Claude Code. Normalize here so tool-use details
   (file paths, commands, patterns) render consistently across backends. *)
let normalize_tool_name = function
  | "read" -> "Read"
  | "write" -> "Write"
  | "edit" -> "Edit"
  | "bash" -> "Bash"
  | "grep" -> "Grep"
  | "glob" -> "Glob"
  | "webfetch" -> "WebFetch"
  | "list" -> "List"
  | "task" -> "Task"
  | "todowrite" -> "TodoWrite"
  | "todoread" -> "TodoRead"
  | other -> other

let normalize_input_json ~tool (json : Yojson.Safe.t) : Yojson.Safe.t =
  match (tool, json) with
  | ("read" | "write" | "edit"), `Assoc fields ->
      `Assoc
        (List.map fields ~f:(fun (k, v) ->
             let k' = if String.equal k "filePath" then "file_path" else k in
             (k', v)))
  | _ -> json

let parse_event (line : string) : Types.Stream_event.t list =
  match Yojson.Safe.from_string line with
  | json -> (
      let open Yojson.Safe.Util in
      let typ = member "type" json |> to_string_option in
      match typ with
      | Some "step_start" -> (
          match member "sessionID" json |> to_string_option with
          | Some id when not (String.is_empty id) ->
              [ Types.Stream_event.Session_init { session_id = id } ]
          | _ -> [])
      | Some "text" ->
          let part = member "part" json in
          let text =
            member "text" part |> to_string_option |> Option.value ~default:""
          in
          if String.is_empty text then []
          else [ Types.Stream_event.Text_delta text ]
      | Some "tool_use" ->
          let part = member "part" json in
          let raw_tool =
            member "tool" part |> to_string_option |> Option.value ~default:""
          in
          let state = member "state" part in
          let input =
            match member "input" state with
            | `Null -> ""
            | v -> Yojson.Safe.to_string (normalize_input_json ~tool:raw_tool v)
          in
          let status = member "status" state |> to_string_option in
          [
            Types.Stream_event.Tool_use
              { name = normalize_tool_name raw_tool; input; status };
          ]
      | Some "step_finish" -> (
          let part = member "part" json in
          let reason =
            member "reason" part |> to_string_option |> Option.value ~default:""
          in
          match reason with
          | "stop" ->
              [
                Types.Stream_event.Final_result
                  { text = ""; stop_reason = Types.Stop_reason.End_turn };
              ]
          | _ -> [])
      | Some "error" ->
          let msg =
            member "message" json |> to_string_option
            |> Option.value ~default:"unknown opencode error"
          in
          [ Types.Stream_event.Error msg ]
      | _ -> [])
  | exception Yojson.Json_error _ -> []
  | exception Yojson.Safe.Util.Type_error _ -> []

let run_streaming ~process_mgr ~clock ~timeout ~cwd ~patch_id ~prompt
    ~resume_session ~on_event =
  ignore (patch_id : Types.Patch_id.t);
  let cwd_path = snd cwd in
  let args = build_args ~cwd_path ~prompt ~resume_session in
  let process_line line =
    let trimmed = String.strip line in
    if String.is_empty trimmed then [] else parse_event trimmed
  in
  Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~args
    ~process_line ~on_event

let create ~process_mgr ~clock ~timeout : Llm_backend.t =
  {
    name = "OpenCode";
    run_streaming =
      (fun ~cwd ~patch_id ~prompt ~resume_session ~on_event ->
        run_streaming ~process_mgr ~clock ~timeout ~cwd ~patch_id ~prompt
          ~resume_session ~on_event);
  }

let%test "build_args without continue" =
  let args =
    build_args ~cwd_path:"/tmp/work" ~prompt:"do stuff" ~resume_session:None
  in
  List.equal String.equal args
    [ "opencode"; "run"; "--format"; "json"; "--dir"; "/tmp/work"; "do stuff" ]

let%test "build_args with continue" =
  let args =
    build_args ~cwd_path:"/tmp/work" ~prompt:"do stuff"
      ~resume_session:(Some "x")
  in
  List.equal String.equal args
    [
      "opencode";
      "run";
      "--format";
      "json";
      "--dir";
      "/tmp/work";
      "--session";
      "x";
      "do stuff";
    ]

let%test "parse_event text" =
  let line = {|{"type":"text","part":{"type":"text","text":"hello"}}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Text_delta "hello" ]

let%test "parse_event tool_use bash normalizes name" =
  let line =
    {|{"type":"tool_use","part":{"type":"tool","tool":"bash","state":{"status":"completed","input":{"command":"ls -la"}}}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        {
          name = "Bash";
          input = {|{"command":"ls -la"}|};
          status = Some "completed";
        };
    ]

let%test "parse_event tool_use read normalizes name and filePath key" =
  let line =
    {|{"type":"tool_use","part":{"type":"tool","tool":"read","state":{"status":"completed","input":{"filePath":"/tmp/foo.txt"}}}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        {
          name = "Read";
          input = {|{"file_path":"/tmp/foo.txt"}|};
          status = Some "completed";
        };
    ]

let%test "parse_event tool_use edit normalizes filePath, leaves other keys" =
  let line =
    {|{"type":"tool_use","part":{"type":"tool","tool":"edit","state":{"status":"completed","input":{"filePath":"/tmp/a","oldString":"x","newString":"y"}}}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        {
          name = "Edit";
          input = {|{"file_path":"/tmp/a","oldString":"x","newString":"y"}|};
          status = Some "completed";
        };
    ]

let%test "parse_event tool_use write normalizes filePath key" =
  let line =
    {|{"type":"tool_use","part":{"type":"tool","tool":"write","state":{"status":"completed","input":{"filePath":"/tmp/b.txt","content":"hello"}}}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        {
          name = "Write";
          input = {|{"file_path":"/tmp/b.txt","content":"hello"}|};
          status = Some "completed";
        };
    ]

let%test "parse_event tool_use pending status survives parse" =
  (* OpenCode emits tool_use events with status=pending before the tool runs
     (e.g. while awaiting sandbox approval). We surface the status so the
     supervisor can distinguish a tool call that completed from one that
     was announced but never executed. *)
  let line =
    {|{"type":"tool_use","part":{"type":"tool","tool":"write","state":{"status":"pending","input":{"filePath":"/tmp/b.txt","content":"hello"}}}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        {
          name = "Write";
          input = {|{"file_path":"/tmp/b.txt","content":"hello"}|};
          status = Some "pending";
        };
    ]

let%test "parse_event tool_use running status survives parse" =
  let line =
    {|{"type":"tool_use","part":{"type":"tool","tool":"bash","state":{"status":"running","input":{"command":"sleep 10"}}}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        {
          name = "Bash";
          input = {|{"command":"sleep 10"}|};
          status = Some "running";
        };
    ]

let%test "parse_event tool_use missing status parses as None" =
  let line =
    {|{"type":"tool_use","part":{"type":"tool","tool":"bash","state":{"input":{"command":"ls"}}}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        { name = "Bash"; input = {|{"command":"ls"}|}; status = None };
    ]

let%test "parse_event step_start emits session_init" =
  let line =
    {|{"type":"step_start","sessionID":"ses_abc","part":{"type":"step-start"}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Session_init { session_id = "ses_abc" } ]

let%test "parse_event step_finish stop" =
  let line =
    {|{"type":"step_finish","part":{"type":"step-finish","reason":"stop"}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Final_result
        { text = ""; stop_reason = Types.Stop_reason.End_turn };
    ]

let%test "parse_event step_finish tool-calls is ignored" =
  let line =
    {|{"type":"step_finish","part":{"type":"step-finish","reason":"tool-calls"}}|}
  in
  List.is_empty (parse_event line)

let%test "parse_event step_start without sessionID is ignored" =
  let line = {|{"type":"step_start","part":{"type":"step-start"}}|} in
  List.is_empty (parse_event line)

let%test "parse_event invalid json" =
  List.is_empty (parse_event "not json at all")

let%test "parse_event unknown type" =
  List.is_empty (parse_event {|{"type":"ping"}|})

open Base

let build_args ~cwd_path ~prompt ~resume_session =
  let base = [ "opencode"; "run"; "--format"; "json"; "--dir"; cwd_path ] in
  let resume_args =
    match resume_session with
    | Some session_id -> [ "--resume"; session_id ]
    | None -> []
  in
  base @ resume_args @ [ prompt ]

let parse_event (line : string) : Types.Stream_event.t list =
  match Yojson.Safe.from_string line with
  | json -> (
      let open Yojson.Safe.Util in
      let typ = member "type" json |> to_string_option in
      match typ with
      | Some "text" ->
          let part = member "part" json in
          let text =
            member "text" part |> to_string_option |> Option.value ~default:""
          in
          if String.is_empty text then []
          else [ Types.Stream_event.Text_delta text ]
      | Some "tool_use" ->
          let part = member "part" json in
          let tool =
            member "tool" part |> to_string_option |> Option.value ~default:""
          in
          let state = member "state" part in
          let input =
            match member "input" state with
            | `Null -> ""
            | v -> Yojson.Safe.to_string v
          in
          [ Types.Stream_event.Tool_use { name = tool; input } ]
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
      "--resume";
      "x";
      "do stuff";
    ]

let%test "parse_event text" =
  let line = {|{"type":"text","part":{"type":"text","text":"hello"}}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Text_delta "hello" ]

let%test "parse_event tool_use" =
  let line =
    {|{"type":"tool_use","part":{"type":"tool","tool":"bash","state":{"status":"completed","input":{"command":"ls -la"}}}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        { name = "bash"; input = {|{"command":"ls -la"}|} };
    ]

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

let%test "parse_event step_start is ignored" =
  let line = {|{"type":"step_start","part":{"type":"step-start"}}|} in
  List.is_empty (parse_event line)

let%test "parse_event invalid json" =
  List.is_empty (parse_event "not json at all")

let%test "parse_event unknown type" =
  List.is_empty (parse_event {|{"type":"ping"}|})

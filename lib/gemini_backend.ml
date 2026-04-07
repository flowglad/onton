open Base

let build_args ~prompt ~continue =
  if continue then
    [ "gemini"; "--approval-mode=yolo"; "-o"; "stream-json"; "-r"; "latest" ]
  else [ "gemini"; "--approval-mode=yolo"; "-o"; "stream-json"; prompt ]

let parse_event (line : string) : Types.Stream_event.t list =
  match Yojson.Safe.from_string line with
  | json -> (
      let open Yojson.Safe.Util in
      let typ = member "type" json |> to_string_option in
      match typ with
      | Some "message" ->
          let role = member "role" json |> to_string_option in
          let delta =
            member "delta" json |> to_bool_option |> Option.value ~default:false
          in
          if Option.equal String.equal role (Some "assistant") && delta then
            let content =
              member "content" json |> to_string_option
              |> Option.value ~default:""
            in
            if String.is_empty content then []
            else [ Types.Stream_event.Text_delta content ]
          else []
      | Some "tool_use" -> (
          let tool_name = member "tool_name" json |> to_string_option in
          let parameters = member "parameters" json in
          match tool_name with
          | Some n ->
              let input = Yojson.Safe.to_string parameters in
              (* Map common Gemini tools to Claude names for TUI coloring / logs *)
              let mapped_name =
                match n with
                | "run_shell_command" -> "Bash"
                | "run_python_script" -> "Bash"
                | "read_file" -> "Read"
                | "write_file" -> "Write"
                | "patch_file" -> "Edit"
                | _ -> n
              in
              [ Types.Stream_event.Tool_use { name = mapped_name; input } ]
          | None -> [])
      | Some "result" ->
          [
            Types.Stream_event.Final_result
              { text = ""; stop_reason = Types.Stop_reason.End_turn };
          ]
      | Some "error" ->
          let msg =
            member "message" json |> to_string_option
            |> Option.value ~default:"unknown gemini error"
          in
          [ Types.Stream_event.Error msg ]
      | _ -> [])
  | exception Yojson.Json_error _ -> []

let run_streaming ~process_mgr ~cwd ~patch_id ~prompt ~continue ~on_event =
  ignore (patch_id : Types.Patch_id.t);
  let args = build_args ~prompt ~continue in
  let process_line line =
    let trimmed = String.strip line in
    if String.is_empty trimmed then [] else parse_event trimmed
  in
  Llm_backend.spawn_and_stream ~process_mgr ~cwd ~args ~process_line ~on_event

let create ~process_mgr : Llm_backend.t =
  {
    name = "Gemini";
    run_streaming =
      (fun ~cwd ~patch_id ~prompt ~continue ~on_event ->
        run_streaming ~process_mgr ~cwd ~patch_id ~prompt ~continue ~on_event);
  }

let%test "build_args without continue" =
  let args = build_args ~prompt:"do stuff" ~continue:false in
  List.equal String.equal args
    [ "gemini"; "--approval-mode=yolo"; "-o"; "stream-json"; "do stuff" ]

let%test "build_args with continue" =
  let args = build_args ~prompt:"do stuff" ~continue:true in
  List.equal String.equal args
    [ "gemini"; "--approval-mode=yolo"; "-o"; "stream-json"; "-r"; "latest" ]

let%test "parse_event message delta" =
  let line =
    {|{"type":"message","timestamp":"2026-04-07T22:57:19.572Z","role":"assistant","content":"hello","delta":true}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Text_delta "hello" ]

let%test "parse_event tool_use" =
  let line =
    {|{"type":"tool_use","timestamp":"...","tool_name":"run_shell_command","tool_id":"123","parameters":{"command":"ls -la"}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        { name = "Bash"; input = {|{"command":"ls -la"}|} };
    ]

let%test "parse_event result" =
  let line = {|{"type":"result","status":"success"}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Final_result
        { text = ""; stop_reason = Types.Stop_reason.End_turn };
    ]

let%test "parse_event not assistant" =
  let line =
    {|{"type":"message","timestamp":"...","role":"user","content":"hello","delta":true}|}
  in
  List.equal Types.Stream_event.equal (parse_event line) []

let%test "parse_event not delta" =
  let line =
    {|{"type":"message","timestamp":"...","role":"assistant","content":"hello","delta":false}|}
  in
  List.equal Types.Stream_event.equal (parse_event line) []

let%test "parse_event missing delta and content" =
  let line = {|{"type":"message","timestamp":"...","role":"assistant"}|} in
  List.equal Types.Stream_event.equal (parse_event line) []

let%test "parse_event empty content" =
  let line =
    {|{"type":"message","timestamp":"...","role":"assistant","content":"","delta":true}|}
  in
  List.equal Types.Stream_event.equal (parse_event line) []

let%test "parse_event alternative tools" =
  let check_tool n mapped =
    let line =
      Printf.sprintf
        {|{"type":"tool_use","tool_name":"%s","parameters":{"foo":"bar"}}|} n
    in
    List.equal Types.Stream_event.equal (parse_event line)
      [
        Types.Stream_event.Tool_use { name = mapped; input = {|{"foo":"bar"}|} };
      ]
  in
  check_tool "run_python_script" "Bash"
  && check_tool "read_file" "Read"
  && check_tool "write_file" "Write"
  && check_tool "patch_file" "Edit"
  && check_tool "some_unknown_tool" "some_unknown_tool"

let%test "parse_event error" =
  let line = {|{"type":"error","message":"internal server error"}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Error "internal server error" ]

let%test "parse_event empty error" =
  let line = {|{"type":"error"}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Error "unknown gemini error" ]

let%test "parse_event unknown payload type" =
  let line = {|{"type":"garbage","foo":"bar"}|} in
  List.equal Types.Stream_event.equal (parse_event line) []

let%test "parse_event invalid JSON" =
  List.equal Types.Stream_event.equal (parse_event "{invalid") []

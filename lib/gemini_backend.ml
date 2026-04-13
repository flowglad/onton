open Base

let build_args ~prompt ~resume_session =
  let base =
    [ "gemini"; "-p"; prompt; "--output-format"; "stream-json"; "--yolo" ]
  in
  let resume_args =
    match resume_session with
    | Some session_id -> [ "-r"; session_id ]
    | None -> []
  in
  base @ resume_args

let parse_event (line : string) : Types.Stream_event.t list =
  match Yojson.Safe.from_string line with
  | json -> (
      let open Yojson.Safe.Util in
      let typ = member "type" json |> to_string_option in
      match typ with
      | Some "init" -> (
          match member "session_id" json |> to_string_option with
          | Some id when not (String.is_empty id) ->
              [ Types.Stream_event.Session_init { session_id = id } ]
          | _ -> [])
      | Some "message" -> (
          let role = member "role" json |> to_string_option in
          match role with
          | Some "assistant" ->
              let text =
                member "content" json |> to_string_option
                |> Option.value ~default:""
              in
              if String.is_empty text then []
              else [ Types.Stream_event.Text_delta text ]
          | _ -> [])
      | Some "tool_use" ->
          let name =
            member "tool_name" json |> to_string_option
            |> Option.value ~default:""
          in
          let input =
            match member "parameters" json with
            | `Null -> ""
            | v -> Yojson.Safe.to_string v
          in
          [ Types.Stream_event.Tool_use { name; input } ]
      | Some "result" -> (
          let status = member "status" json |> to_string_option in
          match status with
          | Some "success" ->
              [
                Types.Stream_event.Final_result
                  { text = ""; stop_reason = Types.Stop_reason.End_turn };
              ]
          | Some other ->
              [ Types.Stream_event.Error (Printf.sprintf "gemini %s" other) ]
          | None -> [])
      | Some "error" ->
          let msg =
            member "message" json |> to_string_option
            |> Option.value ~default:"unknown gemini error"
          in
          [ Types.Stream_event.Error msg ]
      | _ -> [])
  | exception Yojson.Json_error _ -> []
  | exception Yojson.Safe.Util.Type_error _ -> []

let run_streaming ~process_mgr ~clock ~timeout ~cwd ~patch_id ~prompt
    ~resume_session ~on_event =
  ignore (patch_id : Types.Patch_id.t);
  let args = build_args ~prompt ~resume_session in
  let process_line line =
    let trimmed = String.strip line in
    if String.is_empty trimmed then [] else parse_event trimmed
  in
  Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~args
    ~process_line ~on_event

let create ~process_mgr ~clock ~timeout : Llm_backend.t =
  {
    name = "Gemini";
    run_streaming =
      (fun ~cwd ~patch_id ~prompt ~resume_session ~on_event ->
        run_streaming ~process_mgr ~clock ~timeout ~cwd ~patch_id ~prompt
          ~resume_session ~on_event);
  }

let%test "build_args fresh (no resume)" =
  let args = build_args ~prompt:"do stuff" ~resume_session:None in
  List.equal String.equal args
    [ "gemini"; "-p"; "do stuff"; "--output-format"; "stream-json"; "--yolo" ]

let%test "build_args with resume session" =
  let args = build_args ~prompt:"do stuff" ~resume_session:(Some "latest") in
  List.equal String.equal args
    [
      "gemini";
      "-p";
      "do stuff";
      "--output-format";
      "stream-json";
      "--yolo";
      "-r";
      "latest";
    ]

let%test "parse_event init" =
  let line =
    {|{"type":"init","timestamp":"2026-04-13T14:16:47.453Z","session_id":"abc-123","model":"gemini-3-flash-preview"}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Session_init { session_id = "abc-123" } ]

let%test "parse_event assistant message" =
  let line =
    {|{"type":"message","timestamp":"t","role":"assistant","content":"hello","delta":true}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Text_delta "hello" ]

let%test "parse_event user message is ignored" =
  let line =
    {|{"type":"message","timestamp":"t","role":"user","content":"hi"}|}
  in
  List.is_empty (parse_event line)

let%test "parse_event tool_use" =
  let line =
    {|{"type":"tool_use","timestamp":"t","tool_name":"list_directory","tool_id":"id1","parameters":{"dir_path":"."}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        { name = "list_directory"; input = {|{"dir_path":"."}|} };
    ]

let%test "parse_event tool_result is ignored" =
  let line =
    {|{"type":"tool_result","timestamp":"t","tool_id":"id1","status":"success","output":"ok"}|}
  in
  List.is_empty (parse_event line)

let%test "parse_event result success" =
  let line =
    {|{"type":"result","timestamp":"t","status":"success","stats":{}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Final_result
        { text = ""; stop_reason = Types.Stop_reason.End_turn };
    ]

let%test "parse_event error" =
  let line = {|{"type":"error","message":"rate limited"}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Error "rate limited" ]

let%test "parse_event invalid json" = List.is_empty (parse_event "not json")

let%test "parse_event unknown type" =
  List.is_empty (parse_event {|{"type":"ping"}|})

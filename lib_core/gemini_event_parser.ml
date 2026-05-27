open Base

(** Pure NDJSON event parser + CLI-arg builder for
    [gemini --output-format stream-json]. The effectful streaming driver lives
    in [Gemini_backend.run_streaming]. *)

let build_args ~model ~prompt ~resume_session =
  let base =
    [ "gemini"; "-p"; prompt; "--output-format"; "stream-json"; "--yolo" ]
  in
  let model_args =
    match model with
    | Some m when not (String.is_empty m) -> [ "-m"; m ]
    | _ -> []
  in
  let resume_args =
    match resume_session with
    | Some session_id -> [ "-r"; session_id ]
    | None -> []
  in
  base @ model_args @ resume_args

let parse_event (line : string) : Types.Stream_event.t list =
  match Yojson.Safe.from_string line with
  | exception Yojson.Json_error _ -> []
  | json -> (
      let typ = Json.string_field "type" json in
      match typ with
      | Some "init" -> (
          match Json.string_field "session_id" json with
          | Some id when not (String.is_empty id) ->
              [
                Types.Stream_event.Session_init
                  {
                    session_id = id;
                    api_key_source = None;
                    model = None;
                    claude_code_version = None;
                    permission_mode = None;
                  };
              ]
          | _ -> [])
      | Some "message" -> (
          let role = Json.string_field "role" json in
          match role with
          | Some "assistant" ->
              let text =
                Json.string_field "content" json |> Option.value ~default:""
              in
              if String.is_empty text then []
              else [ Types.Stream_event.Text_delta text ]
          | _ -> [])
      | Some "tool_use" ->
          let name =
            Json.string_field "tool_name" json |> Option.value ~default:""
          in
          let input =
            match Json.field "parameters" json with
            | None -> ""
            | Some v -> Yojson.Safe.to_string v
          in
          [ Types.Stream_event.Tool_use { name; input; status = None } ]
      | Some "result" -> (
          let status = Json.string_field "status" json in
          match status with
          | Some "success" ->
              [
                Types.Stream_event.Final_result
                  { text = ""; stop_reason = Types.Stop_reason.End_turn };
              ]
          | Some other ->
              let detail =
                match Json.string_field "message" json with
                | Some m when not (String.is_empty m) ->
                    Printf.sprintf "gemini %s: %s" other m
                | _ -> Printf.sprintf "gemini %s" other
              in
              [ Types.Stream_event.Error detail ]
          | None -> [])
      | Some "error" ->
          let msg =
            Json.string_field "message" json
            |> Option.value ~default:"unknown gemini error"
          in
          [ Types.Stream_event.Error msg ]
      | _ -> [])

let auto_model ~complexity =
  match complexity with
  | Some 1 -> Some "gemini-2.5-flash"
  | Some 2 -> Some "gemini-3-flash-preview"
  | Some 3 -> Some "gemini-3-pro-preview"
  | Some _ | None -> Some "gemini-3-pro-preview"

let%test "build_args fresh (no resume, no model)" =
  let args = build_args ~model:None ~prompt:"do stuff" ~resume_session:None in
  List.equal String.equal args
    [ "gemini"; "-p"; "do stuff"; "--output-format"; "stream-json"; "--yolo" ]

let%test "build_args with model" =
  let args =
    build_args ~model:(Some "gemini-2.5-pro") ~prompt:"do stuff"
      ~resume_session:None
  in
  List.equal String.equal args
    [
      "gemini";
      "-p";
      "do stuff";
      "--output-format";
      "stream-json";
      "--yolo";
      "-m";
      "gemini-2.5-pro";
    ]

let%test "build_args with resume session" =
  let args =
    build_args ~model:None ~prompt:"do stuff" ~resume_session:(Some "latest")
  in
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

let%test "build_args with model and resume session" =
  let args =
    build_args ~model:(Some "gemini-2.5-pro") ~prompt:"do stuff"
      ~resume_session:(Some "latest")
  in
  List.equal String.equal args
    [
      "gemini";
      "-p";
      "do stuff";
      "--output-format";
      "stream-json";
      "--yolo";
      "-m";
      "gemini-2.5-pro";
      "-r";
      "latest";
    ]

let%test "parse_event init" =
  let line =
    {|{"type":"init","timestamp":"2026-04-13T14:16:47.453Z","session_id":"abc-123","model":"gemini-3-flash-preview"}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Session_init
        {
          session_id = "abc-123";
          api_key_source = None;
          model = None;
          claude_code_version = None;
          permission_mode = None;
        };
    ]

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
        { name = "list_directory"; input = {|{"dir_path":"."}|}; status = None };
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

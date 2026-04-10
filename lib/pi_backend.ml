open Base

let build_args ~cwd_path ~patch_id ~prompt ~resume_session =
  let session_dir = cwd_path ^ "/.pi-sessions/" ^ patch_id in
  let base = [ "pi"; "-p"; prompt; "--mode"; "json" ] in
  let resume_args =
    match resume_session with
    | Some session_id -> [ "--resume"; session_id ]
    | None -> []
  in
  let session_args = [ "--session-dir"; session_dir ] in
  base @ resume_args @ session_args

let parse_event (line : string) : Types.Stream_event.t list =
  match Yojson.Safe.from_string line with
  | json -> (
      let open Yojson.Safe.Util in
      let typ = member "type" json |> to_string_option in
      match typ with
      | Some "message_update" -> (
          let evt = member "assistantMessageEvent" json in
          let evt_type = member "type" evt |> to_string_option in
          match evt_type with
          | Some "text_delta" ->
              let delta =
                member "delta" evt |> to_string_option
                |> Option.value ~default:""
              in
              if String.is_empty delta then []
              else [ Types.Stream_event.Text_delta delta ]
          | Some "toolcall_end" ->
              let tool_call = member "toolCall" evt in
              let name =
                member "name" tool_call |> to_string_option
                |> Option.value ~default:""
              in
              let input =
                match member "arguments" tool_call with
                | `Null -> ""
                | v -> Yojson.Safe.to_string v
              in
              [ Types.Stream_event.Tool_use { name; input } ]
          | _ -> [])
      | Some "agent_end" ->
          [
            Types.Stream_event.Final_result
              { text = ""; stop_reason = Types.Stop_reason.End_turn };
          ]
      | _ -> [])
  | exception Yojson.Json_error _ -> []
  | exception Yojson.Safe.Util.Type_error _ -> []

let run_streaming ~process_mgr ~clock ~timeout ~cwd ~patch_id ~prompt
    ~resume_session ~on_event =
  let cwd_path = snd cwd in
  let patch_id_str = Types.Patch_id.to_string patch_id in
  let args =
    build_args ~cwd_path ~patch_id:patch_id_str ~prompt ~resume_session
  in
  let process_line line =
    let trimmed = String.strip line in
    if String.is_empty trimmed then [] else parse_event trimmed
  in
  Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~args
    ~process_line ~on_event

let create ~process_mgr ~clock ~timeout : Llm_backend.t =
  {
    name = "Pi";
    run_streaming =
      (fun ~cwd ~patch_id ~prompt ~resume_session ~on_event ->
        run_streaming ~process_mgr ~clock ~timeout ~cwd ~patch_id ~prompt
          ~resume_session ~on_event);
  }

let%test "build_args without continue" =
  let args =
    build_args ~cwd_path:"/tmp/work" ~patch_id:"patch-1" ~prompt:"do stuff"
      ~resume_session:None
  in
  List.equal String.equal args
    [
      "pi";
      "-p";
      "do stuff";
      "--mode";
      "json";
      "--session-dir";
      "/tmp/work/.pi-sessions/patch-1";
    ]

let%test "build_args with continue" =
  let args =
    build_args ~cwd_path:"/tmp/work" ~patch_id:"patch-1" ~prompt:"do stuff"
      ~resume_session:(Some "x")
  in
  List.equal String.equal args
    [
      "pi";
      "-p";
      "do stuff";
      "--mode";
      "json";
      "--resume";
      "x";
      "--session-dir";
      "/tmp/work/.pi-sessions/patch-1";
    ]

let%test "parse_event text_delta" =
  let line =
    {|{"type":"message_update","assistantMessageEvent":{"type":"text_delta","delta":"hello"}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Text_delta "hello" ]

let%test "parse_event toolcall_end" =
  let line =
    {|{"type":"message_update","assistantMessageEvent":{"type":"toolcall_end","toolCall":{"name":"bash","arguments":{"command":"ls -la"}}}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        { name = "bash"; input = {|{"command":"ls -la"}|} };
    ]

let%test "parse_event agent_end" =
  let line = {|{"type":"agent_end","messages":[]}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Final_result
        { text = ""; stop_reason = Types.Stop_reason.End_turn };
    ]

let%test "parse_event session is ignored" =
  let line =
    {|{"type":"session","version":3,"id":"abc","timestamp":"2026-01-01T00:00:00Z","cwd":"/tmp"}|}
  in
  List.is_empty (parse_event line)

let%test "parse_event message_end stop is ignored" =
  let line =
    {|{"type":"message_end","message":{"role":"assistant","content":[],"stopReason":"stop"}}|}
  in
  List.is_empty (parse_event line)

let%test "parse_event message_end toolUse is ignored" =
  let line =
    {|{"type":"message_end","message":{"role":"assistant","content":[],"stopReason":"toolUse"}}|}
  in
  List.is_empty (parse_event line)

let%test "parse_event toolcall_start is ignored" =
  let line =
    {|{"type":"message_update","assistantMessageEvent":{"type":"toolcall_start"}}|}
  in
  List.is_empty (parse_event line)

let%test "parse_event invalid json" =
  List.is_empty (parse_event "not json at all")

let%test "parse_event unknown type" =
  List.is_empty (parse_event {|{"type":"ping"}|})

open Base

let build_args ~cwd_path ~prompt ~resume_session =
  match resume_session with
  | Some session_id ->
      [
        "codex";
        "exec";
        "resume";
        session_id;
        prompt;
        "--json";
        "--dangerously-bypass-approvals-and-sandbox";
        "-C";
        cwd_path;
      ]
  | None ->
      [
        "codex";
        "exec";
        prompt;
        "--json";
        "--dangerously-bypass-approvals-and-sandbox";
        "-C";
        cwd_path;
      ]

let parse_event (line : string) : Types.Stream_event.t list =
  match Yojson.Safe.from_string line with
  | json -> (
      let open Yojson.Safe.Util in
      let typ = member "type" json |> to_string_option in
      match typ with
      | Some "item.completed" -> (
          let item = member "item" json in
          let item_type = member "type" item |> to_string_option in
          match item_type with
          | Some "agent_message" ->
              (* Support both the modern schema ({text:"..."} on the item)
                 and the older content-array schema. *)
              let text =
                match member "text" item |> to_string_option with
                | Some t -> t
                | None ->
                    let parts =
                      match member "content" item with
                      | `List l ->
                          List.filter_map l ~f:(fun block ->
                              match member "type" block |> to_string_option with
                              | Some "output_text" ->
                                  member "text" block |> to_string_option
                              | _ -> None)
                      | _ -> []
                    in
                    String.concat ~sep:"" parts
              in
              if String.is_empty text then []
              else [ Types.Stream_event.Text_delta text ]
          | Some "command_execution" -> []
          | _ -> [])
      | Some "item.started" -> (
          let item = member "item" json in
          let item_type = member "type" item |> to_string_option in
          match item_type with
          | Some "command_execution" -> (
              match member "command" item |> to_string_option with
              | Some cmd when not (String.is_empty cmd) ->
                  (* Encode as JSON object so the renderer's input parser
                     (which expects {"command":...}) finds it. Matches the
                     convention used by every other backend. *)
                  let input =
                    Yojson.Safe.to_string (`Assoc [ ("command", `String cmd) ])
                  in
                  [ Types.Stream_event.Tool_use { name = "Bash"; input } ]
              | _ -> [])
          | _ -> [])
      | Some "thread.started" -> (
          match member "thread_id" json |> to_string_option with
          | Some id when not (String.is_empty id) ->
              [ Types.Stream_event.Session_init { session_id = id } ]
          | _ -> [])
      | Some "turn.completed" ->
          [
            Types.Stream_event.Final_result
              { text = ""; stop_reason = Types.Stop_reason.End_turn };
          ]
      | Some "error" ->
          let msg =
            member "message" json |> to_string_option
            |> Option.value ~default:"unknown codex error"
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
    name = "Codex";
    run_streaming =
      (fun ~cwd ~patch_id ~prompt ~resume_session ~on_event ->
        run_streaming ~process_mgr ~clock ~timeout ~cwd ~patch_id ~prompt
          ~resume_session ~on_event);
  }

let%test "build_args fresh (no resume)" =
  let args =
    build_args ~cwd_path:"/tmp/work" ~prompt:"do stuff" ~resume_session:None
  in
  List.equal String.equal args
    [
      "codex";
      "exec";
      "do stuff";
      "--json";
      "--dangerously-bypass-approvals-and-sandbox";
      "-C";
      "/tmp/work";
    ]

let%test "build_args with resume session passes prompt and bypass flag" =
  let args =
    build_args ~cwd_path:"/tmp/work" ~prompt:"do stuff"
      ~resume_session:(Some "sess-1")
  in
  List.equal String.equal args
    [
      "codex";
      "exec";
      "resume";
      "sess-1";
      "do stuff";
      "--json";
      "--dangerously-bypass-approvals-and-sandbox";
      "-C";
      "/tmp/work";
    ]

let%test "parse_event thread.started emits Session_init" =
  let line =
    {|{"type":"thread.started","thread_id":"019d91e0-5731-7182-ac68-19922a243e95"}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Session_init
        { session_id = "019d91e0-5731-7182-ac68-19922a243e95" };
    ]

let%test "parse_event thread.started without thread_id is ignored" =
  let line = {|{"type":"thread.started"}|} in
  List.is_empty (parse_event line)

let%test "parse_event agent_message (content-array schema)" =
  let line =
    {|{"type":"item.completed","item":{"type":"agent_message","content":[{"type":"output_text","text":"hello"}]}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Text_delta "hello" ]

let%test "parse_event agent_message (modern text-field schema)" =
  let line =
    {|{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"hello"}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Text_delta "hello" ]

let%test "parse_event command_execution started encodes input as JSON" =
  let line =
    {|{"type":"item.started","item":{"type":"command_execution","command":"ls -la"}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Tool_use
        { name = "Bash"; input = {|{"command":"ls -la"}|} };
    ]

let%test "parse_event command_execution input survives JSON parse + extract" =
  (* Mirrors the renderer's extraction in bin/main.ml: parse [input] as JSON
     and pull the "command" string. Protects parity with other backends. *)
  let line =
    {|{"type":"item.started","item":{"type":"command_execution","command":"echo \"hi\" && ls"}}|}
  in
  let expected =
    Yojson.Safe.to_string (`Assoc [ ("command", `String {|echo "hi" && ls|}) ])
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Tool_use { name = "Bash"; input = expected } ]

let%test "parse_event command_execution completed is ignored" =
  let line =
    {|{"type":"item.completed","item":{"type":"command_execution","command":"ls -la"}}|}
  in
  List.is_empty (parse_event line)

let%test "parse_event turn.completed" =
  let line = {|{"type":"turn.completed"}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Final_result
        { text = ""; stop_reason = Types.Stop_reason.End_turn };
    ]

let%test "parse_event error" =
  let line = {|{"type":"error","message":"rate limited"}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Error "rate limited" ]

let%test "parse_event invalid json" =
  List.is_empty (parse_event "not json at all")

let%test "parse_event unknown type" =
  List.is_empty (parse_event {|{"type":"ping"}|})

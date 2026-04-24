open Base

(** Compiled regex for ANSI escape sequences, carriage returns, and stray
    control characters. Retained as defense-in-depth so any incidental control
    bytes in Claude's stream-json output don't break JSON parsing. Matches:
    - CSI sequences (ESC\[ ... letter)
    - OSC sequences (ESC\] ... BEL/ST)
    - Bare \r
    - C0 control characters (0x00–0x1F except \n and \t). Compiled once at
      module load. *)
let ansi_re =
  let esc = Re.char '\x1b' in
  let csi =
    Re.seq [ esc; Re.set "[("; Re.rep (Re.set "0123456789;?"); Re.rg 'A' 'z' ]
  in
  let osc =
    Re.seq
      [
        esc;
        Re.set "]>";
        Re.rep (Re.compl [ Re.char '\x07'; Re.char '\x1b' ]);
        Re.opt (Re.alt [ Re.char '\x07'; Re.seq [ esc; Re.char '\\' ] ]);
      ]
  in
  let cr = Re.char '\r' in
  (* C0 control chars except \t (0x09) and \n (0x0a) — covers ^D, BS, etc. *)
  let c0 =
    Re.alt
      [
        Re.rg '\x00' '\x08';
        Re.rg '\x0b' '\x0c';
        Re.rg '\x0e' '\x1a';
        (* 0x1b = ESC is handled by csi/osc above *)
        Re.rg '\x1c' '\x1f';
      ]
  in
  Re.compile (Re.alt [ csi; osc; cr; c0 ])

(** Strip ANSI escape sequences and stray control characters. *)
let strip_ansi s = Re.replace_string ansi_re ~by:"" s

let build_args ~prompt ~resume_session =
  let base = [ "claude"; "-p"; prompt; "--output-format"; "text" ] in
  let session_args =
    match resume_session with Some id -> [ "--resume"; id ] | None -> []
  in
  let flags = [ "--dangerously-skip-permissions"; "--max-turns"; "200" ] in
  base @ session_args @ flags

let build_stream_args ~prompt ~resume_session =
  let base =
    [ "claude"; "-p"; prompt; "--output-format"; "stream-json"; "--verbose" ]
  in
  let session_args =
    match resume_session with Some id -> [ "--resume"; id ] | None -> []
  in
  let flags = [ "--dangerously-skip-permissions"; "--max-turns"; "200" ] in
  base @ session_args @ flags

(** Find the first '\{' in [s] and return the substring starting there. Defense
    against any leading garbage in a stream-json line. *)
let find_json_start s =
  match String.lfindi s ~f:(fun _ c -> Char.equal c '{') with
  | Some 0 | None -> s
  | Some n -> String.drop_prefix s n

let parse_stream_events (line : string) : Types.Stream_event.t list =
  match Yojson.Safe.from_string (find_json_start line) with
  | json -> (
      let open Yojson.Safe.Util in
      let typ = member "type" json |> to_string_option in
      match typ with
      | Some "content_block_delta" -> (
          let delta = member "delta" json in
          let delta_type = member "type" delta |> to_string_option in
          match delta_type with
          | Some "text_delta" ->
              let text =
                member "text" delta |> to_string_option
                |> Option.value ~default:""
              in
              [ Types.Stream_event.Text_delta text ]
          | _ -> [])
      | Some "content_block_start" -> (
          let content_block = member "content_block" json in
          let block_type = member "type" content_block |> to_string_option in
          match block_type with
          | Some "tool_use" ->
              let name =
                member "name" content_block
                |> to_string_option |> Option.value ~default:""
              in
              [
                Types.Stream_event.Tool_use { name; input = ""; status = None };
              ]
          | _ -> [])
      | Some "assistant" ->
          (* --verbose format: assistant event contains message.content array *)
          let content =
            match member "message" json |> member "content" with
            | `List l -> l
            | _ -> []
          in
          List.filter_map content ~f:(fun block ->
              let block_type = member "type" block |> to_string_option in
              match block_type with
              | Some "tool_use" ->
                  let name =
                    member "name" block |> to_string_option
                    |> Option.value ~default:""
                  in
                  let input =
                    match member "input" block with
                    | `Null -> ""
                    | v -> Yojson.Safe.to_string v
                  in
                  Some
                    (Types.Stream_event.Tool_use { name; input; status = None })
              | Some "text" ->
                  let text =
                    member "text" block |> to_string_option
                    |> Option.value ~default:""
                  in
                  Some (Types.Stream_event.Text_delta text)
              | _ -> None)
      | Some "message_delta" -> (
          let delta = member "delta" json in
          let raw_reason =
            member "stop_reason" delta |> to_string_option
            |> Option.value ~default:""
          in
          match Types.Stop_reason.of_string raw_reason with
          | None -> []
          | Some stop_reason ->
              [ Types.Stream_event.Final_result { text = ""; stop_reason } ])
      | Some "result" ->
          let is_error =
            member "is_error" json |> to_bool_option
            |> Option.value ~default:false
          in
          (* The result event also carries session_id — extract it as a
             fallback in case the system/init event was lost. *)
          let session_init =
            match member "session_id" json |> to_string_option with
            | Some sid ->
                [ Types.Stream_event.Session_init { session_id = sid } ]
            | None -> []
          in
          if is_error then
            let errors =
              match member "errors" json with
              | `List items ->
                  List.filter_map items ~f:(fun item -> to_string_option item)
              | _ -> []
            in
            let msg =
              match errors with
              | [] -> "unknown error"
              | errs -> String.concat ~sep:"; " errs
            in
            session_init @ [ Types.Stream_event.Error msg ]
          else
            let text =
              member "result" json |> to_string_option
              |> Option.value ~default:""
            in
            let raw_reason =
              member "stop_reason" json |> to_string_option
              |> Option.value ~default:"end_turn"
            in
            let stop_reason =
              Types.Stop_reason.of_string raw_reason
              |> Option.value ~default:Types.Stop_reason.End_turn
            in
            session_init
            @ [ Types.Stream_event.Final_result { text; stop_reason } ]
      | Some "error" ->
          let err =
            member "error" json |> member "message" |> to_string_option
            |> Option.value ~default:"unknown error"
          in
          [ Types.Stream_event.Error err ]
      | Some "system" -> (
          let subtype = member "subtype" json |> to_string_option in
          match subtype with
          | Some "init" -> (
              match member "session_id" json |> to_string_option with
              | Some session_id ->
                  [ Types.Stream_event.Session_init { session_id } ]
              | None -> [])
          | _ -> [])
      | _ -> [])
  | exception Yojson.Json_error _ -> []
  | exception Yojson.Safe.Util.Type_error _ -> []

(** Backward-compatible wrapper returning the first parsed event. *)
let parse_stream_event (line : string) : Types.Stream_event.t option =
  match parse_stream_events line with [] -> None | e :: _ -> Some e

let run ~process_mgr ~cwd ~patch_id ~prompt ~resume_session =
  ignore (patch_id : Types.Patch_id.t);
  let args = build_args ~prompt ~resume_session in
  let stdout_content, stderr_content, exit_code =
    Eio.Switch.run @@ fun sw ->
    let stdin_r, stdin_w = Eio.Process.pipe ~sw process_mgr in
    let stdout_r, stdout_w = Eio.Process.pipe ~sw process_mgr in
    let stderr_r, stderr_w = Eio.Process.pipe ~sw process_mgr in
    let child =
      Eio.Process.spawn ~sw process_mgr ~cwd ~stdin:stdin_r ~stdout:stdout_w
        ~stderr:stderr_w args
    in
    Eio.Switch.on_release sw (fun () ->
        try Eio.Process.signal child Stdlib.Sys.sigterm with _ -> ());
    Eio.Flow.close stdin_r;
    Eio.Flow.close stdin_w;
    Eio.Flow.close stdout_w;
    Eio.Flow.close stderr_w;
    let stdout_buf = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stdout_r in
    let stderr_buf = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stderr_r in
    let drain flow =
      let buf = Bytes.create 4096 in
      try
        while true do
          ignore (Eio.Flow.single_read flow (Cstruct.of_bytes buf))
        done
      with End_of_file -> ()
    in
    let out, err =
      Eio.Fiber.pair
        (fun () ->
          try Eio.Buf_read.take_all stdout_buf
          with Eio.Buf_read.Buffer_limit_exceeded ->
            drain stdout_r;
            "<stdout exceeded 1MB limit, truncated>")
        (fun () ->
          try Eio.Buf_read.take_all stderr_buf
          with Eio.Buf_read.Buffer_limit_exceeded ->
            drain stderr_r;
            "<stderr exceeded 1MB limit, truncated>")
    in
    let status = Eio.Process.await child in
    let code = match status with `Exited c -> c | `Signaled s -> 128 + s in
    (out, err, code)
  in
  let cleaned_stdout = strip_ansi stdout_content in
  let got_events = not (String.is_empty (String.strip cleaned_stdout)) in
  {
    Llm_backend.exit_code;
    stdout = cleaned_stdout;
    stderr = stderr_content;
    got_events;
    saw_final_result = false;
    timed_out = false;
  }

let run_streaming ~process_mgr ~clock ~timeout ~cwd ~patch_id ~prompt
    ~resume_session ~on_event =
  ignore (patch_id : Types.Patch_id.t);
  let args = build_stream_args ~prompt ~resume_session in
  let process_line line =
    let trimmed = strip_ansi (String.strip line) in
    if String.is_empty trimmed then [] else parse_stream_events trimmed
  in
  Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~args
    ~process_line ~on_event

let%test "build_args fresh (no resume)" =
  let args = build_args ~prompt:"do stuff" ~resume_session:None in
  List.equal String.equal args
    [
      "claude";
      "-p";
      "do stuff";
      "--output-format";
      "text";
      "--dangerously-skip-permissions";
      "--max-turns";
      "200";
    ]

let%test "build_args with resume session" =
  let args = build_args ~prompt:"do stuff" ~resume_session:(Some "abc-123") in
  List.equal String.equal args
    [
      "claude";
      "-p";
      "do stuff";
      "--output-format";
      "text";
      "--resume";
      "abc-123";
      "--dangerously-skip-permissions";
      "--max-turns";
      "200";
    ]

let%test "build_stream_args fresh (no resume)" =
  let args = build_stream_args ~prompt:"do stuff" ~resume_session:None in
  List.equal String.equal args
    [
      "claude";
      "-p";
      "do stuff";
      "--output-format";
      "stream-json";
      "--verbose";
      "--dangerously-skip-permissions";
      "--max-turns";
      "200";
    ]

let%test "build_stream_args with resume session" =
  let args =
    build_stream_args ~prompt:"do stuff" ~resume_session:(Some "abc-123")
  in
  List.equal String.equal args
    [
      "claude";
      "-p";
      "do stuff";
      "--output-format";
      "stream-json";
      "--verbose";
      "--resume";
      "abc-123";
      "--dangerously-skip-permissions";
      "--max-turns";
      "200";
    ]

let%test "strip_ansi removes escape sequences" =
  String.equal (strip_ansi "\027[31mhello\027[0m") "hello"

let%test "strip_ansi passes clean text through" =
  String.equal (strip_ansi "hello world") "hello world"

let%test "strip_ansi removes backspaces" =
  String.equal (strip_ansi "\x08\x08hello") "hello"

let%test "strip_ansi removes NUL and other C0 chars" =
  String.equal (strip_ansi "\x04\x00\x01hello") "hello"

let%test "parse_stream_event text_delta" =
  let line =
    {|{"type":"content_block_delta","delta":{"type":"text_delta","text":"hello"}}|}
  in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some (Types.Stream_event.Text_delta "hello"))

let%test "parse_stream_event tool_use" =
  let line =
    {|{"type":"content_block_start","content_block":{"type":"tool_use","name":"write_file"}}|}
  in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some
       (Types.Stream_event.Tool_use
          { name = "write_file"; input = ""; status = None }))

let%test "parse_stream_event error" =
  let line = {|{"type":"error","error":{"message":"rate limited"}}|} in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some (Types.Stream_event.Error "rate limited"))

let%test "parse_stream_event result" =
  let line = {|{"type":"result","result":"done","stop_reason":"end_turn"}|} in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some
       (Types.Stream_event.Final_result
          { text = "done"; stop_reason = Types.Stop_reason.End_turn }))

let%test "parse_stream_event message_delta with stop_reason" =
  let line = {|{"type":"message_delta","delta":{"stop_reason":"end_turn"}}|} in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some
       (Types.Stream_event.Final_result
          { text = ""; stop_reason = Types.Stop_reason.End_turn }))

let%test "parse_stream_event error result" =
  let line =
    {|{"type":"result","subtype":"error_during_execution","is_error":true,"errors":["bad session ID"]}|}
  in
  Option.equal Types.Stream_event.equal (parse_stream_event line)
    (Some (Types.Stream_event.Error "bad session ID"))

let%test "parse_stream_event invalid json returns None" =
  Option.is_none (parse_stream_event "not json at all")

let%test "parse_stream_event unknown type returns None" =
  Option.is_none (parse_stream_event {|{"type":"ping"}|})

let%test "parse_stream_events extracts session_id from system/init" =
  let line =
    {|{"type":"system","subtype":"init","session_id":"d3c2d71e-0399-4ed3-810a-9c1edce7dd00","tools":[]}|}
  in
  List.equal Types.Stream_event.equal (parse_stream_events line)
    [
      Types.Stream_event.Session_init
        { session_id = "d3c2d71e-0399-4ed3-810a-9c1edce7dd00" };
    ]

let%test "parse_stream_events ignores system events without init subtype" =
  let line = {|{"type":"system","subtype":"heartbeat"}|} in
  List.is_empty (parse_stream_events line)

let%test "parse_stream_events ignores system/init without session_id" =
  let line = {|{"type":"system","subtype":"init"}|} in
  List.is_empty (parse_stream_events line)

let%test "parse_stream_events tolerates leading garbage before JSON" =
  (* Defense-in-depth: find_json_start skips any non-'{' prefix. *)
  let line =
    "^D\x08\x08{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"abc-123\",\"tools\":[]}"
  in
  List.equal Types.Stream_event.equal (parse_stream_events line)
    [ Types.Stream_event.Session_init { session_id = "abc-123" } ]

let%test "parse_stream_events extracts session_id from result event" =
  let line =
    {|{"type":"result","result":"done","stop_reason":"end_turn","session_id":"def-456"}|}
  in
  List.equal Types.Stream_event.equal (parse_stream_events line)
    [
      Types.Stream_event.Session_init { session_id = "def-456" };
      Types.Stream_event.Final_result
        { text = "done"; stop_reason = Types.Stop_reason.End_turn };
    ]

let%test "parse_stream_events result without session_id omits Session_init" =
  let line = {|{"type":"result","result":"done","stop_reason":"end_turn"}|} in
  List.equal Types.Stream_event.equal (parse_stream_events line)
    [
      Types.Stream_event.Final_result
        { text = "done"; stop_reason = Types.Stop_reason.End_turn };
    ]

let%test "find_json_start strips leading junk" =
  String.equal (find_json_start "^D\x08\x08{\"type\":\"x\"}") "{\"type\":\"x\"}"

let%test "find_json_start passthrough when clean" =
  String.equal (find_json_start "{\"type\":\"x\"}") "{\"type\":\"x\"}"

let%test "find_json_start returns input when no brace" =
  String.equal (find_json_start "no json here") "no json here"

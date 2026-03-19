open Base

(** Compiled regex for ANSI escape sequences and carriage returns. Matches CSI
    sequences (ESC\[ ... letter), OSC sequences (ESC\] ... BEL/ST), and bare \r
    from PTY line endings. Compiled once at module load. *)
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
  Re.compile (Re.alt [ csi; osc; cr ])

(** Strip ANSI escape sequences injected by the PTY wrapper. *)
let strip_ansi s = Re.replace_string ansi_re ~by:"" s

(** Wrap a command in a pseudo-TTY via [script].

    Claude CLI requires a TTY to produce stream-json output when not in
    [--print] mode. We use [script -q /dev/null] on macOS to allocate a PTY
    without writing a transcript file. *)
let pty_wrap args =
  (* macOS: script -q /dev/null <cmd> <args...> *)
  "/usr/bin/script" :: "-q" :: "/dev/null" :: args

let build_args ~prompt ~continue =
  let base = [ "claude"; "-p"; prompt; "--output-format"; "text" ] in
  let session_args = if continue then [ "--continue" ] else [] in
  let flags = [ "--dangerously-skip-permissions"; "--max-turns"; "200" ] in
  base @ session_args @ flags

let build_stream_args ~prompt ~continue =
  let base =
    [ "claude"; "-p"; prompt; "--output-format"; "stream-json"; "--verbose" ]
  in
  let session_args = if continue then [ "--continue" ] else [] in
  let flags = [ "--dangerously-skip-permissions"; "--max-turns"; "200" ] in
  base @ session_args @ flags

let parse_stream_events (line : string) : Types.Stream_event.t list =
  match Yojson.Safe.from_string line with
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
              [ Types.Stream_event.Tool_use { name; input = "" } ]
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
                  Some (Types.Stream_event.Tool_use { name; input })
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
            [ Types.Stream_event.Error msg ]
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
            [ Types.Stream_event.Final_result { text; stop_reason } ]
      | Some "error" ->
          let err =
            member "error" json |> member "message" |> to_string_option
            |> Option.value ~default:"unknown error"
          in
          [ Types.Stream_event.Error err ]
      | _ -> [])
  | exception Yojson.Json_error _ -> []

(** Backward-compatible wrapper returning the first parsed event. *)
let parse_stream_event (line : string) : Types.Stream_event.t option =
  match parse_stream_events line with [] -> None | e :: _ -> Some e

let run ~process_mgr ~cwd ~patch_id ~prompt ~continue =
  ignore (patch_id : Types.Patch_id.t);
  let args = pty_wrap (build_args ~prompt ~continue) in
  let stdout_content, stderr_content, exit_code =
    Eio.Switch.run @@ fun sw ->
    let stdin_r, stdin_w = Eio.Process.pipe ~sw process_mgr in
    let stdout_r, stdout_w = Eio.Process.pipe ~sw process_mgr in
    let stderr_r, stderr_w = Eio.Process.pipe ~sw process_mgr in
    let child =
      Eio.Process.spawn ~sw process_mgr ~cwd ~stdin:stdin_r ~stdout:stdout_w
        ~stderr:stderr_w args
    in
    Eio.Flow.close stdin_r;
    Eio.Flow.close stdout_w;
    Eio.Flow.close stderr_w;
    let stdout_buf = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stdout_r in
    let stderr_buf = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stderr_r in
    let out, err =
      Eio.Fiber.pair
        (fun () -> Eio.Buf_read.take_all stdout_buf)
        (fun () -> Eio.Buf_read.take_all stderr_buf)
    in
    let status = Eio.Process.await child in
    Eio.Flow.close stdin_w;
    let code = match status with `Exited c -> c | `Signaled s -> 128 + s in
    (out, err, code)
  in
  {
    Llm_backend.exit_code;
    stdout = strip_ansi stdout_content;
    stderr = stderr_content;
    got_events = true;
  }

let run_streaming ~process_mgr ~cwd ~patch_id ~prompt ~continue ~on_event =
  ignore (patch_id : Types.Patch_id.t);
  let args = pty_wrap (build_stream_args ~prompt ~continue) in
  let stderr_content, exit_code, got_events =
    Eio.Switch.run @@ fun sw ->
    let stdin_r, stdin_w = Eio.Process.pipe ~sw process_mgr in
    let stdout_r, stdout_w = Eio.Process.pipe ~sw process_mgr in
    let stderr_r, stderr_w = Eio.Process.pipe ~sw process_mgr in
    let child =
      Eio.Process.spawn ~sw process_mgr ~cwd ~stdin:stdin_r ~stdout:stdout_w
        ~stderr:stderr_w args
    in
    Eio.Flow.close stdin_r;
    Eio.Flow.close stdout_w;
    Eio.Flow.close stderr_w;
    let stdout_buf = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stdout_r in
    let stderr_buf = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stderr_r in
    let err_ref = ref "" in
    let got_events_ref = ref false in
    Eio.Fiber.both
      (fun () ->
        let rec read_lines () =
          match Eio.Buf_read.line stdout_buf with
          | line ->
              let trimmed = strip_ansi (String.strip line) in
              if not (String.is_empty trimmed) then (
                let events = parse_stream_events trimmed in
                if not (List.is_empty events) then got_events_ref := true;
                List.iter events ~f:on_event);
              read_lines ()
          | exception End_of_file -> ()
        in
        read_lines ())
      (fun () -> err_ref := Eio.Buf_read.take_all stderr_buf);
    let status = Eio.Process.await child in
    Eio.Flow.close stdin_w;
    let code = match status with `Exited c -> c | `Signaled s -> 128 + s in
    (!err_ref, code, !got_events_ref)
  in
  { Llm_backend.exit_code; stdout = ""; stderr = stderr_content; got_events }

let%test "build_args without continue" =
  let args = build_args ~prompt:"do stuff" ~continue:false in
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

let%test "build_args with continue" =
  let args = build_args ~prompt:"do stuff" ~continue:true in
  List.equal String.equal args
    [
      "claude";
      "-p";
      "do stuff";
      "--output-format";
      "text";
      "--continue";
      "--dangerously-skip-permissions";
      "--max-turns";
      "200";
    ]

let%test "build_stream_args without continue" =
  let args = build_stream_args ~prompt:"do stuff" ~continue:false in
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

let%test "build_stream_args with continue" =
  let args = build_stream_args ~prompt:"do stuff" ~continue:true in
  List.equal String.equal args
    [
      "claude";
      "-p";
      "do stuff";
      "--output-format";
      "stream-json";
      "--verbose";
      "--continue";
      "--dangerously-skip-permissions";
      "--max-turns";
      "200";
    ]

let%test "strip_ansi removes escape sequences" =
  String.equal (strip_ansi "\027[31mhello\027[0m") "hello"

let%test "strip_ansi passes clean text through" =
  String.equal (strip_ansi "hello world") "hello world"

let%test "pty_wrap prepends script command" =
  let args = pty_wrap [ "claude"; "-p"; "hello" ] in
  List.equal String.equal args
    [ "/usr/bin/script"; "-q"; "/dev/null"; "claude"; "-p"; "hello" ]

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
    (Some (Types.Stream_event.Tool_use { name = "write_file"; input = "" }))

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

open Base

type result = {
  session_id : Types.Session_id.t;
  exit_code : int;
  stdout : string;
  stderr : string;
}
[@@deriving show, eq, sexp_of, compare]

let generate_session_id () =
  (* Random.bits() yields 30 bits per call; combine calls for full-width fields *)
  let r1 = Random.bits () in
  let r2 = Random.bits () in
  let r3 = Random.bits () in
  let r4 = Random.bits () in
  let r5 = Random.bits () in
  let last_raw = (r4 lsr 16) lor (r5 lsl 14) in
  let last_field = last_raw land 0xFFFFFFFFFFFF in
  let uuid =
    Printf.sprintf "%08x-%04x-%04x-%04x-%012x"
      ((r1 lsl 2) lor (r2 land 0x3))
      ((r2 lsr 2) land 0xFFFF)
      (r3 land 0xFFFF) (r4 land 0xFFFF) last_field
  in
  Types.Session_id.of_string uuid

let build_args ~prompt ~session_id =
  let base = [ "claude"; "--print"; "--output-format"; "text" ] in
  let session_args =
    match session_id with
    | Some id -> [ "--resume"; Types.Session_id.to_string id ]
    | None -> []
  in
  base @ session_args @ [ "--"; prompt ]

let build_stream_args ~prompt ~session_id =
  let base = [ "claude"; "--print"; "--output-format"; "stream-json" ] in
  let session_args =
    match session_id with
    | Some id -> [ "--resume"; Types.Session_id.to_string id ]
    | None -> []
  in
  base @ session_args @ [ "--"; prompt ]

let parse_stream_event (line : string) : Types.Stream_event.t option =
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
              Some (Types.Stream_event.Text_delta text)
          | _ -> None)
      | Some "content_block_start" -> (
          let content_block = member "content_block" json in
          let block_type = member "type" content_block |> to_string_option in
          match block_type with
          | Some "tool_use" ->
              let name =
                member "name" content_block
                |> to_string_option |> Option.value ~default:""
              in
              Some (Types.Stream_event.Tool_use { name; input = "" })
          | _ -> None)
      | Some "message_stop" | Some "message_delta" -> (
          let delta = member "delta" json in
          let raw_reason =
            member "stop_reason" delta |> to_string_option
            |> Option.value ~default:""
          in
          match Types.Stop_reason.of_string raw_reason with
          | None -> None
          | Some stop_reason ->
              Some (Types.Stream_event.Final_result { text = ""; stop_reason }))
      | Some "result" ->
          let text =
            member "result" json |> to_string_option |> Option.value ~default:""
          in
          let raw_reason =
            member "stop_reason" json |> to_string_option
            |> Option.value ~default:"end_turn"
          in
          let stop_reason =
            Types.Stop_reason.of_string raw_reason
            |> Option.value ~default:Types.Stop_reason.End_turn
          in
          Some (Types.Stream_event.Final_result { text; stop_reason })
      | Some "error" ->
          let err =
            member "error" json |> member "message" |> to_string_option
            |> Option.value ~default:"unknown error"
          in
          Some (Types.Stream_event.Error err)
      | _ -> None)
  | exception Yojson.Json_error _ -> None

let run ~process_mgr ~cwd ~patch_id ~prompt ~session_id =
  ignore (patch_id : Types.Patch_id.t);
  let fallback_id =
    match session_id with Some id -> id | None -> generate_session_id ()
  in
  let args = build_args ~prompt ~session_id in
  let stdout_content, stderr_content, exit_code =
    Eio.Switch.run @@ fun sw ->
    let stdout_r, stdout_w = Eio.Process.pipe ~sw process_mgr in
    let stderr_r, stderr_w = Eio.Process.pipe ~sw process_mgr in
    let child =
      Eio.Process.spawn ~sw process_mgr ~cwd ~stdout:stdout_w ~stderr:stderr_w
        args
    in
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
    let code = match status with `Exited c -> c | `Signaled s -> 128 + s in
    (out, err, code)
  in
  {
    session_id = fallback_id;
    exit_code;
    stdout = stdout_content;
    stderr = stderr_content;
  }

let run_streaming ~process_mgr ~cwd ~patch_id ~prompt ~session_id ~on_event =
  ignore (patch_id : Types.Patch_id.t);
  let fallback_id =
    match session_id with Some id -> id | None -> generate_session_id ()
  in
  let args = build_stream_args ~prompt ~session_id in
  let stderr_content, exit_code =
    Eio.Switch.run @@ fun sw ->
    let stdout_r, stdout_w = Eio.Process.pipe ~sw process_mgr in
    let stderr_r, stderr_w = Eio.Process.pipe ~sw process_mgr in
    let child =
      Eio.Process.spawn ~sw process_mgr ~cwd ~stdout:stdout_w ~stderr:stderr_w
        args
    in
    Eio.Flow.close stdout_w;
    Eio.Flow.close stderr_w;
    let stdout_buf = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stdout_r in
    let stderr_buf = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stderr_r in
    let err_ref = ref "" in
    Eio.Fiber.both
      (fun () ->
        let rec read_lines () =
          match Eio.Buf_read.line stdout_buf with
          | line ->
              let trimmed = String.strip line in
              (if not (String.is_empty trimmed) then
                 match parse_stream_event trimmed with
                 | Some event -> on_event event
                 | None -> ());
              read_lines ()
          | exception End_of_file -> ()
        in
        read_lines ())
      (fun () -> err_ref := Eio.Buf_read.take_all stderr_buf);
    let status = Eio.Process.await child in
    let code = match status with `Exited c -> c | `Signaled s -> 128 + s in
    (!err_ref, code)
  in
  { session_id = fallback_id; exit_code; stdout = ""; stderr = stderr_content }

let%test "build_args without session" =
  let args = build_args ~prompt:"do stuff" ~session_id:None in
  List.equal String.equal args
    [ "claude"; "--print"; "--output-format"; "text"; "--"; "do stuff" ]

let%test "build_args with session" =
  let sid = Types.Session_id.of_string "abc-123" in
  let args = build_args ~prompt:"do stuff" ~session_id:(Some sid) in
  List.equal String.equal args
    [
      "claude";
      "--print";
      "--output-format";
      "text";
      "--resume";
      "abc-123";
      "--";
      "do stuff";
    ]

let%test "generate_session_id produces non-empty string" =
  let id = generate_session_id () in
  not (String.is_empty (Types.Session_id.to_string id))

let%test "generate_session_id produces unique values across many calls" =
  let ids =
    List.init 1000 ~f:(fun _ ->
        Types.Session_id.to_string (generate_session_id ()))
  in
  let unique_count = Set.length (Set.of_list (module String) ids) in
  unique_count = 1000

let%test "build_stream_args without session" =
  let args = build_stream_args ~prompt:"do stuff" ~session_id:None in
  List.equal String.equal args
    [ "claude"; "--print"; "--output-format"; "stream-json"; "--"; "do stuff" ]

let%test "build_stream_args with session" =
  let sid = Types.Session_id.of_string "abc-123" in
  let args = build_stream_args ~prompt:"do stuff" ~session_id:(Some sid) in
  List.equal String.equal args
    [
      "claude";
      "--print";
      "--output-format";
      "stream-json";
      "--resume";
      "abc-123";
      "--";
      "do stuff";
    ]

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

let%test "parse_stream_event invalid json returns None" =
  Option.is_none (parse_stream_event "not json at all")

let%test "parse_stream_event unknown type returns None" =
  Option.is_none (parse_stream_event {|{"type":"ping"}|})

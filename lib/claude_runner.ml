open Base

type result = {
  session_id : Types.Session_id.t;
  exit_code : int;
  stdout : string;
  stderr : string;
}
[@@deriving show, eq, sexp_of, compare]

let generate_session_id () =
  let uuid =
    Printf.sprintf "%08x-%04x-%04x-%04x-%012x"
      (Random.bits () land 0xFFFFFFFF)
      (Random.bits () land 0xFFFF)
      (Random.bits () land 0xFFFF)
      (Random.bits () land 0xFFFF)
      (Random.bits () land 0xFFFFFFFFFFFF)
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

let run ~process_mgr ~cwd ~patch_id:_ ~prompt ~session_id =
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
    let out = Eio.Buf_read.take_all stdout_buf in
    let err = Eio.Buf_read.take_all stderr_buf in
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

let%test "generate_session_id produces unique values" =
  let id1 = generate_session_id () in
  let id2 = generate_session_id () in
  not
    (String.equal
       (Types.Session_id.to_string id1)
       (Types.Session_id.to_string id2))

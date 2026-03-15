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

open Base

type result = {
  exit_code : int;
  stdout : string;
  stderr : string;
  got_events : bool;
  timed_out : bool;
}
[@@deriving show, eq, sexp_of, compare]

let spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~args
    ~(process_line : string -> Types.Stream_event.t list) ~on_event =
  let run () =
    let stderr_content, exit_code, got_events =
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
      let err_ref = ref "" in
      let got_events_ref = ref false in
      Eio.Fiber.both
        (fun () ->
          let rec read_lines () =
            match Eio.Buf_read.line stdout_buf with
            | line ->
                let events = process_line line in
                if not (List.is_empty events) then got_events_ref := true;
                List.iter events ~f:on_event;
                read_lines ()
            | exception End_of_file -> ()
          in
          read_lines ())
        (fun () ->
          try err_ref := Eio.Buf_read.take_all stderr_buf
          with Eio.Buf_read.Buffer_limit_exceeded ->
            err_ref := "<stderr exceeded 1MB limit, truncated>");
      let status = Eio.Process.await child in
      let code = match status with `Exited c -> c | `Signaled s -> 128 + s in
      (!err_ref, code, !got_events_ref)
    in
    Ok
      {
        exit_code;
        stdout = "";
        stderr = stderr_content;
        got_events;
        timed_out = false;
      }
  in
  match Eio.Time.with_timeout clock timeout run with
  | Ok result -> result
  | Error `Timeout ->
      {
        exit_code = 128 + 15 (* SIGTERM — sent via on_release hook *);
        stdout = "";
        stderr = "process timed out";
        got_events = false;
        timed_out = true;
      }

type t = {
  name : string;
  run_streaming :
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    patch_id:Types.Patch_id.t ->
    prompt:string ->
    continue:bool ->
    on_event:(Types.Stream_event.t -> unit) ->
    result;
}

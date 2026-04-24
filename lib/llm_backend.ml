open Base

type result = {
  exit_code : int;
  stdout : string;
  stderr : string;
  got_events : bool;
  saw_final_result : bool;
  timed_out : bool;
}
[@@deriving show, eq, sexp_of, compare]

(* Signal the whole process group led by [pid]. Requires the child to have
   called [setsid()] at exec-time; otherwise [-pid] refers to onton's own
   group and we'd signal ourselves, so we only call this when we spawned
   through the setsid shim. ESRCH/EPERM are swallowed — they mean the group
   is already gone or we lost the race. *)
let kill_group ~pid ~signal =
  try Unix.kill (-pid) signal
  with Unix.Unix_error ((ESRCH | EPERM), _, _) -> ()

let spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~setsid_exec ~args
    ~(process_line : string -> Types.Stream_event.t list) ~on_event =
  let args =
    match setsid_exec with Some path -> path :: args | None -> args
  in
  let saw_final_result_ref = ref false in
  let got_events_ref = ref false in
  let run () =
    let stderr_content, exit_code =
      Eio.Switch.run @@ fun sw ->
      let stdin_r, stdin_w = Eio.Process.pipe ~sw process_mgr in
      let stdout_r, stdout_w = Eio.Process.pipe ~sw process_mgr in
      let stderr_r, stderr_w = Eio.Process.pipe ~sw process_mgr in
      let child =
        Eio.Process.spawn ~sw process_mgr ~cwd ~stdin:stdin_r ~stdout:stdout_w
          ~stderr:stderr_w args
      in
      let pid = Eio.Process.pid child in
      let have_group = Option.is_some setsid_exec in
      let signal_tree signal =
        if have_group then kill_group ~pid ~signal
        else try Eio.Process.signal child signal with _ -> ()
      in
      Eio.Switch.on_release sw (fun () ->
          (* Release path fires on timeout / cancellation / exception. Be
             firm: SIGKILL the whole group (or just the direct child in the
             no-shim fallback) rather than SIGTERM, since by this point
             we've already given up on graceful exit. *)
          signal_tree Stdlib.Sys.sigkill);
      Eio.Flow.close stdin_r;
      Eio.Flow.close stdin_w;
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
                let events = process_line line in
                if not (List.is_empty events) then got_events_ref := true;
                List.iter events ~f:on_event;
                let saw_final =
                  List.exists events ~f:(function
                    | Types.Stream_event.Final_result _ -> true
                    | Types.Stream_event.Text_delta _
                    | Types.Stream_event.Tool_use _ | Types.Stream_event.Error _
                    | Types.Stream_event.Session_init _ ->
                        false)
                in
                if saw_final then saw_final_result_ref := true
                else read_lines ()
            | exception End_of_file -> ()
          in
          read_lines ();
          if !saw_final_result_ref then (
            (* Tear down immediately: the model's turn is over. Descendants
               of the child (e.g. Bash-tool zsh processes) may keep the
               inherited stderr write-end open indefinitely, which would
               leave the sibling [take_all] fiber blocked on EOF. Signal
               the whole group (when available) and close our read end so
               the fiber unblocks. *)
            signal_tree Stdlib.Sys.sigterm;
            try Eio.Flow.close stderr_r with _ -> ()))
        (fun () ->
          (* Only swallow the expected teardown exceptions: Eio.Buf_read
             raises when the buffer fills up; End_of_file and Eio.Exn.Io
             fire when the stdout fiber closes stderr_r out from under us
             after saw_final_result. Letting anything else propagate — in
             particular Eio.Cancel.Cancelled — is required so this fiber
             can honour cancellation and release Eio.Fiber.both. *)
          try err_ref := Eio.Buf_read.take_all stderr_buf with
          | Eio.Buf_read.Buffer_limit_exceeded -> (
              err_ref := "<stderr exceeded 1MB limit, truncated>";
              let drain_buf = Bytes.create 4096 in
              try
                while true do
                  ignore
                    (Eio.Flow.single_read stderr_r (Cstruct.of_bytes drain_buf))
                done
              with
              | End_of_file -> ()
              | Eio.Exn.Io _ -> ())
          | End_of_file -> ()
          | Eio.Exn.Io _ -> ());
      let status =
        if !saw_final_result_ref then
          (* SIGTERM was just delivered; cap the await so a child that
             ignores it doesn't stall us, then escalate to SIGKILL. *)
          match
            Eio.Time.with_timeout clock 2.0 (fun () ->
                Ok (Eio.Process.await child))
          with
          | Ok status -> status
          | Error `Timeout -> (
              signal_tree Stdlib.Sys.sigkill;
              (* SIGKILL is unconditional; 1s is more than enough for the
                 kernel to deliver it. The outer session timeout is a final
                 backstop, but it can be very long (e.g. 1800s), so we cap
                 locally to keep the guarantee tight here. *)
              match
                Eio.Time.with_timeout clock 1.0 (fun () ->
                    Ok (Eio.Process.await child))
              with
              | Ok status -> status
              | Error `Timeout -> `Signaled 9)
        else Eio.Process.await child
      in
      let code = match status with `Exited c -> c | `Signaled s -> 128 + s in
      (!err_ref, code)
    in
    Ok
      {
        exit_code;
        stdout = "";
        stderr = stderr_content;
        got_events = !got_events_ref;
        saw_final_result = !saw_final_result_ref;
        timed_out = false;
      }
  in
  match Eio.Time.with_timeout clock timeout run with
  | Ok result -> result
  | Error `Timeout ->
      {
        exit_code = 128 + 9 (* SIGKILL — sent via on_release hook *);
        stdout = "";
        stderr = "process timed out";
        got_events = !got_events_ref;
        saw_final_result = !saw_final_result_ref;
        timed_out = true;
      }

type t = {
  name : string;
  run_streaming :
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    patch_id:Types.Patch_id.t ->
    prompt:string ->
    resume_session:string option ->
    on_event:(Types.Stream_event.t -> unit) ->
    result;
}

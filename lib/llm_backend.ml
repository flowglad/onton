(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)

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
   group and we'd signal ourselves, so callers only use this after spawning
   through the setsid shim. *)
let signal_group ~pid ~signal =
  try
    Unix.kill (-pid) signal;
    `Ok
  with
  | Unix.Unix_error (ESRCH, _, _) -> `Gone
  | Unix.Unix_error (EPERM, _, _) -> `Permission_denied

let kill_group ~pid ~signal = ignore (signal_group ~pid ~signal)

let spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~env ~setsid_exec ~args
    ~session_uuid ~patch_id
    ~(process_line : string -> Types.Stream_event.t list) ~on_event =
  let args =
    match setsid_exec with Some path -> path :: args | None -> args
  in
  let stdout_max_size = 64 * 1024 * 1024 in
  let stderr_max_size = 1024 * 1024 in
  let stdout_capture_max_size = 64 * 1024 in
  let saw_final_result_ref = ref false in
  let saw_terminal_event_ref = ref false in
  let got_events_ref = ref false in
  let stdout_capture = Buffer.create 4096 in
  let stdout_capture_truncated = ref false in
  let capture_stdout_line line =
    let remaining = stdout_capture_max_size - Buffer.length stdout_capture in
    if remaining <= 0 then stdout_capture_truncated := true
    else
      let text = line ^ "\n" in
      let len = String.length text in
      if len <= remaining then Buffer.add_string stdout_capture text
      else (
        Buffer.add_substring stdout_capture text ~pos:0 ~len:remaining;
        stdout_capture_truncated := true)
  in
  let emit_stream channel raw =
    match session_uuid with
    | None -> ()
    | Some _ ->
        Telemetry_dispatch.emit
          (Telemetry.Event.Stream { patch_id; session_uuid; channel; raw })
  in
  let captured_stdout () =
    let s = Buffer.contents stdout_capture in
    if !stdout_capture_truncated then
      s ^ "\n<stdout exceeded 64KB capture limit, truncated>"
    else s
  in
  let run () =
    let stderr_content, exit_code =
      Eio.Switch.run @@ fun sw ->
      let stdin_r, stdin_w = Eio.Process.pipe ~sw process_mgr in
      let stdout_r, stdout_w = Eio.Process.pipe ~sw process_mgr in
      let stderr_r, stderr_w = Eio.Process.pipe ~sw process_mgr in
      let child =
        Eio.Process.spawn ~sw process_mgr ~cwd ~stdin:stdin_r ~stdout:stdout_w
          ~stderr:stderr_w ~env args
      in
      (* Await the process exactly once.  Timeout races below wait on this
         promise rather than repeatedly cancelling [Eio.Process.await], which
         cannot reliably be resumed after its waiting fiber is cancelled. *)
      let await_promise =
        Eio.Fiber.fork_promise ~sw (fun () -> Eio.Process.await child)
      in
      let await_child () = Eio.Promise.await_exn await_promise in
      let pid = Eio.Process.pid child in
      let have_group = Option.is_some setsid_exec in
      let cleanup_warning_ref = ref None in
      let signal_child signal =
        try Eio.Process.signal child signal with _ -> ()
      in
      let signal_tree signal =
        if have_group then kill_group ~pid ~signal else signal_child signal
      in
      let await_child_with_timeout seconds =
        Eio.Time.with_timeout clock seconds (fun () -> Ok (await_child ()))
      in
      let await_terminal_child () =
        (* Give the CLI a short un-signalled grace period to flush session
           persistence and exit normally.  If it remains alive, retain the
           bounded teardown guarantee by escalating through TERM and KILL for
           the direct child only. Persistence helpers in its process group get
           a separate grace period after the child exits. *)
        match await_child_with_timeout 2.0 with
        | Ok status -> status
        | Error `Timeout -> (
            signal_child Stdlib.Sys.sigterm;
            match await_child_with_timeout 2.0 with
            | Ok status -> status
            | Error `Timeout -> (
                signal_child Stdlib.Sys.sigkill;
                (* SIGKILL is unconditional; 1s is more than enough for the
                   kernel to deliver it. The outer session timeout is a final
                   backstop, but it can be very long (e.g. 1800s), so cap
                   locally to keep the guarantee tight here. *)
                match await_child_with_timeout 1.0 with
                | Ok status -> status
                | Error `Timeout -> `Signaled 9))
      in
      let finish_process_group () =
        if not have_group then Ok ()
        else
          (* The CLI may delegate persistence to a short-lived helper that
             remains in its process group after the direct child exits.  Give
             those descendants the same chance to flush cleanly before the
             final sweep. *)
          let deadline = Eio.Time.now clock +. 2.0 in
          let rec wait_for_exit () =
            match signal_group ~pid ~signal:0 with
            | `Gone -> Ok false
            | `Permission_denied ->
                Error
                  (Printf.sprintf
                     "process-group cleanup skipped: permission denied for \
                      group %d"
                     pid)
            | `Ok ->
                if Float.(Eio.Time.now clock < deadline) then (
                  Eio.Time.sleep clock 0.05;
                  wait_for_exit ())
                else Ok true
          in
          match wait_for_exit () with
          | Error _ as error -> error
          | Ok false -> Ok ()
          | Ok true -> (
              match signal_group ~pid ~signal:Stdlib.Sys.sigkill with
              | `Ok | `Gone -> Ok ()
              | `Permission_denied ->
                  Error
                    (Printf.sprintf
                       "process-group cleanup failed: permission denied for \
                        group %d"
                       pid))
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
      let stdout_buf =
        Eio.Buf_read.of_flow ~max_size:stdout_max_size stdout_r
      in
      let stderr_buf =
        Eio.Buf_read.of_flow ~max_size:stderr_max_size stderr_r
      in
      let err_ref = ref "" in
      let terminal_status_ref = ref None in
      let stop_stdout, stop_stdout_u = Eio.Promise.create () in
      let stop_stderr, stop_stderr_u = Eio.Promise.create () in
      let stop_or stop read =
        Eio.Fiber.first
          (fun () ->
            Eio.Promise.await stop;
            `Stop)
          (fun () -> `Read (read ()))
      in
      let drain_flow_until_stopped ~stop flow =
        let drain_buf = Bytes.create 4096 in
        let rec drain () =
          match
            stop_or stop (fun () ->
                Eio.Flow.single_read flow (Cstruct.of_bytes drain_buf))
          with
          | `Stop -> ()
          | `Read _ -> drain ()
        in
        try drain () with
        | End_of_file -> ()
        | Eio.Exn.Io _ | Invalid_argument _ -> ()
      in
      let record_cleanup_warning warning =
        cleanup_warning_ref := Some warning;
        Stdio.eprintf "onton: %s\n%!" warning
      in
      Eio.Fiber.both
        (fun () ->
          let rec read_lines () =
            match Eio.Buf_read.line stdout_buf with
            | line ->
                capture_stdout_line line;
                emit_stream `Stdout line;
                let events = process_line line in
                if not (List.is_empty events) then got_events_ref := true;
                List.iter events ~f:on_event;
                let saw_final =
                  List.exists events ~f:(function
                    | Types.Stream_event.Final_result _ -> true
                    | Types.Stream_event.Turn_started
                    | Types.Stream_event.Text_delta _
                    | Types.Stream_event.Tool_use _ | Types.Stream_event.Error _
                    | Types.Stream_event.Session_init _ ->
                        false)
                in
                let saw_terminal =
                  saw_final
                  || List.exists events ~f:(function
                    | Types.Stream_event.Error _ -> true
                    | Types.Stream_event.Final_result _
                    | Types.Stream_event.Turn_started
                    | Types.Stream_event.Text_delta _
                    | Types.Stream_event.Tool_use _
                    | Types.Stream_event.Session_init _ ->
                        false)
                in
                (* [saw_terminal] = [saw_final] || [saw_error]; only
                   [saw_terminal] stops recursion. *)
                if saw_final then saw_final_result_ref := true;
                if saw_terminal then saw_terminal_event_ref := true
                else read_lines ()
            | exception End_of_file -> ()
          in
          read_lines ();
          if !saw_terminal_event_ref then
            (* The model's turn is over, but the CLI may still be flushing
               persistent session state.  Do not signal it yet: Codex, in
               particular, releases its thread-store writer during graceful
               shutdown, and interrupting that cleanup can make the next
               resume fail with an "active writer" conflict. Keep draining
               stderr during that grace period so diagnostics cannot fill the
               pipe and stall the flush. *)
            Eio.Fiber.both
              (fun () ->
                let status = await_terminal_child () in
                terminal_status_ref := Some status;
                (* The direct child is done. Give persistence helpers their
                   own grace before sweeping the remaining group. *)
                (match finish_process_group () with
                | Ok () -> ()
                | Error warning -> record_cleanup_warning warning);
                (* Release both drainers without closing either pipe ourselves.
                   The read ends remain open until switch teardown, avoiding
                   EPIPE/SIGPIPE for writers racing with process exit. *)
                ignore (Eio.Promise.try_resolve stop_stdout_u ());
                ignore (Eio.Promise.try_resolve stop_stderr_u ()))
              (fun () ->
                (* A CLI may emit shutdown diagnostics after its terminal
                   event. Keep consuming stdout so a full pipe cannot prevent
                   it from completing the persistence flush. *)
                drain_flow_until_stopped ~stop:stop_stdout stdout_r))
        (fun () ->
          (* Only swallow the expected teardown exceptions: Eio.Buf_read
             raises when the buffer fills up, and End_of_file/Eio.Exn.Io can
             fire when the subprocess closes the pipe. Letting anything else
             propagate — in particular Eio.Cancel.Cancelled — is required so
             this fiber can honour cancellation and release Eio.Fiber.both. *)
          let add_stderr_line line =
            if not (String.is_empty !err_ref) then err_ref := !err_ref ^ "\n";
            err_ref := !err_ref ^ line;
            emit_stream `Stderr line
          in
          let rec read_stderr_lines () =
            match
              stop_or stop_stderr (fun () -> Eio.Buf_read.line stderr_buf)
            with
            | `Stop -> ()
            | `Read line ->
                add_stderr_line line;
                read_stderr_lines ()
          in
          try read_stderr_lines () with
          | Eio.Buf_read.Buffer_limit_exceeded ->
              add_stderr_line "<stderr exceeded 1MB limit, truncated>";
              drain_flow_until_stopped ~stop:stop_stderr stderr_r
          | End_of_file -> ()
          | Eio.Exn.Io _ -> ());
      let status =
        match !terminal_status_ref with
        | Some status -> status
        | None -> await_child ()
      in
      let code = match status with `Exited c -> c | `Signaled s -> 128 + s in
      let stderr_content =
        match !cleanup_warning_ref with
        | None -> !err_ref
        | Some warning when String.is_empty !err_ref -> warning
        | Some warning -> !err_ref ^ "\n" ^ warning
      in
      (stderr_content, code)
    in
    Ok
      {
        exit_code;
        stdout = captured_stdout ();
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
        stdout = captured_stdout ();
        stderr = "process timed out";
        got_events = !got_events_ref;
        saw_final_result = !saw_final_result_ref;
        timed_out = true;
      }

type t = {
  name : string;
  run_streaming :
    project_name:string ->
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    patch_id:Types.Patch_id.t ->
    prompt:string ->
    resume_session:string option ->
    session_uuid:string ->
    complexity:int option ->
    on_event:(Types.Stream_event.t -> unit) ->
    result;
}

(** Resolve a model selector for a single backend invocation.

    [Some "auto"] (case-insensitive) routes through the per-backend [auto_model]
    mapping. [None] / empty / any other string passes through unchanged. This
    keeps "auto" as a documented sentinel string rather than introducing a new
    type, so it round-trips cleanly through Project_store and the CLI without
    any migration.

    [auto_model] is the backend's complexity → model name function. It may
    return [None] when complexity is missing AND the backend has no sensible
    fallback — in that case we drop [--model] and let the CLI's own default
    apply. *)
let resolve_auto_model ~model ~complexity ~auto_model : string option =
  let is_auto = function
    | Some s -> Base.String.equal (Base.String.lowercase s) "auto"
    | None -> false
  in
  if is_auto model then auto_model ~complexity else model

let%test "resolve_auto_model passes None through" =
  let auto_model ~complexity:_ = Some "fallback" in
  Option.equal Base.String.equal
    (resolve_auto_model ~model:None ~complexity:None ~auto_model)
    None

let%test "resolve_auto_model passes explicit model through unchanged" =
  let auto_model ~complexity:_ = Some "fallback" in
  Option.equal Base.String.equal
    (resolve_auto_model ~model:(Some "sonnet") ~complexity:(Some 1) ~auto_model)
    (Some "sonnet")

let%test "resolve_auto_model: 'auto' routes through auto_model" =
  let auto_model ~complexity =
    match complexity with Some 1 -> Some "fast" | _ -> Some "strong"
  in
  Option.equal Base.String.equal
    (resolve_auto_model ~model:(Some "auto") ~complexity:(Some 1) ~auto_model)
    (Some "fast")

let%test "resolve_auto_model: 'AUTO' is also recognised (case-insensitive)" =
  let auto_model ~complexity:_ = Some "picked" in
  Option.equal Base.String.equal
    (resolve_auto_model ~model:(Some "AUTO") ~complexity:(Some 2) ~auto_model)
    (Some "picked")

let%test "resolve_auto_model: 'auto' with no complexity hits fallback tier" =
  let auto_model ~complexity =
    match complexity with None -> Some "strong" | _ -> Some "wrong"
  in
  Option.equal Base.String.equal
    (resolve_auto_model ~model:(Some "auto") ~complexity:None ~auto_model)
    (Some "strong")

let redact_env env = Array.map env ~f:Token_scrub.redact_env_entry

let emit_spawn_started ~patch_id ~session_uuid ~prompt ~args ~env =
  Telemetry_dispatch.emit
    (Telemetry.Event.Spawn_started
       {
         patch_id;
         session_uuid;
         prompt;
         argv = args;
         env_redacted = redact_env env;
       })

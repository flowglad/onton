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

let spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~env ~setsid_exec ~args
    ~session_uuid ~patch_id
    ~(process_line : string -> Types.Stream_event.t list) ~on_event =
  let supervisor =
    match setsid_exec with
    | Some path ->
        if (not (String.is_empty path)) && Stdlib.Filename.is_relative path then
          Stdlib.Filename.concat (Stdlib.Sys.getcwd ()) path
        else path
    | None -> Process_tree.supervisor_path ()
  in
  let args = Process_tree.supervised_command ~supervisor ~streaming:true args in
  let exception Idle_timeout in
  (* Publish the deadline only once the child exists. Pipe setup and spawning
     are not child inactivity, and the watchdog must not cancel them. *)
  let idle_started, idle_started_u = Eio.Promise.create () in
  (* Observe bytes at the pipe boundary, before buffering or backend parsing.
     Partial lines and diagnostic output are activity too. The watchdog covers
     event callbacks and waiting for process exit as well as reads. *)
  let observe_output flow =
    let module Source = struct
      type t = unit

      let read_methods = []

      let single_read () buf =
        let count = Eio.Flow.single_read flow buf in
        let idle_deadline = Eio.Promise.await idle_started in
        idle_deadline := Eio.Time.now clock +. timeout;
        count
    end in
    Eio.Resource.T ((), Eio.Flow.Pi.source (module Source))
  in
  let watch_idle () =
    let idle_deadline = Eio.Promise.await idle_started in
    let rec watch () =
      let remaining = !idle_deadline -. Eio.Time.now clock in
      if Float.(remaining <= 0.0) then raise Idle_timeout;
      Eio.Time.sleep clock remaining;
      watch ()
    in
    watch ()
  in
  let rec is_idle_timeout = function
    | Idle_timeout -> true
    | Eio.Exn.Multiple errors ->
        List.exists errors ~f:(fun (exn, _) -> is_idle_timeout exn)
    | _ -> false
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
      Eio.Cancel.sub (fun caller ->
          Eio.Cancel.protect (fun () ->
              Eio.Switch.run (fun sw ->
                  let stdin_r, stdin_w = Eio.Process.pipe ~sw process_mgr in
                  let stdout_r, stdout_w = Eio.Process.pipe ~sw process_mgr in
                  let stderr_r, stderr_w = Eio.Process.pipe ~sw process_mgr in
                  Eio.Cancel.check caller;
                  let child =
                    Eio.Process.spawn ~sw process_mgr ~cwd ~stdin:stdin_r
                      ~stdout:stdout_w ~stderr:stderr_w ~env args
                  in
                  Eio.Promise.resolve idle_started_u
                    (ref (Eio.Time.now clock +. timeout));
                  (* Await the process exactly once.  Timeout races below wait on this
         promise rather than repeatedly cancelling [Eio.Process.await], which
         cannot reliably be resumed after its waiting fiber is cancelled. *)
                  let await_promise =
                    Eio.Fiber.fork_promise ~sw (fun () ->
                        Eio.Process.await child)
                  in
                  let await_child () = Eio.Promise.await_exn await_promise in
                  let signal_child signal =
                    try Eio.Process.signal child signal with _ -> ()
                  in
                  let await_terminal_child () =
                    (* The supervisor owns both leader and helper flush windows. *)
                    signal_child Stdlib.Sys.sigusr1;
                    await_child ()
                  in
                  (* The waiter and process resources live in a protected scope. A caller
         cancellation stops streaming, then waits for supervised cleanup before
         releasing the checkout to another operation. *)
                  Exn.protect
                    ~finally:(fun () ->
                      signal_child Stdlib.Sys.sigterm;
                      ignore (await_child ()))
                    ~f:(fun () ->
                      Eio.Fiber.first
                        (fun () ->
                          Eio.Flow.close stdin_r;
                          Eio.Flow.close stdin_w;
                          Eio.Flow.close stdout_w;
                          Eio.Flow.close stderr_w;
                          let stdout_buf =
                            Eio.Buf_read.of_flow ~max_size:stdout_max_size
                              (observe_output stdout_r)
                          in
                          let stderr_buf =
                            Eio.Buf_read.of_flow ~max_size:stderr_max_size
                              (observe_output stderr_r)
                          in
                          let err_ref = ref "" in
                          let terminal_status_ref = ref None in
                          let stop_stdout, stop_stdout_u =
                            Eio.Promise.create ()
                          in
                          let stop_stderr, stop_stderr_u =
                            Eio.Promise.create ()
                          in
                          let stop_or stop read =
                            Eio.Fiber.first
                              (fun () ->
                                Eio.Promise.await stop;
                                `Stop)
                              (fun () -> `Read (read ()))
                          in
                          let drain_flow_until_stopped ~stop flow =
                            let flow = observe_output flow in
                            let drain_buf = Bytes.create 4096 in
                            let rec drain () =
                              match
                                stop_or stop (fun () ->
                                    Eio.Flow.single_read flow
                                      (Cstruct.of_bytes drain_buf))
                              with
                              | `Stop -> ()
                              | `Read _ -> drain ()
                            in
                            try drain () with
                            | End_of_file -> ()
                            | Eio.Exn.Io _ | Invalid_argument _ -> ()
                          in
                          Eio.Fiber.both
                            (fun () ->
                              let rec read_lines () =
                                match Eio.Buf_read.line stdout_buf with
                                | line ->
                                    capture_stdout_line line;
                                    emit_stream `Stdout line;
                                    let events = process_line line in
                                    if not (List.is_empty events) then
                                      got_events_ref := true;
                                    List.iter events ~f:on_event;
                                    let saw_final =
                                      List.exists events ~f:(function
                                        | Types.Stream_event.Final_result _ ->
                                            true
                                        | Types.Stream_event.Turn_started
                                        | Types.Stream_event.Text_delta _
                                        | Types.Stream_event.Tool_use _
                                        | Types.Stream_event.Error _
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
                                    if saw_final then
                                      saw_final_result_ref := true;
                                    if saw_terminal then
                                      saw_terminal_event_ref := true
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
                                    (* Release both drainers without closing either pipe ourselves.
                   The read ends remain open until switch teardown, avoiding
                   EPIPE/SIGPIPE for writers racing with process exit. *)
                                    ignore
                                      (Eio.Promise.try_resolve stop_stdout_u ());
                                    ignore
                                      (Eio.Promise.try_resolve stop_stderr_u ()))
                                  (fun () ->
                                    (* A CLI may emit shutdown diagnostics after its terminal
                   event. Keep consuming stdout so a full pipe cannot prevent
                   it from completing the persistence flush. *)
                                    drain_flow_until_stopped ~stop:stop_stdout
                                      stdout_r))
                            (fun () ->
                              (* Only swallow the expected teardown exceptions: Eio.Buf_read
             raises when the buffer fills up, and End_of_file/Eio.Exn.Io can
             fire when the subprocess closes the pipe. Letting anything else
             propagate — in particular Eio.Cancel.Cancelled — is required so
             this fiber can honour cancellation and release Eio.Fiber.both. *)
                              let add_stderr_line line =
                                if not (String.is_empty !err_ref) then
                                  err_ref := !err_ref ^ "\n";
                                err_ref := !err_ref ^ line;
                                emit_stream `Stderr line
                              in
                              let rec read_stderr_lines () =
                                match
                                  stop_or stop_stderr (fun () ->
                                      Eio.Buf_read.line stderr_buf)
                                with
                                | `Stop -> ()
                                | `Read line ->
                                    add_stderr_line line;
                                    read_stderr_lines ()
                              in
                              try read_stderr_lines () with
                              | Eio.Buf_read.Buffer_limit_exceeded ->
                                  add_stderr_line
                                    "<stderr exceeded 1MB limit, truncated>";
                                  drain_flow_until_stopped ~stop:stop_stderr
                                    stderr_r
                              | End_of_file -> ()
                              | Eio.Exn.Io _ -> ());
                          let status =
                            match !terminal_status_ref with
                            | Some status -> status
                            | None -> await_child ()
                          in
                          let code =
                            match status with
                            | `Exited c -> c
                            | `Signaled s -> 128 + s
                          in
                          (!err_ref, code))
                        (fun () ->
                          let rec watch_caller () =
                            Eio.Cancel.check caller;
                            Eio.Time.sleep clock 0.01;
                            watch_caller ()
                          in
                          watch_caller ())))))
    in
    {
      exit_code;
      stdout = captured_stdout ();
      stderr = stderr_content;
      got_events = !got_events_ref;
      saw_final_result = !saw_final_result_ref;
      timed_out = false;
    }
  in
  match Eio.Fiber.first run watch_idle with
  | result -> result
  (* [first] combines concurrent failures, including errors raised while the
     losing fiber tears down. Preserve timeout classification in that case;
     unrelated failures and external cancellation still propagate unchanged. *)
  | exception exn when is_idle_timeout exn ->
      {
        exit_code = 128 + 9 (* Timed-out sessions are reported as terminated. *);
        stdout = captured_stdout ();
        stderr = "process idle timed out";
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

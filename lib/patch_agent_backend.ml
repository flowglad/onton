open Base
module Long_lived = Llm_backend_long_lived

type handle = {
  child : [ `Process | `Platform of [ `Generic ] ] Eio.Resource.t;
  stdin_w : [ `Close | `Flow | `W ] Eio.Resource.t;
  stderr_r : [ `Close | `Flow | `R ] Eio.Resource.t;
  stdout_buf : Eio.Buf_read.t;
  stderr_capture : Buffer.t;
  stderr_truncated : bool ref;
  pid : int;
  have_group : bool;
  mutable shutdown_requested : bool;
  mutable prompt_count : int;
}

type Long_lived.handle += Handle of handle

let stdout_max_size = 64 * 1024 * 1024
let stdout_capture_max_size = 64 * 1024
let stderr_capture_max_size = 1024 * 1024
let shutdown_grace_seconds = 2.0
let shutdown_kill_seconds = 1.0

let kill_group ~pid ~signal =
  try Unix.kill (-pid) signal
  with Unix.Unix_error ((ESRCH | EPERM), _, _) -> ()

let signal_process handle signal =
  if handle.have_group then kill_group ~pid:handle.pid ~signal
  else try Eio.Process.signal handle.child signal with _ -> ()

let exit_code_of_status = function
  | `Exited code -> code
  | `Signaled signal -> 128 + signal

let write_line handle line = Eio.Flow.copy_string line handle.stdin_w
let captured_stderr handle = Buffer.contents handle.stderr_capture

let append_bounded buffer truncated ~limit text =
  let remaining = limit - Buffer.length buffer in
  if remaining <= 0 then truncated := true
  else
    let len = String.length text in
    if len <= remaining then Buffer.add_string buffer text
    else (
      Buffer.add_substring buffer text ~pos:0 ~len:remaining;
      truncated := true)

let captured_stderr_for_result handle =
  let stderr = captured_stderr handle in
  if !(handle.stderr_truncated) then
    stderr ^ "\n<stderr exceeded 1MB capture limit, truncated>"
  else stderr

let request_id handle kind =
  handle.prompt_count <- handle.prompt_count + 1;
  Printf.sprintf "patch-agent-%s-%d" kind handle.prompt_count

let emit_error on_event message =
  on_event (Types.Stream_event.Error message);
  {
    Llm_backend.exit_code = 1;
    stdout = "";
    stderr = message;
    got_events = true;
    saw_final_result = false;
    timed_out = false;
  }

let is_terminal = function
  | Types.Stream_event.Final_result _ | Types.Stream_event.Error _ -> true
  | Types.Stream_event.Turn_started | Types.Stream_event.Text_delta _
  | Types.Stream_event.Tool_use _ | Types.Stream_event.Session_init _ ->
      false

let is_final = function
  | Types.Stream_event.Final_result _ -> true
  | Types.Stream_event.Error _ | Types.Stream_event.Turn_started
  | Types.Stream_event.Text_delta _ | Types.Stream_event.Tool_use _
  | Types.Stream_event.Session_init _ ->
      false

let spawn_args ~binary_path ~setsid_exec ~gameplan_path ~patch_path
    ~worktree_path ~provider ~model ~effort =
  let args =
    [
      binary_path;
      "--gameplan-prompt-file";
      gameplan_path;
      "--patch-prompt-file";
      patch_path;
      "--worktree";
      worktree_path;
      "--provider";
      provider;
      "--model";
      model;
      "--effort";
      effort;
    ]
  in
  match setsid_exec with Some path -> path :: args | None -> args

let write_prompt_files worktree ~gameplan_prompt ~patch_prompt =
  let dir = Eio.Path.(worktree / ".patch-agent") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir;
  let gameplan_path = Eio.Path.(dir / "gameplan.md") in
  let patch_path = Eio.Path.(dir / "patch.md") in
  Eio.Path.save ~create:(`Or_truncate 0o644) gameplan_path gameplan_prompt;
  Eio.Path.save ~create:(`Or_truncate 0o644) patch_path patch_prompt;
  (Eio.Path.native_exn gameplan_path, Eio.Path.native_exn patch_path)

let start ~process_mgr ~binary_path ~setsid_exec ~sw
    ({ worktree; provider; model; effort; gameplan_prompt; patch_prompt; _ } :
      Long_lived.start_config) =
  let gameplan_path, patch_path =
    write_prompt_files worktree ~gameplan_prompt ~patch_prompt
  in
  let worktree_path = Eio.Path.native_exn worktree in
  let args =
    spawn_args ~binary_path ~setsid_exec ~gameplan_path ~patch_path
      ~worktree_path ~provider ~model ~effort
  in
  let stdin_r, stdin_w = Eio.Process.pipe ~sw process_mgr in
  let stdout_r, stdout_w = Eio.Process.pipe ~sw process_mgr in
  let stderr_r, stderr_w = Eio.Process.pipe ~sw process_mgr in
  let child =
    Eio.Process.spawn ~sw process_mgr ~cwd:worktree ~stdin:stdin_r
      ~stdout:stdout_w ~stderr:stderr_w ~env:(Unix.environment ()) args
  in
  Eio.Flow.close stdin_r;
  Eio.Flow.close stdout_w;
  Eio.Flow.close stderr_w;
  let handle =
    {
      child;
      stdin_w;
      stderr_r;
      stdout_buf = Eio.Buf_read.of_flow ~max_size:stdout_max_size stdout_r;
      stderr_capture = Buffer.create 4096;
      stderr_truncated = ref false;
      pid = Eio.Process.pid child;
      have_group = Option.is_some setsid_exec;
      shutdown_requested = false;
      prompt_count = 0;
    }
  in
  Eio.Switch.on_release sw (fun () ->
      if not handle.shutdown_requested then
        signal_process handle Stdlib.Sys.sigkill);
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let buf = Bytes.create 4096 in
      (try
         while true do
           let read =
             Eio.Flow.single_read handle.stderr_r (Cstruct.of_bytes buf)
           in
           append_bounded handle.stderr_capture handle.stderr_truncated
             ~limit:stderr_capture_max_size
             (Stdlib.Bytes.sub_string buf 0 read)
         done
       with End_of_file | Eio.Exn.Io _ | Invalid_argument _ -> ());
      `Stop_daemon);
  Handle handle

let prompt_session ~clock long_lived_handle ~prompt ~timeout ~on_event =
  match long_lived_handle with
  | Handle handle -> (
      if handle.shutdown_requested then
        emit_error on_event "patch-agent prompt requested after shutdown"
      else
        let stdout_capture = Buffer.create 4096 in
        let stdout_truncated = ref false in
        let got_events = ref false in
        let saw_final_result = ref false in
        let exit_code = ref 0 in
        let capture_stdout line =
          append_bounded stdout_capture stdout_truncated
            ~limit:stdout_capture_max_size (line ^ "\n")
        in
        let captured_stdout () =
          let stdout = Buffer.contents stdout_capture in
          if !stdout_truncated then
            stdout ^ "\n<stdout exceeded 64KB capture limit, truncated>"
          else stdout
        in
        let run () =
          write_line handle
            (Patch_agent_rpc.serialize_command
               (Prompt
                  { request_id = request_id handle "prompt"; content = prompt }));
          let rec loop () =
            match Eio.Buf_read.line handle.stdout_buf with
            | line ->
                capture_stdout line;
                let event =
                  match Patch_agent_rpc.parse_event line with
                  | Ok rpc_event -> Patch_agent_event_mapper.map_event rpc_event
                  | Error reason ->
                      Types.Stream_event.Error
                        (Printf.sprintf "patch-agent RPC parse error: %s" reason)
                in
                got_events := true;
                if is_final event then saw_final_result := true;
                on_event event;
                if is_terminal event then () else loop ()
            | exception End_of_file ->
                let status = Eio.Process.await handle.child in
                let code = exit_code_of_status status in
                exit_code := code;
                if code <> 0 then (
                  got_events := true;
                  on_event
                    (Types.Stream_event.Error
                       (Printf.sprintf "patch-agent exited with code %d" code)));
                ()
          in
          loop ();
          Ok
            {
              Llm_backend.exit_code = !exit_code;
              stdout = captured_stdout ();
              stderr = captured_stderr_for_result handle;
              got_events = !got_events;
              saw_final_result = !saw_final_result;
              timed_out = false;
            }
        in
        match Eio.Time.with_timeout clock timeout run with
        | Ok result -> result
        | Error `Timeout ->
            signal_process handle Stdlib.Sys.sigkill;
            {
              Llm_backend.exit_code = 128 + 9;
              stdout = captured_stdout ();
              stderr = "patch-agent prompt timed out";
              got_events = !got_events;
              saw_final_result = !saw_final_result;
              timed_out = true;
            })
  | _ -> invalid_arg "Patch_agent_backend.prompt: foreign long-lived handle"

let abort long_lived_handle =
  match long_lived_handle with
  | Handle handle ->
      if not handle.shutdown_requested then
        write_line handle
          (Patch_agent_rpc.serialize_command
             (Abort { request_id = request_id handle "abort" }))
  | _ -> invalid_arg "Patch_agent_backend.abort: foreign long-lived handle"

let shutdown ~clock long_lived_handle =
  match long_lived_handle with
  | Handle handle ->
      if not handle.shutdown_requested then (
        handle.shutdown_requested <- true;
        (try
           write_line handle
             (Patch_agent_rpc.serialize_command
                (Shutdown { request_id = request_id handle "shutdown" }))
         with _ -> ());
        let await_with_timeout seconds =
          Eio.Time.with_timeout clock seconds (fun () ->
              Ok (Eio.Process.await handle.child))
        in
        match await_with_timeout shutdown_grace_seconds with
        | Ok _ -> ()
        | Error `Timeout -> (
            signal_process handle Stdlib.Sys.sigkill;
            match await_with_timeout shutdown_kill_seconds with
            | Ok _ | Error `Timeout -> ()))
  | _ -> invalid_arg "Patch_agent_backend.shutdown: foreign long-lived handle"

let create ~process_mgr ~clock ~binary_path ~setsid_exec : Long_lived.t =
  {
    name = "Patch-agent";
    start =
      (fun ~sw config ->
        start ~process_mgr ~binary_path ~setsid_exec ~sw config);
    prompt =
      (fun handle ~prompt ~timeout ~on_event ->
        prompt_session ~clock handle ~prompt ~timeout ~on_event);
    abort;
    shutdown = (fun handle -> shutdown ~clock handle);
  }

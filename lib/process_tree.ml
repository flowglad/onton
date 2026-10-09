(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)

(* Owns Eio process resources and cancellation directly; application decisions
   remain with callers rather than a worktree decision domain. *)

open Base

let rec has_cancellation = function
  | Eio.Cancel.Cancelled _ -> true
  | Eio.Exn.Multiple exns ->
      List.exists exns ~f:(fun (exn, _bt) -> has_cancellation exn)
  | _ -> false

(* Retain the worktree spawn policy: retry failures before a process verdict,
   such as EAGAIN under process-table pressure, but never retry cancellation or
   a process error (including executable discovery and child exit failures). *)
let rec has_process_error = function
  | Eio.Process.E _ -> true
  | Eio.Exn.Multiple_io errors ->
      List.exists errors ~f:(fun (error, _context, _bt) ->
          has_process_error error)
  | _ -> false

let rec is_transient_spawn_failure = function
  | Eio.Cancel.Cancelled _ -> false
  (* Io and Multiple_io carry typed error codes, not exception values.
     Cancellation can appear in Multiple, which is traversed below. *)
  | Eio.Io (error, _) -> not (has_process_error error)
  | Eio.Exn.Multiple exns ->
      List.for_all exns ~f:(fun (exn, _bt) -> is_transient_spawn_failure exn)
  | _ -> true

let rec retry_transient_spawn ?(attempts = 4) f =
  match f () with
  | x -> x
  | exception e when attempts <= 1 || not (is_transient_spawn_failure e) ->
      raise e
  | exception _ ->
      Eio.Fiber.yield ();
      retry_transient_spawn ~attempts:(attempts - 1) f

let supervisor_path () =
  let supervisor =
    match Stdlib.Sys.getenv_opt "ONTON_SETSID_EXEC" with
    | Some path -> path
    | None ->
        let sibling =
          Stdlib.Filename.concat
            (Stdlib.Filename.dirname Stdlib.Sys.executable_name)
            "onton-setsid-exec"
        in
        if Stdlib.Sys.file_exists sibling then sibling
        else
          Option.value
            (Backend_preflight.find_executable "onton-setsid-exec")
            ~default:sibling
  in
  if
    (not (String.is_empty supervisor)) && Stdlib.Filename.is_relative supervisor
  then Stdlib.Filename.concat (Stdlib.Sys.getcwd ()) supervisor
  else supervisor

let supervised_command ~supervisor ~streaming args =
  let guards = Project_lifecycle.command_guards () in
  supervisor
  :: (if streaming then "--supervise-stream-parent" else "--supervise-parent")
  :: Int.to_string (Unix.getpid ())
  :: "--command-guards"
  :: Int.to_string (List.length guards)
  :: (guards @ args)

let run_sync ~env args =
  let supervisor = supervisor_path () in
  if String.is_empty supervisor || not (Stdlib.Sys.file_exists supervisor) then
    (Unix.WEXITED 127, "", "Process-tree supervisor unavailable: " ^ supervisor)
  else
    let owned = ref [] and child = ref None in
    let stdout = Buffer.create 256 and stderr = Buffer.create 256 in
    let own fd =
      owned := fd :: !owned;
      fd
    in
    let close fd =
      owned :=
        List.filter !owned ~f:(fun current -> not (Poly.equal current fd));
      try Unix.close fd with _ -> ()
    in
    let pipe () =
      let rd, wr = Unix.pipe ~cloexec:true () in
      (own rd, own wr)
    in
    let rec wait flags pid =
      try Unix.waitpid flags pid
      with Unix.Unix_error (Unix.EINTR, _, _) -> wait flags pid
    in
    let stop pid =
      (* Check ownership before signaling, including if a signal handler raised
         between waitpid returning and clearing [child]. An exited but unreaped
         child still anchors its PID; ECHILD forbids signaling a recycled PID. *)
      match wait [ Unix.WNOHANG ] pid with
      | 0, _ ->
          (try Unix.kill pid Stdlib.Sys.sigterm
           with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
          ignore (wait [] pid)
      | _, _ -> ()
      | exception Unix.Unix_error (Unix.ECHILD, _, _) -> ()
    in
    Stdlib.Fun.protect
      ~finally:(fun () ->
        Stdlib.Fun.protect
          ~finally:(fun () ->
            List.iter !owned ~f:(fun fd -> try Unix.close fd with _ -> ()))
          (fun () -> Option.iter !child ~f:stop))
      (fun () ->
        let stdin =
          own (Unix.openfile "/dev/null" [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0)
        in
        let out_r, out_w = pipe () and err_r, err_w = pipe () in
        let argv =
          supervised_command ~supervisor ~streaming:false args |> Array.of_list
        in
        let pid =
          Unix.create_process_env supervisor argv env stdin out_w err_w
        in
        child := Some pid;
        List.iter [ stdin; out_w; err_w ] ~f:close;
        let bytes = Bytes.create 4096 in
        let rec read fd =
          try Unix.read fd bytes 0 (Bytes.length bytes)
          with Unix.Unix_error (Unix.EINTR, _, _) -> read fd
        in
        let rec ready fds =
          try
            let readable, _, _ = Unix.select fds [] [] (-1.) in
            readable
          with Unix.Unix_error (Unix.EINTR, _, _) -> ready fds
        in
        let rec drain streams =
          if not (List.is_empty streams) then
            let readable = ready (List.map streams ~f:fst) in
            let remaining =
              List.filter streams ~f:(fun (fd, buffer) ->
                  if not (List.mem readable fd ~equal:Poly.equal) then true
                  else
                    match read fd with
                    | 0 -> false
                    | count ->
                        Stdlib.Buffer.add_subbytes buffer bytes 0 count;
                        true)
            in
            drain remaining
        in
        drain [ (out_r, stdout); (err_r, stderr) ];
        let _, status = wait [] pid in
        child := None;
        (status, Buffer.contents stdout, Buffer.contents stderr))

let run_status ?cwd ?stdout ?stderr ~process_mgr ?clock ~env args =
  let sleep =
    match clock with
    | Some clock -> Eio.Time.sleep clock
    | None -> Eio_unix.sleep
  in
  let stdout = Option.value stdout ~default:(Buffer.create 256) in
  let stderr = Option.value stderr ~default:(Buffer.create 256) in
  let supervisor = supervisor_path () in
  if String.is_empty supervisor || not (Stdlib.Sys.file_exists supervisor) then (
    Buffer.add_string stderr
      ("Process-tree supervisor unavailable: " ^ supervisor);
    (`Exited 127, Buffer.contents stdout, Buffer.contents stderr))
  else
    let status =
      Eio.Cancel.sub (fun caller ->
          (* Own the supervisor and its waiter together. Only the command wait
             is cancellable: check caller cancellation every 10ms, then wait for
             the supervisor to reap its tree before releasing resources. *)
          Eio.Cancel.protect (fun () ->
              Eio.Switch.run (fun sw ->
                  let child =
                    retry_transient_spawn (fun () ->
                        Eio.Cancel.check caller;
                        Eio.Process.spawn ~sw process_mgr ~env ?cwd
                          ~stdin:(Eio.Flow.string_source "")
                          ~stdout:(Eio.Flow.buffer_sink stdout)
                          ~stderr:(Eio.Flow.buffer_sink stderr)
                          (supervised_command ~supervisor ~streaming:false args))
                  in
                  let waited =
                    Eio.Fiber.fork_promise ~sw (fun () ->
                        Eio.Process.await child)
                  in
                  Stdlib.Fun.protect
                    ~finally:(fun () ->
                      Eio.Cancel.protect (fun () ->
                          (try Eio.Process.signal child Stdlib.Sys.sigterm
                           with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
                          ignore (Eio.Promise.await_exn waited)))
                    (fun () ->
                      Eio.Fiber.first
                        (fun () -> Eio.Promise.await_exn waited)
                        (fun () ->
                          let rec watch () =
                            Eio.Cancel.check caller;
                            sleep 0.01;
                            watch ()
                          in
                          watch ())))))
    in
    (status, Buffer.contents stdout, Buffer.contents stderr)

let run ?cwd ?stdout ?stderr ~process_mgr ?clock ~env args =
  let status, stdout, stderr =
    run_status ?cwd ?stdout ?stderr ~process_mgr ?clock ~env args
  in
  let code = match status with `Exited c -> c | `Signaled s -> 128 + s in
  (code, stdout, stderr)

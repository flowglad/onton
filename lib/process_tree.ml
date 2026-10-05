(* @archlint.module shell
   @archlint.domain worktree-parser *)

open Base

let run ~process_mgr ~clock ~env args =
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
  if String.is_empty supervisor || not (Stdlib.Sys.file_exists supervisor) then
    (127, "", "Process-tree supervisor unavailable: " ^ supervisor)
  else
    let stdout = Buffer.create 256 in
    let stderr = Buffer.create 256 in
    let status =
      Eio.Cancel.sub (fun caller ->
          (* Own the supervisor and its waiter together. Only the command wait
             is cancellable: check caller cancellation every 10ms, then wait for
             the supervisor to reap its tree before releasing resources. *)
          Eio.Cancel.protect (fun () ->
              Eio.Switch.run (fun sw ->
                  let child =
                    Eio.Process.spawn ~sw process_mgr ~env
                      ~stdin:(Eio.Flow.string_source "")
                      ~stdout:(Eio.Flow.buffer_sink stdout)
                      ~stderr:(Eio.Flow.buffer_sink stderr)
                      (supervisor :: "--supervise" :: args)
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
                            Eio.Time.sleep clock 0.01;
                            watch ()
                          in
                          watch ())))))
    in
    let code = match status with `Exited c -> c | `Signaled s -> 128 + s in
    (code, Buffer.contents stdout, Buffer.contents stderr)

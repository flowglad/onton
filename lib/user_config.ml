open Base

type t = { on_worktree_create : string option }

let config_dir ~github_owner ~github_repo =
  let home = Stdlib.Sys.getenv "HOME" in
  Stdlib.Filename.concat
    (Stdlib.Filename.concat
       (Stdlib.Filename.concat home ".config/onton")
       github_owner)
    github_repo

let load ~github_owner ~github_repo =
  let dir = config_dir ~github_owner ~github_repo in
  let script_path = Stdlib.Filename.concat dir "on_worktree_create" in
  let on_worktree_create =
    if Stdlib.Sys.file_exists script_path then Some script_path else None
  in
  { on_worktree_create }

let env_float name default =
  match Stdlib.Sys.getenv_opt name with
  | Some s -> ( try Float.of_string s with _ -> default)
  | None -> default

let env_int name default =
  match Stdlib.Sys.getenv_opt name with
  | Some s -> ( try Int.of_string s with _ -> default)
  | None -> default

let default_timeout () = env_float "ONTON_HOOK_TIMEOUT" 600.0
let default_fd_limit () = env_int "ONTON_HOOK_FD_LIMIT" 256

let build_error ~status_msg stdout_buf stderr_buf =
  let stdout = String.strip (Buffer.contents stdout_buf) in
  let stderr = String.strip (Buffer.contents stderr_buf) in
  let sections =
    List.filter_opt
      [
        Some status_msg;
        (if String.is_empty stdout then None
         else Some (Printf.sprintf "stdout:\n%s" stdout));
        (if String.is_empty stderr then None
         else Some (Printf.sprintf "stderr:\n%s" stderr));
      ]
  in
  Error (String.concat ~sep:"\n" sections)

(** Wrap the user's script in a [/bin/sh -c "ulimit -n N; exec SCRIPT"]
    invocation. [ulimit -n] lowers the child's [RLIMIT_NOFILE] before the script
    runs, so runaway subtree fan-out (npm install spawning node, dune spawning
    ocamlc, etc.) can't exhaust the shared file-descriptor table on the host. We
    clamp the requested target against both the hard and soft caps — the hard
    cap prevents shells like dash from printing an error when the target exceeds
    it, and the soft cap prevents us from accidentally *raising* a parent who
    already ran [ulimit -n] to something below our default. *)
let wrap_with_ulimit ~fd_limit script =
  [
    "/bin/sh";
    "-c";
    Printf.sprintf
      {|hlimit=$(ulimit -Hn); slimit=$(ulimit -Sn); target=%d; [ "$hlimit" != unlimited ] && [ "$target" -gt "$hlimit" ] && target=$hlimit; [ "$slimit" != unlimited ] && [ "$target" -gt "$slimit" ] && target=$slimit; ulimit -n "$target"; exec %s|}
      fd_limit
      (Stdlib.Filename.quote script);
  ]

let run_hook ~process_mgr ~clock ~script ~cwd ~env ?(timeout : float option)
    ?(fd_limit : int option) () : (unit, string) Result.t =
  let timeout = Option.value timeout ~default:(default_timeout ()) in
  let fd_limit = Option.value fd_limit ~default:(default_fd_limit ()) in
  let stdout_buf = Buffer.create 256 in
  let stderr_buf = Buffer.create 256 in
  let env_array =
    (* Overlay keys must override inherited ones. [execve] accepts duplicate
       keys but [getenv] returns the first match on glibc, so we filter
       inherited entries whose key is in [overlay] before appending. *)
    let inherited = Unix.environment () |> Array.to_list in
    let override_keys = Set.of_list (module String) (List.map env ~f:fst) in
    let inherited =
      List.filter inherited ~f:(fun binding ->
          match String.lsplit2 binding ~on:'=' with
          | Some (k, _) -> not (Set.mem override_keys k)
          | None -> true)
    in
    let overlay = List.map env ~f:(fun (k, v) -> Printf.sprintf "%s=%s" k v) in
    Array.of_list (List.append inherited overlay)
  in
  let cmd = wrap_with_ulimit ~fd_limit script in
  try
    Eio.Switch.run (fun sw ->
        let child =
          Eio.Process.spawn ~sw process_mgr ~cwd ~env:env_array
            ~stdout:(Eio.Flow.buffer_sink stdout_buf)
            ~stderr:(Eio.Flow.buffer_sink stderr_buf)
            cmd
        in
        (* [Fun.protect] guarantees SIGKILL + await run on *every* exit path,
           including cancellation from the outer fiber while we're blocked in
           [Eio.Process.await] on the normal-exit branch. Signalling an
           already-exited child is harmless (ESRCH), and [Eio.Process.await]
           is idempotent, so the cleanup is safe even when the timeout arm
           already handled things. SIGKILL only reaches the direct child
           ([/bin/sh -c]); grandchildren from [npm install]/etc may outlive
           the hook — killing the whole process tree needs [setpgid] or an
           Eio fork-action pre-exec, both out of scope here. *)
        Stdlib.Fun.protect
          ~finally:(fun () ->
            (try Eio.Process.signal child Stdlib.Sys.sigkill with _ -> ());
            try ignore (Eio.Process.await child) with _ -> ())
          (fun () ->
            match
              Eio.Time.with_timeout clock timeout (fun () ->
                  Ok (Eio.Process.await child))
            with
            | Ok (`Exited 0) -> Ok ()
            | Ok (`Exited code) ->
                build_error
                  ~status_msg:(Printf.sprintf "hook exited with code %d" code)
                  stdout_buf stderr_buf
            | Ok (`Signaled signal) ->
                build_error
                  ~status_msg:(Printf.sprintf "hook killed by signal %d" signal)
                  stdout_buf stderr_buf
            | Error `Timeout ->
                build_error
                  ~status_msg:
                    (Printf.sprintf
                       "hook timed out after %.1fs (ONTON_HOOK_TIMEOUT)" timeout)
                  stdout_buf stderr_buf))
  with
  | Eio.Cancel.Cancelled _ as exn ->
      (* Cancellation must propagate to the caller — never format it as a
         hook failure. See Eio.Cancel docs. *)
      raise exn
  | exn ->
      build_error
        ~status_msg:(Stdlib.Printexc.to_string exn)
        stdout_buf stderr_buf

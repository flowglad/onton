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
    ocamlc, etc.) can't exhaust the shared file-descriptor table on the host.
    The [2>/dev/null] suppresses a noisy error if the current hard limit is
    below the target — we still want [exec] to run either way. *)
let wrap_with_ulimit ~fd_limit script =
  [
    "/bin/sh";
    "-c";
    Printf.sprintf "ulimit -n %d 2>/dev/null; exec %s" fd_limit
      (Stdlib.Filename.quote script);
  ]

let run_hook ~process_mgr ~clock ~script ~cwd ~env ?(timeout : float option)
    ?(fd_limit : int option) () : (unit, string) Result.t =
  let timeout = Option.value timeout ~default:(default_timeout ()) in
  let fd_limit = Option.value fd_limit ~default:(default_fd_limit ()) in
  let stdout_buf = Buffer.create 256 in
  let stderr_buf = Buffer.create 256 in
  let env_array =
    let inherited = Unix.environment () |> Array.to_list in
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
            (* SIGKILL reaches the direct child (our [/bin/sh -c]). If the
               hook forked grandchildren (e.g. [npm install] spawning node
               subprocesses), those orphans may keep running past the
               timeout until they notice their parent is gone. Killing the
               whole process tree requires [setpgid] (not in OCaml's
               Stdlib.Unix) or a fork-action pre-exec hook (Eio private
               API); both are out of scope here. The timeout still bounds
               *our* blocking on the hook, which is the property the
               orchestrator needs. *)
            (try Eio.Process.signal child Stdlib.Sys.sigkill with _ -> ());
            build_error
              ~status_msg:
                (Printf.sprintf
                   "hook timed out after %.1fs (ONTON_HOOK_TIMEOUT)" timeout)
              stdout_buf stderr_buf)
  with exn ->
    build_error
      ~status_msg:(Stdlib.Printexc.to_string exn)
      stdout_buf stderr_buf

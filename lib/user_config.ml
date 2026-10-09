(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)

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

(* Positive-only parsers: zero/negative/NaN/infinity in these env vars would
   either break the hook (ulimit -n 0 makes the child unable to open anything)
   or weaken containment (negative timeout, infinite timeout). Fall back to
   the default rather than propagating a footgun. *)
let env_positive_float name default =
  match Stdlib.Sys.getenv_opt name with
  | Some s -> (
      try
        let v = Float.of_string s in
        if Float.is_finite v && Float.(v > 0.) then v else default
      with _ -> default)
  | None -> default

let env_positive_int name default =
  match Stdlib.Sys.getenv_opt name with
  | Some s -> (
      try
        let v = Int.of_string s in
        if v > 0 then v else default
      with _ -> default)
  | None -> default

let default_timeout () = env_positive_float "ONTON_HOOK_TIMEOUT" 600.0
let default_fd_limit () = env_positive_int "ONTON_HOOK_FD_LIMIT" 256

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
      {|hlimit=$(ulimit -Hn); slimit=$(ulimit -Sn); target=%d; [ "$hlimit" != unlimited ] && [ "$target" -gt "$hlimit" ] && target=$hlimit; [ "$slimit" != unlimited ] && [ "$target" -gt "$slimit" ] && target=$slimit; ulimit -n "$target" && exec %s|}
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
    match
      Eio.Time.with_timeout clock timeout (fun () ->
          Ok
            (Process_tree.run ~process_mgr ~clock ~env:env_array ~cwd
               ~stdout:stdout_buf ~stderr:stderr_buf cmd))
    with
    | Ok (0, _, _) -> Ok ()
    | Ok (code, _, _) ->
        build_error
          ~status_msg:(Printf.sprintf "hook exited with code %d" code)
          stdout_buf stderr_buf
    | Error `Timeout ->
        build_error
          ~status_msg:
            (Printf.sprintf "hook timed out after %.1fs (ONTON_HOOK_TIMEOUT)"
               timeout)
          stdout_buf stderr_buf
  with
  | exn when Process_tree.has_cancellation exn -> raise exn
  | exn ->
      build_error
        ~status_msg:(Stdlib.Printexc.to_string exn)
        stdout_buf stderr_buf

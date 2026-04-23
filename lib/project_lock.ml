open Base

type t = { fd : Unix.file_descr }
type error = Held_by of { pid : int; path : string } | Io_error of string

let path_for ~project_dir = Stdlib.Filename.concat project_dir "onton.lock"

(** Rewind [fd], read up to [cap] bytes, return the trimmed contents as an
    integer if parseable. *)
let read_pid fd =
  try
    let _ = Unix.lseek fd 0 Unix.SEEK_SET in
    let buf = Bytes.create 32 in
    let n = Unix.read fd buf 0 (Bytes.length buf) in
    let s = Stdlib.Bytes.sub_string buf 0 n |> String.strip in
    Int.of_string_opt s
  with _ -> None

(** Does [pid] name a running process? [kill(pid, 0)] returns:
    - ok → process exists and we could signal it
    - [EPERM] → exists, different user: still counts as alive
    - [ESRCH] → does not exist → stale *)
let pid_alive pid =
  if pid <= 0 then false
  else
    try
      Unix.kill pid 0;
      true
    with
    | Unix.Unix_error (Unix.ESRCH, _, _) -> false
    | Unix.Unix_error (Unix.EPERM, _, _) -> true
    | _ -> false

let write_self_pid fd =
  try
    let _ = Unix.lseek fd 0 Unix.SEEK_SET in
    Unix.ftruncate fd 0;
    let s = Printf.sprintf "%d\n" (Unix.getpid ()) in
    let _ = Unix.single_write_substring fd s 0 (String.length s) in
    ()
  with _ -> ()

let try_lock fd =
  try
    Unix.lockf fd Unix.F_TLOCK 0;
    `Acquired
  with
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) -> `Busy
  | Unix.Unix_error (e, _, _) -> `Error (Unix.error_message e)

let ensure_dir dir =
  let rec mkdir_p p =
    if not (Stdlib.Sys.file_exists p) then (
      mkdir_p (Stdlib.Filename.dirname p);
      try Stdlib.Sys.mkdir p 0o755 with Sys_error _ -> ())
  in
  mkdir_p dir

let acquire ~project_dir ~on_stale =
  let path = path_for ~project_dir in
  ensure_dir project_dir;
  match
    Unix.openfile path [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC ] 0o644
  with
  | exception Unix.Unix_error (e, _, _) ->
      Error (Io_error (Unix.error_message e))
  | fd -> (
      match try_lock fd with
      | `Acquired ->
          write_self_pid fd;
          Ok { fd }
      | `Error msg ->
          (try Unix.close fd with _ -> ());
          Error (Io_error msg)
      | `Busy -> (
          let recorded = read_pid fd in
          match recorded with
          | Some pid when pid_alive pid ->
              (try Unix.close fd with _ -> ());
              Error (Held_by { pid; path })
          | _ -> (
              (* No PID recorded, or recorded PID is dead → stale. *)
              on_stale (Option.value recorded ~default:(-1));
              match try_lock fd with
              | `Acquired ->
                  write_self_pid fd;
                  Ok { fd }
              | `Busy ->
                  (* A third process raced in between our check and retry. *)
                  let pid = Option.value (read_pid fd) ~default:(-1) in
                  (try Unix.close fd with _ -> ());
                  Error (Held_by { pid; path })
              | `Error msg ->
                  (try Unix.close fd with _ -> ());
                  Error (Io_error msg))))

let release t =
  (try
     let _ = Unix.lseek t.fd 0 Unix.SEEK_SET in
     Unix.ftruncate t.fd 0
   with _ -> ());
  (try Unix.lockf t.fd Unix.F_ULOCK 0 with _ -> ());
  try Unix.close t.fd with _ -> ()

let pp_error ppf = function
  | Held_by { pid; path } ->
      Stdlib.Format.fprintf ppf
        "another onton is already running (pid %d, lock %s)" pid path
  | Io_error s -> Stdlib.Format.fprintf ppf "lock i/o error: %s" s

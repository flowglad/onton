(** Process-level advisory lock on a project data directory.

    At most one onton process may hold the lock for a given project at a time.
    Uses [Unix.lockf] on [<project_dir>/onton.lock]; the lock stays held for the
    process lifetime and is released via [Fun.protect] by the caller.

    The lock file records the holder's PID so [acquire] can render actionable
    error messages. A stale lock (PID no longer running) is reclaimed
    automatically — the caller is notified via [~on_stale]. *)

type t

type error =
  | Held_by of { pid : int; path : string }
      (** Another onton is alive and holds the lock. *)
  | Io_error of string  (** Opening or locking the file failed. *)

val acquire : project_dir:string -> on_stale:(int -> unit) -> (t, error) result
(** [acquire ~project_dir ~on_stale] creates [<project_dir>/onton.lock] if
    needed, obtains [F_TLOCK], and writes the current PID.

    On contention, reads the PID stored in the file:
    - If [Unix.kill pid 0] confirms the holder is alive → [Error (Held_by ...)].
    - If [Unix.kill] raises [ESRCH] → [on_stale pid] is called and the lock is
      reclaimed.

    [on_stale] is invoked with the stale PID (or [-1] if no PID was recorded in
    the file). *)

val release : t -> unit
(** Truncate the PID file and release the lock. Idempotent; never raises. *)

val pp_error : Format.formatter -> error -> unit
(** Human-readable error rendering for CLI diagnostics. *)

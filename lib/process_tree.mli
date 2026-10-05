(* @archlint.module shell
   @archlint.domain worktree-parser *)

val has_cancellation : exn -> bool
val is_transient_spawn_failure : exn -> bool

val retry_transient_spawn : ?attempts:int -> (unit -> 'a) -> 'a
(** Retry transient spawn exceptions, yielding between attempts (four by
    default). Cancellation, including inside [Eio.Exn.Multiple], and process
    errors, including inside either composite form, propagate immediately.
    [Eio.Io] and [Eio.Exn.Multiple_io] carry error codes rather than exception
    values. Exhausted attempts propagate the last exception. Apply only to
    process creation. *)

val run :
  process_mgr:_ Eio.Process.mgr ->
  clock:float Eio.Time.clock_ty Eio.Time.clock ->
  env:string array ->
  string list ->
  int * string * string
(** Capture a command through the installed onton-setsid-exec supervisor. On
    return or cancellation, its process group has terminated and been reaped
    before the supervisor is reaped. Supervisor creation uses the bounded spawn
    retry policy; a started command is never retried. A missing supervisor fails
    closed. *)

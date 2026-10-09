(* @archlint.module interface
   @archlint.domain process-tree *)

val has_cancellation : exn -> bool
val is_transient_spawn_failure : exn -> bool

val retry_transient_spawn : ?attempts:int -> (unit -> 'a) -> 'a
(** Retry transient spawn exceptions, yielding between attempts (four by
    default). Cancellation, including inside [Eio.Exn.Multiple], and process
    errors, including inside either composite form, propagate immediately.
    [Eio.Io] and [Eio.Exn.Multiple_io] carry error codes rather than exception
    values. Exhausted attempts propagate the last exception. Apply only to
    process creation. *)

val supervisor_path : unit -> string
(** Resolve the configured or installed supervisor to an absolute path. An empty
    explicit override stays empty so dispatch fails rather than bypassing it. *)

val supervised_command :
  supervisor:string -> streaming:bool -> string list -> string list
(** Capture the current parent PID and held project command guards into the
    supervisor dispatch protocol. *)

val run_sync :
  env:string array -> string list -> Unix.process_status * string * string
(** Synchronous startup counterpart of [run_status], with the same supervisor
    and project guards. Drains both output pipes together and closes stdin.
    Returns after group cleanup; exception paths stop and reap the supervisor.
    This does not require an Eio scheduler. *)

val run :
  ?cwd:Eio.Fs.dir_ty Eio.Path.t ->
  ?stdout:Buffer.t ->
  ?stderr:Buffer.t ->
  process_mgr:_ Eio.Process.mgr ->
  ?clock:_ Eio.Time.clock ->
  env:string array ->
  string list ->
  int * string * string
(** Capture a command through the installed onton-setsid-exec supervisor. On
    return or cancellation, its process group has terminated and been reaped
    before the supervisor is reaped. Supervisor creation uses the bounded spawn
    retry policy; a started command is never retried. A missing supervisor fails
    closed. Optional output buffers retain captured diagnostics when
    cancellation or an enclosing timeout interrupts the call. [cwd] selects the
    command's working directory without changing the supervisor's cleanup
    ownership. The supervisor is bound to the spawning process's PID: it refuses
    dispatch if that parent is gone and tears down an in-flight group on
    reparenting. Currently held project leases supply shared command guards; the
    supervisor retains them until group cleanup completes, preventing a
    replacement writer or retirement from racing a dead runtime's commands. If
    [clock] is omitted, cancellation polling uses [Eio_unix.sleep]. *)

val run_status :
  ?cwd:Eio.Fs.dir_ty Eio.Path.t ->
  ?stdout:Buffer.t ->
  ?stderr:Buffer.t ->
  process_mgr:_ Eio.Process.mgr ->
  ?clock:_ Eio.Time.clock ->
  env:string array ->
  string list ->
  [ `Exited of int | `Signaled of int ] * string * string
(** Same ownership and cleanup as [run], preserving native exit/signal status.
    When [clock] is omitted, cancellation polling uses [Eio_unix.sleep]. *)

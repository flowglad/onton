(* @archlint.module interface
   @archlint.domain project-lifecycle *)

type error = Project_retirement.error = Busy of string | Io_error of string
type registration
type use
type retirement

val acquire_registration : unit -> (registration, error) result
val release_registration : registration -> unit
val acquire_use : registration -> project_name:string -> (use, error) result

val acquire_writer : registration -> project_name:string -> (use, error) result
(** Exclusive process-lifetime authority for a supervisor. Blocks other readers,
    writers and retirement until release or process exit; supervised commands
    retain an additional admission guard until cleanup completes. Required even
    when the legacy PID-file supervisor lock is bypassed. *)

val release_use : use -> unit

val command_guards : unit -> string list
(** Shared-lock paths for the currently held project leases in this process.
    Command supervisors acquire these before dispatch and retain them until all
    descendants are reaped. New project admission checks them exclusively, so a
    crashed runtime's commands must drain before another writer or retirement
    starts. Registration alone and inherited or released leases grant no guard.
    Callers retain their project lease until command cleanup completes. *)

val acquire_retirement :
  registration -> project_name:string -> (retirement, error) result

val release_retirement : retirement -> unit
val retire : registration -> retirement -> (unit, error) result
val cleanup_retired : registration -> (unit, error) result

val error_message : error -> string
(** Registration serializes startup/configuration and pruning. A shared use
    lease survives registration and prevents retirement for the process
    lifetime. Supervisors hold the exclusive writer counterpart even when they
    bypass the PID-file lock. Retirement also takes the exclusive counterpart,
    then atomically moves the owned project into a private journal. Cleanup only
    traverses journal payloads; interrupted cleanup cannot delete a new project
    at the original path. Releases are idempotent and inherited handles cannot
    unlock a parent's lease. *)

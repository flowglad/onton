open Types

(** Shared mutable runtime state, protected by an Eio mutex.

    All fibers (TUI, poller, Claude agent runners) access the application state
    through this module. A single [Eio.Mutex.t] serializes access — coarse but
    sufficient for the expected concurrency level. *)

type snapshot = {
  orchestrator : Orchestrator.t;
  activity_log : Activity_log.t;
  gameplan : Gameplan.t;
}

type t

val create :
  gameplan:Gameplan.t -> main_branch:Branch.t -> ?snapshot:snapshot -> unit -> t
(** Build initial runtime state from a gameplan, optionally restoring a previous
    [snapshot]. *)

(** {2 Atomic read access} *)

val read : t -> (snapshot -> 'a) -> 'a
(** [read t f] acquires the mutex, passes a consistent snapshot to [f], and
    releases. *)

(** {2 Atomic read-modify-write} *)

val update : t -> (snapshot -> snapshot) -> unit
(** [update t f] acquires the mutex, applies [f] to the current snapshot, stores
    the result, and releases. *)

val update_orchestrator : t -> (Orchestrator.t -> Orchestrator.t) -> unit
(** Convenience: update only the orchestrator. *)

val update_activity_log : t -> (Activity_log.t -> Activity_log.t) -> unit
(** Convenience: update only the activity log. *)

(** Runtime side-table mapping a review-service finding id to the backend it
    came from.

    The pure {!Onton_core.Review_service.finding} record deliberately does not
    carry backend attribution — adding a runtime concept to a pure type would
    muddy the layering. The poller registers each finding here as it fetches
    them; the post-session resolve flow looks up the backend by finding id when
    it needs to POST a resolve verb.

    Concurrency: protected internally by an [Eio.Mutex.t]. Callers do not need
    to coordinate across the poller and runner fibers. *)

type t

type entry = {
  backend : Onton_core.Review_backend.t;
  owner : string;
  repo : string;
  pr_number : int;
}
(** The information needed to address a resolve verb back to the originating
    backend, on the same PR coordinates Onton observed. *)

val create : unit -> t

val register : t -> finding_id:string -> entry -> unit
(** Idempotent: re-registering the same id replaces the prior entry. The poller
    calls this on every successful findings fetch; if a finding moves between
    backends (operator misconfiguration) the latest fetch wins. *)

val find : t -> finding_id:string -> entry option

val forget : t -> finding_id:string -> unit
(** Drop the entry. Called after a resolve POST so the table doesn't grow
    unbounded across the lifetime of the process. *)

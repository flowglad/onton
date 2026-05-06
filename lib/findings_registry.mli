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
  finding_id : string;
}
(** The information needed to address a resolve verb back to the originating
    backend, on the same PR coordinates Onton observed. [finding_id] is the raw
    backend-local id used in the review-service API path. *)

val create : unit -> t

val make_key :
  backend_name:string ->
  owner:string ->
  repo:string ->
  pr_number:int ->
  finding_id:string ->
  string
(** Composite id used in prompts, artifacts, and this registry. The key
    namespaces backend-local finding ids by backend and PR coordinates so two
    review backends can emit the same raw [finding_id] without colliding. *)

val register : t -> key:string -> entry -> unit
(** Idempotent: re-registering the same composite key replaces the prior entry.
    The poller calls this on every successful findings fetch. *)

val find : t -> key:string -> entry option

val forget : t -> key:string -> unit
(** Drop the entry. Called after a resolve POST so the table doesn't grow
    unbounded across the lifetime of the process. *)

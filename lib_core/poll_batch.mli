(* @archlint.module interface
   @archlint.domain orchestrator *)

type t
(** Volatile completion ownership for one checkpointed forge request batch.
    Request IDs must be unique across batches. Response acceptance belongs to
    the forge observation owner; failed and rejected responses still complete
    their request here. Cancellation discards this scheduling obligation. *)

val create : string list -> t

val complete : t -> string option -> t * bool
(** [complete state (Some id)] consumes a request at most once. [None] completes
    an empty batch. The boolean grants exactly one project reconciliation pass,
    only after every request has completed. Unknown IDs are ignored. *)

val cancel : t -> t
(** Cancelled batches never grant reconciliation, including on late completion.
*)

open Base
open Types

(** Priority queue for operation ordering per patch.

    Lower numeric priority = higher urgency.
    Implements the Pantagruel spec fragment for [priority], [is-feedback],
    and [highest-priority]. *)

type t [@@deriving show, eq, sexp_of]

val empty : t
val is_empty : t -> bool
val enqueue : t -> Operation_kind.t -> t
val dequeue_highest : t -> (Operation_kind.t * t) option
val peek_highest : t -> Operation_kind.t option
val mem : t -> Operation_kind.t -> bool
val to_list : t -> Operation_kind.t list
val priority : Operation_kind.t -> int
val is_feedback : Operation_kind.t -> bool

val highest_priority :
  t -> Operation_kind.t -> bool
(** [highest_priority q k] is true iff [k] is in [q] and no item in [q]
    has strictly lower (more urgent) priority value. *)

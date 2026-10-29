(* @archlint.module interface
   @archlint.domain coordinator-state *)

type phase = Pending | Applied of string | Attention

type entry = private {
  id : string;
  scope : string;
  attempts : int;
  phase : phase;
}

type t

val yojson_of_t : t -> Yojson.Safe.t
val encode : t -> string
val decode : string -> (t, string) result
val empty : t
val entries : t -> entry list
val find : t -> string -> entry option
val valid : t -> bool
val begin_attempt : t -> id:string -> scope:string -> (t, string) result

val settle : t -> id:string -> result:string option -> (t, string) result
(** Attempt budget is durable and never reset by replay/restart. Applied results
    are immutable. Three unresolved attempts become attention, not permission to
    repeat a non-idempotent external effect. *)

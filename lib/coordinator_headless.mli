(* @archlint.module interface
   @archlint.domain coordinator *)

val json : Coordinator_state.t -> Yojson.Safe.t
(** Bounded, non-authoritative aggregate projection of committed state only. *)

val emit : write:(string -> unit) -> Coordinator_state.t -> unit
(** Emit one JSONL record. Never includes capsule, control payload, note or
    resource request bytes. The caller must supply committed state. *)

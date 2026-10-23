(* @archlint.module interface
   @archlint.domain worker-checkpoint *)

(** Attempt-scoped durable checkpoint boundary. Implementations must freeze the
    submitted candidate before waiting for an acknowledgement. *)
module type S = sig
  val persist : Runtime.snapshot -> (unit, string) result

  val clear_session_id : patch_id:Types.Patch_id.t -> (unit, string) result
  (** Durably invalidate the rejected backend identity for this patch before
      returning [Ok]. Recovery must not restore it from either the snapshot or a
      sidecar. Local persistence scopes its sidecar tombstone to the old
      snapshot identity so a checkpointed replacement survives without another
      write; workers checkpoint the cleared identity through the host
      acknowledgement. A later accepted conversation may record a new identity.
  *)

  val record_session_id :
    patch_id:Types.Patch_id.t -> session_id:string -> (unit, string) result
end

module Local (_ : sig
  val project_name : string
end) : S

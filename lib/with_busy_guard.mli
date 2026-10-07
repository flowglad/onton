(* @archlint.module interface
   @archlint.domain with-busy-guard *)

module type ENV = sig
  val runtime : Runtime.t
  val event_log : Event_log.t
end

module Make (_ : ENV) : sig
  val run :
    ?with_capacity:((unit -> unit) -> unit) ->
    patch_id:Types.Patch_id.t ->
    message_id:Types.Message_id.t ->
    (Runtime.patch_write -> unit) ->
    unit
  (** Acquire optional worker capacity before Git ownership. Cleanup also covers
      cancellation while waiting for capacity or ownership. *)
end

module type ENV = sig
  val runtime : Runtime.t
  val event_log : Event_log.t
end

module Make (_ : ENV) : sig
  val run : patch_id:Types.Patch_id.t -> (unit -> unit) -> unit
end

module type SESSION_DRIVER = sig
  type long_lived_session

  val shutdown_long_lived_session : long_lived_session -> unit
end

module Make (Session_driver : SESSION_DRIVER) : sig
  type session = Session_driver.long_lived_session
  type t

  val create : unit -> t
  val find : t -> Types.Patch_id.t -> session option
  val register : t -> Types.Patch_id.t -> session -> unit
  val remove : t -> Types.Patch_id.t -> unit
  val to_alist : t -> (Types.Patch_id.t * session) list
  val shutdown_all : t -> unit
end

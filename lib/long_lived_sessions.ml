module type SESSION_DRIVER = sig
  type long_lived_session

  val shutdown_long_lived_session : long_lived_session -> unit
end

module Make (Session_driver : SESSION_DRIVER) = struct
  type session = Session_driver.long_lived_session
  type t = (Types.Patch_id.t, session) Stdlib.Hashtbl.t

  let create () = Stdlib.Hashtbl.create 16
  let find t patch_id = Stdlib.Hashtbl.find_opt t patch_id
  let register t patch_id session = Stdlib.Hashtbl.replace t patch_id session
  let remove t patch_id = Stdlib.Hashtbl.remove t patch_id
  let to_alist t = Stdlib.Hashtbl.fold (fun k v acc -> (k, v) :: acc) t []

  let shutdown_all t =
    Stdlib.Hashtbl.iter
      (fun _patch_id session ->
        Session_driver.shutdown_long_lived_session session)
      t
end

module Persistence_env : sig
  module type S = sig
    val runtime : Runtime.t
    val clock : float Eio.Time.clock_ty Eio.Time.clock
    val project_name : string
  end
end

module Make (_ : Persistence_env.S) : sig
  val run : transcripts:(Types.Patch_id.t, string) Stdlib.Hashtbl.t -> unit -> unit
end

(* @archlint.module interface
   @archlint.domain headless-fiber *)

module Headless_env : sig
  module type S = sig
    val runtime : Runtime.t
    val clock : float Eio.Time.clock_ty Eio.Time.clock
    val stdout : Eio_unix.sink_ty Eio.Resource.t
  end
end

module Make
    (_ : Forge.S with type error = Github.error)
    (_ : Worktree.S)
    (_ : Headless_env.S) : sig
  val run : unit -> unit
end

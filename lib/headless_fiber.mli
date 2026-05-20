module Headless_env : sig
  module type S = sig
    val runtime : Runtime.t
    val clock : float Eio.Time.clock_ty Eio.Time.clock
  end
end

module Make (_ : Headless_env.S) : sig
  val run : stdout:_ Eio.Flow.sink -> unit -> unit
end

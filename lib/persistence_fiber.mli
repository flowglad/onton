open Onton_core.Types

module Persistence_env : sig
  module type S = sig
    val runtime : Runtime.t
    val clock : float Eio.Time.clock_ty Eio.Time.clock
    val project_name : string
    val transcripts : (Patch_id.t, string) Stdlib.Hashtbl.t
  end
end

module Make
    (_ : Forge.S with type error = Github.error)
    (_ : Worktree.S)
    (_ : Persistence_env.S) : sig
  val run : unit -> unit
end

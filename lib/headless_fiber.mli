(* @archlint.module interface
   @archlint.domain headless-fiber *)

module Headless_env : sig
  module type S = sig
    val runtime : Runtime.t
    val clock : float Eio.Time.clock_ty Eio.Time.clock
    val stdout : Eio_unix.sink_ty Eio.Resource.t
    val transcript_updates : (Types.Patch_id.t, string) Stdlib.Hashtbl.t
    val initial_transcript_positions : (Types.Patch_id.t, int) Stdlib.Hashtbl.t
    val include_transcripts : bool
  end
end

module Make (_ : Forge.S) (_ : Worktree.S) (_ : Headless_env.S) : sig
  val run : unit -> unit
end

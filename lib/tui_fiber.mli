(* @archlint.module interface
   @archlint.domain tui-fiber *)

open Onton_core.Types

module Tui_env : sig
  module type S = sig
    include Run_env.S

    val process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t
    val stdout : Eio_unix.sink_ty Eio.Resource.t
    val owner : string
    val repo : string
    val transcripts : (Patch_id.t, string) Stdlib.Hashtbl.t
    val tui_state : Tui_state.t
    val backend_name : string
    val version : string
    val resolve_routing : complexity:int option -> Backend_routing.decision
    val find_pr_number : patch_id:Patch_id.t -> Pr_number.t option

    val register_pr_number :
      patch_id:Patch_id.t -> pr_number:Pr_number.t -> unit

    val unregister_pr_number : patch_id:Patch_id.t -> unit
  end
end

module Make
    (_ : Forge.S with type error = Github.error)
    (_ : Worktree.S)
    (_ : Tui_env.S) : sig
  exception Quit

  val run : unit -> unit
  val run_input : unit -> unit
end

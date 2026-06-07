(* @archlint.module interface
   @archlint.domain runner-fiber *)

open Onton_core.Types

module Runner_env : sig
  module type S = sig
    include Run_env.S

    val owner : string
    val repo : string
    val main_branch : Branch.t
    val max_concurrency : int
    val patch_agent_provider : string option
    val patch_agent_effort : string option
    val findings_registry : Findings_registry.t

    val review_clients :
      (module Review_service_client.S
         with type error = Review_service_client.error)
      list

    val transcripts : (Patch_id.t, string) Stdlib.Hashtbl.t
    val event_log : Event_log.t

    val pick_backend :
      complexity:int option -> Backend_registry.kind * Backend_routing.decision

    val register_pr : patch_id:Patch_id.t -> pr_number:Pr_number.t -> unit
  end
end

module Make
    (_ : Forge.S with type error = Github.error)
    (_ : Worktree.S)
    (_ : Runner_env.S) : sig
  val run : ?status_msg:Tui.status_msg option ref -> unit -> unit
end

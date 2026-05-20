open Types

(** Minimal construction-time environment for the poller fiber. *)
module Poller_env : sig
  module type S = sig
    include Run_env.S

    val process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t
    val github_owner : string
    val github_repo : string
    val main_branch : Branch.t
    val poll_interval : float
    val repo_root : string
    val find_pr_number : patch_id:Patch_id.t -> Pr_number.t option

    val register_pr_number :
      patch_id:Patch_id.t -> pr_number:Pr_number.t -> unit

    val unregister_pr_number : patch_id:Patch_id.t -> unit
    val findings_registry : Findings_registry.t

    val review_clients :
      (module Review_service_client.S
         with type error = Review_service_client.error)
      list

    val event_log : Event_log.t
    val branch_of : Patch_id.t -> Branch.t
  end
end

module type STARTUP_RECONCILER = sig
  val discover_pr :
    branch:Branch.t -> ((Pr_number.t * Branch.t * bool) option, string) Result.t
end

module Make
    (_ : Forge.S with type error = Github.error)
    (_ : Worktree.S)
    (_ : Poller_env.S) : sig
  val run : (module STARTUP_RECONCILER) -> unit
end

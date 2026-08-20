(* @archlint.module interface
   @archlint.domain adhoc-branch *)

(** Effectful registration of an existing remote branch as an ad-hoc change. *)

open Onton_core.Types

type outcome =
  | Added of { patch_id : Patch_id.t; change_id : Pr_number.t }
  | Already_registered of Patch_id.t
  | Unsupported_forge of string
  | Remote_not_found
  | Fetch_failed of string
  | No_open_change
  | Handle_collision of Patch_id.t
  | Forge_failed of string

module type FORGE = sig
  type error

  val name : string
  val show_error : error -> string
  val supports_reviews : bool

  val list_prs :
    branch:Branch.t ->
    ?base:Branch.t ->
    state:[ `Open | `All ] ->
    unit ->
    ((Pr_number.t * Branch.t * bool) list, error) Result.t
end

module type WORKTREE = sig
  val fetch_origin_branch :
    fetch_lock:Eio.Mutex.t -> branch:string -> Worktree.fetch_branch_result
end

module Make (_ : FORGE) (_ : WORKTREE) : sig
  val add :
    runtime:Runtime.t ->
    fetch_mutex:Eio.Mutex.t ->
    register_change:(patch_id:Patch_id.t -> pr_number:Pr_number.t -> unit) ->
    branch:Branch.t ->
    outcome
end

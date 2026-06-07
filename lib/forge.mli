(* @archlint.module interface
   @archlint.domain forge *)

(** Forge interface: consumer-facing abstraction over forge operations.

    Each forge implementation (GitHub, GitLab, etc.) satisfies this signature.
    Implementations capture any runtime capabilities they need at construction
    time; for GitHub, {!Github.make} is the canonical constructor. *)

module type S = sig
  type error

  val show_error : error -> string
  val owner : string

  type merge_result =
    | Merge_succeeded
    | Merge_queued of string
    | Merge_unconfirmed

  type enqueue_result =
    | Enqueued of Pr_state.merge_queue_entry
    | Already_enqueued of Pr_state.merge_queue_entry

  val pr_state : Types.Pr_number.t -> (Pr_state.t, error) Result.t

  val list_prs :
    branch:Types.Branch.t ->
    ?base:Types.Branch.t ->
    state:[ `Open | `All ] ->
    unit ->
    ((Types.Pr_number.t * Types.Branch.t * bool) list, error) Result.t

  val update_pr_body :
    pr_number:Types.Pr_number.t -> body:string -> (unit, error) Result.t

  val create_pull_request :
    title:string ->
    head:Types.Branch.t ->
    base:Types.Branch.t ->
    body:string ->
    draft:bool ->
    (Types.Pr_number.t, error) Result.t

  val update_pr_base :
    pr_number:Types.Pr_number.t -> base:Types.Branch.t -> (unit, error) Result.t

  val set_draft :
    pr_number:Types.Pr_number.t -> draft:bool -> (unit, error) Result.t

  val merge_pr : pr_number:Types.Pr_number.t -> (merge_result, error) Result.t
  (** Merge the PR, choosing a merge method the repository actually permits. The
      implementation detects the allowed methods (squash/merge/rebase) and picks
      one — callers must not assume squash. *)

  val enqueue_pr :
    pr_number:Types.Pr_number.t -> (enqueue_result, error) Result.t

  val dequeue_pr : entry_id:string -> (unit, error) Result.t
  val check_repo_access : unit -> (unit, error) Result.t
end

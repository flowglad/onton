module type S = sig
  val execute :
    patch_id:Types.Patch_id.t ->
    agent:Patch_agent.t ->
    fetch_lock:Eio.Mutex.t ->
    fail_label:string ->
    ancestor_ids:Types.Patch_id.t list ->
    Worktree_plan.t ->
    Worktree.rebase_result * string * Worktree_plan.anchor_event list
  (** Execute a {!Worktree_plan.t} sequentially. Returns the final rebase result
      (defaulting to [Ok] for plans without any [Rebase_onto] op), the resolved
      worktree path, and the list of {!Worktree_plan.anchor_event} observations
      emitted as the plan ran, in execution order.

      Loop semantics:
      - [Conflict] / [Error] from a [Rebase_onto] short-circuit the plan; any
        later [Record_anchor_on_success] is NOT executed.
      - [Ok] and [Noop] both allow the plan to continue. A
        [Record_anchor_on_success] following a successful [Rebase_onto] (or
        appearing in a [Capture_anchor]-only plan such as [for_start]) emits its
        event. *)
end

module Make (_ : Worktree.S) (_ : Run_env.S) : S

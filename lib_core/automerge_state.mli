(* @archlint.module interface
   @archlint.domain automerge-state *)

val merge_queue_timer_invariant : Patch_agent.t -> bool
(** [true] when an agent is not simultaneously known to be in GitHub's merge
    queue and carrying an armed automerge deadline. Once a PR has a merge-queue
    entry, the automerge timer has no useful action left to take. *)

val entered_merge_queue :
  Patch_agent.t -> Pr_state.merge_queue_entry -> Patch_agent.t
(** Apply the semantic fact that this PR is now in GitHub's merge queue,
    regardless of how that happened: automerge enqueue response, manual enqueue,
    or a later poll observation. Records the entry, clears the automerge
    deadline and inflight flag, and resets the consecutive failure count. *)

val observe_merge_queue :
  Patch_agent.t ->
  required:bool ->
  entry:Pr_state.merge_queue_entry option ->
  Patch_agent.t
(** Apply a poll observation of GitHub merge-queue state. A present queue entry
    uses {!entered_merge_queue}; future queue movement is learned by polling,
    not by another automerge timer fire. *)

val arm_deadline : Patch_agent.t -> float -> Patch_agent.t
(** Arm an automerge deadline only when the agent is not already known to be in
    the merge queue. If it is already enqueued, clear the deadline instead. *)

val merge_call_failed :
  Patch_agent.t -> retry_deadline:float -> max_failures:int -> Patch_agent.t
(** Apply a failed merge/enqueue/dequeue call. Clears inflight, increments the
    failure counter, and arms [retry_deadline] only if automerge remains
    enabled, the failure cap has not been reached, and the PR is not already
    known to be in the merge queue. *)

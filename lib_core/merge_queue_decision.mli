(* @archlint.module interface
   @archlint.domain merge-queue-decision *)

type application = { poll_result : Poller.t; merge_queue_ejected : bool }
[@@deriving show, eq]

val should_treat_ejection_as_ci_failure :
  previous_entry:Pr_state.merge_queue_entry option -> Poller.t -> bool
(** True when a PR left the merge queue since the previous poll while its own
    head checks are still green and it still looks mergeable. This is the
    {e candidate} condition for a hidden merge-group check failure; it is also
    satisfied by a conflict-driven ejection, so it only gates whether the
    confirming removal-event fetch is worth doing — it does not, by itself,
    decide that a CI failure occurred. *)

val apply :
  previous_entry:Pr_state.merge_queue_entry option ->
  ejection_confirmed:bool ->
  Poller.t ->
  application
(** Rewrite [poll_result] into a synthetic CI failure (enqueue [Ci], mark not
    merge-ready, append the merge-queue placeholder check) only when
    [should_treat_ejection_as_ci_failure] holds {e and} [~ejection_confirmed] is
    true. [ejection_confirmed] is supplied by the effectful poller from the
    removal event's merge-group rollup: [true] for a real check failure (or an
    errored fetch, erring toward surfacing it), [false] for a conflict-driven
    ejection. When not confirmed, [poll_result] is returned unchanged and the
    conflict is left to surface through normal [Merge_conflict] handling. *)

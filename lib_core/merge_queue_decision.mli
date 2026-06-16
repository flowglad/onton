(* @archlint.module interface
   @archlint.domain merge-queue-decision *)

type application = {
  poll_result : Poller.t;
  merge_queue_ejected : bool;
  merge_queue_unmergeable : bool;
}
[@@deriving show, eq]

val should_treat_ejection_as_ci_failure :
  previous_entry:Pr_state.merge_queue_entry option -> Poller.t -> bool

val should_treat_unmergeable_as_ci_failure : Poller.t -> bool

val apply :
  previous_entry:Pr_state.merge_queue_entry option -> Poller.t -> application

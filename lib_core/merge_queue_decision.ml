(* @archlint.module core
   @archlint.domain merge-queue-decision *)

open Base
open Types

type application = {
  poll_result : Poller.t;
  merge_queue_ejected : bool;
  merge_queue_unmergeable : bool;
}
[@@deriving show, eq]

let has_current_failure (poll_result : Poller.t) =
  List.exists poll_result.ci_checks ~f:Ci_check.is_failure

let should_treat_ejection_as_ci_failure ~previous_entry (poll_result : Poller.t)
    =
  let had_merge_queue_entry = Option.is_some previous_entry in
  had_merge_queue_entry && poll_result.merge_queue_required
  && Option.is_none poll_result.merge_queue_entry
  && (not poll_result.merged) && (not poll_result.closed)
  && poll_result.merge_ready && poll_result.checks_passing
  && not (has_current_failure poll_result)

let should_treat_unmergeable_as_ci_failure (poll_result : Poller.t) =
  let merge_queue_unmergeable =
    Option.exists poll_result.merge_queue_entry ~f:(fun entry ->
        Pr_state.equal_merge_queue_entry_state entry.state
          Pr_state.Mq_unmergeable)
  in
  poll_result.merge_queue_required && merge_queue_unmergeable
  && (not poll_result.merged) && (not poll_result.closed)
  && poll_result.checks_passing
  && not (has_current_failure poll_result)

let apply_failure (poll_result : Poller.t) =
  {
    poll_result with
    queue = Operation_kind.Ci :: poll_result.queue;
    merge_ready = false;
    checks_passing = false;
    ci_checks = Ci_check.merge_queue_failure () :: poll_result.ci_checks;
  }

let apply ~previous_entry poll_result =
  if should_treat_ejection_as_ci_failure ~previous_entry poll_result then
    {
      poll_result = apply_failure poll_result;
      merge_queue_ejected = true;
      merge_queue_unmergeable = false;
    }
  else if should_treat_unmergeable_as_ci_failure poll_result then
    {
      poll_result = apply_failure poll_result;
      merge_queue_ejected = false;
      merge_queue_unmergeable = true;
    }
  else
    {
      poll_result;
      merge_queue_ejected = false;
      merge_queue_unmergeable = false;
    }

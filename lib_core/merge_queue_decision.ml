(* @archlint.module core
   @archlint.domain merge-queue-decision *)

open Base
open Types

type application = { poll_result : Poller.t; merge_queue_ejected : bool }
[@@deriving show, eq]

let should_treat_ejection_as_ci_failure ~previous_entry (poll_result : Poller.t)
    =
  let had_merge_queue_entry = Option.is_some previous_entry in
  let current_failures =
    List.exists poll_result.ci_checks ~f:Ci_check.is_failure
  in
  had_merge_queue_entry && poll_result.merge_queue_required
  && Option.is_none poll_result.merge_queue_entry
  && (not poll_result.merged) && (not poll_result.closed)
  && poll_result.merge_ready && poll_result.checks_passing
  && not current_failures

let apply ~previous_entry poll_result =
  if should_treat_ejection_as_ci_failure ~previous_entry poll_result then
    {
      poll_result =
        {
          poll_result with
          queue = Operation_kind.Ci :: poll_result.queue;
          merge_ready = false;
          checks_passing = false;
          ci_checks = Ci_check.merge_queue_failure () :: poll_result.ci_checks;
        };
      merge_queue_ejected = true;
    }
  else { poll_result; merge_queue_ejected = false }

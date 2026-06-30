(* @archlint.module core
   @archlint.domain merge-queue-decision *)

open Base
open Types

type application = { poll_result : Poller.t; merge_queue_ejected : bool }
[@@deriving show, eq]

let has_current_failure (poll_result : Poller.t) =
  List.exists poll_result.ci_checks ~f:Ci_check.is_failure

(* A PR that was in the merge queue last poll and is now gone, while its own
   head checks are still green and it still looks mergeable. GitHub runs
   merge-queue CI on the ephemeral merge-group commit (not the PR head), so a
   PR ejected because a {e required check} failed there shows all-green
   PR-head signals — this predicate is how we notice that hidden failure.

   It is NOT, on its own, proof of a check failure: a PR ejected because its
   speculative merge {e conflicts} with a sibling ahead of it in the queue
   looks identical at the poll level (still clean against [main], head checks
   green). The two are only distinguishable by the removal event's
   [beforeCommit]: a check-failure ejection carries a failing merge-group
   rollup, a conflict ejection carries none. The effectful poller fetches that
   and passes the verdict in via [~ejection_confirmed]; this predicate just
   gates {e whether} the fetch is worth doing. *)
let should_treat_ejection_as_ci_failure ~previous_entry (poll_result : Poller.t)
    =
  let had_merge_queue_entry = Option.is_some previous_entry in
  had_merge_queue_entry && poll_result.merge_queue_required
  && Option.is_none poll_result.merge_queue_entry
  && (not poll_result.merged) && (not poll_result.closed)
  && poll_result.merge_ready && poll_result.checks_passing
  && not (has_current_failure poll_result)

let apply_failure (poll_result : Poller.t) =
  {
    poll_result with
    queue = Operation_kind.Ci :: poll_result.queue;
    merge_ready = false;
    checks_passing = false;
    ci_checks = Ci_check.merge_queue_failure () :: poll_result.ci_checks;
  }

(* Synthesize a CI failure for a merge-queue ejection only when the caller has
   [~ejection_confirmed] it was a real check failure (the removal event's
   merge-group commit carried failing checks, or the confirming fetch errored
   and we err on the side of surfacing it). When the ejection was a conflict
   with a sibling — [ejection_confirmed = false] — we do nothing: the patch
   agent must not be told to "look for failing checks" that do not exist. The
   conflict surfaces on its own once the sibling merges and the PR's
   [merge_state] flips to [Conflicting], which enqueues [Merge_conflict]
   through the normal path. *)
let apply ~previous_entry ~ejection_confirmed poll_result =
  if
    ejection_confirmed
    && should_treat_ejection_as_ci_failure ~previous_entry poll_result
  then { poll_result = apply_failure poll_result; merge_queue_ejected = true }
  else { poll_result; merge_queue_ejected = false }

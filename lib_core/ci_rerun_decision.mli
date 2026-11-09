(* @archlint.module core
   @archlint.domain ci-rerun *)

(** Pure decisions used when requesting GitHub Actions failed-job reruns. *)

type workflow_status = Failed | Pending | Resolved | Malformed
[@@deriving show, eq]

val workflow_run_id_from_url : string -> int option
(** Extract the numeric run id after [/actions/runs/] from a check URL. Returns
    [None] for missing, empty, non-numeric, or overflowing ids. *)

val unique_workflow_checks : Types.Ci_check.t list -> Types.Ci_check.t list
(** Keep the first check for each identifiable workflow run, preserving input
    order. Checks without an identifiable run are retained so the handler can
    report their individual errors instead of silently dropping them. *)

val workflow_status_of_response : string -> workflow_status
(** Decode the current workflow observation. Only a completed failed conclusion
    permits a retry. Running/queued work is [Pending],
    successful/neutral/skipped/cancelled/stale completion is [Resolved], and
    incomplete or unknown data is [Malformed]. Never raises. *)

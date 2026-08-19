(* @archlint.module core
   @archlint.domain ci-rerun *)

(** Pure decisions used when requesting GitHub Actions failed-job reruns. *)

type workflow_status = Completed | Pending | Malformed [@@deriving show, eq]

val workflow_run_id_from_url : string -> int option
(** Extract the numeric run id after [/actions/runs/] from a check URL. Returns
    [None] for missing, empty, non-numeric, or overflowing ids. *)

val unique_workflow_checks : Types.Ci_check.t list -> Types.Ci_check.t list
(** Keep the first check for each identifiable workflow run, preserving input
    order. Checks without an identifiable run are retained so the handler can
    report their individual errors instead of silently dropping them. *)

val workflow_status_of_response : string -> workflow_status
(** Decode [GET /actions/runs/:id]. [Completed] is returned only for
    [{"status":"completed"}], any other string status is [Pending], and
    malformed JSON or a missing/non-string status is [Malformed]. Never raises.
*)

(* @archlint.module interface
   @archlint.domain display-status *)
open Types

(** Observable patch activity and waiting reasons. Historical constructors
    remain decodable in persisted activity logs. *)
type t =
  | Merged
  | Wontdo
  | Needs_help
  | In_merge_queue
  | Approved_idle
  | Approved_running
  | Fixing_ci
  | Addressing_review
  | Addressing_findings
  | Resolving_conflict
  | Responding_to_human
  | Writing_pr_body
  | Cleaning_worktree
  | Rebasing
  | Starting
  | Updating
  | Ci_queued
  | Review_queued
  | Findings_queued
  | Cleanup_queued
  | Awaiting_feedback
  | Waiting_for_integration_root
  | Blocked_by_dep
  | Pending
  | Human_queued
  | Conflict_queued
  | Pr_body_queued
  | Rebase_queued
  | Waiting_for_ci
  | Waiting_for_review
  | Waiting_for_forge
  | Ci_failed
  | Conflict
  | Draft
  | Ready_for_integration
  | Reconciling
  | Checking_branch
  | Verifying_publication
  | Integrating
  | Repairing_branch
  | Publishing
  | Waiting_to_retry
[@@deriving show, eq, sexp_of, compare, yojson]

val is_on_main :
  State.Patch_ctx.t -> patch_id:Patch_id.t -> main_branch:Branch.t -> bool

val feedback_status :
  confirmed:bool ->
  has_conflict:bool ->
  checks_passing:bool ->
  ci_checks:Ci_check.t list ->
  review_decision:string option ->
  unresolved_comment_count:int ->
  is_draft:bool ->
  t

val reconciliation_status : Branch_reconcile.t -> t option
(** Current owner command, including inspection, mutation and retry waits.
    Settled owners have no reconciliation status. *)

val derive :
  ?feedback:t ->
  ?reconciliation:t ->
  State.Patch_ctx.t ->
  patch_id:Patch_id.t ->
  current_op:Operation_kind.t option ->
  main_branch:Branch.t ->
  t
(** Terminal states precede active work, owner work, queued operations,
    readiness and passive waits, in that order. *)

val of_agent :
  execution_mode:Execution_mode.t -> main_branch:Branch.t -> Patch_agent.t -> t
(** Project authoritative agent and owner state for a live patch. *)

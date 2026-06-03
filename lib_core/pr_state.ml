open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type merge_state = Mergeable | Conflicting | Unknown [@@deriving show, eq]
type check_status = Passing | Failing | Pending [@@deriving show, eq]
type pr_status = Open | Merged | Closed [@@deriving show, eq]

type merge_queue_entry_state =
  | Mq_queued
  | Mq_awaiting_checks
  | Mq_mergeable
  | Mq_unmergeable
  | Mq_locked
[@@deriving show, eq, sexp_of, compare, yojson]

type merge_queue_entry = {
  id : string;
  state : merge_queue_entry_state;
  position : int;
}
[@@deriving show, eq, sexp_of, compare, yojson]

type t = {
  status : pr_status;
  is_draft : bool;
  merge_state : merge_state;
  merge_ready : bool;
  merge_state_status : string option;
      (** Raw GitHub [mergeStateStatus] (CLEAN / UNKNOWN / BEHIND / BLOCKED /
          DIRTY / UNSTABLE / HAS_HOOKS / DRAFT). [merge_ready] collapses this to
          [= CLEAN]; the raw value is retained purely for diagnostics so the
          event log can explain *why* [merge_ready] flipped — a transient
          [UNKNOWN] (GitHub recomputing mergeability after the base advanced)
          vs. a real [BLOCKED]/[DIRTY]. [None] when the forge did not report it.
      *)
  review_decision : string option;
      (** Raw GitHub [reviewDecision] (APPROVED / REVIEW_REQUIRED /
          CHANGES_REQUESTED / null). Captured for diagnostics only — approval is
          currently inferred from [merge_ready], not this field. *)
  check_status : check_status;
  ci_checks : Types.Ci_check.t list;
  ci_checks_truncated : bool;
  comments : Types.Comment.t list;
  unresolved_comment_count : int;
  findings : Review_service.finding list;
      (** Unresolved findings from review-service backends, merged across all
          configured sources. Empty when no review backends are configured or
          all returned empty. Distinct from [comments] (GitHub review threads)
          because the agent's resolution surface differs — see
          {!Operation_kind.Findings}. *)
  node_id : string option;
  merge_queue_required : bool;
  merge_queue_entry : merge_queue_entry option;
  head_branch : Types.Branch.t option;
  head_oid : string option;
  merge_commit_sha : string option;
      (** For a merged PR, the squash/merge commit SHA on the base branch
          (GitHub [mergeCommit.oid]). [None] for open/closed PRs or when the
          forge has not yet reported it. *)
  base_branch : Types.Branch.t option;
  is_fork : bool;
}
[@@deriving show, eq]

let merged (st : t) = equal_pr_status st.status Merged
let closed (st : t) = equal_pr_status st.status Closed
let is_draft (st : t) = st.is_draft

let mergeable (st : t) =
  equal_pr_status st.status Open && equal_merge_state st.merge_state Mergeable

let merge_ready (st : t) = equal_pr_status st.status Open && st.merge_ready
let checks_passing (st : t) = equal_check_status st.check_status Passing
let no_unresolved_comments (st : t) = st.unresolved_comment_count = 0
let has_conflict (st : t) = equal_merge_state st.merge_state Conflicting
let ci_failed (st : t) = equal_check_status st.check_status Failing
let is_fork (st : t) = st.is_fork
let requires_merge_queue (st : t) = st.merge_queue_required
let enqueued (st : t) = Option.is_some st.merge_queue_entry

(** Derive the aggregate [check_status] from a list of individual CI check
    conclusions. This is the source of truth for what the orchestrator considers
    a "failing", "passing", or "pending" CI state — bypassing GitHub's
    [statusCheckRollup.state] which conflates cancelled/superseded runs with
    actual failures.

    Semantics:
    - [Failing] if at least one check has a conclusion in
      [Ci_check.failure_conclusions].
    - [Passing] if the list is non-empty and every check has a conclusion in
      [Ci_check.success_conclusions].
    - [Pending] otherwise (empty list, or any mix containing cancelled, pending,
      in_progress, queued, unknown conclusions, …). *)
let derive_check_status (checks : Types.Ci_check.t list) : check_status =
  if List.exists checks ~f:Types.Ci_check.is_failure then Failing
  else if
    (not (List.is_empty checks))
    && List.for_all checks ~f:Types.Ci_check.is_success
  then Passing
  else Pending

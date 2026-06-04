open Types

(** Forge-agnostic pull request state.

    Contains the data model and predicates for WorldCtx from the spec: merged,
    mergeable, checks-passing, no-unresolved-comments, world-has-comment,
    world-has-conflict, world-ci-failed. *)

type merge_state = Mergeable | Conflicting | Unknown
[@@deriving show, eq, yojson]

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

type merge_ready_divergence = {
  github_merge_state_status : string;  (** What GitHub's rollup reported. *)
  derived_merge_ready : bool;  (** What {!merge_ready_of} derived. *)
}
[@@deriving show, eq]
(** A disagreement between onton's component-derived readiness and GitHub's own
    [mergeStateStatus] rollup, captured at poll time for diagnostics. Surfaces
    the stale-[BLOCKED] smell (derived ready, GitHub not) and the conservative
    optional-check case (GitHub CLEAN, derived not-ready) without onton carrying
    [mergeStateStatus] as state or letting it drive any decision. *)

type t = {
  status : pr_status;
  is_draft : bool;
  merge_state : merge_state;
  merge_ready : bool;
      (** Component-derived merge readiness (see {!merge_ready_of}): the PR is
          mergeable, CI is passing, and the review decision is non-blocking. NOT
          GitHub's [mergeStateStatus] — that field is a stale, event-driven
          cache and is no longer carried as state or used by any decision. *)
  merge_ready_divergence : merge_ready_divergence option;
      (** Set at poll time when [merge_ready] disagrees with GitHub's
          [mergeStateStatus] rollup (see {!merge_ready_divergence_of}).
          Diagnostics only — never persisted, never an input to a decision; the
          poller fiber logs it once and discards it. [None] when the verdicts
          agree or GitHub reported no status. *)
  review_decision : string option;
      (** Raw GitHub [reviewDecision] (APPROVED / REVIEW_REQUIRED /
          CHANGES_REQUESTED / null). An input to {!merge_ready_of}: a
          [REVIEW_REQUIRED]/[CHANGES_REQUESTED] decision blocks readiness. *)
  check_status : check_status;
  ci_checks : Ci_check.t list;
  ci_checks_truncated : bool;
  comments : Comment.t list;
  unresolved_comment_count : int;
  findings : Review_service.finding list;
  node_id : string option;
  merge_queue_required : bool;
  merge_queue_entry : merge_queue_entry option;
  head_branch : Branch.t option;
  head_oid : string option;
  merge_commit_sha : string option;
      (** For a merged PR, the squash/merge commit SHA on the base branch
          (GitHub [mergeCommit.oid]). [None] for open/closed PRs or when the
          forge has not yet reported it. *)
  base_branch : Branch.t option;
  is_fork : bool;
}
[@@deriving show, eq]

(** {2 WorldCtx predicates} *)

val merged : t -> bool
val closed : t -> bool
val is_draft : t -> bool
val mergeable : t -> bool
val merge_ready : t -> bool
val checks_passing : t -> bool
val no_unresolved_comments : t -> bool
val has_conflict : t -> bool
val ci_failed : t -> bool
val is_fork : t -> bool
val requires_merge_queue : t -> bool
val enqueued : t -> bool

val derive_check_status : Ci_check.t list -> check_status
(** Aggregate [check_status] from individual CI check conclusions. Source of
    truth for the orchestrator's notion of passing/failing/pending CI,
    independent of GitHub's [statusCheckRollup.state] (which treats cancelled
    runs as FAILURE). See implementation for exact semantics. *)

val review_blocking : string option -> bool
(** [true] when a GitHub [reviewDecision] blocks merging
    ([REVIEW_REQUIRED]/[CHANGES_REQUESTED]). [APPROVED] and [None] (no review
    required, or none yet) are non-blocking. *)

val merge_ready_divergence_of :
  merge_ready:bool ->
  github_merge_state_status:string option ->
  merge_ready_divergence option
(** Compare onton's derived [merge_ready] against GitHub's [mergeStateStatus]
    rollup. [Some _] iff GitHub reported a status and the two disagree
    ([merge_ready <> (status = "CLEAN")]); [None] when they agree or no status
    was reported. Pure; the result is logged by the poller fiber, not stored. *)

val merge_ready_of :
  merge_state:merge_state ->
  check_status:check_status ->
  review_decision:string option ->
  bool
(** The single source of truth for merge readiness, shared by the merge decision
    ([Patch_controller.is_automerge_candidate] via [Patch_agent.is_approved])
    and the UI label ([Display_status.derive]). [true] iff the PR is
    [Mergeable], CI is [Passing], and the review decision is non-blocking.
    Replaces the former [mergeStateStatus = CLEAN] gate, which coupled both to a
    cached GitHub field that goes stale; here readiness is derived from facts
    refreshed every poll, with the merge *attempt* as the final authority.
    Conversation-resolution and conflict gating are handled out of band (they
    enqueue operations), so they are not folded in here. *)

open Base

(** Per-patch agent state machine.

    Encodes the spec fragment for actions: Start, Respond, Complete. The type
    [t] is private — external code can inspect fields but must use smart
    constructors that enforce spec preconditions. *)

type pending_comment = private { comment : Types.Comment.t; valid : bool }
[@@deriving show, eq, sexp_of, compare]

type session_fallback = Fresh_available | Tried_fresh | Given_up
[@@deriving show, eq, sexp_of, compare]

type t = private {
  patch_id : Types.Patch_id.t;
  has_pr : bool;
  pr_number : Types.Pr_number.t option;
  has_session : bool;
  busy : bool;
  merged : bool;
  needs_intervention : bool;
  queue : Types.Operation_kind.t list;
  satisfies : bool;
  changed : bool;
  has_conflict : bool;
  base_branch : Types.Branch.t option;
  ci_failure_count : int;
  session_fallback : session_fallback;
  pending_comments : pending_comment list;
  last_session_id : Types.Session_id.t option;
  ci_checks : Types.Ci_check.t list;
  addressed_comment_ids : Set.M(Types.Comment_id).t;
  removed : bool;
}
[@@deriving show, eq, sexp_of, compare]

val create : Types.Patch_id.t -> t
(** Initial state for a patch: no PR, not busy, empty queue. *)

(** {2 Spec actions} *)

val start : t -> base_branch:Types.Branch.t -> t
(** [PatchCtx ~> Start] — begin work on a patch. Preconditions (checked):
    [~has_pr]. Caller must verify [in_gameplan] and [deps_satisfied] externally.
    Postconditions: [has_pr], [has_session], [busy], [satisfies],
    [base_branch = Some base_branch]. *)

val respond : t -> Types.Operation_kind.t -> t
(** [PatchCtx, Comments ~> Respond] — respond to queued feedback. Preconditions
    (checked): [has_pr], [~merged], [~busy], [~needs_intervention], [k] in
    [queue], [k] is [highest_priority]. Postconditions per spec: sets
    [has_session], [busy]; dequeues [k]; conditionally updates [satisfies],
    [changed], [has_conflict], and resolves [pending_comments]. *)

val complete : t -> t
(** [PatchCtx ~> Complete] — session finished. Preconditions (checked): [busy].
    Postconditions: [~busy]; recalculates [needs_intervention] from
    [ci_failure_count], [session_fallback], and [Human] in queue. *)

(** {2 State mutation helpers} *)

val enqueue : t -> Types.Operation_kind.t -> t
(** Add an operation to the queue (idempotent). *)

val mark_merged : t -> t
(** Mark the patch as merged. *)

val mark_removed : t -> t
(** Mark the patch as removed from orchestration. Unlike [mark_merged], removed
    patches are not treated as satisfied dependencies — dependents remain
    blocked. *)

val add_pending_comment : t -> Types.Comment.t -> valid:bool -> t
(** Add a pending review comment. *)

val set_session_failed : t -> t
(** Mark session fallback as [Given_up]. *)

val set_last_session_id : t -> Types.Session_id.t -> t
(** Record the session ID from the most recent Claude run. *)

val set_tried_fresh : t -> t
(** Advance session fallback to [Tried_fresh]. No-op if already [Tried_fresh] or
    [Given_up] — the fallback state only moves forward. *)

val clear_session_fallback : t -> t
(** Reset session fallback to [Fresh_available]. *)

val set_has_conflict : t -> t
(** Mark the patch as having a merge conflict. *)

val increment_ci_failure_count : t -> t
(** Increment the CI failure counter. *)

val clear_needs_intervention : t -> t
(** Clear the needs-intervention flag (e.g., after manual resolution). *)

val set_ci_checks : t -> Types.Ci_check.t list -> t
(** Replace the stored CI check details. *)

val add_addressed_comment_id : t -> Types.Comment_id.t -> t
(** Record a comment ID as addressed. *)

val is_comment_addressed : t -> Types.Comment_id.t -> bool
(** Check whether a comment has been addressed. *)

val reset_busy : t -> t
(** Reset a stale [busy] flag from a crashed session. If [busy], clears it and
    re-evaluates [needs_intervention] using the same logic as [complete]
    ([ci_failure_count >= 3 || session_failed], unless [Human] is queued). No-op
    if not busy. *)

(** {2 Queries} *)

val highest_priority : t -> Types.Operation_kind.t option
(** The highest-priority operation in the queue, or [None] if empty. *)

(** {2 Persistence support} *)

val set_pr_number : t -> Types.Pr_number.t -> t
(** Set the PR number for the patch. *)

val restore :
  patch_id:Types.Patch_id.t ->
  has_pr:bool ->
  pr_number:Types.Pr_number.t option ->
  has_session:bool ->
  busy:bool ->
  merged:bool ->
  needs_intervention:bool ->
  queue:Types.Operation_kind.t list ->
  satisfies:bool ->
  changed:bool ->
  has_conflict:bool ->
  base_branch:Types.Branch.t option ->
  ci_failure_count:int ->
  session_fallback:session_fallback ->
  pending_comments:pending_comment list ->
  last_session_id:Types.Session_id.t option ->
  ci_checks:Types.Ci_check.t list ->
  addressed_comment_ids:Set.M(Types.Comment_id).t ->
  removed:bool ->
  t
(** Reconstruct agent state from persisted field values. Bypasses precondition
    checks — use only for deserialization. *)

val restore_pending_comment :
  comment:Types.Comment.t -> valid:bool -> pending_comment
(** Reconstruct a pending comment from persisted values. *)

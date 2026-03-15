open Base

(** Per-patch agent state machine.

    Encodes the spec fragment for actions: Start, Respond, Complete. The type
    [t] is private — external code can inspect fields but must use smart
    constructors that enforce spec preconditions. *)

type pending_comment = private { comment : Types.Comment.t; valid : bool }
[@@deriving show, eq, sexp_of, compare]

type t = private {
  patch_id : Types.Patch_id.t;
  has_pr : bool;
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
  session_failed : bool;
  pending_comments : pending_comment list;
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
    [ci_failure_count], [session_failed], and [Human] in queue. *)

(** {2 State mutation helpers} *)

val enqueue : t -> Types.Operation_kind.t -> t
(** Add an operation to the queue (idempotent). *)

val mark_merged : t -> t
(** Mark the patch as merged. *)

val add_pending_comment : t -> Types.Comment.t -> valid:bool -> t
(** Add a pending review comment. *)

val set_session_failed : t -> t
(** Mark the current session as failed. *)

val set_has_conflict : t -> t
(** Mark the patch as having a merge conflict. *)

val increment_ci_failure_count : t -> t
(** Increment the CI failure counter. *)

val clear_needs_intervention : t -> t
(** Clear the needs-intervention flag (e.g., after manual resolution). *)

(** {2 Queries} *)

val priority : Types.Operation_kind.t -> int
(** Priority ordering for operation kinds. Higher is more urgent. *)

val highest_priority : t -> Types.Operation_kind.t option
(** The highest-priority operation in the queue, or [None] if empty. *)

val is_feedback : Types.Operation_kind.t -> bool
(** All operation kinds are feedback kinds. *)

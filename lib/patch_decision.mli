open Types

(** Pure decision functions for patch agents.

    Each function inspects agent state and returns a decision value. The caller
    (runner/poller) acts on the decision. No I/O is performed here. *)

(** {2 Disposition — what should the orchestrator do with this patch?} *)

type disposition =
  | Skip  (** Patch is merged — nothing to do. *)
  | Blocked  (** Patch needs intervention — waiting for human. *)
  | Busy  (** Patch is already executing — queue any new work. *)
  | Idle  (** Patch is idle with no queued work. *)
  | Ready_start  (** Patch is ready to start (no PR yet). *)
  | Ready_respond of Operation_kind.t
      (** Patch has queued feedback to address. *)
  | Ready_rebase  (** Patch has a queued rebase as highest priority. *)
[@@deriving show, eq, sexp_of, compare]

val disposition : Patch_agent.t -> disposition
(** Determine the current disposition of a patch agent. *)

(** {2 Event decisions} *)

type ci_decision =
  | Enqueue_ci  (** CI failure count below cap — enqueue Ci feedback. *)
  | Ci_already_queued  (** Ci already in queue — no action needed. *)
  | Cap_reached  (** CI failure count >= 3 — do not enqueue, flag. *)
[@@deriving show, eq, sexp_of, compare]

val on_ci_failure : Patch_agent.t -> ci_decision
(** Decide whether to enqueue a CI failure response or cap. *)

type comment_decision = { new_comments : Comment.t list; should_enqueue : bool }
[@@deriving show, eq]

val on_review_comments :
  Patch_agent.t -> comments:Comment.t list -> comment_decision
(** Filter comments by addressed IDs. Returns new (unaddressed) comments and
    whether to enqueue review feedback. *)

type human_decision =
  | Enqueue_human  (** Queue human feedback for processing. *)
  | Already_queued  (** Human feedback already in queue. *)
[@@deriving show, eq, sexp_of, compare]

val on_human_message : Patch_agent.t -> human_decision
(** Decide whether to enqueue a human message. *)

type conflict_decision =
  | Enqueue_conflict  (** Queue merge conflict resolution. *)
  | Already_conflicting  (** Conflict already tracked. *)
[@@deriving show, eq, sexp_of, compare]

val on_merge_conflict : Patch_agent.t -> conflict_decision
(** Decide whether to enqueue merge conflict resolution. *)

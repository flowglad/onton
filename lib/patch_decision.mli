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
  | Ci_fix_in_progress
      (** Agent is already fixing CI — suppress until checks pass. *)
  | Cap_reached  (** CI failure count >= 3 — do not enqueue, flag. *)
[@@deriving show, eq, sexp_of, compare]

val on_ci_failure : Patch_agent.t -> ci_decision
(** Decide whether to enqueue a CI failure response or cap. *)

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

type checks_passing_decision =
  | Reset_ci_failure_count
      (** CI checks now pass after prior failures — reset the counter. *)
  | No_ci_reset
      (** No reset needed (no prior failures, or checks not passing). *)
[@@deriving show, eq, sexp_of, compare]

val on_checks_passing :
  Patch_agent.t -> checks_passing:bool -> checks_passing_decision
(** Decide whether to reset [ci_failure_count] based on current check status.
    Returns [Reset_ci_failure_count] when failures existed and checks now pass.
*)

val should_clear_conflict : Patch_agent.t -> bool
(** Whether it is safe to clear [has_conflict]. Returns [false] when a
    Merge_conflict operation is queued or in-flight, since clearing would race
    with the active resolution. *)

(** {2 Delivery decision — should the runner skip an empty delivery?} *)

val ci_failure_conclusions : string list
(** CI conclusion strings that count as failures. *)

val filter_failed_ci_checks : Ci_check.t list -> Ci_check.t list
(** Return only checks whose conclusion is in [ci_failure_conclusions]. *)

val has_failed_ci_checks : Ci_check.t list -> bool
(** Whether any check has a failure conclusion. *)

type ci_prompt_kind =
  | Known_failures of Ci_check.t list
      (** Non-empty list of checks with failure conclusions. *)
  | Unknown_failure  (** CI failed but no check matches failure conclusions. *)
[@@deriving show, eq, sexp_of, compare]

val ci_prompt_kind : Ci_check.t list -> ci_prompt_kind
(** Decide which CI prompt variant to render. Returns [Known_failures] with the
    filtered list when at least one check has a failure conclusion,
    [Unknown_failure] otherwise. *)

val is_stale : Patch_agent.t -> bool
(** Whether an action should be skipped because the agent state changed while
    waiting for a Claude slot. True when any of: merged, needs_intervention,
    branch_blocked, or not busy. *)

type delivery_decision =
  | Deliver  (** There is content to deliver to the agent. *)
  | Skip_empty  (** Nothing to deliver — skip this operation. *)
[@@deriving show, eq, sexp_of, compare]

val delivery_decision :
  kind:Operation_kind.t ->
  inflight_human_messages:string list ->
  review_comment_count:int ->
  ci_checks:Ci_check.t list ->
  delivery_decision
(** Pure decision: given the operation kind and the relevant payload data,
    decide whether there is content to deliver. The caller must pass:
    - [inflight_human_messages]: post-fire agent's inflight messages
    - [review_comment_count]: number of prefetched review comments
    - [ci_checks]: agent's CI check list at fire time *)

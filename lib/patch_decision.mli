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

(** {2 Respond delivery — pre-session decisions for the runner} *)

val failure_conclusions : string list
(** CI check conclusions that indicate failure. *)

type base_change = { old_base : string; new_base : string }
[@@deriving show, eq, sexp_of, compare]

type delivery_payload =
  | Human_payload of { messages : string list }
  | Ci_payload of { failed_checks : Ci_check.t list }
  | Review_payload of { comments : Comment.t list }
  | Pr_body_payload
  | Merge_conflict_payload
[@@deriving show, eq, sexp_of, compare]

type respond_delivery =
  | Deliver of { payload : delivery_payload; base_change : base_change option }
  | Skip_empty
  | Respond_stale
[@@deriving show, eq, sexp_of, compare]

val respond_delivery :
  agent:Patch_agent.t ->
  kind:Operation_kind.t ->
  pre_fire_agent:Patch_agent.t option ->
  prefetched_comments:Comment.t list ->
  main_branch:string ->
  respond_delivery
(** Pure pre-session decision for Respond actions. Determines whether the
    delivery should proceed, be skipped (empty payload), or is stale.

    When [pre_fire_agent] is [Some pfa], human messages and CI checks are read
    from [pfa] (the snapshot before fire moved messages to inflight). When
    [None], falls back to [agent]. Review comments come from
    [prefetched_comments] (fetched from GitHub before the decision). *)

(* @archlint.module interface
   @archlint.domain patch-decision *)

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
  | Ci_already_delivered
      (** Every currently failing check with a stable run id was already
          delivered to the agent. *)
  | Cap_reached
      (** CI failure count >= [max_ci_failures] — do not enqueue, flag. *)
[@@deriving show, eq, sexp_of, compare]

val on_ci_failure : Patch_agent.t -> ci_decision
(** Decide whether to enqueue a CI failure response or cap. The cap is the
    agent's [max_ci_failures] field (per-project config, default
    {!Patch_agent.default_max_ci_failures}). *)

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
  | Findings_payload of { findings : Review_service.finding list }
      (** Review-service findings — distinct from GitHub review comments.
          Carries the data needed both for prompting the agent and (after the
          session) issuing resolve verbs back to the originating backend. *)
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
  prefetched_findings:Review_service.finding list ->
  main_branch:string ->
  respond_delivery
(** Pure pre-session decision for Respond actions. Determines whether the
    delivery should proceed, be skipped (empty payload), or is stale.

    When [pre_fire_agent] is [Some pfa], human messages are read from [pfa] (the
    snapshot before fire moved messages to inflight); when [None], falls back to
    [agent]. Review comments come from [prefetched_comments] (fetched from
    GitHub before the decision). CI checks are read from [agent] — the caller
    re-polls GitHub before delivery and writes the fresh list into
    [agent.ci_checks] via [Orchestrator.set_ci_checks], then skips the call
    entirely if no failures remain. *)

(** {2 Pr_body post-session classification} *)

val classify_pr_body_respond :
  artifact_outcome:[ `Ok | `Missing | `Empty | `Patch_failed ] ->
  tool_failures:(string * string) list ->
  [ `Ok | `Pr_body_miss ]
(** Pure post-session rule for Pr_body. Returns [`Pr_body_miss] when:
    - artifact is [`Patch_failed] (GitHub [update_pr_body] call failed, PR body
      is stale and the runner must retry rather than mark delivered); or
    - artifact is [`Missing] or [`Empty] AND at least one [(name, status)] pair
      in [tool_failures] has [name = "Write"] (agent blocked mid-write).

    Any other combination returns [`Ok].

    Only invoked after a successful session — session-level failures
    (stale/failed/retry_push) short-circuit upstream in the handler and never
    reach this function. *)

(** {2 Opportunistic pr-body artifact sync from any session}

    The patch agent can in principle update [pr-body.md] during any session (CI,
    Review_comments, Human, Merge_conflict, Rebase) — not only [Pr_body]. The
    runner takes a content snapshot before launching the agent and another after
    the session, then asks these pure functions whether to PATCH the PR and what
    state changes to apply.

    [Pr_body] sessions are deliberately excluded here: their delivery path is
    owned by [classify_pr_body_respond] (with its own miss-counter and
    intervention semantics). [plan_artifact_sync] returns [Sync_skip] for
    [Pr_body] so the opportunistic block is a no-op for that kind. *)

val pr_body_artifact_changed : pre:string option -> post:string option -> bool
(** Pure: did the effective body change between the pre- and post-session
    snapshots? Whitespace-only and empty strings collapse to [None] before
    comparison, so a whitespace-only post is equivalent to no artifact at all.
    Truncation from non-empty content to empty (e.g. [Some "x" -> Some ""])
    collapses post to [None] but pre stays [Some "x"], so it is treated as a
    change and is later mapped to [Sync_no_op] by
    [classify_artifact_sync_outcome] (since [apply_pr_body_artifact] returns
    [`Empty] and keeps the initial PR body). *)

type artifact_sync_plan =
  | Sync_skip
      (** No PATCH attempt; no state change. Used when (a) the session failed,
          (b) [kind = Pr_body] (which has its own classify path), or (c) the
          artifact was unchanged. *)
  | Sync_attempt_pr_body
      (** Caller should run [apply_pr_body_artifact] and feed the result into
          [classify_artifact_sync_outcome]. *)
[@@deriving show, eq, sexp_of, compare]

val plan_artifact_sync :
  kind:Operation_kind.t ->
  session_ok:bool ->
  pre:string option ->
  post:string option ->
  artifact_sync_plan
(** Pure pre-PATCH planner. Returns [Sync_attempt_pr_body] only when the session
    succeeded, the operation kind is not [Pr_body], and the artifact actually
    changed. Otherwise [Sync_skip]. *)

type artifact_sync_outcome =
  | Sync_no_op
  | Sync_delivered
      (** PATCH succeeded — caller should set [pr_body_delivered = true] and
          reset [pr_body_artifact_miss_count] to 0. *)
  | Sync_patch_failed
      (** PATCH failed — caller should log only; do NOT mutate
          [pr_body_delivered] or the miss count. The next [Pr_body] cycle will
          retry through the regular classifier path. *)
[@@deriving show, eq, sexp_of, compare]

val classify_artifact_sync_outcome :
  plan:artifact_sync_plan ->
  patch_result:[ `Ok | `Missing | `Empty | `Patch_failed ] option ->
  artifact_sync_outcome
(** Pure post-PATCH classifier. Total over every (plan, patch_result)
    combination so it is safe to apply over arbitrary generators in property
    tests, even on inputs the runner never produces.

    - [plan = Sync_skip] → [Sync_no_op] regardless of [patch_result].
    - [plan = Sync_attempt_pr_body], [patch_result = Some `Ok] →
      [Sync_delivered].
    - [plan = Sync_attempt_pr_body], [patch_result = Some `Patch_failed] →
      [Sync_patch_failed].
    - [plan = Sync_attempt_pr_body], [patch_result = Some (`Missing | `Empty)]
      or [None] → [Sync_no_op]. The [`Empty] arm is reachable when a file
      changes from non-empty to empty: [normalize_artifact] (planner) and
      [apply_pr_body_artifact]'s [String.trim] check are independent, so both
      can fire on the same content change. [`Missing] is reachable if the file
      is deleted between the post-snapshot and the [apply_pr_body_artifact]
      call. [None] is reachable when the caller skips the PATCH despite the plan
      (e.g. an ad-hoc patch with no gameplan entry). *)

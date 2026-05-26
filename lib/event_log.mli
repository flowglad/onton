open Types

(** Append-only JSONL event log for diagnosing session state issues.

    Writes one JSON object per line to a file. Each entry includes a timestamp,
    event kind, and kind-specific context (agent state before/after, poll
    results, enqueue decisions). The log is never trimmed — use file rotation
    externally or rely on the built-in size-based rotation on [create]. *)

type t

val create : path:string -> t
(** Create an event log that appends to the given file path. If the file exceeds
    50 MB, it is rotated to [path.1] before returning. *)

val sink : t -> Telemetry.Sink.t
(** Telemetry sink that appends interested events to this JSONL log. *)

val log_poll :
  t ->
  patch_id:Patch_id.t ->
  poll_result:Poller.t ->
  agent_before:Patch_agent.t ->
  agent_after:Patch_agent.t ->
  logs:string list ->
  unit
(** Log a poll result with before/after agent state and enqueue decision
    messages. *)

val log_action :
  t -> action:Orchestrator.action -> agent_before:Patch_agent.t -> unit
(** Log an action being fired. *)

val log_complete :
  t ->
  patch_id:Patch_id.t ->
  result:Orchestrator.session_result ->
  agent_before:Patch_agent.t ->
  agent_after:Patch_agent.t ->
  unit
(** Log a session completion with before/after state. This facade receives only
    [session_result], so it records a coarse subkind; richer backend-tail
    classification is emitted by lower-level telemetry events. *)

val log_force_complete :
  t ->
  patch_id:Patch_id.t ->
  reason:Orchestrator.force_complete_reason ->
  agent_before:Patch_agent.t ->
  agent_after:Patch_agent.t ->
  unit
(** Log a forced completion triggered by an abnormally-exiting runner fiber
    (cancellation or unexpected exception). Distinct from [log_complete]: the
    LLM session itself produced no [session_result]. Emitted by both the
    [with_busy_guard] finally and [mark_session_failed] so every dispatched
    [Orchestrator.Respond] action has a matching close in the event log. *)

val log_conflict_rebase :
  t ->
  patch_id:Patch_id.t ->
  result:Worktree.rebase_result ->
  decision:Orchestrator.conflict_rebase_decision ->
  pre_rebase_head:string option ->
  post_rebase_head:string option ->
  target_base_sha:string option ->
  agent_before:Patch_agent.t ->
  agent_after:Patch_agent.t ->
  unit
(** Log a conflict rebase decision with before/after state. [pre_rebase_head] is
    the worktree's [refs/heads/<branch>] SHA before the rebase ran;
    [post_rebase_head] after; [target_base_sha] is the SHA the rebase used as
    [--onto]. Including these SHAs lets readers see at a glance whether the
    rebase actually moved the branch and where it landed, without
    cross-referencing the worktree filesystem. *)

val log_conflict_delivery :
  t ->
  patch_id:Patch_id.t ->
  path:string ->
  rebase_in_progress:bool ->
  git_status:string ->
  git_diff:string ->
  unit
(** Log the state at the moment a merge-conflict prompt is delivered to the
    agent. Captures the worktree path, whether a rebase was already in progress,
    and the git status/diff that were embedded in the prompt. *)

val log_rebase :
  t ->
  patch_id:Patch_id.t ->
  result:Worktree.rebase_result ->
  pre_rebase_head:string option ->
  post_rebase_head:string option ->
  target_base_sha:string option ->
  agent_before:Patch_agent.t ->
  agent_after:Patch_agent.t ->
  unit
(** Log a rebase result with before/after state and the SHAs involved.
    [pre_rebase_head] is the worktree's [refs/heads/<branch>] SHA before the
    rebase ran; [post_rebase_head] after; [target_base_sha] is the SHA the
    rebase used as [--onto]. Including these SHAs lets readers diagnose "rebase
    produced Noop / Ok — but did the branch actually move?" without consulting
    the worktree. *)

(** Distinguishes the push site that produced the event. [Session_end_push] is
    the supervisor-owned end-of-session push in [session_driver.ml].
    [Rebase_resolution_push] is the post-rebase push in [runner_fiber_impl.ml]'s
    rebase branch. [Conflict_resolution_push] is the push that follows the
    conflict-resolution rebase in the same module. Knowing which site fired lets
    log readers correlate a refused push with the specific flow that triggered
    it. *)
type push_kind =
  | Session_end_push
  | Rebase_resolution_push
  | Conflict_resolution_push
[@@deriving show, eq, sexp_of, compare]

val log_push :
  t ->
  patch_id:Patch_id.t ->
  kind:push_kind ->
  result:Worktree.push_result ->
  local_sha:string option ->
  remote_tracking_sha:string option ->
  base_sha:string option ->
  agent_before:Patch_agent.t ->
  agent_after:Patch_agent.t ->
  unit
(** Log a [git push] outcome. Captures the push site ([kind]), the
    [Worktree.push_result] variant (which carries the planner refusal label
    inside [Push_rejected] via [Push_reject_classify.short_label]), and a
    snapshot of [refs/heads/<branch>] / [refs/remotes/origin/<branch>] /
    [refs/heads/<base>] SHAs at push time. Surfacing the SHAs directly avoids
    the cross-checking dance that would otherwise be required to diagnose
    post-rebase divergence symptoms after the fact. *)

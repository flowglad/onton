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
(** Log a session completion with before/after state. *)

val log_conflict_rebase :
  t ->
  patch_id:Patch_id.t ->
  decision:Orchestrator.conflict_rebase_decision ->
  agent_before:Patch_agent.t ->
  agent_after:Patch_agent.t ->
  unit
(** Log a conflict rebase decision with before/after state. *)

val log_rebase :
  t ->
  patch_id:Patch_id.t ->
  result:Worktree.rebase_result ->
  agent_before:Patch_agent.t ->
  agent_after:Patch_agent.t ->
  unit
(** Log a rebase result with before/after state. *)

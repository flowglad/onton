(* @archlint.module interface
   @archlint.domain worktree-setup *)

(** Worktree provisioning for a patch.

    The runner and the worktree-plan executor both need to materialise a
    worktree on disk before they can do anything else with a patch. This module
    owns that logic so both callers go through the same code path — same
    persistence, same hook invocation, same logging. *)

module type ENV = Run_env.S
(** Construction-time environment for worktree provisioning. Values here are
    fixed for the lifetime of the module instance and never vary per call. *)

type ensure_result = Path of string | Missing | Refused

module type S = sig
  val resolve_worktree_path :
    patch_id:Types.Patch_id.t ->
    agent:Patch_agent.t ->
    ?branch:Types.Branch.t ->
    unit ->
    string
  (** Resolve the worktree path for a patch. Checks the stored path first, then
      searches git worktrees by branch, then falls back to the canonical
      [Worktree.worktree_dir] path. Pure-ish — no side effects on disk; the
      persisted path on the agent record is consulted but not written. *)

  val ensure_worktree :
    patch_id:Types.Patch_id.t ->
    agent:Patch_agent.t ->
    ?branch:Types.Branch.t ->
    ?base_ref:string ->
    unit ->
    ensure_result
  (** Ensure a validated checkout is ready for the patch. Returns [Path path] on
      success, [Refused] for unsafe start points, unfinished creation or failed
      readiness/recovery checks (routed through [Session_push_failed]), or
      [Missing] when creation failed without an adoptable checkout. On creation,
      runs the user's [on_worktree_create] hook serialised through
      [Env.hook_mutex] and persists the path on the agent record. All log lines
      go through [Runtime_logging.log_event]. *)
end

module Make (_ : Worktree.S) (_ : ENV) : S

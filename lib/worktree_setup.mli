(** Worktree provisioning for a patch.

    The runner and the worktree-plan executor both need to materialise a
    worktree on disk before they can do anything else with a patch. This module
    owns that logic so both callers go through the same code path — same
    persistence, same hook invocation, same logging. *)

module type ENV = sig
  val runtime : Runtime.t
  val clock : float Eio.Time.clock_ty Eio.Time.clock
  val fs : Eio.Fs.dir_ty Eio.Path.t
  val project_name : string
  val user_config : User_config.t
  val worktree_mutex : Eio.Mutex.t
  val hook_mutex : Eio.Mutex.t
end
(** Construction-time environment for worktree provisioning. Values here are
    fixed for the lifetime of the module instance and never vary per call. *)

module Make (_ : Worktree.S) (_ : ENV) : sig
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
    string option
  (** Ensure a worktree exists for the patch. Returns [Some path] on success or
      [None] when the branch's worktree cannot be created (e.g. it's checked out
      in the main tree). On creation, runs the user's [on_worktree_create] hook
      serialised through [Env.hook_mutex] and persists the path on the agent
      record. All log lines go through [Runtime_logging.log_event]. *)
end

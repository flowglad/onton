(* @archlint.module interface
   @archlint.domain worktree-setup *)

(** Worktree provisioning for a patch.

    Session and scheduled work request materialization through the
    reconciliation owner. This module handles checkout creation, durable
    creation hooks and readiness inspection under that owner's write authority.
*)

module type ENV = Run_env.S
(** Construction-time environment for worktree provisioning. Values here are
    fixed for the lifetime of the module instance and never vary per call. *)

type ensure_result =
  | Path of string
  | Unavailable of Worktree_provision.failure

type recovery_result =
  | Recovery_ready of string
      (** A confirmed checkout at the observed patch revision or current base.
      *)
  | Recovery_wait of string
      (** Evidence or a prerequisite is unavailable; retry after observation. *)
  | Recovery_attention of { reason : string; renewal_id : string }
      (** A formerly published branch disappeared without a confirmed merge. A
          new operation must cite this exact receipt to authorize recreation. *)
  | Recovery_merged of Types.Pr_number.t option
      (** Forge confirmed that the formerly published work was merged. *)

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
  (** Low-level command handler. Report typed failures without changing session
      fallback or failure budgets. A materialization checkpoint must succeed
      before setup reports success or runs a creation hook. Production callers
      reach this handler through the reconciliation owner. *)

  val execute_reconciliation :
    patch_id:Types.Patch_id.t ->
    operation:Branch_reconcile.operation ->
    Branch_reconcile.command ->
    Branch_reconcile.result
  (** Execute a checkpointed owned command. Provisioning settles with
      [Checkout_ready]; other commands retain the regular Git executor. *)

  val ensure_owned :
    owner:Runtime.patch_write -> ?base_ref:string -> unit -> ensure_result
  (** Request a fresh checkpointed inspection under existing write ownership.
      Resume pending provisioning first, honor owner backoff and intervention,
      and defer to any unrelated unfinished reconciliation. *)
end

module Make (_ : Worktree.S) (_ : ENV) : S

module type RECOVERY_S = sig
  include S

  exception Checkpoint_failed of string
  (** Host checkpoint rejection. The strict worker stops before dependent
      effects; callers may catch this to terminate the attempt. *)

  val recover_owned :
    owner:Runtime.patch_write ->
    request:string ->
    renewal_id:string option ->
    ?intent:Branch_reconcile.intent ->
    ?observe_forge:
      (unit ->
      ( Branch_reconcile.workspace_forge * Types.Pr_number.t option,
        string )
      Result.t) ->
    unit ->
    recovery_result
  (** Reconcile a lost workspace from remote and forge observations. Supply
      [renewal_id] only on an explicitly renewed operation after an outstanding
      [Recovery_attention] receipt; ordinary retry passes [None]. In the strict
      checkpoint variant, an unacknowledged state change raises before dependent
      effects, including [ensure_owned] inherited from [S]. *)
end

module Make_with_checkpoint
    (_ : Execution_checkpoint.S)
    (_ : Worktree.S)
    (_ : ENV) : RECOVERY_S

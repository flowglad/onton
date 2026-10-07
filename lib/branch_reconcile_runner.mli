(* @archlint.module interface
   @archlint.domain branch-reconcile *)

type outcome =
  | Idle
  | Repair_needed of Branch_reconcile.token
  | Waiting
  | Intervention of string
  | Checkpoint_failed of string

val run :
  runtime:Runtime.t ->
  persist:(Runtime.snapshot -> (unit, string) result) ->
  patch_id:Types.Patch_id.t ->
  now:(unit -> float) ->
  execute:
    (operation:Branch_reconcile.operation ->
    Branch_reconcile.command ->
    Branch_reconcile.result) ->
  Branch_reconcile.event ->
  outcome
(** Checkpoint each decision before executing its command. Failed checkpoints
    prevent the command and all its successors. The runner owns the existing
    patch/root write lock during execution. Repair and backoff return to the
    caller, releasing capacity rather than sleeping under a lock. *)

val run_owned :
  owner:Runtime.patch_write ->
  persist:(Runtime.snapshot -> (unit, string) result) ->
  now:(unit -> float) ->
  execute:
    (operation:Branch_reconcile.operation ->
    Branch_reconcile.command ->
    Branch_reconcile.result) ->
  Branch_reconcile.event ->
  outcome
(** Same checkpoint protocol under a session's existing write ownership. *)

val run_repair :
  runtime:Runtime.t ->
  persist:(Runtime.snapshot -> (unit, string) result) ->
  patch_id:Types.Patch_id.t ->
  with_capacity:((unit -> outcome) -> outcome) ->
  now:(unit -> float) ->
  execute:
    (agent:Patch_agent.t ->
    operation:Branch_reconcile.operation ->
    Branch_reconcile.command ->
    Branch_reconcile.result) ->
  perform:
    (agent:Patch_agent.t ->
    turn:Branch_reconcile.repair_turn ->
    Branch_reconcile.event) ->
  Branch_reconcile.token ->
  outcome
(** Wait for agent capacity without Git ownership, then acquire ownership and
    revalidate the durable claim. Stale claims never invoke [perform]. The turn
    and its resulting checkpointed Git commands share scoped write ownership. *)

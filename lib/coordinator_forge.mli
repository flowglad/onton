(* @archlint.module interface
   @archlint.domain coordinator-forge *)

open Types

type scope = Coordinator_observation.scope = {
  ticket : string;
  pr : Pr_number.t;
  head_branch : Branch.t;
  base_branch : Branch.t;
  head_revision : string;
  base_revision : string;
  push_destination : string;
  generation : int;
}

type observation =
  | Confirmed of Pr_state.t
  | Unconfirmed of string
  | Unavailable of string

val observe :
  (module Forge.S with type error = 'e) ->
  io:Branch_reconcile_executor.io ->
  scope ->
  observation
(** Read-only provider and direct Git confirmation. The head probe uses the
    captured push destination; the base probe uses origin's fetch destination.
    The caller must durably record and recheck the ticket before adopting this
    observation. An unavailable probe never proves a missing ref. *)

val observe_committed :
  ?now:float ->
  (module Forge.S with type error = 'e) ->
  host:Coordinator_host.t ->
  io:Branch_reconcile_executor.io ->
  patch:Patch_id.t ->
  scope:scope ->
  observation
(** Commit an immutable scoped observation ticket in the coordinator envelope
    before network reads, then atomically commit the result and versioned worker
    input. Reject changed ticket reuse and superseded tickets; a repeated ticket
    returns its committed snapshot, not a new read. Neither a request nor an
    uncommitted provider response grants Git/worker authority. *)

val ready :
  expected:scope -> current:scope -> policy_ready:bool -> observation -> bool
(** Diagnostic predicate only, not a mutation authorization API. Changed
    ticket/identity/revision denies readiness; [change] recomputes shared policy
    and its durable idle window instead of accepting this boolean. *)

val contribution_credit :
  expected:scope ->
  current:scope ->
  matching_receipt:bool ->
  already_credited:bool ->
  bool
(** Only a current captured contributor/root revision pair and owner receipt may
    credit integration. *)

type create_result =
  | Found of Pr_number.t
  | Pending of string
  | Blocked of string

val create_or_discover :
  (module Forge.S with type error = 'e) ->
  host:Coordinator_host.t ->
  io:Branch_reconcile_executor.io ->
  scope:scope ->
  patch:Patch_id.t ->
  effect_id:string ->
  renewal_id:string option ->
  title:string ->
  body:string ->
  draft:bool ->
  create_result
(** The run-scoped effect identity and exact scope commit before any provider
    write. After any uncertain create, including a lost response or restart,
    discover the actual PR; absence alone NEVER permits another create. A new
    explicit renewal must have its own independently authorized identity. *)

type change =
  | Set_body of string
  | Set_base of Branch.t
  | Set_draft of bool
  | Request_review of string
  | Enqueue
  | Dequeue
  | Merge

type change_result =
  | Applied
  | Change_pending of string
  | Change_blocked of string

val change :
  (module Forge.S with type error = 'e) ->
  host:Coordinator_host.t ->
  io:Branch_reconcile_executor.io ->
  scope:scope ->
  patch:Patch_id.t ->
  effect_id:string ->
  ?renewal_id:string ->
  view:Scheduler_decision.input ->
  now:float ->
  change ->
  change_result
(** Commit the exact effect identity and target before provider IO. Fresh
    committed pair proof, shared scheduler policy and the persisted idle window
    gate a first merge/queue attempt. Successful outcomes commit before return;
    unresolved effects reach durable attention. Replay discovers actual provider
    state and never repeats an ambiguous mutation because a read is unchanged. A
    new identity cannot bypass unresolved intent without explicit renewal. *)

val render_notes :
  gameplan:Gameplan.t ->
  patches:Patch.t list ->
  publications:(Patch_id.t * string) list ->
  projections:Worker_checkpoint.projection list ->
  string
(** Only validated checkpoint projections with current patch/publication
    identity contribute note bytes; never read worker filesystem paths. Callers
    must supply projections from committed host checkpoints. *)

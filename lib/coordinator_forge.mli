(* @archlint.module interface
   @archlint.domain coordinator-forge *)

open Types

type scope = {
  ticket : string;
  pr : Pr_number.t;
  head_branch : Branch.t;
  base_branch : Branch.t;
  head_revision : string;
  base_revision : string;
  push_destination : string;
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
  (module Forge.S with type error = 'e) ->
  host:Coordinator_host.t ->
  io:Branch_reconcile_executor.io ->
  patch:Patch_id.t ->
  scope:scope ->
  observation
(** Commit an immutable scoped observation ticket in the coordinator envelope
    before network reads. Reject changed ticket reuse and superseded tickets;
    neither a request nor a provider response grants Git/worker authority. *)

val ready :
  expected:scope -> current:scope -> policy_ready:bool -> observation -> bool
(** Policy readiness is supplied by the shared scheduler, never derived from a
    forge's candidate flag. Changed ticket/identity/revision denies readiness.
*)

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

val render_notes :
  gameplan:Gameplan.t ->
  patches:Patch.t list ->
  publications:(Patch_id.t * string) list ->
  projections:Worker_checkpoint.projection list ->
  string
(** Only validated checkpoint projections with current patch/publication
    identity contribute note bytes; never read worker filesystem paths. Callers
    must supply projections from committed host checkpoints. *)

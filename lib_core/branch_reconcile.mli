(* @archlint.module interface
   @archlint.domain branch-reconcile *)

open Base

module Commit : sig
  type t [@@deriving eq, compare, sexp_of]

  val make : string -> t option
  val to_string : t -> string
end

module Remote_id : sig
  type t [@@deriving eq, compare, sexp_of]

  val of_destination : string -> t
  (** Fingerprint the resolved destination without persisting URL credentials.
  *)
end

type materialization = New_branch of Commit.t | Adopted_branch of Commit.t
[@@deriving eq, compare, sexp_of]

type remote_binding [@@deriving eq, compare, sexp_of]

val materialization_head : materialization -> Commit.t
val materialization_boundary : materialization -> Commit.t option

type policy = Rewrite | Preserve_ancestry [@@deriving eq, compare, sexp_of]

type purpose =
  | Provision_checkout of string
      (** Provision or validate a checkout without integrating or publishing.
          Only [Checkout_ready] can settle this intent. Retry and permanent
          failures use the ordinary durable owner phases and command identity.
      *)
  | Reconcile_base
  | Reconcile_request of string
  | Reconcile_scoped of {
      request : string;
      project : string;
      ancestors : Types.Patch_id.t list;
    }
  | Integrate_revision of { contributor : string; revision : Commit.t }
  | Publish_revision of Commit.t
  | Publish_session of string
  | Verify_publication
[@@deriving eq, compare, sexp_of]

val is_provisioning : purpose -> bool

type intent = { base : string; policy : policy; purpose : purpose }
[@@deriving eq, compare, sexp_of]

type boundary =
  | Recorded of Commit.t
  | Inferred of Commit.t
  | Reconstructed of { original : Commit.t; upstream : Commit.t }
  | Subject_inferred of Commit.t
  | Patch_equivalent of Commit.t
  | Plain
[@@deriving eq, compare, sexp_of]

type boundary_choice = Chosen of boundary | Probe of Commit.t
[@@deriving eq, compare, sexp_of]

val choose_boundary :
  candidates:boundary list -> evidence:(Commit.t * bool) list -> boundary_choice
(** Select the first verified reachable boundary, or request the next ancestry
    observation. Missing evidence never counts as a negative observation. *)

val patch_equivalent_boundary :
  source:Commit.t -> string -> (Commit.t option, string) Result.t
(** Decode NUL-delimited cherry-mark/revision/parent records in newest-first
    topological order. A complete linear source chain with an equivalent prefix
    yields the parent of its oldest remaining unique commit. All-equivalent
    history yields [source]. Malformed/truncated probes are errors; merges and
    non-linear histories provide no boundary. This is best-effort evidence. *)

val reconstructed_boundary :
  source:Commit.t ->
  recorded:(Commit.t * string) list ->
  history:string ->
  (boundary option, string) Result.t
(** Reconstruct an unreachable recorded boundary using exact tree identity in a
    complete first-parent history (NUL-delimited revision/parents/tree).
    Recorded candidates take precedence in supplied order; the newest matching
    ancestor wins for each candidate. Both revisions remain explicit evidence.
    Missing/malformed topology is a failed probe. This grants no recorded
    publication authority. *)

val subject_scope : purpose -> (string * Types.Patch_id.t list) option
(** Dependency-subject heuristics require an explicitly captured project and
    nonempty dependency set. Unscoped legacy requests cannot infer them. *)

val subject_boundary :
  source:Commit.t ->
  project:string ->
  ancestors:Types.Patch_id.t list ->
  string ->
  (Commit.t option, string) Result.t
(** Read newest-first NUL-delimited revision/parents/subject records. Only a
    linear leading prefix matching the captured dependency subjects can be
    excluded. The result is best-effort inference, never recorded provenance. *)

type token = private { operation : int; command : int }
[@@deriving eq, compare, sexp_of]

type topology = Equal | Includes | Behind | Diverged | Unproven
[@@deriving eq, compare, sexp_of]

type observation = {
  destination : Remote_id.t;
  head : Commit.t;
  source : Commit.t;
  target : Commit.t;
  remote : Commit.t option;
  boundary : boundary;
  topology : topology;
  clean : bool;
  sequencer : string option;
  conflicts : int;
  target_included : bool;
  base_contains_source : bool;
      (** Checked against the desired base for initial publication observations.
      *)
  completed_integration : bool;
}
[@@deriving eq, compare, sexp_of]

type remote_replay = {
  preserved : Commit.t;
  incoming : Commit.t;
  upstream : Commit.t;
}
[@@deriving eq, compare, sexp_of]

type remote_replay_request = {
  preserved : Commit.t;
  incoming : Commit.t;
  boundaries : boundary list;
}
[@@deriving eq, compare, sexp_of]

type merge_completion = {
  head : Commit.t;
  target : Commit.t;
  parents : Commit.t list;
  sequencer : string;
  resumed_sequencer : string;
}
[@@deriving eq, compare, sexp_of]

type merge_progress =
  | Pending_merge of merge_completion
  | Completed_merge of { capture : merge_completion; head : Commit.t }
[@@deriving eq, compare, sexp_of]

type command_kind =
  | Observe
  | Commit_merge of merge_completion
  | Plan_remote_replay of remote_replay_request
  | Checkout_remote of remote_replay
  | Pin of { source : Commit.t; target : Commit.t }
  | Integrate of {
      source : Commit.t;
      target : Commit.t;
      boundary : boundary;
      policy : policy;
    }
  | Inspect
  | Verify_recovery
  | Continue of { head : Commit.t; target : Commit.t; sequencer : string }
  | Publish of { candidate : Commit.t; expected : Commit.t option }
  | Confirm of Commit.t
[@@deriving eq, compare, sexp_of]

type command = private { token : token; kind : command_kind }
[@@deriving eq, compare, sexp_of]

type repair_mode =
  | Content_repair
  | History_recovery of { reason : string; baseline : Commit.t option }
[@@deriving eq, compare, sexp_of]

type repair = {
  mode : repair_mode;
  head : Commit.t;
  sequencer : string;
  conflicts : int;
  attempts_without_progress : int;
  attempt_completed : bool;
  attempt_started : bool;
  last_turn : token option;
      (** Last claimed agent turn, retained across inspection and retries. Older
          snapshots without this evidence decode as [None]. *)
}
[@@deriving eq, compare, sexp_of]

type phase =
  | Preparing
  | Integrating
  | Repairing of repair
  | Publishing
  | Confirming
  | Waiting of { until : float; reason : string }
  | Recovering
  | Settled
  | Intervention of string
[@@deriving eq, compare, sexp_of]

type local_action =
  | Publish_source
  | Integrate_source of policy
  | Prepare_remote_replay of remote_replay_request
[@@deriving eq, compare, sexp_of]

type integration_capture = private {
  base_branch : string option;
  source_revision : Commit.t;
  target_revision : Commit.t;
  replay_boundary : boundary;
  integration_policy : policy;
}
[@@deriving eq, compare, sexp_of]

type integration_evidence = Executed | Recovered_completion | Verified_history
[@@deriving eq, compare, sexp_of]

type integration_receipt = private {
  operation_id : int;
  capture : integration_capture;
  integrated_revision : Commit.t;
  evidence : integration_evidence;
}
[@@deriving eq, compare, sexp_of]

type publication_receipt = private {
  operation_id : int;
  published_intent : intent;
  published_revision : Commit.t;
}
[@@deriving eq, compare, sexp_of]

type operation = private {
  id : int;
  intent : intent;
  local_action : local_action;
  preservation_policy : policy;
      (** Captured preservation requirement. An adopted Git strategy may
          strengthen this requirement, but cannot weaken the request. *)
  integration : integration_capture option;
  remote_integration : integration_capture option;
      (** Durable deterministic attempt, retained through publication and
          recovery. Source is incoming remote work; target is the preserved
          candidate, under either policy. This is separate from a base replay
          boundary and prevents repeated attempts against the same capture. *)
  reobserve_after_completion : bool;
  destination : remote_binding;
  source : Commit.t option;
  target : Commit.t option;
  boundary : boundary;
  boundary_candidates : boundary list;
  remote_replay : remote_replay option;
  candidate : Commit.t option;
  expected : Commit.t option;
  recovery_revisions : Commit.t list;
  phase : phase;
  pending : command option;
  command_sequence : int;
  failures : int;
  repair : repair option;
  merge_progress : merge_progress option;
}
[@@deriving eq, compare, sexp_of]

type t [@@deriving eq, compare, sexp_of]

val yojson_of_t : t -> Yojson.Safe.t

type result =
  | Checkout_ready
      (** Completion of the owner's provisioning inspection. This cannot settle
          an integration or publication operation. *)
  | Observed of observation
  | Observed_active of { observation : observation; policy : policy }
  | Pinned
  | Merge_completion_needed of merge_completion
  | Merge_completed of Commit.t
  | Remote_replay_selected of boundary
  | Remote_checked_out
  | Integrated of Commit.t
  | Conflict of { head : Commit.t; sequencer : string; conflicts : int }
  | Recovery_verified of {
      observation : observation;
      source_preserved : bool;
      remote_preserved : bool;
    }
  | Inspected of observation
  | Inspected_active of { observation : observation; ready : bool }
  | Published
  | Remote of { sha : Commit.t option; topology : topology }
  | Retryable of { reason : string; retry_after : float option }
  | Recovery_required of string
  | Permanent of string
[@@deriving eq, compare, sexp_of]

type publication_identity = private { operation_id : int; revision : Commit.t }
[@@deriving eq, compare, sexp_of]

type event =
  | Worktree_hook of Worktree_hook.event
  | Publication_observed of {
      publication : publication_identity;
      head : Commit.t;
      remote : Commit.t option;
    }
  | Materialized of materialization
  | Request of intent
  | Result of { token : token; at : float; result : result }
  | Repair_invalidated of { token : token; reason : string }
  | Repair_denied of { token : token; reason : string }
  | Repair_started of token
  | Repair_interrupted of { token : token; at : float; reason : string }
  | Repair_completed of { token : token; at : float }
  | Tick of float
  | Recover
  | Reconfirm_publication
      (** Explicitly revalidate a settled candidate; ordinary polling and
          duplicate requests remain fixed points. *)
  | Resume
  | Refresh of observation
[@@deriving eq, compare, sexp_of]

type effect_command =
  | Execute of command
  | Repair of token
  | Start_repair of token
  | Completed of int
[@@deriving eq, compare, sexp_of]

val verification_checkout_valid :
  branch:string -> candidate:Commit.t -> Git_observation.t -> bool

val command_allowed_for_purpose : purpose -> command_kind -> bool
(** Verification and provisioning cannot acquire ordinary Git mutation
    authority. *)

val import_legacy_publication : t -> base:string -> t
(** Queue verification of an old publication claim without granting publication
    authority or replacing an in-flight command or desired reconciliation. *)

val empty : t

val import_legacy_anchors : ?revision:Yojson.Safe.t -> t -> Yojson.Safe.t -> t
(** Retain valid legacy anchor revisions for recovery-ref lifetime management.
    This grants no replay boundary, integration receipt, publication authority,
    or ancestry requirement. Malformed entries are ignored independently;
    imports are cumulative, commutative and idempotent. [revision] retains the
    older scalar anchor under the same rules, even without a history list. *)

val forge_observations : t -> Forge_observation.history

val worktree_hook : t -> Worktree_hook.t
(** Durable checkout initialization obligation. Pending or uncertain hooks keep
    the owner unsettled and cannot be bypassed by a successful Git result. *)

val begin_forge_observation :
  t ->
  request:Forge_observation.request ->
  scope:Forge_observation.scope ->
  t * Forge_observation.ticket option

val accept_forge_observation :
  ?confirmed_base:string ->
  t ->
  ticket:Forge_observation.ticket ->
  current:Forge_observation.scope ->
  confirmed_head:string option ->
  Forge_observation.t ->
  t * (Poll_outcome.t, string) Result.t
(** Durable forge request/evidence transitions. They do not emit Git commands,
    settle repair, alter command identity, or grant publication authority. *)

val operation : t -> operation option

val execution_policy : operation -> policy
(** The preservation requirement is at least as strong as the request and
    captured integration strategy. An existing rebase cannot weaken a request to
    preserve ancestry; an existing merge can strengthen a rewrite request. *)

val ancestry_requirements : operation -> Commit.t list
(** Revisions that must remain ancestors under an ancestry-preserving contract.
    Includes retained requirements from preceding steps. Rewrite contracts
    return an empty list; their preservation uses the separate replay proofs. *)

val observation_boundaries : operation -> boundary list
val materialization : t -> materialization option
val publications : t -> publication_receipt list
val publication_identity : publication_receipt -> publication_identity

val publication_observation_target : t -> publication_identity option
(** Current candidate being published, or latest confirmed publication. *)

val publication_observation_pending : t -> publication_identity option
(** Observation target without a matching durable acknowledgement. Observation
    before Git confirmation survives later confirmation of the same candidate.
*)

val can_observe_publication :
  t ->
  publication:publication_identity ->
  head:Commit.t ->
  remote:Commit.t option ->
  bool
(** The candidate itself may be observed before Git confirmation. A differing
    head requires direct remote evidence after that publication is confirmed;
    the old remote tip cannot pre-acknowledge an upcoming publication. *)

val unobserved_publication : t -> publication_receipt option
(** Latest confirmed publication not yet observed by the forge. A matching head
    acknowledges that exact receipt; a different head requires direct remote
    confirmation. Old receipts cannot acknowledge newer publication. *)

val integrations : t -> integration_receipt list
(** Newest-first local base integration receipts, retained independently of
    publication. *)

val forge_conflict : t -> scope:Forge_observation.scope -> bool

val has_conflict : t -> scope:Forge_observation.scope -> bool
(** Project unresolved local conflict work and current forge evidence. Only an
    exact settled integration satisfies a retained forge pair; session success
    and mutable scheduling flags cannot clear this projection. *)

val conflict_satisfied :
  t -> head:Commit.t -> base:Commit.t -> base_branch:string -> bool
(** A settled publication and local integration receipt satisfy exactly this
    reported pair. The caller separately confirms the current remote head. *)

val completion_proven :
  policy:policy -> source_included:bool -> reflog_receipt:bool -> bool
(** Reflog completion cannot authorize an ancestry-preserving operation whose
    captured source was rewritten. Failed probes are handled before this
    decision. *)

val rewrite_publication_input :
  operation ->
  candidate:Commit.t ->
  expected:Commit.t ->
  (Commit.t * Commit.t) option
(** Captured source and recorded upstream for this operation's publication. The
    executor must prove the leased remote lies between them. Inferred/plain
    boundaries and preservation-policy integrations confer no such authority. *)

val initial_publication_candidate :
  operation -> remote:Commit.t option -> Commit.t option
(** Initial publication is confirmed only when the absent-ref operation's
    captured candidate is now the directly observed destination tip. *)

val initial_tracking_edits :
  branch:string ->
  base:string ->
  fetch_destination:string ->
  push_destination:string ->
  remotes:string list ->
  merges:string list ->
  (string * string) list option
(** Local tracking setup after confirmed initial publication. Accept absent
    configuration or an inherited origin/base upstream; preserve unrelated or
    ambiguous configuration and differing fetch/push destinations. [Some []]
    means the configuration is already correct. *)

val publication_destination : string list -> (string, string) Stdlib.result
(** One explicit push destination is supported. Ambiguous destinations stop
    before mutation rather than treating partial publication as completion. *)

val check_observation_destination :
  operation -> observed:Remote_id.t -> (unit, string) Result.t
(** Polling may use only the checkpointed destination. Unlike owned execution,
    it cannot authorize rebinding during an explicit resume. *)

val check_destination :
  operation -> observed:Remote_id.t -> (unit, string) Stdlib.result
(** An unfinished operation keeps its recorded destination across commands and
    restart. Missing legacy evidence cannot silently authorize publication. *)

val remote_integrations : t -> integration_receipt list
(** Remote integration receipts retain the incoming revision, preserved
    candidate, policy, selected boundary and completion evidence for either
    policy. They do not turn the preserved candidate into a base replay
    boundary. Publication retries keep the original evidence for an unchanged
    capture and candidate. *)

val required_revisions : t -> Commit.t list
(** Sorted, duplicate-free inventory of every revision retained by this owner's
    checkpoint, including queued intent, commands, repair, replay and receipts.
    A surviving owner may still need these objects after restart. This is not an
    ancestry or reachability proof: pruning handlers must establish which Git
    refs keep these revisions reachable before removing recovery refs. *)

val pending : t -> command option
val phase : t -> phase option

val publication_status : t -> [ `Published | `No_work | `Pending ]
(** A retained candidate is not confirmation while its operation is unfinished.
*)

val is_unsettled : t -> bool
(** An owned operation is unfinished, including an intervention that retains
    work. An empty state does not block legacy or not-yet-managed branches. *)

val is_pending : t -> bool

val step : t -> event -> t * effect_command list
(** Requests use the same intent validity rules as checkpoint decoding.
    Malformed requests leave state unchanged and emit no effects, including when
    another operation already owns the branch. *)

val decode : Yojson.Safe.t -> (t, string) Result.t

val integration_result :
  operation ->
  candidate:Commit.t ->
  target_preserved:bool ->
  source_preserved:bool ->
  result
(** Interpret post-command ancestry observations against the captured
    integration. *)

val check_checkout :
  command -> branch:string -> Git_observation.t -> (unit, string) Result.t
(** Validate a fresh checkout observation against the captured command before
    mutation. The core owns branch, revision, index and sequencer preconditions.
    A matching continuation may still need content repair; [Git_observation]
    determines whether to continue, skip an empty step, or wait for resolution.
*)

val recovery_prefix : project:string -> branch:string -> string
(** Injective encoding of project and branch names into a private Git namespace.
*)

val recovery_project_prefix : project:string -> string
(** Exact project namespace, including its trailing slash. *)

val can_execute_git : at:float -> t -> bool
(** Whether scheduled branch work can advance now, including inspection before
    resuming a repair turn. Waiting never occupies a worker. *)

val wake_event : at:float -> t -> event option
(** Recovery or a due retry, selected from durable state using the supplied
    clock. *)

type repair_turn = private {
  token : token;
  head : Commit.t;
  mode : repair_mode;
  prompt : string;
}

val repair_turn : t -> branch:string -> token -> repair_turn option
(** Available only for the current durably claimed repair turn. *)

val repair_head_matches : repair_turn -> Commit.t option -> bool

val repair_result :
  turn:repair_turn ->
  at:float ->
  before_head:Commit.t option ->
  after_head:Commit.t option ->
  timed_out:bool ->
  final_result:bool ->
  detail:string ->
  event
(** Validate repair-turn observations. Unknown HEAD evidence or interrupted
    backends retry without spending repair budget. Changed HEAD invalidates
    content-repair authority and enters history recovery. History recovery may
    change HEAD, but only independent verification can authorize publication. *)

val continuation :
  operation -> Git_observation.t -> Git_observation.continuation

val inspection_result :
  operation -> branch:string -> observation -> Git_observation.t -> result
(** Classify observed checkout evidence against captured source, target, branch,
    and integration policy. Only identified active integrations report
    readiness; malformed or inconsistent observations remain retryable probe
    failures. *)

val stopped_integration_result : Git_observation.t -> detail:string -> result
(** An active sequencer alone does not establish a content conflict. Stops with
    resolved, staged content retry inspection without dispatching agent repair.
*)

val diagnostics : t -> (string * string) list
(** Derived operator details. Unknown observations remain distinct from an
    absent remote ref, and inferred replay evidence is never described as
    proven. *)

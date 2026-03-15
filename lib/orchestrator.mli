open Base
open Types

(** Top-level orchestrator wiring.

    Manages the dependency graph and per-patch agent state. The [tick] function
    implements the spec's liveness property: all actions whose preconditions
    hold are fired in a single step.

    Spec fragment:
    {v
    > The orchestrator ensures liveness: all actions that can fire, do fire.
    > Start is biconditional with its preconditions.
    v} *)

type t

(** {2 Construction} *)

val create : patches:Patch.t list -> main_branch:Branch.t -> t
(** Build orchestrator state from a list of patches and a main branch. *)

(** {2 Liveness tick} *)

type action =
  | Start of Patch_id.t * Branch.t
  | Respond of Patch_id.t * Operation_kind.t
[@@deriving sexp_of]

val tick : t -> patches:Patch.t list -> t * action list
(** Fire all actions whose preconditions hold. Returns updated state and the
    list of actions that were fired. *)

val pending_actions : t -> patches:Patch.t list -> action list
(** Compute actions that would fire without executing them. *)

(** {2 External event application} *)

val complete : t -> Patch_id.t -> t
val enqueue : t -> Patch_id.t -> Operation_kind.t -> t
val mark_merged : t -> Patch_id.t -> t
val add_pending_comment : t -> Patch_id.t -> Comment.t -> valid:bool -> t
val set_session_failed : t -> Patch_id.t -> t
val set_has_conflict : t -> Patch_id.t -> t
val increment_ci_failure_count : t -> Patch_id.t -> t
val clear_needs_intervention : t -> Patch_id.t -> t

(** {2 Queries} *)

val agent : t -> Patch_id.t -> Patch_agent.t
val all_agents : t -> Patch_agent.t list
val graph : t -> Graph.t
val main_branch : t -> Branch.t
val agents_map : t -> Patch_agent.t Map.M(Patch_id).t

(** {2 Persistence support} *)

val restore :
  graph:Graph.t ->
  agents:Patch_agent.t Map.M(Patch_id).t ->
  main_branch:Branch.t ->
  t
(** Reconstruct orchestrator from persisted components. *)

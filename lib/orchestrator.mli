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
  | Rebase of Patch_id.t * Branch.t
[@@deriving sexp_of]

val tick : t -> patches:Patch.t list -> t * action list
(** Fire all actions whose preconditions hold. Returns updated state and the
    list of actions that were fired. *)

val pending_actions : t -> patches:Patch.t list -> action list
(** Compute actions that would fire without executing them. *)

val fire : t -> action -> t
(** Apply a single action to the orchestrator state. *)

(** {2 External event application} *)

val complete : t -> Patch_id.t -> t
val enqueue : t -> Patch_id.t -> Operation_kind.t -> t
val mark_merged : t -> Patch_id.t -> t
val mark_removed : t -> Patch_id.t -> t
val add_pending_comment : t -> Patch_id.t -> Comment.t -> valid:bool -> t

val send_human_message : t -> Patch_id.t -> string -> t
(** Convenience: add a human message as a pending comment and enqueue
    [Operation_kind.Human] for the given patch. *)

val set_pr_number : t -> Patch_id.t -> Pr_number.t -> t
val set_session_failed : t -> Patch_id.t -> t
val set_last_session_id : t -> Patch_id.t -> Session_id.t -> t
val set_tried_fresh : t -> Patch_id.t -> t
val clear_session_fallback : t -> Patch_id.t -> t
val on_session_failure : t -> Patch_id.t -> is_fresh:bool -> t
val on_pr_discovery_failure : t -> Patch_id.t -> t
val set_has_conflict : t -> Patch_id.t -> t
val increment_ci_failure_count : t -> Patch_id.t -> t
val set_ci_checks : t -> Patch_id.t -> Ci_check.t list -> t
val clear_needs_intervention : t -> Patch_id.t -> t
val add_addressed_comment_id : t -> Patch_id.t -> Types.Comment_id.t -> t
val reset_busy : t -> Patch_id.t -> t

(** {2 Queries} *)

val agent : t -> Patch_id.t -> Patch_agent.t
val find_agent : t -> Patch_id.t -> Patch_agent.t option
val all_agents : t -> Patch_agent.t list
val graph : t -> Graph.t
val main_branch : t -> Branch.t
val agents_map : t -> Patch_agent.t Map.M(Patch_id).t

val add_agent : t -> patch_id:Patch_id.t -> pr_number:Pr_number.t -> t
(** Add an ad-hoc agent for a PR not in the gameplan. No-op if the patch_id
    already exists. The agent starts with [has_pr = true] and no dependencies.
    Corresponds to the spec's Add action. *)

(** {2 Persistence support} *)

val restore :
  graph:Graph.t ->
  agents:Patch_agent.t Map.M(Patch_id).t ->
  main_branch:Branch.t ->
  t
(** Reconstruct orchestrator from persisted components. *)

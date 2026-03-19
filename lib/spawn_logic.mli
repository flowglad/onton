open Base
open Types

(** Pure spawn-planning logic extracted from the runner fiber.

    Given the current orchestrator state and gameplan patches, [plan_spawns]
    computes the set of actions to fire in the next tick. This module exists to
    make the runner fiber's decision logic testable without Eio or Runtime
    dependencies.

    Spec fragment:
    {v
    > The orchestrator ensures liveness: all actions that can fire, do fire.
    > Start is biconditional with its preconditions.
    v} *)

type spawn = Orchestrator.action
(** An action to spawn: Start, Respond, or Rebase. *)

val plan_spawns : Orchestrator.t -> patches:Patch.t list -> spawn list
(** Compute all actions whose preconditions hold. Pure function — no side
    effects, no state mutation. The caller is responsible for calling
    [Orchestrator.fire] on each returned action.

    Properties:
    - Only non-busy, non-merged agents produce actions.
    - Start only for patches without PRs where deps are satisfied.
    - Respond only for patches with PRs, picking highest-priority feedback.
    - Rebase only when Rebase is the highest-priority queued operation.
    - At most one action per patch. *)

val classify :
  spawn ->
  [ `Start of Patch_id.t | `Respond of Patch_id.t | `Rebase of Patch_id.t ]
(** Extract the patch id and action kind from a spawn. *)

val patch_id_of : spawn -> Patch_id.t
(** Extract the patch id from any spawn action. *)

open Base

(** Pure reconciliation logic.

    Encodes the spec fragments:
    - [PatchCtx ~> Rebase]: rebase when has-pr, ~merged, ~busy, queue rebase,
      highest-priority rebase.
    - [merge-target b: Branch => Branch]: compute new base after dep merges.
    - [Liveness]: pending operations on an idle, living patch must be addressed.
*)

type patch_view = {
  id : Types.Patch_id.t;
  has_pr : bool;
  merged : bool;
  busy : bool;
  needs_intervention : bool;
  queue : Types.Operation_kind.t list;
  base_branch : Types.Branch.t;
}
[@@deriving sexp_of]
(** Observable state of a single patch, projected for reconciliation. *)

(** Actions the reconciler instructs the orchestrator to perform. *)
type action =
  | Mark_merged of Types.Patch_id.t
  | Enqueue_rebase of Types.Patch_id.t
  | Start_operation of {
      patch_id : Types.Patch_id.t;
      kind : Types.Operation_kind.t;
      new_base : Types.Branch.t option;
    }
[@@deriving sexp_of]

val merge_target :
  Graph.t ->
  Types.Patch_id.t ->
  has_merged:(Types.Patch_id.t -> bool) ->
  branch_of:(Types.Patch_id.t -> Types.Branch.t) ->
  main:Types.Branch.t ->
  Types.Branch.t
(** [merge_target graph p ~has_merged ~branch_of ~main] computes the base branch
    a patch should rebase onto. Uses [Graph.initial_base] under the hood — when
    a dependency merges, the dependent's base shifts to [main] (if no other open
    deps) or to the sole remaining open dep's branch. *)

val detect_merges :
  patch_view list -> merged_pr_patches:Types.Patch_id.t list -> action list
(** [detect_merges views ~merged_pr_patches] returns [Mark_merged] for every
    patch in [merged_pr_patches] that is not already marked merged. *)

val detect_rebases :
  Graph.t ->
  patch_view list ->
  newly_merged:Types.Patch_id.t list ->
  action list
(** [detect_rebases graph views ~newly_merged] returns [Enqueue_rebase] for
    dependents of [newly_merged] patches that have a PR, are not merged, and do
    not already have a rebase queued. *)

val plan_operations :
  patch_view list ->
  has_merged:(Types.Patch_id.t -> bool) ->
  branch_of:(Types.Patch_id.t -> Types.Branch.t) ->
  graph:Graph.t ->
  main:Types.Branch.t ->
  action list
(** [plan_operations views ~has_merged ~branch_of ~graph ~main] enforces the
    Liveness invariant: for each idle patch (has-pr, ~merged, ~busy,
    ~needs-intervention) with a non-empty queue, emits [Start_operation] for the
    highest-priority queued operation. For [Rebase], includes [new_base]. *)

val reconcile :
  graph:Graph.t ->
  main:Types.Branch.t ->
  merged_pr_patches:Types.Patch_id.t list ->
  branch_of:(Types.Patch_id.t -> Types.Branch.t) ->
  patch_view list ->
  action list
(** [reconcile] composes [detect_merges], [detect_rebases], and
    [plan_operations] into a single pass. Returns actions in priority order:
    merges first, then rebases, then operations. *)

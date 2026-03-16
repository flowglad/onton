open Base
open Types

(** GitHub poller: translates PR world state into patch state updates.

    Implements the Poll action from the Pantagruel spec:
    {v
    PatchCtx, WorldCtx ~> Poll.
    ---
    all c: Comment, p: Patch |
        world-has-comment c p and ~resolved c -> queue' p review-comments.
    all p: Patch | world-has-conflict p -> queue' p merge-conflict.
    all p: Patch | world-has-conflict p -> has-conflict' p.
    all p: Patch | world-ci-failed p -> queue' p ci.
    all p: Patch | world-merged p -> merged' p.
    all p: Patch | merged p -> merged' p.
    all p: Patch | mergeable' p = world-mergeable p.
    all p: Patch | checks-passing' p = world-checks-passing p.
    v} *)

type t = {
  queue : Operation_kind.t list;  (** Operations to enqueue for this patch. *)
  merged : bool;  (** [true] if the patch is now merged (absorbing). *)
  has_conflict : bool;  (** [true] if the PR has a merge conflict. *)
  mergeable : bool;  (** [true] if the PR is currently mergeable. *)
  checks_passing : bool;  (** [true] if CI checks are currently passing. *)
  ci_checks : Ci_check.t list;
      (** Individual CI check results from the most recent poll. *)
  new_comments : Types.Comment.t list;
      (** Unresolved comments not yet in [addressed_ids]. *)
}
[@@deriving show, eq]
(** The result of polling a single patch's PR state. *)

val poll :
  was_merged:bool ->
  addressed_ids:Set.M(Types.Comment_id).t ->
  Github.Pr_state.t ->
  t
(** [poll ~was_merged ~addressed_ids pr_state] computes the patch state updates
    from the current GitHub PR state.

    [was_merged] is the patch's current merged status (from PatchCtx). Once
    merged, a patch stays merged regardless of what the world reports.

    [addressed_ids] is the set of comment IDs already addressed — comments with
    these IDs are excluded from [new_comments] and do not trigger
    [Review_comments] queuing. *)

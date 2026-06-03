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
  closed : bool;
      (** [true] if the PR was closed without merging. The poller should
          re-discover the current open PR for this branch. *)
  is_draft : bool;  (** [true] if the PR is still a draft. *)
  has_conflict : bool;  (** [true] if the PR has a merge conflict. *)
  merge_ready : bool;
      (** [true] if GitHub's [mergeStateStatus] is [CLEAN] — all branch
          protection rules (required reviews, checks, etc.) are satisfied. *)
  merge_queue_required : bool;
      (** [true] when the repository config says this PR's target branch
          requires GitHub's native merge queue. Inert in Patch 1. *)
  merge_queue_entry : Pr_state.merge_queue_entry option; [@yojson.option]
      (** The current merge-queue entry when the PR is enqueued. Inert in Patch
          1. [@yojson.option] omits the field from event JSON when [None]. *)
  checks_passing : bool;  (** [true] if CI checks are currently passing. *)
  ci_checks : Ci_check.t list;
      (** Individual CI check results from the most recent poll. *)
  merge_commit_sha : string option; [@yojson.option]
      (** Squash/merge commit SHA when the PR is [merged]; [None] otherwise.
          Recorded on the agent when [merged] flips true so the base-containment
          gate can ancestry-check it. [Poller.t] is only serialized (to the
          event log), never deserialized; [@yojson.option] just omits the field
          from event JSON when [None]. *)
}
[@@deriving show, eq, yojson]
(** The result of polling a single patch's PR state. *)

val poll : was_merged:bool -> Pr_state.t -> t
(** [poll ~was_merged pr_state] computes the patch state updates from the
    current PR state.

    [was_merged] is the patch's current merged status (from PatchCtx). Once
    merged, a patch stays merged regardless of what the world reports.

    Review comments trigger [Review_comments] queuing if any unresolved comments
    exist in the PR state. The runner fetches fresh comments lazily at delivery
    time. *)

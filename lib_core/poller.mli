(* @archlint.module interface
   @archlint.domain poller *)

open Base
open Types

(** GitHub poller: translates PR world state into patch state updates.

    Implements the Poll action from the Pantagruel spec:
    {v
    PatchCtx, WorldCtx ~> Poll.
    ---
    all c: Comment, p: Patch |
        world-has-comment c p and ~resolved c -> queue' p review-comments.
    all p: Patch |
        world-has-conflict p and ~world-enqueued p -> queue' p merge-conflict.
    all p: Patch | world-has-conflict p -> has-conflict' p.
    all p: Patch | world-ci-failed p -> queue' p ci.
    all p: Patch | world-merged p -> merged' p.
    all p: Patch | merged p -> merged' p.
    all p: Patch | mergeable' p = world-mergeable p.
    all p: Patch | checks-passing' p = world-checks-passing p.
    v}

    The [~world-enqueued] conjunct on the merge-conflict rule is the merge-queue
    gate: while a PR sits in a merge queue its head branch is push-locked (GH006
    [Merge_queue_locked]), so a [Merge_conflict] op could only no-op locally and
    bounce off the lock, walking [conflict_noop_count] toward spurious
    intervention. The [has-conflict'] {e flag} rule stays ungated — it purely
    mirrors GitHub state, and a genuine conflict makes GitHub eject the PR from
    the queue, after which the next poll enqueues the op normally. *)

type t = {
  queue : Operation_kind.t list;  (** Operations to enqueue for this patch. *)
  merged : bool;  (** [true] if the patch is now merged (absorbing). *)
  closed : bool;
      (** [true] if the PR was closed without merging. The poller should
          re-discover the current open PR for this branch. *)
  is_draft : bool;  (** [true] if the PR is still a draft. *)
  merge_state : Pr_state.merge_state;
      (** GitHub's tri-state mergeability ([Mergeable]/[Conflicting]/[Unknown]).
          Source for the derived [has_conflict] and [mergeability_unknown]
          accessors. [Unknown] means GitHub is recomputing the test-merge (e.g.
          a sibling merge advanced the base). *)
  merge_ready : bool;
      (** Component-derived merge readiness ([Pr_state.merge_ready_of]:
          mergeable + CI passing + non-blocking review). NOT GitHub's
          [mergeStateStatus]. *)
  head_oid : string option; [@yojson.option]
  review_decision : string option; [@yojson.option]
      (** Raw GitHub [reviewDecision]. An input to [merge_ready] upstream
          ([Pr_state.merge_ready_of]); retained here for the event log. *)
  unresolved_comment_count : int; [@yojson.default 0]
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

val has_conflict : t -> bool
(** [true] when [merge_state = Conflicting]. *)

val mergeability_unknown : t -> bool
(** [true] when [merge_state = Unknown] — GitHub is recomputing mergeability.
    Drives [Patch_controller.automerge_transient_hold]. *)

val poll : was_merged:bool -> Pr_state.t -> t
(** [poll ~was_merged pr_state] computes the patch state updates from the
    current PR state.

    [was_merged] is the patch's current merged status (from PatchCtx). Once
    merged, a patch stays merged regardless of what the world reports.

    Review comments trigger [Review_comments] queuing if any unresolved comments
    exist in the PR state. The runner fetches fresh comments lazily at delivery
    time. *)

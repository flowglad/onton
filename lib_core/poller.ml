(* @archlint.module core
   @archlint.domain poller *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Types

type t = {
  queue : Operation_kind.t list;
  merged : bool;
  closed : bool;
  is_draft : bool;
  merge_state : Pr_state.merge_state;
      (** GitHub's tri-state mergeability; source for the [has_conflict] and
          [mergeability_unknown] accessors. *)
  merge_ready : bool;
  head_oid : string option; [@yojson.option]
  review_decision : string option; [@yojson.option]
      (** Raw GitHub [reviewDecision]. An input to [merge_ready] via
          [Pr_state.merge_ready_of] (REVIEW_REQUIRED/CHANGES_REQUESTED block).
      *)
  unresolved_comment_count : int; [@yojson.default 0]
  merge_queue_required : bool;
  merge_queue_entry : Pr_state.merge_queue_entry option; [@yojson.option]
  checks_passing : bool;
  ci_checks : Types.Ci_check.t list;
  merge_commit_sha : string option; [@yojson.option]
      (** Squash/merge commit SHA when [merged]; [None] otherwise. [Poller.t] is
          only serialized (to the event log), never deserialized; the
          [@yojson.option] annotation just omits the field from event JSON when
          [None]. *)
}
[@@deriving show, eq, yojson]

let has_conflict (t : t) =
  Pr_state.equal_merge_state t.merge_state Pr_state.Conflicting

let mergeability_unknown (t : t) =
  Pr_state.equal_merge_state t.merge_state Pr_state.Unknown

let poll ~was_merged (pr : Pr_state.t) =
  let unresolved = not (List.is_empty pr.comments) in
  let has_findings = not (List.is_empty pr.findings) in
  let queue =
    let acc = [] in
    (* world-has-comment c p and ~resolved c -> queue' p review-comments *)
    let acc =
      if unresolved then Operation_kind.Review_comments :: acc else acc
    in
    (* world-has-finding f p -> queue' p findings.
       Mirror of the comment rule, but for review-service backends. *)
    let acc = if has_findings then Operation_kind.Findings :: acc else acc in
    (* world-has-conflict p and ~enqueued p -> queue' p merge-conflict.
       While the PR sits in a merge queue its head branch is push-locked
       (GH006), so a Merge_conflict op could only no-op locally and bounce off
       the lock — walking conflict_noop_count toward spurious intervention. A
       Conflicting report while queued is transient: a genuine conflict makes
       GitHub eject the PR, the entry clears, and the next poll enqueues
       normally (the gate self-opens). *)
    let acc =
      if Pr_state.has_conflict pr && not (Pr_state.enqueued pr) then
        Operation_kind.Merge_conflict :: acc
      else acc
    in
    (* world-ci-failed p -> queue' p ci *)
    let acc = if Pr_state.ci_failed pr then Operation_kind.Ci :: acc else acc in
    List.rev acc
  in
  {
    queue;
    merged = was_merged || Pr_state.merged pr;
    closed = Pr_state.closed pr;
    is_draft = Pr_state.is_draft pr;
    merge_state = pr.Pr_state.merge_state;
    merge_ready = Pr_state.merge_ready pr;
    head_oid = pr.Pr_state.head_oid;
    review_decision = pr.Pr_state.review_decision;
    unresolved_comment_count = pr.Pr_state.unresolved_comment_count;
    merge_queue_required = Pr_state.requires_merge_queue pr;
    merge_queue_entry = pr.Pr_state.merge_queue_entry;
    checks_passing = Pr_state.checks_passing pr;
    ci_checks = pr.Pr_state.ci_checks;
    merge_commit_sha = pr.Pr_state.merge_commit_sha;
  }

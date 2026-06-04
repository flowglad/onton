open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type merge_state = Mergeable | Conflicting | Unknown
[@@deriving show, eq, yojson]

type check_status = Passing | Failing | Pending [@@deriving show, eq]
type pr_status = Open | Merged | Closed [@@deriving show, eq]

type merge_queue_entry_state =
  | Mq_queued
  | Mq_awaiting_checks
  | Mq_mergeable
  | Mq_unmergeable
  | Mq_locked
[@@deriving show, eq, sexp_of, compare, yojson]

type merge_queue_entry = {
  id : string;
  state : merge_queue_entry_state;
  position : int;
}
[@@deriving show, eq, sexp_of, compare, yojson]

type merge_ready_divergence = {
  github_merge_state_status : string;
  derived_merge_ready : bool;
}
[@@deriving show, eq]

type t = {
  status : pr_status;
  is_draft : bool;
  merge_state : merge_state;
  merge_ready : bool;
      (** Component-derived merge readiness ({!merge_ready_of}): mergeable + CI
          passing + non-blocking review. NOT GitHub's [mergeStateStatus] — that
          field is a stale cache, no longer carried as state or used by any
          decision. *)
  merge_ready_divergence : merge_ready_divergence option;
      (** Set at poll time when [merge_ready] disagrees with GitHub's
          [mergeStateStatus] ({!merge_ready_divergence_of}). Diagnostics only:
          never persisted, never a decision input — the poller fiber logs it
          once and discards it. *)
  review_decision : string option;
      (** Raw GitHub [reviewDecision] (APPROVED / REVIEW_REQUIRED /
          CHANGES_REQUESTED / null). An input to {!merge_ready_of}: a
          [REVIEW_REQUIRED]/[CHANGES_REQUESTED] decision blocks readiness. *)
  check_status : check_status;
  ci_checks : Types.Ci_check.t list;
  ci_checks_truncated : bool;
  comments : Types.Comment.t list;
  unresolved_comment_count : int;
  findings : Review_service.finding list;
      (** Unresolved findings from review-service backends, merged across all
          configured sources. Empty when no review backends are configured or
          all returned empty. Distinct from [comments] (GitHub review threads)
          because the agent's resolution surface differs — see
          {!Operation_kind.Findings}. *)
  node_id : string option;
  merge_queue_required : bool;
  merge_queue_entry : merge_queue_entry option;
  head_branch : Types.Branch.t option;
  head_oid : string option;
  merge_commit_sha : string option;
      (** For a merged PR, the squash/merge commit SHA on the base branch
          (GitHub [mergeCommit.oid]). [None] for open/closed PRs or when the
          forge has not yet reported it. *)
  base_branch : Types.Branch.t option;
  is_fork : bool;
}
[@@deriving show, eq]

let merged (st : t) = equal_pr_status st.status Merged
let closed (st : t) = equal_pr_status st.status Closed
let is_draft (st : t) = st.is_draft

let mergeable (st : t) =
  equal_pr_status st.status Open && equal_merge_state st.merge_state Mergeable

let merge_ready (st : t) = equal_pr_status st.status Open && st.merge_ready
let checks_passing (st : t) = equal_check_status st.check_status Passing
let no_unresolved_comments (st : t) = st.unresolved_comment_count = 0
let has_conflict (st : t) = equal_merge_state st.merge_state Conflicting
let ci_failed (st : t) = equal_check_status st.check_status Failing
let is_fork (st : t) = st.is_fork
let requires_merge_queue (st : t) = st.merge_queue_required
let enqueued (st : t) = Option.is_some st.merge_queue_entry

(* A blocking GitHub [reviewDecision]: merging is refused until it clears.
   [APPROVED] and [None] (no review required, or none reported yet) are both
   non-blocking. *)
let review_blocking = function
  | Some ("REVIEW_REQUIRED" | "CHANGES_REQUESTED") -> true
  | Some _ | None -> false

(* Component-derived merge readiness — the single source of truth shared by the
   merge *decision* ([Patch_controller.is_automerge_candidate] via
   [Patch_agent.is_approved]) and the *label* ([Display_status.derive]).

   Replaces the former [mergeStateStatus = CLEAN] gate, which keyed readiness on
   a cached, event-driven GitHub field that goes stale: it can keep reporting
   [BLOCKED] long after every requirement is met because nothing recomputes it
   until the PR is poked, stranding a genuinely-mergeable PR forever. We derive
   readiness instead from the component facts every poll refreshes —
   mergeability, CI conclusions, the review decision — and let the merge
   *attempt* be the final authority: a PR onton deems ready but GitHub still
   blocks is rejected by the merge call and backs off via
   [automerge_failure_count], never silently waiting.

   Conversation-resolution and merge-conflict gating are deliberately NOT folded
   in: unresolved review threads enqueue [Review_comments] and a conflict
   enqueues [Merge_conflict], both of which already make the patch a
   non-candidate (non-empty queue) and surface distinctly in the label. *)
let merge_ready_of ~merge_state ~check_status ~review_decision =
  equal_merge_state merge_state Mergeable
  && equal_check_status check_status Passing
  && not (review_blocking review_decision)

(* Compare onton's derived [merge_ready] against GitHub's [mergeStateStatus]
   rollup, purely for diagnostics. GitHub's rollup considers the PR fully
   mergeable exactly when [mergeStateStatus = CLEAN]; we no longer trust that
   field, so a disagreement is worth surfacing in the log:

   - derived ready, GitHub not CLEAN (e.g. a stale [BEHIND]/[BLOCKED]): the
     PR #4026 smell — GitHub's verdict is likely stale, or names a required gate
     we deliberately don't model (component-approx). We merge anyway; the merge
     attempt is the authority.
   - GitHub CLEAN, derived not-ready: our conservative side — e.g. an optional
     check we treat as required, or a truncation downgrade.

   [None] when GitHub reported no status or the two agree. *)
let merge_ready_divergence_of ~merge_ready ~github_merge_state_status =
  match github_merge_state_status with
  | Some status ->
      let github_ready = String.equal status "CLEAN" in
      if Bool.equal merge_ready github_ready then None
      else
        Some
          {
            github_merge_state_status = status;
            derived_merge_ready = merge_ready;
          }
  | None -> None

(** Derive the aggregate [check_status] from a list of individual CI check
    conclusions. This is the source of truth for what the orchestrator considers
    a "failing", "passing", or "pending" CI state — bypassing GitHub's
    [statusCheckRollup.state] which conflates cancelled/superseded runs with
    actual failures.

    Semantics:
    - [Failing] if at least one check has a conclusion in
      [Ci_check.failure_conclusions].
    - [Passing] if the list is non-empty and every check has a conclusion in
      [Ci_check.success_conclusions].
    - [Pending] otherwise (empty list, or any mix containing cancelled, pending,
      in_progress, queued, unknown conclusions, …). *)
let derive_check_status (checks : Types.Ci_check.t list) : check_status =
  if List.exists checks ~f:Types.Ci_check.is_failure then Failing
  else if
    (not (List.is_empty checks))
    && List.for_all checks ~f:Types.Ci_check.is_success
  then Passing
  else Pending

(* -- review_blocking -- *)

let%test "review_blocking: REVIEW_REQUIRED blocks" =
  review_blocking (Some "REVIEW_REQUIRED")

let%test "review_blocking: CHANGES_REQUESTED blocks" =
  review_blocking (Some "CHANGES_REQUESTED")

let%test "review_blocking: APPROVED does not block" =
  not (review_blocking (Some "APPROVED"))

let%test "review_blocking: None (no review required) does not block" =
  not (review_blocking None)

let%test "review_blocking: unknown decision string does not block" =
  not (review_blocking (Some "SOME_FUTURE_STATE"))

(* -- merge_ready_of --

   Spec: ready iff Mergeable && Passing && not (blocking review). The whole
   point is that GitHub's [mergeStateStatus] is NOT an argument — a stale
   [BLOCKED] reading cannot affect the result. *)

let%test "merge_ready_of: mergeable + passing + no review = ready" =
  merge_ready_of ~merge_state:Mergeable ~check_status:Passing
    ~review_decision:None

let%test "merge_ready_of: mergeable + passing + APPROVED = ready" =
  merge_ready_of ~merge_state:Mergeable ~check_status:Passing
    ~review_decision:(Some "APPROVED")

(* The PR #4026 case: GitHub serves a stale [mergeStateStatus = BLOCKED] while
   the PR is genuinely mergeable, CI is green, and no review is required.
   Readiness must not see that field at all and so must be [true]. *)
let%test "merge_ready_of: ready despite a (stale) GitHub block" =
  (* No [mergeStateStatus] parameter exists to pass BLOCKED into — that is the
     fix. With the component facts all green, readiness is unconditionally
     true. *)
  merge_ready_of ~merge_state:Mergeable ~check_status:Passing
    ~review_decision:None

let%test "merge_ready_of: conflict is not ready" =
  not
    (merge_ready_of ~merge_state:Conflicting ~check_status:Passing
       ~review_decision:None)

let%test "merge_ready_of: unknown mergeability (recomputing) is not ready" =
  not
    (merge_ready_of ~merge_state:Unknown ~check_status:Passing
       ~review_decision:None)

let%test "merge_ready_of: failing checks is not ready" =
  not
    (merge_ready_of ~merge_state:Mergeable ~check_status:Failing
       ~review_decision:None)

let%test "merge_ready_of: pending checks is not ready" =
  not
    (merge_ready_of ~merge_state:Mergeable ~check_status:Pending
       ~review_decision:None)

let%test "merge_ready_of: REVIEW_REQUIRED is not ready" =
  not
    (merge_ready_of ~merge_state:Mergeable ~check_status:Passing
       ~review_decision:(Some "REVIEW_REQUIRED"))

let%test "merge_ready_of: CHANGES_REQUESTED is not ready" =
  not
    (merge_ready_of ~merge_state:Mergeable ~check_status:Passing
       ~review_decision:(Some "CHANGES_REQUESTED"))

(* Exhaustive over the finite input space: merge_ready_of agrees with its spec
   conjunction for every (merge_state, check_status, review_decision) triple.
   The review-decision axis samples blocking, non-blocking, and unknown
   strings; the enum axes are total. *)
let%test "merge_ready_of: exhaustive spec agreement" =
  let merge_states = [ Mergeable; Conflicting; Unknown ] in
  let check_statuses = [ Passing; Failing; Pending ] in
  let reviews =
    [
      None;
      Some "APPROVED";
      Some "REVIEW_REQUIRED";
      Some "CHANGES_REQUESTED";
      Some "SOME_FUTURE_STATE";
    ]
  in
  List.for_all merge_states ~f:(fun merge_state ->
      List.for_all check_statuses ~f:(fun check_status ->
          List.for_all reviews ~f:(fun review_decision ->
              let spec =
                equal_merge_state merge_state Mergeable
                && equal_check_status check_status Passing
                && not (review_blocking review_decision)
              in
              Bool.equal
                (merge_ready_of ~merge_state ~check_status ~review_decision)
                spec)))

(* -- merge_ready_divergence_of -- *)

let%test "divergence: no GitHub status reported -> None" =
  Option.is_none
    (merge_ready_divergence_of ~merge_ready:true ~github_merge_state_status:None)
  && Option.is_none
       (merge_ready_divergence_of ~merge_ready:false
          ~github_merge_state_status:None)

let%test "divergence: agreement (ready + CLEAN) -> None" =
  Option.is_none
    (merge_ready_divergence_of ~merge_ready:true
       ~github_merge_state_status:(Some "CLEAN"))

let%test "divergence: agreement (not ready + non-CLEAN) -> None" =
  Option.is_none
    (merge_ready_divergence_of ~merge_ready:false
       ~github_merge_state_status:(Some "BLOCKED"))

(* The PR #4026 smell: we derive ready, GitHub's rollup still says BLOCKED. *)
let%test "divergence: ready + stale BLOCKED -> Some, payload echoes inputs" =
  match
    merge_ready_divergence_of ~merge_ready:true
      ~github_merge_state_status:(Some "BLOCKED")
  with
  | Some { github_merge_state_status = "BLOCKED"; derived_merge_ready = true }
    ->
      true
  | _ -> false

(* The conservative side: GitHub says CLEAN, we derived not-ready. *)
let%test "divergence: not ready + CLEAN -> Some" =
  match
    merge_ready_divergence_of ~merge_ready:false
      ~github_merge_state_status:(Some "CLEAN")
  with
  | Some { github_merge_state_status = "CLEAN"; derived_merge_ready = false } ->
      true
  | _ -> false

(* Exhaustive: Some iff a status was reported and the verdicts disagree;
   payload faithfully echoes both inputs. *)
let%test "divergence: exhaustive spec agreement" =
  let readies = [ true; false ] in
  let statuses =
    [ None; Some "CLEAN"; Some "BLOCKED"; Some "UNKNOWN"; Some "BEHIND" ]
  in
  List.for_all readies ~f:(fun merge_ready ->
      List.for_all statuses ~f:(fun github_merge_state_status ->
          let result =
            merge_ready_divergence_of ~merge_ready ~github_merge_state_status
          in
          match github_merge_state_status with
          | None -> Option.is_none result
          | Some status -> (
              let disagree =
                not (Bool.equal merge_ready (String.equal status "CLEAN"))
              in
              if not disagree then Option.is_none result
              else
                match result with
                | Some d ->
                    String.equal d.github_merge_state_status status
                    && Bool.equal d.derived_merge_ready merge_ready
                | None -> false)))

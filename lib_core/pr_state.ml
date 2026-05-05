open Base

type merge_state = Mergeable | Conflicting | Unknown [@@deriving show, eq]
type check_status = Passing | Failing | Pending [@@deriving show, eq]
type pr_status = Open | Merged | Closed [@@deriving show, eq]

type t = {
  status : pr_status;
  is_draft : bool;
  merge_state : merge_state;
  merge_ready : bool;
  check_status : check_status;
  ci_checks : Types.Ci_check.t list;
  ci_checks_truncated : bool;
  comments : Types.Comment.t list;
  unresolved_comment_count : int;
  head_branch : Types.Branch.t option;
  head_oid : string option;
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

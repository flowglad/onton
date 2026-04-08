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

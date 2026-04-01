open Types

(** Forge-agnostic pull request state.

    Contains the data model and predicates for WorldCtx from the spec: merged,
    mergeable, checks-passing, no-unresolved-comments, world-has-comment,
    world-has-conflict, world-ci-failed. *)

type merge_state = Mergeable | Conflicting | Unknown [@@deriving show, eq]
type check_status = Passing | Failing | Pending [@@deriving show, eq]
type pr_status = Open | Merged | Closed [@@deriving show, eq]

type t = {
  status : pr_status;
  is_draft : bool;
  merge_state : merge_state;
  merge_ready : bool;
  check_status : check_status;
  ci_checks : Ci_check.t list;
  ci_checks_truncated : bool;
  comments : Comment.t list;
  unresolved_comment_count : int;
  head_branch : Branch.t option;
  base_branch : Branch.t option;
}
[@@deriving show, eq]

(** {2 WorldCtx predicates} *)

val merged : t -> bool
val closed : t -> bool
val is_draft : t -> bool
val mergeable : t -> bool
val merge_ready : t -> bool
val checks_passing : t -> bool
val no_unresolved_comments : t -> bool
val has_conflict : t -> bool
val ci_failed : t -> bool

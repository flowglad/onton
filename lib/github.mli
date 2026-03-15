open Types

(** GitHub API client for querying PR/world state.

    Provides the data source for WorldCtx predicates from the spec: merged,
    mergeable, checks-passing, no-unresolved-comments, world-has-comment,
    world-has-conflict, world-ci-failed. *)

module Pr_state : sig
  type merge_state = Mergeable | Conflicting | Unknown [@@deriving show, eq]
  type check_status = Passing | Failing | Pending [@@deriving show, eq]

  type t = {
    merged : bool;
    merge_state : merge_state;
    check_status : check_status;
    comments : Comment.t list;
    unresolved_comment_count : int;
  }
  [@@deriving show, eq]
end

type error =
  | Http_error of int * string
  | Json_parse_error of string
  | Graphql_error of string list
[@@deriving show, eq]

type t

val create : token:string -> owner:string -> repo:string -> t
(** [create ~token ~owner ~repo] creates a GitHub API client. *)

val pr_state :
  net:_ Eio.Net.t -> t -> Pr_number.t -> (Pr_state.t, error) Result.t
(** [pr_state ~net client pr] fetches the current state of a pull request via
    the GitHub GraphQL API over HTTPS. This is the primary data source for
    WorldCtx predicates. *)

val merged : Pr_state.t -> bool
val mergeable : Pr_state.t -> bool
val checks_passing : Pr_state.t -> bool
val no_unresolved_comments : Pr_state.t -> bool
val has_conflict : Pr_state.t -> bool
val ci_failed : Pr_state.t -> bool

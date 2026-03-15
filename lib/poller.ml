open Base
open Types

type t = {
  queue : Operation_kind.t list;
  merged : bool;
  has_conflict : bool;
  mergeable : bool;
  checks_passing : bool;
}
[@@deriving show, eq]

let poll ~was_merged (pr : Github.Pr_state.t) =
  let unresolved =
    let open Github.Pr_state in
    pr.unresolved_comment_count > 0
  in
  let queue =
    let acc = [] in
    (* world-has-comment c p and ~resolved c -> queue' p review-comments *)
    let acc =
      if unresolved then Operation_kind.Review_comments :: acc else acc
    in
    (* world-has-conflict p -> queue' p merge-conflict *)
    let acc =
      if Github.has_conflict pr then Operation_kind.Merge_conflict :: acc
      else acc
    in
    (* world-ci-failed p -> queue' p ci *)
    let acc = if Github.ci_failed pr then Operation_kind.Ci :: acc else acc in
    List.rev acc
  in
  {
    queue;
    merged = was_merged || Github.merged pr;
    has_conflict = Github.has_conflict pr;
    mergeable = Github.mergeable pr;
    checks_passing = Github.checks_passing pr;
  }

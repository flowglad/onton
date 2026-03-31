open Base
open Types

type t = {
  queue : Operation_kind.t list;
  merged : bool;
  closed : bool;
  is_draft : bool;
  has_conflict : bool;
  mergeable : bool;
  merge_ready : bool;
  checks_passing : bool;
  ci_checks : Types.Ci_check.t list;
}
[@@deriving show, eq]

let poll ~was_merged (pr : Pr_state.t) =
  let unresolved = not (List.is_empty pr.comments) in
  let queue =
    let acc = [] in
    (* world-has-comment c p and ~resolved c -> queue' p review-comments *)
    let acc =
      if unresolved then Operation_kind.Review_comments :: acc else acc
    in
    (* world-has-conflict p -> queue' p merge-conflict *)
    let acc =
      if Pr_state.has_conflict pr then Operation_kind.Merge_conflict :: acc
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
    has_conflict = Pr_state.has_conflict pr;
    mergeable = Pr_state.mergeable pr;
    merge_ready = Pr_state.merge_ready pr;
    checks_passing = Pr_state.checks_passing pr;
    ci_checks = pr.Pr_state.ci_checks;
  }

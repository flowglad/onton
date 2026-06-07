(* @archlint.module interface
   @archlint.domain rebase-decision *)

(** Pure decision: what arguments should the orchestrator pass to [git rebase]
    when re-basing a patch branch?

    The decision answers two related questions:

    + {!plan} — what [<target>] / [<upstream>] should be on the command line?
    + {!anchor_after_result} — what {!Anchor.t} should be recorded after a
      rebase attempt completes?

    Both functions are pure; the effectful runner supplies live git observations
    (a HEAD SHA, an [is-ancestor] oracle) and the resolved-remote SHA captured
    before the rebase.

    {2 Why anchors matter — the history-rewriting-merge trap}

    A patch agent branched off a dependency's tip and never rebased before that
    dependency's PR merged to [main] will, on its first rebase, try to replay
    the dep's pre-merge commits on top of the post-merge version of [main] —
    every file the dep added produces a "both added" conflict. This bites
    whenever the merge rewrote the dep's history so its original commit SHAs are
    not ancestors of [main]: a {b squash} merge (one new commit) or a {b rebase}
    merge (cherry-picked commits with fresh SHAs). (A plain merge {e commit}
    keeps the original SHAs as ancestors, so a 2-arg rebase is a no-op there —
    but anchors are still correct, so the same path handles all three.) Today
    the orchestrator records an anchor SHA only after a successful rebase, so
    such a patch has no anchor at first-rebase time and falls back to the legacy
    2-arg rebase, which is the failure mode.

    With anchors recorded at every base transition (Start, Noop, Conflict-
    resolved, base retarget), {!plan} returns an [Onto] invocation whose
    [<upstream>] is the dep's pre-merge tip — git's 3-arg rebase then replays
    exactly the patch's own commits, the dep's commits stay on [main] untouched,
    and no conflict arises, regardless of the merge method. *)

(** {2 Structured plan + reasoning} *)

type reason =
  | Anchor_matches_head
      (** The newest anchor's SHA is an ancestor of the patch's HEAD, so
          [anchor.sha..HEAD] is exactly the patch's own work. The standard
          success path. *)
  | History_fallback of string
      (** The newest anchor was unreachable from HEAD (e.g. HEAD was reset), so
          we fell back to the named older anchor's SHA from {!Anchor_history.t}.
      *)
  | No_anchor
      (** No anchor has ever been recorded for this agent and the history is
          empty. The legacy 2-arg rebase is the only option. *)
  | Anchor_unreachable_from_head
      (** At least one anchor exists, but none of them are reachable from HEAD.
          Fall back to the legacy 2-arg rebase. *)
  | Head_unobservable
      (** The runner could not resolve the patch's HEAD SHA (e.g. worktree
          missing). Cannot validate anchors, so fall back to the legacy 2-arg
          rebase. *)
[@@deriving show, eq, sexp_of, compare]

type plan =
  | Onto of { target : string; upstream : string; reason : reason }
      (** Issue [git rebase --onto <target> <upstream>]. *)
  | Plain of { target : string; reason : reason }
      (** Issue legacy [git rebase <target>]. *)
[@@deriving show, eq, sexp_of, compare]

type input = {
  anchor : Anchor.t option;
      (** The newest anchor known for this agent. Should equal
          [List.hd recorded_history] when both are present, but is passed
          separately for ergonomics. *)
  recorded_history : Anchor.t list;
      (** Newest-first anchor history, used for divergence fallback when the
          newest anchor is unreachable from HEAD. May be empty. *)
  base_branch : Types.Branch.t;
      (** The branch to rebase onto. Becomes the [<target>] component of the
          issued plan (rendered via [Types.Branch.to_string]). *)
  head_sha : string option;
      (** The patch's local HEAD, resolved by the runner immediately before
          calling {!plan}. [None] when the worktree is missing or HEAD is
          unresolvable; in that case no anchor can be validated. *)
}

val plan :
  input -> ancestor_oracle:(string -> descendant:string -> bool) -> plan
(** [plan input ~ancestor_oracle] picks the safest rebase invocation given the
    recorded anchors and live observations. [ancestor_oracle sha ~descendant]
    returns [true] iff [sha] is an ancestor of [descendant] (i.e. reachable from
    it in the commit DAG). The runner supplies [git merge-base --is-ancestor];
    tests supply an in-memory DAG. *)

(** {2 Anchor recording after a rebase attempt} *)

val anchor_after_result :
  prev:Anchor.t option ->
  result:Worktree_parser.rebase_result ->
  resolved_remote_sha:string option ->
  base_branch:Types.Branch.t ->
  Anchor.t option
(** Compute the anchor to record after a rebase attempt. Policy:

    - {!Worktree_parser.Ok} or {!Worktree_parser.Noop} with a resolvable remote
      SHA — produce a fresh anchor for [base_branch] with that SHA. Noop
      refreshes too: a noop proves local HEAD already contains the remote tip,
      which is exactly the post-condition we want anchored.
    - {!Worktree_parser.Conflict} or {!Worktree_parser.Error} — return [prev]
      unchanged. A failed attempt must not erase or corrupt the
      previously-recorded anchor.
    - Missing remote SHA on Ok/Noop — return [prev]. The handler observed a
      rebase outcome but couldn't record a SHA, so keep the older anchor rather
      than fabricate one. *)

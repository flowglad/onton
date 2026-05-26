(** Pure decision: should a [git push] proceed, given the observed state of the
    worktree, the named branch, and the remote-tracking ref?

    [Worktree.force_push_with_lease] used to gate on
    [git rev-list --count <base>..HEAD] (worktree HEAD) but push the named
    branch — so an agent that [git switch]ed to a different local branch
    mid-session could trip the gate with commits the push command would never
    upload. Combined with [--force-with-lease]'s blind spot for local commit
    loss, that pair of mismatches put PR #315 in the position where a push of a
    stale branch wiped its remote.

    This module makes both pre-conditions explicit. The effectful caller in
    [Worktree.force_push_with_lease] reads:

    - whether the worktree directory still exists,
    - the worktree's current HEAD-branch name (via
      [git rev-parse --abbrev-ref HEAD]),
    - the SHA of [refs/heads/<expected_branch>],
    - the SHA of [refs/remotes/origin/<expected_branch>] (if any),
    - the two-way ancestry between those two SHAs,
    - [git rev-list --count <base>..refs/heads/<expected_branch>] (commits ahead
      of base, measured on the named branch — NOT HEAD),

    and passes them to [plan]. The decision is either [Push action] (proceed
    with the named git command) or [Refuse reason] (skip the push and route the
    reason to the orchestrator).

    Refusals that map to a planner-only state ([Branch_ref_missing],
    [Branch_switched], [Local_missing_remote_commits]) are translated to
    {!Push_reject_classify.Local_state_unsafe} so the existing [is_permanent] →
    [needs_intervention] escalation path applies without new orchestrator state.

    Note: [ancestry = Local_diverged_from_remote] is NOT refused here. After a
    conflict-resolution rebase, the local branch's new commits and the remote's
    pre-rebase commits are legitimately divergent (same content, different SHAs
    from the replay). Refusing here would strand the orchestrator on
    [conflict_noop_count >= 2] needs-intervention. The git-level
    [--force-if-includes] flag covers the actually-unsafe case (remote has
    commits local doesn't reach via its reflog), so the planner delegates that
    check to git. [Local_missing_remote_commits] — strictly-behind, the PR #315
    incident shape — remains as the layered defense the planner enforces. *)

type sha = string [@@deriving show, eq, sexp_of, compare]

(** Two-way ancestor relationship between [refs/heads/<branch>] and
    [refs/remotes/origin/<branch>], as observed by the caller. *)
type ancestry =
  | Local_includes_remote
      (** Local is at or ahead of remote — safe to force-push. *)
  | Local_missing_remote
      (** Remote has commits the local branch does not reach — a force-push
          would lose them. *)
  | Local_diverged_from_remote
      (** Local and remote each have commits the other does not reach. *)
  | No_remote_yet  (** The branch has never been pushed. *)
  | Unknown  (** Ancestry could not be determined; treat conservatively. *)
[@@deriving show, eq, sexp_of, compare]

type action =
  | Force_push_if_includes
      (** [git push --porcelain --force-with-lease --force-if-includes origin
           <branch>] — the normal supervisor push. *)
  | Initial_push
      (** [git push --porcelain -u origin <branch>] — first push of a brand-new
          branch; no lease/include checks apply because remote ref is absent. *)
[@@deriving show, eq, sexp_of, compare]

type refusal =
  | No_commits_ahead_of_base
      (** The named branch has no commits beyond [base]. GitHub would reject the
          push with "everything up-to-date" or accept a no-op; skip it. *)
  | Worktree_missing
      (** The worktree directory was deleted out from under the supervisor;
          there is no local state to push from. *)
  | Branch_ref_missing of { branch : string }
      (** [refs/heads/<branch>] does not exist locally — we cannot push it. *)
  | Branch_switched of { expected : string; got : string option }
      (** The worktree's current HEAD does not match [expected_branch]. An agent
          switched to a different branch mid-session, so the gate's commit count
          does not represent what the push command would upload. *)
  | Local_missing_remote_commits of { local_sha : sha; remote_sha : sha }
      (** [ancestry = Local_missing_remote] — pushing now would force-push a
          local that is strictly behind remote on real content, wiping commits.
          [ancestry = Local_diverged_from_remote] is intentionally NOT a refusal
          here; see the module-level comment. *)
[@@deriving show, eq, sexp_of, compare]

type decision = Push of action | Refuse of refusal
[@@deriving show, eq, sexp_of, compare]

val plan :
  expected_branch:string ->
  worktree_path_exists:bool ->
  worktree_head_branch:string option ->
  branch_ref_sha:sha option ->
  remote_tracking_sha:sha option ->
  ancestry:ancestry ->
  commits_ahead_of_base:int option ->
  decision
(** [plan] is total and deterministic. Pre-emption order, applied top-down:

    + [worktree_path_exists = false] → [Refuse Worktree_missing]
    + [worktree_head_branch <> Some expected_branch] →
      [Refuse (Branch_switched { expected; got })]
    + [branch_ref_sha = None] → [Refuse (Branch_ref_missing { branch })]
    + [commits_ahead_of_base = Some 0] → [Refuse No_commits_ahead_of_base]
    + [ancestry = Local_missing_remote] (and both refs present) →
      [Refuse (Local_missing_remote_commits _)]
    + [remote_tracking_sha = None] → [Push Initial_push]
    + otherwise (including [ancestry = Local_diverged_from_remote]) →
      [Push Force_push_if_includes] *)

val short_label : decision -> string
(** A short, lowercase, snake_case identifier for the planner arm that fired,
    suitable for the activity log. Always non-empty and ≤ 32 characters.
    Examples: ["force_push"], ["initial_push"], ["refuse_no_commits"],
    ["refuse_wt_missing"], ["refuse_ref_missing"], ["refuse_branch_switched"],
    ["refuse_local_behind"]. *)

val to_push_reject_classify_rejection :
  refusal -> Push_reject_classify.rejection option
(** Map planner refusals onto the rejection variant used by the orchestrator's
    permanent-rejection escalation:

    - [Branch_switched] / [Local_missing_remote_commits] / [Branch_ref_missing]
      → [Some (Local_state_unsafe { reason = short_label_of_refusal })] — route
      through [needs_intervention].
    - [No_commits_ahead_of_base] / [Worktree_missing] → [None] — the
      orchestrator already has dedicated non-rejection handlers
      ([Push_no_commits], [Push_worktree_missing]). *)

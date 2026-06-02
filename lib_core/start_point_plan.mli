(** Pure decision: what start point should a worktree's branch be created from,
    given the observed state of local and remote refs?

    Onton's failure mode of record (PR #315) was [Worktree.create] silently
    reusing a stale local branch ref — the user's working clone had
    [refs/heads/<branch>] left at the PR's base, so the new worktree started
    missing every PR commit. The agent's session pushed an empty branch over the
    real PR, and GitHub auto-closed it.

    This module exists so that decision lives in one inspectable, testable
    place. The effectful caller in [Worktree] reads the inputs from git (after a
    [fetch_origin <branch>] performed by [Worktree_setup]), passes them in, and
    executes the [action] this returns — or reports the [refusal] upward so the
    orchestrator can route to {!Patch_agent.needs_intervention}.

    {1 Authority}

    For an onton-managed branch, the {e remote} is authoritative whenever a
    remote ref exists. A local ref that matches the remote or is strictly behind
    it ([Equal] / [Remote_ahead]) is safe to reset to the remote; a local ref
    that has commits the remote doesn't ([Local_ahead] / [Diverged]) is treated
    as suspect — a human pushed something the supervisor doesn't know about, and
    silently overwriting it would lose work. *)

type sha = string [@@deriving show, eq, sexp_of, compare]

(** Pre-computed two-way ancestor relationship between a local ref and a remote
    tracking ref. The effectful caller fills this in with the result of two
    [git merge-base --is-ancestor] probes, or [Unknown] if either probe failed.
*)
type ancestry =
  | Local_ahead  (** local has commits not reachable from remote *)
  | Remote_ahead
      (** remote has commits not reachable from local — local is stale *)
  | Equal  (** local and remote point at the same commit *)
  | Diverged  (** each side has unique commits the other lacks *)
  | Unknown  (** ancestry could not be determined (e.g. probe failed) *)
[@@deriving show, eq, sexp_of, compare]

(** The action a successful plan instructs the caller to take. *)
type action =
  | Reset_and_use_remote_tracking of { remote_sha : sha }
      (** [git worktree add -B <branch> <path> origin/<branch>] — recreate the
          local branch at the remote tip. Used whenever a remote ref exists and
          the local ref is absent, equal, or strictly behind. *)
  | Use_local_branch_unchanged of { local_sha : sha }
      (** [git worktree add <path> <branch>] — the remote ref is unknown (never
          been pushed yet), so the local ref is the only source of truth. *)
  | Create_new_branch_from_base of { base_branch : string }
      (** [git worktree add -b <branch> <path> origin/<base>] — neither local
          nor remote ref exists; the worktree starts a brand-new branch at the
          tip of the configured base. *)
[@@deriving show, eq, sexp_of, compare]

(** The refusal an unsuccessful plan reports — every variant is permanent in the
    sense that retrying without a human resolving the underlying state will hit
    the same refusal. The effectful caller logs the refusal and surfaces it
    through the orchestrator's intervention path. *)
type refusal =
  | Local_diverged_from_remote of { local_sha : sha; remote_sha : sha }
      (** Ancestry is [Diverged] (or [Unknown]) and both refs are present — a
          human's local commits would be lost by [-B] reset. *)
  | Local_has_unpushed_commits of { local_sha : sha; remote_sha : sha }
      (** Ancestry is [Local_ahead] and both refs are present — the local ref is
          strictly ahead of remote. On a create path this is suspicious: onton
          is normally the sole writer for these branches. *)
  | Branch_checked_out_in_main_root
      (** The named branch is currently HEAD of the user's main working tree;
          git would refuse to register a second worktree for it. *)
  | Worktree_already_registered of { existing_path : string }
      (** Git already lists a worktree for this branch elsewhere; recreating
          would race. *)
[@@deriving show, eq, sexp_of, compare]

(** A plan is either an action to execute or a refusal to report. *)
type decision = Plan of action | Refuse of refusal
[@@deriving show, eq, sexp_of, compare]

val plan :
  local_ref:sha option ->
  remote_ref:sha option ->
  ancestry:ancestry ->
  base_branch:string ->
  branch_checked_out_in_main_root:bool ->
  existing_worktree_path:string option ->
  decision
(** [plan] is total and deterministic. Pre-emption order, applied top-down:

    + [branch_checked_out_in_main_root = true] →
      [Refuse Branch_checked_out_in_main_root]
    + [existing_worktree_path = Some p] →
      [Refuse (Worktree_already_registered { existing_path = p })]
    + [(local_ref, remote_ref) = (None, None)] →
      [Plan (Create_new_branch_from_base { base_branch })]
    + [(None, Some r)] →
      [Plan (Reset_and_use_remote_tracking { remote_sha = r })]
    + [(Some l, None)] → [Plan (Use_local_branch_unchanged { local_sha = l })]
    + [(Some l, Some r)] with [ancestry = Equal | Remote_ahead] →
      [Plan (Reset_and_use_remote_tracking { remote_sha = r })]
    + [(Some l, Some r)] with [ancestry = Local_ahead] →
      [Refuse (Local_has_unpushed_commits ...)]
    + [(Some l, Some r)] with [ancestry = Diverged | Unknown] →
      [Refuse (Local_diverged_from_remote ...)] (conservative — refuse rather
      than risk silent commit loss).

    The base is consumed only in the brand-new-branch arm
    ([Create_new_branch_from_base]). Whether that base is up to date with
    [origin/<main>] is intentionally {e not} gated here: freshness is
    dependency-scoped and enforced by the orchestrator's scheduling gate (see
    {!Start_eligibility}). A stacked dependent cut from its base's tip contains
    everything in that base; main integration cascades at merge boundaries, so a
    base lagging main for unrelated reasons must not block the cut. Which
    {e ref} the base is read from (freshly-fetched remote SHA vs local branch)
    is the separate {!base_start_point} decision below, supplied by the caller
    as [base_branch]. *)

val short_label : decision -> string
(** A short, lowercase, snake_case identifier for the planner arm that fired,
    suitable for the activity log. Always non-empty and ≤ 32 characters.
    Examples: ["reset_to_remote"], ["use_local_unchanged"],
    ["create_from_base"], ["refuse_local_diverged"], ["refuse_local_ahead"],
    ["refuse_main_checkout"], ["refuse_wt_registered"]. *)

(** {1 Base start point for the brand-new-branch cut}

    Which ref should [Create_new_branch_from_base] actually cut from? The answer
    is asymmetric, because authority differs by base kind:

    - {b Base is the main branch}: the {e remote} is authoritative. The
      orchestrator never advances the managed clone's local [<main>] — it only
      observes merges via the API — so the local ref lags origin by however long
      since the last incidental fetch. Cutting from it right after a
      dependency's squash-merge produces a branch missing that dependency's
      commits (the connector-adapter-shape-unification patch-4 stale cut: local
      [main] was fetched seconds {e before} the dep's squash landed, and the
      branch was born needing a freshen rebase that then conflicted). The caller
      fetches [origin/<main>] and cuts from the {e resolved SHA} — a SHA rather
      than the [origin/<main>] name so the cut is pinned to exactly what was
      fetched and the new branch picks up no upstream tracking.
    - {b Base is a dependency patch's branch}: the {e local} ref is
      authoritative. The dep's worktree shares the managed repo's ref store, so
      its commits land on the local branch first and origin lags until the
      post-session push — fetching and preferring origin here could travel
      {e backwards}. The scheduling gate ({!Start_eligibility}) already
      guarantees the dep is settled (not mid-rebase, not mid-conflict) when the
      cut fires.

    Cutting a new branch from the freshest main is {e not} a main-currency gate:
    nothing is rebased or re-validated on main movement, and an open PR behind
    main still merges directly. It only stops a brand-new branch from being born
    on yesterday's main. *)

type base_start_point =
  | Base_at_fetched_remote_sha of { sha : sha }
      (** Cut from [sha], the just-fetched tip of [origin/<base>]. Chosen only
          for a main-branch base whose remote tip was successfully fetched and
          resolved. *)
  | Base_at_local_ref of { base_branch : string }
      (** Cut from the local [base_branch] ref — the dependency-base case, and
          the fail-open fallback when fetching/resolving the main base's remote
          tip failed (the freshen-rebase detectors remain the backstop, exactly
          as before this decision existed). *)
[@@deriving show, eq, sexp_of, compare]

val base_start_point :
  base_branch:string ->
  base_is_main:bool ->
  fetched_remote_sha:sha option ->
  base_start_point
(** [base_start_point] is total and deterministic:

    + [base_is_main = true] and [fetched_remote_sha = Some sha] →
      [Base_at_fetched_remote_sha { sha }].
    + Otherwise → [Base_at_local_ref { base_branch }]. A non-main base never
      cuts from a remote SHA, even if the caller supplies one — local is
      authoritative for dependency branches. *)

val base_start_point_ref : base_start_point -> string
(** The string to hand to [git worktree add -b <branch> <path> <start-point>]:
    the SHA for [Base_at_fetched_remote_sha], the branch name for
    [Base_at_local_ref]. *)

val base_start_point_short_label : base_start_point -> string
(** Activity-log identifier for the chosen arm. Non-empty, ≤ 32 characters,
    lowercase snake_case: ["base_at_origin_sha"] / ["base_at_local_ref"]. *)

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

(** Whether the base branch (used only when cutting a brand-new branch) already
    contains the latest [origin/<main>]. The effectful caller fills this in with
    the result of a [git merge-base --is-ancestor origin/<main> <base>] probe:
    [Fresh] when main is an ancestor of base, [Stale] when it is not,
    [Unknown_freshness] when the probe could not be run (main not tracked, probe
    error). Only [Stale] blocks; see {!plan}. *)
type base_freshness = Fresh | Stale | Unknown_freshness
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
  | Base_branch_stale_vs_main of { base_branch : string }
      (** About to cut a brand-new branch from [base_branch], but
          [origin/<main>] is not an ancestor of [base_branch] — the base lags a
          merged upstream and cutting from it would silently drop the upstream's
          commits. Defense-in-depth behind the orchestrator's scheduling gate
          (see {!Start_eligibility}); reaching this refusal means a [Start] was
          executed without going through {!Orchestrator.runnable_messages}. *)
[@@deriving show, eq, sexp_of, compare]

(** A plan is either an action to execute or a refusal to report. *)
type decision = Plan of action | Refuse of refusal
[@@deriving show, eq, sexp_of, compare]

val plan :
  local_ref:sha option ->
  remote_ref:sha option ->
  ancestry:ancestry ->
  base_branch:string ->
  base_freshness:base_freshness ->
  branch_checked_out_in_main_root:bool ->
  existing_worktree_path:string option ->
  decision
(** [plan] is total and deterministic. Pre-emption order, applied top-down:

    + [branch_checked_out_in_main_root = true] →
      [Refuse Branch_checked_out_in_main_root]
    + [existing_worktree_path = Some p] →
      [Refuse (Worktree_already_registered { existing_path = p })]
    + [(local_ref, remote_ref) = (None, None)] and [base_freshness = Stale] →
      [Refuse (Base_branch_stale_vs_main { base_branch })]
    + [(local_ref, remote_ref) = (None, None)] otherwise →
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

    [base_freshness] only matters in the brand-new-branch arm: it is the only
    decision that consumes [base_branch]. When a local or remote ref for the
    patch's own branch already exists, the base is irrelevant and freshness is
    not consulted. *)

val short_label : decision -> string
(** A short, lowercase, snake_case identifier for the planner arm that fired,
    suitable for the activity log. Always non-empty and ≤ 32 characters.
    Examples: ["reset_to_remote"], ["use_local_unchanged"],
    ["create_from_base"], ["refuse_local_diverged"], ["refuse_local_ahead"],
    ["refuse_main_checkout"], ["refuse_wt_registered"]. *)

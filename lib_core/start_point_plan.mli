(* @archlint.module interface
   @archlint.domain start-point-plan *)

(** Provision a checkout without discarding unique branch history. Proven
    remote-ahead history replaces a stale local ref (the PR #315 guard). Ahead
    or diverged local history is adopted unchanged; Branch_reconcile owns later
    remote integration, history recovery and verified publication. Failed
    ancestry observations cannot authorize a checkout mutation. *)

type sha = string [@@deriving show, eq, sexp_of, compare]

(** [Unknown] represents a failed ancestry probe, not proof of divergence. *)
type ancestry = Local_ahead | Remote_ahead | Equal | Diverged | Unknown
[@@deriving show, eq, sexp_of, compare]

type action =
  | Reset_and_use_remote_tracking of { remote_sha : sha }
      (** Use the observed remote commit when local is absent, equal, or a
          proven ancestor. The executor compares the captured local ref before
          changing it. *)
  | Use_local_branch_unchanged of { local_sha : sha }
      (** Adopt the captured local commit without resetting its branch. This
          grants no publication authority over remote work. *)
  | Create_new_branch_from_base of { base_branch : string }
      (** Both branch refs are absent. Create from the supplied base. *)
[@@deriving show, eq, sexp_of, compare]

type refusal =
  | Ancestry_unavailable of { local_sha : sha; remote_sha : sha }
      (** Retry observation; this is an infrastructure failure. *)
  | Branch_checked_out_in_main_root
  | Worktree_already_registered of { existing_path : string }
[@@deriving show, eq, sexp_of, compare]

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
(** Total and deterministic. Existing checkout ownership takes precedence. With
    both refs present, [Equal]/[Remote_ahead] uses the remote;
    [Local_ahead]/[Diverged] preserves local; [Unknown] retries observation. A
    lone local or remote ref supplies the checkout; absent refs create from the
    supplied base. Patch-equivalence is not a provisioning reset authority. *)

val short_label : decision -> string
(** Nonempty lowercase snake_case diagnostic, at most 32 characters. *)

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

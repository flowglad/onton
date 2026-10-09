(* @archlint.module interface
   @archlint.domain worktree *)

open Base

type t = private {
  patch_id : Types.Patch_id.t;
  branch : Types.Branch.t;
  path : string;
  newly_created : bool;
}
[@@deriving show, eq, sexp_of, compare]

val worktree_dir : project_name:string -> patch_id:Types.Patch_id.t -> string

val has_cancellation : exn -> bool
(** Includes cancellation nested in [Eio.Exn.Multiple]. *)

val is_transient_spawn_failure : exn -> bool
(** [true] for exceptions that mean a subprocess could not be spawned/run to
    completion (e.g. [posix_spawn] failing with EAGAIN under process-table
    pressure) — i.e. anything that is {e not} a process verdict
    ([Eio.Process.E _]: [Child_error]/[Executable_not_found], where git actually
    ran) and {e not} a cancellation. These are the failures
    {!retry_transient_spawn} retries. Exposed for testing. *)

val retry_transient_spawn : ?attempts:int -> (unit -> 'a) -> 'a
(** Run [f], retrying up to [attempts] times (default 4) while it raises a
    {!is_transient_spawn_failure}. A process verdict or cancellation is
    re-raised immediately; the last attempt's exception is re-raised once
    attempts are exhausted. Yields to the scheduler between attempts. Exposed
    for testing. *)

val resolve_main_root :
  process_mgr:_ Eio.Process.mgr -> repo_root:string -> string
(** Resolve the main working tree (git common dir's parent) from any repo path.
    If [repo_root] is itself a worktree, this returns the path of the main
    checkout, not the worktree. Falls back to [repo_root] on error. *)

val is_checked_out_in_repo_root :
  process_mgr:_ Eio.Process.mgr -> repo_root:string -> Types.Branch.t -> bool
(** Returns [true] if [branch] is currently the HEAD of the main working tree
    (resolved via the git common dir, not necessarily [repo_root] itself). A
    worktree cannot be created for a branch that is checked out there. *)

val remote_branch_exists :
  process_mgr:_ Eio.Process.mgr -> repo_root:string -> string -> bool
(** Returns [true] if [origin/<branch>] exists as a remote tracking ref. *)

type create_io = {
  worktree_exists : path:string -> bool;
  check_ref_collision : branch_str:string -> unit;
  read_ref : ref_name:string -> (string option, string) Result.t;
  ancestry : local:string -> remote:string -> Start_point_plan.ancestry;
  execute_action :
    path:string ->
    branch_str:string ->
    expected_local:string option ->
    Start_point_plan.action ->
    bool;
}
(** The effectful operations used by {!S.create}. Factored out so the wiring can
    be tested with in-memory fakes instead of a live git repository:
    [worktree_exists] drives the short-circuit, ref/ancestry reads feed
    {!Start_point_plan.plan}, and [execute_action] realises its decision. *)

val create_with_io :
  io:create_io ->
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  branch:Types.Branch.t ->
  base_ref:string ->
  (t, Start_point_plan.refusal) Result.t
(** Start-point planning, parameterised over validated lifecycle effects. Reads
    the local and remote refs via [io], computes ancestry only when both exist,
    consults {!Start_point_plan.plan}, and either runs [io.execute_action] or
    returns the refusal. {!make} supplies backend-aware [io]. Exposed so the
    short-circuit, planner dispatch, and refusal mapping can be exercised
    without spawning git. *)

val read_repo_ref_sha :
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  ref_name:string ->
  (string option, string) Result.t
(** Observe a full commit ref. [Ok None] is verified absence; process failures,
    non-commit refs, and disappearing or malformed objects return [Error]. *)

val compute_repo_ancestry :
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  local:string ->
  remote:string ->
  Start_point_plan.ancestry
(** Classify the two-way ancestor relationship between [local] and [remote] via
    two [git merge-base --is-ancestor] probes (or [Unknown] if either fails).
    The git-backed primitive behind {!create_io.ancestry}; exposed so an
    integration test can verify the [Equal]/[Remote_ahead]/[Local_ahead]/
    [Diverged] classification against real commits. *)

(** Outcome of [fetch_origin_branch]. The [Fetch_branch_no_remote_ref] case is
    the routine "brand-new branch — no upstream yet" state, which trips on the
    very first creation of every patch worktree and is not a failure; callers
    log it calmly to avoid misleading the operator. Real fetch failures
    (network, auth, ref-lock) surface as [Fetch_branch_error msg]. *)
type fetch_branch_result = Worktree_parser.fetch_branch_result =
  | Fetch_branch_ok
  | Fetch_branch_no_remote_ref
  | Fetch_branch_error of string
[@@deriving show, eq, sexp_of, compare]

val classify_fetch_branch_result :
  code:int -> stderr:string -> fetch_branch_result
(** Pure: classify a branch-scoped [git fetch origin <branch>:...] invocation
    from its exit code and stderr. Detects the no-upstream case by matching
    git's canonical phrasing ["couldn't find remote ref"]. Split out from
    [fetch_origin_branch] so the decision can be property-tested without
    spawning processes. *)

val fetch_origin_branch :
  fetch_lock:Eio.Mutex.t ->
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  branch_str:string ->
  fetch_branch_result
(** Run [git -C <repo_root> fetch origin <branch>:refs/remotes/origin/<branch>]
    under the shared [fetch_lock]. The planner correctly handles
    [remote_ref = None] for the brand-new-branch case, so callers proceed on
    every result variant; the distinction matters for log clarity. *)

val detect_branch :
  process_mgr:_ Eio.Process.mgr -> path:string -> Types.Branch.t

val parse_porcelain :
  repo_root:string -> string -> (string * Types.Branch.t) list
(** Parse [git worktree list --porcelain] output into [(path, branch)] pairs.
    Excludes the repo root entry and detached-HEAD worktrees. Pure function. *)

val git_status : process_mgr:_ Eio.Process.mgr -> path:string -> string
(** Run [git status] in the worktree and return its output. Returns empty string
    on failure. *)

val has_uncommitted_changes :
  process_mgr:_ Eio.Process.mgr -> path:string -> (bool, string) Result.t
(** Whether porcelain status reports staged, unstaged, or untracked changes.
    Returns an error with the exit status and stderr if the command fails. *)

val conflict_diff : process_mgr:_ Eio.Process.mgr -> path:string -> string
(** Run [git diff --diff-filter=U] to show conflict markers for unmerged files.
    Returns empty string if no conflicts or on failure. Truncates at 4000 chars.
*)

val classify_fetch_result : code:int -> stderr:string -> (unit, string) Result.t
(** Pure: classify a [git fetch origin] invocation from its exit code and stderr
    into [Ok ()] (exit 0) or [Error msg] (non-zero, with the exit code and
    stripped stderr embedded in the message). Split out from [fetch_origin] so
    the decision can be property-tested independently of the subprocess and
    mutex. *)

val fetch_origin :
  fetch_lock:Eio.Mutex.t ->
  process_mgr:_ Eio.Process.mgr ->
  path:string ->
  (unit, string) Result.t
(** Run [git fetch origin] in the worktree at [path] to update remote tracking
    refs. Returns [Ok ()] on success, [Error msg] on failure.

    [fetch_lock] must be shared across all worktrees of the same repo. Git
    worktrees share the main repo's ref store, so concurrent fetches race on the
    compare-and-swap update of [refs/remotes/origin/*] and the losing process
    fails with "cannot lock ref". The lock serializes fetches to prevent this.
*)

val parse_push_porcelain : string -> char option
(** Pure: extract the status flag character from [git push --porcelain] stdout.
    Returns [Some '!'] for rejected, [Some '+'] for forced update, etc. Returns
    [None] if no status line is found. *)

type push_result = Worktree_parser.push_result =
  | Push_ok
  | Push_up_to_date
  | Push_rejected of Push_reject_classify.rejection
  | Push_error of string
[@@deriving show, eq, sexp_of, compare]

type push_gate = Worktree_parser.push_gate = Proceed | Skip_no_commits
[@@deriving show, eq, sexp_of]

val parse_commit_count : code:int -> stdout:string -> int option
(** Pure: parse [git rev-list --count base..HEAD] output into a commit count.
    [None] when [code <> 0] or stdout is unparseable. *)

val push_gate_from_count : int option -> push_gate
(** Pure: decide whether to push given a commit-count result.
    - [Some 0] → [Skip_no_commits] (branch is base-equal; GitHub would reject).
    - [None] or [Some _] → [Proceed] (unknown counts default to proceeding so
      real failures surface via the push step, not via a silent skip). *)

val classify_push_result :
  code:int -> stdout:string -> stderr:string -> push_result
(** Pure: classify [git push --porcelain] output into a [push_result],
    regardless of publication strategy. The result describes the Git command;
    checkout availability and publication eligibility belong to reconciliation.
*)

val commit_gameplan :
  clock:float Eio.Time.clock_ty Eio.Time.clock ->
  process_mgr:_ Eio.Process.mgr ->
  path:string ->
  publication:Gameplan_publication.t ->
  message:string ->
  (unit, string) Result.t
(** Commit source bytes without a model. Refuses unrelated changes, symlink
    destinations, and existing different contents. Repeated calls are
    idempotent. Git commands have closed stdin and a bounded timeout. *)

module type S = sig
  val resolve_main_root : unit -> string
  val is_checked_out_in_repo_root : Types.Branch.t -> bool
  val remote_branch_exists : string -> bool

  val create :
    project_name:string ->
    patch_id:Types.Patch_id.t ->
    branch:Types.Branch.t ->
    base_ref:string ->
    (t, Start_point_plan.refusal) Result.t

  val fetch_origin_branch :
    fetch_lock:Eio.Mutex.t -> branch:string -> fetch_branch_result

  val remove : discard:bool -> t -> unit
  val detect_branch : path:string -> Types.Branch.t
  val list_with_branches : unit -> (string * Types.Branch.t) list
  val find_for_branch : Types.Branch.t -> string option
  val prune_stale_for_branch : Types.Branch.t -> unit

  val inspect_existing :
    path:string -> branch:Types.Branch.t -> (bool, string) Result.t
  (** Read-only validation of an existing linked checkout, including repository
      ownership. Never creates, cleans up or repairs a checkout. *)

  val ensure_ready :
    path:string -> branch:Types.Branch.t -> (bool, string) Result.t
  (** Validate or repair a published checkout. [Ok false] means absent;
      unfinished creation, ownership conflicts and repair failures return
      [Error] and must not trigger replacement creation. *)

  val run_hook :
    clock:_ Eio.Time.clock ->
    script:string ->
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    env:(string * string) list ->
    unit ->
    (unit, string) Result.t

  val fetch_origin :
    fetch_lock:Eio.Mutex.t -> path:string -> (unit, string) Result.t

  val git_status : path:string -> string
  val has_uncommitted_changes : path:string -> (bool, string) Result.t
  val conflict_diff : path:string -> string
  val read_branch_sha : path:string -> ref_name:string -> string option

  val is_ancestor : path:string -> ancestor:string -> descendant:string -> bool
  (** [is_ancestor ~path ~ancestor ~descendant] returns [true] iff [ancestor] is
      an ancestor of [descendant] in the repo at [path], via
      [git merge-base --is-ancestor]. Returns [false] on any error (including
      unresolvable SHAs), so callers can treat it as a total
      pure-from-the-outside oracle. *)

  val commit_gameplan :
    path:string ->
    publication:Gameplan_publication.t ->
    message:string ->
    (unit, string) Result.t

  val materialization :
    path:string ->
    project_name:string ->
    branch:Types.Branch.t ->
    (Branch_reconcile.materialization option, string) Result.t

  val reconcile :
    path:string ->
    project_name:string ->
    branch:Types.Branch.t ->
    operation:Branch_reconcile.operation ->
    Branch_reconcile.command ->
    Branch_reconcile.result
  (** Mutates Git state. The caller must hold the patch/root write lock for this
      checkout throughout the call. *)
end

type client = (module S)

val make :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  config:Worktree_lifecycle.config ->
  clock:float Eio.Time.clock_ty Eio.Time.clock ->
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  client

val normalize_path : string -> string
(** Resolve a relative path to absolute using the current working directory. *)

val branch_prefixes : string -> string list
(** Pure: collect all path prefixes of a branch name. For ["a/b/c"] returns
    [["a"; "a/b"]]. *)

val find_ci_ref_collision :
  existing_branches:string list -> string -> string option
(** Pure: find the first existing branch that case-insensitively matches a path
    prefix of the given branch name. Returns [Some colliding_branch] or [None].
    Used to detect macOS case-insensitive filesystem ref collisions. *)

val newly_created : t -> bool
val path : t -> string
val patch_id : t -> Types.Patch_id.t
val branch : t -> Types.Branch.t

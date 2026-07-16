(* @archlint.module interface
   @archlint.domain worktree *)

open Base

type t = private {
  patch_id : Types.Patch_id.t;
  branch : Types.Branch.t;
  path : string;
}
[@@deriving show, eq, sexp_of, compare]

val worktree_dir : project_name:string -> patch_id:Types.Patch_id.t -> string

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

val create :
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  branch:Types.Branch.t ->
  base_ref:string ->
  (t, Start_point_plan.refusal) Result.t
(** Create a git worktree for [branch] under [project_name]/[patch_id].

    Consults {!Start_point_plan.plan} before invoking git: reads the local
    [refs/heads/<branch>] and remote-tracking [refs/remotes/origin/<branch>]
    SHAs from [repo_root], computes their two-way ancestry, and executes the
    action the planner returns. The supervisor is responsible for running
    [fetch_origin_branch] beforehand so [refs/remotes/origin/<branch>] is fresh;
    without that step the planner would see a stale remote ref.

    Base freshness vs [origin/<main>] is intentionally not gated here — it is
    dependency-scoped and enforced by the orchestrator's {!Start_eligibility}
    scheduling gate. A stacked dependent cut from its base's tip contains
    everything in that base, so a base lagging main for unrelated reasons must
    not block the cut.

    Returns [Error refusal] when the planner refuses (local diverged from
    remote, branch already checked out elsewhere, etc.). The caller surfaces
    refusals through the orchestrator's intervention path.

    Pre-empted inputs [branch_checked_out_in_main_root] and
    [existing_worktree_path] are checked by the caller
    ({!Worktree_setup.ensure_worktree}) before this function runs; this function
    passes [false] / [None] to the planner. *)

type create_io = {
  worktree_exists : path:string -> bool;
  check_ref_collision : branch_str:string -> unit;
  read_ref : ref_name:string -> string option;
  ancestry : local:string -> remote:string -> Start_point_plan.ancestry;
  execute_action :
    path:string -> branch_str:string -> Start_point_plan.action -> unit;
}
(** The effectful operations {!create} performs. Factored out so the wiring can
    be tested with in-memory fakes instead of a live git repository:
    [worktree_exists] drives the short-circuit, [read_ref]/[ancestry] feed
    {!Start_point_plan.plan}, and [execute_action] realises its decision. *)

val create_with_io :
  io:create_io ->
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  branch:Types.Branch.t ->
  base_ref:string ->
  (t, Start_point_plan.refusal) Result.t
(** The control flow of {!create}, parameterised over its effects. Reads the
    local and remote refs via [io], computes ancestry only when both exist,
    consults {!Start_point_plan.plan}, and either runs [io.execute_action] or
    returns the refusal. {!create} is this with the git-backed [io]. Exposed so
    the short-circuit, planner dispatch, and refusal mapping can be exercised
    without spawning git. *)

val read_repo_ref_sha :
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  ref_name:string ->
  string option
(** Resolve [ref_name] to its SHA in [repo_root] via [git rev-parse --verify],
    or [None] if the ref does not exist or git fails. The git-backed primitive
    behind {!create_io.read_ref}; exposed so an integration test can verify it
    reads [refs/heads/<branch>] and [refs/remotes/origin/<branch>] correctly
    against a real repo. *)

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

val remove : process_mgr:_ Eio.Process.mgr -> repo_root:string -> t -> unit

val detect_branch :
  process_mgr:_ Eio.Process.mgr -> path:string -> Types.Branch.t

val parse_porcelain :
  repo_root:string -> string -> (string * Types.Branch.t) list
(** Parse [git worktree list --porcelain] output into [(path, branch)] pairs.
    Excludes the repo root entry and detached-HEAD worktrees. Pure function. *)

val list_with_branches :
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  (string * Types.Branch.t) list

val is_ancestor_patch_subject :
  project_name:string -> ancestor_ids:Types.Patch_id.t list -> string -> bool
(** Pure: does a commit subject match the convention
    [[<project_name>] Patch <N>:] for some [N] in [ancestor_ids]? Used to strip
    squash-merged ancestor commits whose patch-ids no longer match any commit on
    [main]. *)

type unique_commit = Worktree_parser.unique_commit = {
  sha : string;
  subject : string;
}
[@@deriving show, eq, sexp_of, compare]

val classify_unique_commits :
  project_name:string ->
  ancestor_ids:Types.Patch_id.t list ->
  string ->
  (unique_commit list * string, string) Result.t
(** Pure: parse
    [git log --cherry-pick --right-only --no-merges --no-show-signature
     --format=%H %s] output into [unique_commit] records, dropping entries whose
    subject matches an ancestor patch. Preserves git's newest-first emission
    order. Returns the list together with the oldest SHA (the [~1] of which
    becomes the rebase's [--onto] anchor); callers cannot accidentally request
    one without the other and lose the recovery info needed by the conflict
    prompt. Both [--no-merges] and [--no-show-signature] are load-bearing for
    callers who build their own [git log] invocation: [--no-merges] prevents a
    merge-commit line like [<sha> Merge branch 'x'] from being picked as the
    oldest SHA (its [~1] parent is the wrong side of the merge), and
    [--no-show-signature] keeps GPG annotation lines from being mistaken for
    SHAs. *)

val oldest_non_ancestor_commit :
  project_name:string ->
  ancestor_ids:Types.Patch_id.t list ->
  string ->
  (string, string) Result.t
(** Pure back-compat wrapper around [classify_unique_commits] that returns only
    the oldest SHA (drops the per-commit list). New code should call
    [classify_unique_commits] directly. *)

val git_status : process_mgr:_ Eio.Process.mgr -> path:string -> string
(** Run [git status] in the worktree and return its output. Returns empty string
    on failure. *)

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

type rebase_strategy = Worktree_parser.rebase_strategy = Onto | Plain
[@@deriving show, eq, sexp_of, compare]

type conflict_info = Worktree_parser.conflict_info = {
  target : string;
  old_base : string;
  unique_commits : unique_commit list;
  strategy : rebase_strategy;
  orig_head : string;
}
[@@deriving show, eq, sexp_of, compare]
(** Recovery payload threaded into the conflict prompt so an agent that aborts
    the in-progress rebase can reconstruct the correct restart command. When
    [strategy = Onto], the supervisor ran [git rebase --onto target old_base]
    and the agent should restart with the same arguments; [unique_commits] is
    the set of commits that belong to this patch in git-log emission order (head
    of the list = newest commit, last element = oldest). The prompt renderer
    reverses the list to display oldest-first as a bullet list. May be empty
    when reconstructed from in-progress rebase-merge state and the ancestor
    filter excludes every commit; the recovery command is still valid because
    [target] and [old_base] are read from the rebase-merge files. When
    [strategy = Plain], the supervisor fell back to [git rebase target] because
    no unique commits could be isolated; [old_base = ""] and
    [unique_commits = []]. [orig_head] is the pre-rebase HEAD SHA — captured
    before [git rebase] ran, so an agent that loses worktree state can
    [git reset --hard <orig_head>] back to their starting point. Empty when
    capture failed (best-effort). *)

val parse_rebase_merge_state :
  onto_contents:string ->
  upstream_contents:string ->
  orig_head_contents:string ->
  log_format_h_s:string ->
  project_name:string ->
  ancestor_ids:Types.Patch_id.t list ->
  target:string ->
  conflict_info option
(** Pure: assemble a [conflict_info] from the contents of
    [.git/rebase-merge/{onto,upstream,orig-head}] together with
    [git log --format=%H %s <upstream>..<orig-head>] output. Returns [None] only
    when [onto_contents] or [upstream_contents] is blank — both are required to
    render the recovery command. An empty / all-filtered log degrades to an
    empty [unique_commits] list, not to [None], so a restarted orchestrator
    still surfaces the [git rebase --onto target old_base] command even when the
    commits cannot be enumerated. [onto_contents] is the rebase destination SHA;
    [upstream_contents] is the old-base SHA used as the recovery [old_base]. *)

type onto_anchor = Worktree_parser.onto_anchor =
  | Anchor of string
  | No_anchor of string
[@@deriving show, eq, sexp_of, compare]

val classify_onto_anchor : code:int -> stdout:string -> onto_anchor
(** Pure: interpret the [git rev-parse <oldest>~1] probe whose result becomes
    the [--onto] old-base anchor in [find_old_base], from its exit [code] and
    [stdout]. [Anchor sha] on success with a non-blank SHA; [No_anchor reason]
    when git failed {e or} returned a blank SHA on exit 0. The blank-on-success
    case occurs when the oldest unique commit is a root commit (no parent) on
    git versions that resolve [<root>~1] to "" rather than erroring; feeding
    that "" to [git rebase --onto target ""] aborts with [invalid upstream ''],
    so the caller treats [No_anchor] as a detection failure and falls back to
    the plain/upstream rebase form. *)

type rebase_result = Worktree_parser.rebase_result =
  | Ok
  | Noop
  | Conflict of conflict_info
  | Error of string
[@@deriving show, eq, sexp_of, compare]

val rebase_onto :
  upstream:string ->
  process_mgr:_ Eio.Process.mgr ->
  path:string ->
  target:Types.Branch.t ->
  project_name:string ->
  ancestor_ids:Types.Patch_id.t list ->
  unit ->
  rebase_result
(** Rebase HEAD onto [target]. [upstream] is the resolved [<upstream>] argument
    for [git rebase --onto <target> <upstream> HEAD], computed by the caller via
    {!Rebase_decision.plan} from the agent's anchor history. If [upstream]
    equals the rendered [target], the legacy 2-arg [git rebase <target>] form is
    used instead.

    Uses [git log --cherry-pick] to detect already-applied commits, and
    supplements that with subject-pattern matching against
    [[<project_name>] Patch N:] for every [N] in [ancestor_ids] — the fallback
    is load-bearing when [target] is a squash-merging trunk like [main], because
    squash-merged ancestor commits carry a fresh patch-id that cherry-pick
    cannot equate with the original feature-branch commits. Pass
    [~project_name:""] or [~ancestor_ids:[]] to opt out of the subject filter
    entirely; cherry-pick deduplication still applies.

    On [Conflict], the rebase is left in progress and the returned
    [conflict_info] carries the recovery info the patch-agent prompt threads
    through to the agent. *)

val read_in_progress_conflict_info :
  process_mgr:_ Eio.Process.mgr ->
  path:string ->
  target:Types.Branch.t ->
  project_name:string ->
  ancestor_ids:Types.Patch_id.t list ->
  conflict_info option
(** Effectful: reconstruct [conflict_info] when a rebase is already in progress
    in the worktree at [path] (the orchestrator restarts mid-rebase, or a
    previous run left state behind). Reads
    [.git/rebase-merge/{onto,upstream,orig-head}] to recover the [--onto]
    destination, the upstream (old-base limit), and the pre-rebase HEAD, then
    runs [git log upstream..orig-head] to enumerate the patch's unique commits.
    Delegates to [parse_rebase_merge_state]. [onto] and [upstream] are required;
    a missing or blank [orig-head] degrades to an empty [orig_head] (the prompt
    skips the [git reset --hard] block) and the log range falls back to
    [upstream..HEAD]. Returns [None] best-effort when [onto]/[upstream] reads
    fail or the [git log] call errors. Only handles [.git/rebase-merge] rebases;
    a [.git/rebase-apply] state returns [None]. *)

val parse_push_porcelain : string -> char option
(** Pure: extract the status flag character from [git push --porcelain] stdout.
    Returns [Some '!'] for rejected, [Some '+'] for forced update, etc. Returns
    [None] if no status line is found. *)

type push_result = Worktree_parser.push_result =
  | Push_ok
  | Push_up_to_date
  | Push_no_commits
  | Push_rejected of Push_reject_classify.rejection
  | Push_worktree_missing
      (** Returned when the worktree directory was deleted between session start
          and push — typically a manual [rm -rf] or a leftover failed checkout.
          The caller should route this to a clean state-reset (e.g.
          [Session_worktree_missing]) rather than retry-push, since the local
          commits the LLM produced are gone with the directory and retry-push
          would just keep failing the same way. *)
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
(** Pure: classify a
    [git push --porcelain --force-with-lease --force-if-includes] invocation
    into a [push_result]. [Push_no_commits] is never returned by this function —
    that variant is only produced by the gate (the push is skipped entirely when
    the branch has no commits ahead of base). *)

val force_push_with_lease :
  ?timeout_seconds:float ->
  clock:float Eio.Time.clock_ty Eio.Time.clock ->
  process_mgr:_ Eio.Process.mgr ->
  path:string ->
  branch:Types.Branch.t ->
  base:Types.Branch.t ->
  unit ->
  push_result
(** Force-push with lease the given branch from the worktree at [path], bounded
    by [timeout_seconds] (120 seconds by default). A deadline expiry cancels the
    git process and returns [Push_error], allowing the runner to complete the
    current operation and retry instead of remaining busy indefinitely. Thin
    effectful orchestrator: runs [git rev-list --count base..HEAD], applies
    [push_gate_from_count] to decide whether to push, and classifies the push
    output via [classify_push_result]. See [push_gate_from_count] and
    [classify_push_result] for the pure decision logic. *)

val rebase_in_progress : process_mgr:_ Eio.Process.mgr -> path:string -> bool
(** Returns [true] if there is a rebase currently in progress in the worktree at
    [path] (checks for [rebase-merge] or [rebase-apply] in the gitdir). *)

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

  val remove : t -> unit
  val detect_branch : path:string -> Types.Branch.t
  val list_with_branches : unit -> (string * Types.Branch.t) list
  val find_for_branch : Types.Branch.t -> string option
  val prune_admin : unit -> unit

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
  val conflict_diff : path:string -> string

  val rebase_onto :
    path:string ->
    target:Types.Branch.t ->
    upstream:string ->
    project_name:string ->
    ancestor_ids:Types.Patch_id.t list ->
    unit ->
    rebase_result
  (** [upstream] is the resolved [<upstream>] argument for
      [git rebase --onto <target> <upstream> HEAD]. It is computed by the caller
      from {!Rebase_decision.plan} (which consults the agent's
      {!Anchor_history.t}). If [upstream] equals [target]'s rendered string, the
      2-arg [git rebase <target>] form is used instead. *)

  val read_branch_sha : path:string -> ref_name:string -> string option

  val is_ancestor : path:string -> ancestor:string -> descendant:string -> bool
  (** [is_ancestor ~path ~ancestor ~descendant] returns [true] iff [ancestor] is
      an ancestor of [descendant] in the repo at [path], via
      [git merge-base --is-ancestor]. Returns [false] on any error (including
      unresolvable SHAs), so callers can treat it as a total
      pure-from-the-outside oracle. *)

  val read_in_progress_conflict_info :
    path:string ->
    target:Types.Branch.t ->
    project_name:string ->
    ancestor_ids:Types.Patch_id.t list ->
    conflict_info option

  val force_push_with_lease :
    path:string -> branch:Types.Branch.t -> base:Types.Branch.t -> push_result

  val rebase_in_progress : path:string -> bool
end

type client = (module S)

val make :
  clock:float Eio.Time.clock_ty Eio.Time.clock ->
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  client

val find_for_branch :
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  Types.Branch.t ->
  string option
(** Search existing git worktrees for one checked out at [branch]. Returns
    [None] if no matching worktree is found or if listing fails. The result
    reflects what [git worktree list] reports, which can include stale entries
    whose directories were deleted on disk; callers that need disk truth should
    additionally check [Sys.file_exists] or call {!prune_admin} first. *)

val prune_admin : process_mgr:_ Eio.Process.mgr -> repo_root:string -> unit
(** Run [git worktree prune] in [repo_root] to drop registry entries whose
    directories were deleted on disk. Idempotent; silent on failure. Callers use
    this to bring git's worktree bookkeeping in sync with disk state before
    {!find_for_branch} or {!create}. *)

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

val exists : t -> bool
val path : t -> string
val patch_id : t -> Types.Patch_id.t
val branch : t -> Types.Branch.t

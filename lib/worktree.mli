open Base

type t = private {
  patch_id : Types.Patch_id.t;
  branch : Types.Branch.t;
  path : string;
}
[@@deriving show, eq, sexp_of, compare]

val worktree_dir : project_name:string -> patch_id:Types.Patch_id.t -> string

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
  t

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

type unique_commit = { sha : string; subject : string }
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

type rebase_strategy = Onto | Plain [@@deriving show, eq, sexp_of, compare]

type conflict_info = {
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
    the full set of commits that belong to this patch (oldest-last). When
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
    [git log --format=%H %s <upstream>..<orig-head>] output. Returns [None] when
    [onto_contents] or [upstream_contents] is blank or the log produces zero
    kept commits. [onto_contents] is the rebase destination SHA;
    [upstream_contents] is the old-base SHA used as the recovery [old_base]. *)

type rebase_result = Ok | Noop | Conflict of conflict_info | Error of string
[@@deriving show, eq, sexp_of, compare]

val rebase_onto :
  process_mgr:_ Eio.Process.mgr ->
  path:string ->
  target:Types.Branch.t ->
  project_name:string ->
  ancestor_ids:Types.Patch_id.t list ->
  rebase_result
(** Rebase HEAD onto [target]. Uses [git log --cherry-pick] to detect
    already-applied commits, and supplements that with subject-pattern matching
    against [[<project_name>] Patch N:] for every [N] in [ancestor_ids] — the
    fallback is load-bearing when [target] is a squash-merging trunk like
    [main], because squash-merged ancestor commits carry a fresh patch-id that
    cherry-pick cannot equate with the original feature-branch commits. Pass
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
    previous run left state behind). Reads [.git/rebase-merge/{onto,orig-head}]
    and runs [git log onto..orig-head] to enumerate the patch's unique commits,
    then delegates to [parse_rebase_merge_state]. Returns [None] best-effort:
    any read or git failure degrades to a no-recovery-section prompt rather than
    blocking delivery. Only handles [.git/rebase-merge] rebases; a
    [.git/rebase-apply] state returns [None]. *)

val parse_push_porcelain : string -> char option
(** Pure: extract the status flag character from [git push --porcelain] stdout.
    Returns [Some '!'] for rejected, [Some '+'] for forced update, etc. Returns
    [None] if no status line is found. *)

type push_result =
  | Push_ok
  | Push_up_to_date
  | Push_no_commits
  | Push_rejected
  | Push_error of string
[@@deriving show, eq, sexp_of, compare]

type push_gate = Proceed | Skip_no_commits [@@deriving show, eq, sexp_of]

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
(** Pure: classify a [git push --porcelain --force-with-lease] invocation into a
    [push_result]. [Push_no_commits] is never returned by this function — that
    variant is only produced by the gate (the push is skipped entirely when the
    branch has no commits ahead of base). *)

val force_push_with_lease :
  process_mgr:_ Eio.Process.mgr ->
  path:string ->
  branch:Types.Branch.t ->
  base:Types.Branch.t ->
  push_result
(** Force-push with lease the given branch from the worktree at [path]. Thin
    effectful orchestrator: runs [git rev-list --count base..HEAD], applies
    [push_gate_from_count] to decide whether to push, and classifies the push
    output via [classify_push_result]. See [push_gate_from_count] and
    [classify_push_result] for the pure decision logic. *)

val rebase_in_progress : process_mgr:_ Eio.Process.mgr -> path:string -> bool
(** Returns [true] if there is a rebase currently in progress in the worktree at
    [path] (checks for [rebase-merge] or [rebase-apply] in the gitdir). *)

val find_for_branch :
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  Types.Branch.t ->
  string option
(** Search existing git worktrees for one checked out at [branch]. Returns
    [None] if no matching worktree is found or if listing fails. *)

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

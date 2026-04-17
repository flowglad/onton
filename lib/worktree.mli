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

val oldest_non_ancestor_commit :
  project_name:string ->
  ancestor_ids:Types.Patch_id.t list ->
  string ->
  (string, string) Result.t
(** Pure: parse
    [git log --cherry-pick --right-only --no-merges --no-show-signature
     --format=%H %s] output and return the oldest SHA whose subject is not
    [is_ancestor_patch_subject]. Returns [Error] when the filtered list is
    empty. [--no-merges] keeps merge-commit lines out of the input; callers
    emitting raw [git log] should also pass [--no-show-signature] so GPG
    annotation lines aren't mistaken for SHAs. *)

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

type rebase_result = Ok | Noop | Conflict | Error of string
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
    entirely; cherry-pick deduplication still applies. *)

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

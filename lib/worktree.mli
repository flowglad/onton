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

val oldest_unique_commit : string -> (string, string) Result.t
(** Pure: extract the oldest unique commit SHA from
    [git rev-list --cherry-pick --right-only] output (newest-first). Returns
    [Error] when the output is empty (all commits already in target). *)

type rebase_result = Ok | Noop | Conflict | Error of string
[@@deriving show, eq, sexp_of, compare]

val rebase_onto :
  process_mgr:_ Eio.Process.mgr ->
  path:string ->
  target:Types.Branch.t ->
  rebase_result

val parse_push_porcelain : string -> char option
(** Pure: extract the status flag character from [git push --porcelain] stdout.
    Returns [Some '!'] for rejected, [Some '+'] for forced update, etc. Returns
    [None] if no status line is found. *)

type push_result =
  | Push_ok
  | Push_up_to_date
  | Push_rejected
  | Push_error of string
[@@deriving show, eq, sexp_of, compare]

val force_push_with_lease :
  process_mgr:_ Eio.Process.mgr ->
  path:string ->
  branch:Types.Branch.t ->
  push_result
(** Force-push with lease the given branch from the worktree at [path]. Returns
    [Push_ok] on success, [Push_up_to_date] when the remote already has the same
    SHA (nothing changed), [Push_rejected] if the lease check fails (remote was
    updated by someone else), or [Push_error] on other failures. *)

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

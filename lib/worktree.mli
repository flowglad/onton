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
    (resolved via the git common dir, not necessarily [repo_root] itself).
    A worktree cannot be created for a branch that is checked out there. *)

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

val find_for_branch :
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  Types.Branch.t ->
  string option
(** Search existing git worktrees for one checked out at [branch]. Returns
    [None] if no matching worktree is found or if listing fails. *)

val normalize_path : string -> string
(** Resolve a relative path to absolute using the current working directory. *)

val exists : t -> bool
val path : t -> string
val patch_id : t -> Types.Patch_id.t
val branch : t -> Types.Branch.t

open Base

type t = private {
  patch_id : Types.Patch_id.t;
  branch : Types.Branch.t;
  path : string;
}
[@@deriving show, eq, sexp_of, compare]

val worktree_dir : repo_root:string -> patch_id:Types.Patch_id.t -> string

val create :
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  patch:Types.Patch.t ->
  base_ref:string ->
  t

val remove : process_mgr:_ Eio.Process.mgr -> repo_root:string -> t -> unit

val detect_branch :
  process_mgr:_ Eio.Process.mgr -> path:string -> Types.Branch.t

val list_with_branches :
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  (string * Types.Branch.t) list

type rebase_result = Ok | Noop | Conflict | Error of string
[@@deriving show, eq, sexp_of, compare]

val rebase_onto :
  process_mgr:_ Eio.Process.mgr ->
  path:string ->
  target:Types.Branch.t ->
  rebase_result

val normalize_path : string -> string
(** Resolve a relative path to absolute using the current working directory. *)

val exists : t -> bool
val path : t -> string
val patch_id : t -> Types.Patch_id.t
val branch : t -> Types.Branch.t

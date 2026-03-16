open Base

type t = private {
  patch_id : Types.Patch_id.t;
  branch : Types.Branch.t;
  path : string;
}
[@@deriving show, eq, sexp_of, compare]

val worktree_dir : repo_root:string -> patch_id:Types.Patch_id.t -> string

val create :
  process_mgr:_ Eio.Process.mgr -> repo_root:string -> patch:Types.Patch.t -> t

val remove : process_mgr:_ Eio.Process.mgr -> repo_root:string -> t -> unit

val add_existing :
  patch_id:Types.Patch_id.t -> branch:Types.Branch.t -> path:string -> t

val detect_branch :
  process_mgr:_ Eio.Process.mgr -> path:string -> Types.Branch.t

val list_with_branches :
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  (string * Types.Branch.t option) list

val exists : t -> bool
val path : t -> string
val patch_id : t -> Types.Patch_id.t
val branch : t -> Types.Branch.t

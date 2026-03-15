open Base

type t = { patch_id : Types.Patch_id.t; branch : Types.Branch.t; path : string }
[@@deriving show, eq, sexp_of, compare]

let normalize_path path =
  if Stdlib.Filename.is_relative path then
    Stdlib.Filename.concat (Stdlib.Sys.getcwd ()) path
  else path

let worktree_dir ~repo_root ~patch_id =
  let repo_root = normalize_path repo_root in
  let id_str = Int.to_string (Types.Patch_id.to_int patch_id) in
  Stdlib.Filename.concat repo_root ("worktrees/patch-" ^ id_str)

let create ~process_mgr ~repo_root ~patch =
  let open Types in
  let path = worktree_dir ~repo_root ~patch_id:patch.Patch.id in
  let branch_str = Branch.to_string patch.Patch.branch in
  Eio.Process.run process_mgr
    [
      "git"; "-C"; repo_root; "worktree"; "add"; "-b"; branch_str; path; "HEAD";
    ];
  { patch_id = patch.Patch.id; branch = patch.Patch.branch; path }

let remove ~process_mgr ~repo_root t =
  Eio.Process.run process_mgr
    [ "git"; "-C"; repo_root; "worktree"; "remove"; "--force"; t.path ]

let exists t = Stdlib.Sys.file_exists t.path
let path t = t.path
let patch_id t = t.patch_id
let branch t = t.branch

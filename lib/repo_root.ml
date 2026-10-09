(* @archlint.module shell
   @archlint.domain worktree-parser *)

open Base

(** Run [git -C path rev-parse --path-format=absolute --git-common-dir] and
    return the parent directory of the reported common dir — i.e. the main
    working tree. Returns [None] if [path] is not inside a git repository or the
    command fails for any reason. *)
let resolve_main_working_tree path =
  match
    Process_tree.run_sync ~env:(Git_env.clean_env ())
      [
        "git";
        "-C";
        path;
        "rev-parse";
        "--path-format=absolute";
        "--git-common-dir";
      ]
  with
  | Unix.WEXITED 0, stdout, _ ->
      let common_dir = String.strip stdout in
      if String.is_empty common_dir then None
      else
        let parent = Stdlib.Filename.dirname common_dir in
        (* Older Git versions can ignore --path-format; never resolve their
           relative answer against the wrong checkout. *)
        if Stdlib.Filename.is_relative parent then None else Some parent
  | (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _), _, _ -> None
  | exception _ -> None

let normalize rr =
  let absolute =
    if Stdlib.Filename.is_relative rr then
      Stdlib.Filename.concat (Stdlib.Sys.getcwd ()) rr
    else rr
  in
  let normalize_path path =
    Worktree_parser.normalize_path ~cwd:(Stdlib.Sys.getcwd ()) path
  in
  let normalized = normalize_path absolute in
  match resolve_main_working_tree normalized with
  | Some main -> normalize_path main
  | None -> normalized

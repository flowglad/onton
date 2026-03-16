open Base

type t = { patch_id : Types.Patch_id.t; branch : Types.Branch.t; path : string }
[@@deriving show, eq, sexp_of, compare]

let normalize_path path =
  if Stdlib.Filename.is_relative path then
    Stdlib.Filename.concat (Stdlib.Sys.getcwd ()) path
  else path

let worktree_dir ~repo_root ~patch_id =
  let repo_root = normalize_path repo_root in
  let id_str = Types.Patch_id.to_string patch_id in
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

let add_existing ~patch_id ~branch ~path =
  let path = normalize_path path in
  (match Stdlib.Sys.file_exists path with
  | false -> failwith ("Worktree path does not exist: " ^ path)
  | true ->
      if not (Stdlib.Sys.is_directory path) then
        failwith ("Worktree path is not a directory: " ^ path));
  { patch_id; branch; path }

let detect_branch ~process_mgr ~path =
  let buf = Buffer.create 128 in
  let path = normalize_path path in
  Eio.Process.run process_mgr ~stdout:(Eio.Flow.buffer_sink buf)
    [ "git"; "-C"; path; "rev-parse"; "--abbrev-ref"; "HEAD" ];
  let raw = Buffer.contents buf in
  let branch_str = String.strip raw in
  if String.equal branch_str "HEAD" then
    failwith ("Worktree at " ^ path ^ " has detached HEAD; cannot detect branch");
  Types.Branch.of_string branch_str

let list_with_branches ~process_mgr ~repo_root =
  let buf = Buffer.create 512 in
  Eio.Process.run process_mgr ~stdout:(Eio.Flow.buffer_sink buf)
    [ "git"; "-C"; repo_root; "worktree"; "list"; "--porcelain" ];
  let raw = Buffer.contents buf in
  let lines = String.split_lines raw in
  let rec parse acc current_path current_branch = function
    | [] ->
        let acc =
          match current_path with
          | Some p -> (p, current_branch) :: acc
          | None -> acc
        in
        List.rev acc
    | line :: rest -> (
        match String.lsplit2 line ~on:' ' with
        | Some ("worktree", p) ->
            (* Flush any pending entry that wasn't terminated by a blank line *)
            let acc =
              match current_path with
              | Some prev_p -> (prev_p, current_branch) :: acc
              | None -> acc
            in
            parse acc (Some p) None rest
        | Some ("branch", b) ->
            let branch_name =
              match String.chop_prefix b ~prefix:"refs/heads/" with
              | Some short -> short
              | None -> b
            in
            parse acc current_path
              (Some (Types.Branch.of_string branch_name))
              rest
        | _ ->
            if String.is_empty line then
              let acc =
                match current_path with
                | Some p -> (p, current_branch) :: acc
                | None -> acc
              in
              parse acc None None rest
            else parse acc current_path current_branch rest)
  in
  parse [] None None lines

let exists t = Stdlib.Sys.file_exists t.path
let path t = t.path
let patch_id t = t.patch_id
let branch t = t.branch

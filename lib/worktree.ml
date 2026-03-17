open Base

type t = { patch_id : Types.Patch_id.t; branch : Types.Branch.t; path : string }
[@@deriving show, eq, sexp_of, compare]

let normalize_path path =
  let p =
    if Stdlib.Filename.is_relative path then
      Stdlib.Filename.concat (Stdlib.Sys.getcwd ()) path
    else path
  in
  if String.length p > 1 && String.is_suffix p ~suffix:"/" then
    let stripped = String.rstrip p ~drop:(Char.equal '/') in
    if String.is_empty stripped then p else stripped
  else p

let worktree_dir ~project_name ~patch_id =
  let home =
    match Stdlib.Sys.getenv_opt "HOME" with Some h -> h | None -> "."
  in
  let id_str = Types.Patch_id.to_string patch_id in
  Stdlib.Filename.concat
    (Stdlib.Filename.concat home ("worktrees/" ^ project_name))
    ("patch-" ^ id_str)

let rec has_cancellation = function
  | Eio.Cancel.Cancelled _ -> true
  | Eio.Exn.Multiple exns ->
      List.exists exns ~f:(fun (exn, _bt) -> has_cancellation exn)
  | _ -> false

let branch_exists ~process_mgr ~repo_root branch_str =
  let buf = Buffer.create 16 in
  match
    Eio.Process.run process_mgr ~stdout:(Eio.Flow.buffer_sink buf)
      ~stderr:(Eio.Flow.buffer_sink (Buffer.create 16))
      [
        "git";
        "-C";
        repo_root;
        "rev-parse";
        "--verify";
        "refs/heads/" ^ branch_str;
      ]
  with
  | () -> true
  | exception e when has_cancellation e -> raise e
  | exception _ -> false

let create ~process_mgr ~repo_root ~project_name ~patch ~base_ref =
  let open Types in
  let path = worktree_dir ~project_name ~patch_id:patch.Patch.id in
  let branch_str = Branch.to_string patch.Patch.branch in
  if Stdlib.Sys.file_exists path then
    { patch_id = patch.Patch.id; branch = patch.Patch.branch; path }
  else if branch_exists ~process_mgr ~repo_root branch_str then (
    Eio.Process.run process_mgr
      [ "git"; "-C"; repo_root; "worktree"; "add"; path; branch_str ];
    { patch_id = patch.Patch.id; branch = patch.Patch.branch; path })
  else (
    Eio.Process.run process_mgr
      [
        "git";
        "-C";
        repo_root;
        "worktree";
        "add";
        "-b";
        branch_str;
        path;
        base_ref;
      ];
    { patch_id = patch.Patch.id; branch = patch.Patch.branch; path })

let remove ~process_mgr ~repo_root t =
  Eio.Process.run process_mgr
    [ "git"; "-C"; repo_root; "worktree"; "remove"; "--force"; t.path ]

let detect_branch ~process_mgr ~path =
  let buf = Buffer.create 128 in
  let path = normalize_path path in
  let stderr_buf = Buffer.create 64 in
  (match
     Eio.Process.run process_mgr ~stdout:(Eio.Flow.buffer_sink buf)
       ~stderr:(Eio.Flow.buffer_sink stderr_buf)
       [ "git"; "-C"; path; "rev-parse"; "--abbrev-ref"; "HEAD" ]
   with
  | () -> ()
  | exception e when has_cancellation e -> raise e
  | exception exn ->
      let msg = Buffer.contents stderr_buf in
      failwith
        (Printf.sprintf "detect_branch failed at %s: %s\ngit stderr: %s" path
           (Exn.to_string exn) msg));
  let raw = Buffer.contents buf in
  let branch_str = String.strip raw in
  if String.is_empty branch_str then
    failwith ("detect_branch: git rev-parse returned empty output at " ^ path);
  if String.equal branch_str "HEAD" then
    failwith ("Worktree at " ^ path ^ " has detached HEAD; cannot detect branch");
  Types.Branch.of_string branch_str

let list_with_branches ~process_mgr ~repo_root =
  let buf = Buffer.create 512 in
  let stderr_buf = Buffer.create 64 in
  (match
     Eio.Process.run process_mgr ~stdout:(Eio.Flow.buffer_sink buf)
       ~stderr:(Eio.Flow.buffer_sink stderr_buf)
       [ "git"; "-C"; repo_root; "worktree"; "list"; "--porcelain" ]
   with
  | () -> ()
  | exception e when has_cancellation e -> raise e
  | exception exn ->
      let msg = Buffer.contents stderr_buf in
      failwith
        (Printf.sprintf "list_with_branches failed at %s: %s\ngit stderr: %s"
           repo_root (Exn.to_string exn) msg));
  let raw = Buffer.contents buf in
  let lines = String.split_lines raw in
  let repo_root = normalize_path repo_root in
  let flush_entry acc p branch =
    let p = normalize_path p in
    match branch with
    | None -> acc (* skip detached-HEAD worktrees *)
    | Some b -> if String.( <> ) p repo_root then (p, b) :: acc else acc
  in
  let rec parse acc current_path current_branch = function
    | [] ->
        let acc =
          match current_path with
          | Some p -> flush_entry acc p current_branch
          | None -> acc
        in
        List.rev acc
    | line :: rest -> (
        match () with
        | () when String.is_prefix line ~prefix:"worktree " ->
            let p = String.drop_prefix line (String.length "worktree ") in
            (* Flush any pending entry that wasn't terminated by a blank line *)
            let acc =
              match current_path with
              | Some prev_p -> flush_entry acc prev_p current_branch
              | None -> acc
            in
            parse acc (Some p) None rest
        | () when String.is_prefix line ~prefix:"branch " ->
            let b = String.drop_prefix line (String.length "branch ") in
            let branch =
              match String.chop_prefix b ~prefix:"refs/heads/" with
              | Some short when not (String.is_empty short) ->
                  Some (Types.Branch.of_string short)
              | _ -> None (* detached, non-local ref, or empty name *)
            in
            parse acc current_path branch rest
        | () ->
            if String.is_empty line then
              let acc =
                match current_path with
                | Some p -> flush_entry acc p current_branch
                | None -> acc
              in
              parse acc None None rest
            else parse acc current_path current_branch rest)
  in
  parse [] None None lines

type rebase_result = Ok | Noop | Conflict | Error of string
[@@deriving show, eq, sexp_of, compare]

let run_git_exit_code ~process_mgr args =
  Eio.Switch.run @@ fun sw ->
  let stdout_buf = Buffer.create 0 in
  let stderr_buf = Buffer.create 64 in
  let child =
    Eio.Process.spawn ~sw process_mgr
      ~stdout:(Eio.Flow.buffer_sink stdout_buf)
      ~stderr:(Eio.Flow.buffer_sink stderr_buf)
      args
  in
  let code =
    match Eio.Process.await child with `Exited c -> c | `Signaled s -> 128 + s
  in
  (code, Buffer.contents stderr_buf)

let rebase_onto ~process_mgr ~path ~target =
  let target = Types.Branch.to_string target in
  let ancestor_code, ancestor_stderr =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "merge-base"; "--is-ancestor"; target; "HEAD" ]
  in
  if ancestor_code = 0 then Noop
  else if ancestor_code <> 1 then
    Error
      (Printf.sprintf
         "merge-base --is-ancestor failed for target %s (exit %d): %s" target
         ancestor_code
         (String.strip ancestor_stderr))
  else
    let rebase_code, rebase_stderr =
      run_git_exit_code ~process_mgr [ "git"; "-C"; path; "rebase"; target ]
    in
    if rebase_code = 0 then Ok
    else if rebase_code <> 1 then
      Error
        (Printf.sprintf "rebase failed (exit %d): %s" rebase_code
           (String.strip rebase_stderr))
    else begin
      let abort_code, abort_stderr =
        run_git_exit_code ~process_mgr
          [ "git"; "-C"; path; "rebase"; "--abort" ]
      in
      if abort_code <> 0 then
        Error
          (Printf.sprintf "rebase conflict but abort also failed (exit %d): %s"
             abort_code
             (String.strip abort_stderr))
      else Conflict
    end

let exists t = Stdlib.Sys.file_exists t.path
let path t = t.path
let patch_id t = t.patch_id
let branch t = t.branch

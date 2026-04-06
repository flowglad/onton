open Base

type t = { patch_id : Types.Patch_id.t; branch : Types.Branch.t; path : string }
[@@deriving show, eq, sexp_of, compare]

let normalize_path path =
  let p =
    if Stdlib.Filename.is_relative path then
      Stdlib.Filename.concat (Stdlib.Sys.getcwd ()) path
    else path
  in
  let p =
    if String.length p > 1 && String.is_suffix p ~suffix:"/" then
      let stripped = String.rstrip p ~drop:(Char.equal '/') in
      if String.is_empty stripped then p else stripped
    else p
  in
  (* Strip trailing "/." segments so "foo/." compares equal to "foo" *)
  let rec strip_dot_suffix s =
    if String.length s > 2 && String.is_suffix s ~suffix:"/." then
      strip_dot_suffix (String.chop_suffix_exn s ~suffix:"/.")
    else s
  in
  strip_dot_suffix p

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

let clean_git_env () =
  Unix.environment () |> Array.to_list
  |> List.filter ~f:(fun s ->
      (not (String.is_prefix s ~prefix:"GIT_DIR="))
      && (not (String.is_prefix s ~prefix:"GIT_WORK_TREE="))
      && not (String.is_prefix s ~prefix:"GIT_INDEX_FILE="))
  |> Array.of_list

let ref_exists ~process_mgr ~repo_root ref_path =
  let buf = Buffer.create 16 in
  match
    Eio.Process.run process_mgr ~env:(clean_git_env ())
      ~stdout:(Eio.Flow.buffer_sink buf)
      ~stderr:(Eio.Flow.buffer_sink (Buffer.create 16))
      [ "git"; "-C"; repo_root; "rev-parse"; "--verify"; ref_path ]
  with
  | () -> true
  | exception e when has_cancellation e -> raise e
  | exception _ -> false

let branch_exists ~process_mgr ~repo_root branch_str =
  ref_exists ~process_mgr ~repo_root ("refs/heads/" ^ branch_str)

let remote_branch_exists ~process_mgr ~repo_root branch_str =
  ref_exists ~process_mgr ~repo_root ("refs/remotes/origin/" ^ branch_str)

let resolve_main_root ~process_mgr ~repo_root =
  let buf = Buffer.create 128 in
  let stderr_buf = Buffer.create 64 in
  match
    Eio.Process.run process_mgr ~env:(clean_git_env ())
      ~stdout:(Eio.Flow.buffer_sink buf)
      ~stderr:(Eio.Flow.buffer_sink stderr_buf)
      [
        "git";
        "-C";
        repo_root;
        "rev-parse";
        "--path-format=absolute";
        "--git-common-dir";
      ]
  with
  | () ->
      let common_git_dir = String.strip (Buffer.contents buf) in
      (* The common git dir is the .git directory of the main working tree.
         Its parent is the main working tree root. *)
      Stdlib.Filename.dirname common_git_dir
  | exception e when has_cancellation e -> raise e
  | exception _ -> repo_root

let is_checked_out_in_repo_root ~process_mgr ~repo_root branch =
  let main_root = resolve_main_root ~process_mgr ~repo_root in
  let buf = Buffer.create 128 in
  let stderr_buf = Buffer.create 64 in
  match
    Eio.Process.run process_mgr ~env:(clean_git_env ())
      ~stdout:(Eio.Flow.buffer_sink buf)
      ~stderr:(Eio.Flow.buffer_sink stderr_buf)
      [ "git"; "-C"; main_root; "rev-parse"; "--abbrev-ref"; "HEAD" ]
  with
  | () ->
      let current = String.strip (Buffer.contents buf) in
      String.equal current (Types.Branch.to_string branch)
  | exception e when has_cancellation e -> raise e
  | exception _ -> false

let create ~process_mgr ~repo_root ~project_name ~patch_id ~branch ~base_ref =
  let path = worktree_dir ~project_name ~patch_id in
  let branch_str = Types.Branch.to_string branch in
  if Stdlib.Sys.file_exists path then { patch_id; branch; path }
  else if branch_exists ~process_mgr ~repo_root branch_str then (
    Eio.Process.run process_mgr ~env:(clean_git_env ())
      [ "git"; "-C"; repo_root; "worktree"; "add"; path; branch_str ];
    { patch_id; branch; path })
  else if remote_branch_exists ~process_mgr ~repo_root branch_str then (
    (* Branch exists on remote but not locally — create local branch from remote *)
    Eio.Process.run process_mgr ~env:(clean_git_env ())
      [
        "git";
        "-C";
        repo_root;
        "worktree";
        "add";
        "-b";
        branch_str;
        path;
        "origin/" ^ branch_str;
      ];
    { patch_id; branch; path })
  else (
    Eio.Process.run process_mgr ~env:(clean_git_env ())
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
    { patch_id; branch; path })

let remove ~process_mgr ~repo_root t =
  Eio.Process.run process_mgr ~env:(clean_git_env ())
    [ "git"; "-C"; repo_root; "worktree"; "remove"; "--force"; t.path ]

let detect_branch ~process_mgr ~path =
  let buf = Buffer.create 128 in
  let path = normalize_path path in
  let stderr_buf = Buffer.create 64 in
  (match
     Eio.Process.run process_mgr ~env:(clean_git_env ())
       ~stdout:(Eio.Flow.buffer_sink buf)
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

let parse_porcelain ~repo_root raw =
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

let list_with_branches ~process_mgr ~repo_root =
  let main_root = resolve_main_root ~process_mgr ~repo_root in
  let buf = Buffer.create 512 in
  let stderr_buf = Buffer.create 64 in
  (match
     Eio.Process.run process_mgr ~env:(clean_git_env ())
       ~stdout:(Eio.Flow.buffer_sink buf)
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
  parse_porcelain ~repo_root:main_root (Buffer.contents buf)

type rebase_result = Ok | Noop | Conflict | Error of string
[@@deriving show, eq, sexp_of, compare]

let run_git_exit_code ~process_mgr args =
  Eio.Switch.run @@ fun sw ->
  let stdout_buf = Buffer.create 0 in
  let stderr_buf = Buffer.create 64 in
  let env = clean_git_env () in
  let child =
    Eio.Process.spawn ~sw process_mgr ~env
      ~stdout:(Eio.Flow.buffer_sink stdout_buf)
      ~stderr:(Eio.Flow.buffer_sink stderr_buf)
      args
  in
  let code =
    match Eio.Process.await child with `Exited c -> c | `Signaled s -> 128 + s
  in
  (code, Buffer.contents stdout_buf, Buffer.contents stderr_buf)

(** Pure: extract the oldest unique commit SHA from [rev-list --cherry-pick]
    output. Returns [Error] when the output is empty (all commits already in
    target). The output is newest-first, so the last line is the oldest. *)
let oldest_unique_commit rev_list_output =
  let trimmed = String.strip rev_list_output in
  if String.is_empty trimmed then Result.Error "no unique commits found"
  else
    let lines = String.split_lines trimmed in
    Result.Ok (List.last_exn lines)

(** Find the old base commit for [--onto] rebase by identifying which commits on
    our branch are unique (not in target by patch-id). Returns the parent of the
    oldest unique commit — i.e., the last dependency commit in our branch's
    history. *)
let find_old_base ~process_mgr ~path ~target =
  let code, stdout, stderr =
    run_git_exit_code ~process_mgr
      [
        "git";
        "-C";
        path;
        "rev-list";
        "--cherry-pick";
        "--right-only";
        "--no-merges";
        Printf.sprintf "%s...HEAD" target;
      ]
  in
  if code <> 0 then
    Result.Error
      (Printf.sprintf "rev-list cherry-pick failed (exit %d): %s" code
         (String.strip stderr))
  else
    match oldest_unique_commit stdout with
    | Result.Error _ as e -> e
    | Result.Ok oldest_sha ->
        let code, stdout, stderr =
          run_git_exit_code ~process_mgr
            [ "git"; "-C"; path; "rev-parse"; Printf.sprintf "%s~1" oldest_sha ]
        in
        if code <> 0 then
          Result.Error
            (Printf.sprintf "rev-parse oldest~1 failed (exit %d): %s" code
               (String.strip stderr))
        else Result.Ok (String.strip stdout)

let rebase_onto ~process_mgr ~path ~target =
  let target = Types.Branch.to_string target in
  let ancestor_code, _, ancestor_stderr =
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
    match find_old_base ~process_mgr ~path ~target with
    | Result.Error msg ->
        (* If we can't find unique commits, fall back to plain rebase *)
        let rebase_code, _, rebase_stderr =
          run_git_exit_code ~process_mgr [ "git"; "-C"; path; "rebase"; target ]
        in
        if rebase_code = 0 then Ok
        else if rebase_code <> 1 then
          Error
            (Printf.sprintf "rebase failed (fallback, %s) (exit %d): %s" msg
               rebase_code
               (String.strip rebase_stderr))
        else
          (* Leave rebase in progress for agent to resolve *)
          Conflict
    | Result.Ok old_base ->
        let rebase_code, _, rebase_stderr =
          run_git_exit_code ~process_mgr
            [ "git"; "-C"; path; "rebase"; "--onto"; target; old_base ]
        in
        if rebase_code = 0 then Ok
        else if rebase_code <> 1 then
          Error
            (Printf.sprintf "rebase --onto failed (exit %d): %s" rebase_code
               (String.strip rebase_stderr))
        else
          (* Leave rebase in progress for agent to resolve *)
          Conflict

type push_result = Push_ok | Push_rejected | Push_error of string
[@@deriving show, eq, sexp_of, compare]

let force_push_with_lease ~process_mgr ~path ~branch =
  let branch_str = Types.Branch.to_string branch in
  let code, _stdout, stderr =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "push"; "--force-with-lease"; "origin"; branch_str ]
  in
  if code = 0 then Push_ok
  else
    let msg = String.strip stderr in
    if
      String.is_substring msg ~substring:"stale info"
      || String.is_substring msg ~substring:"failed to push"
      || String.is_substring msg ~substring:"rejected"
    then Push_rejected
    else Push_error (Printf.sprintf "push failed (exit %d): %s" code msg)

let rebase_in_progress ~process_mgr ~path =
  let code, stdout, _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "rev-parse"; "--git-dir" ]
  in
  if code <> 0 then false
  else
    let git_dir = String.strip stdout in
    let git_dir =
      if Stdlib.Filename.is_relative git_dir then
        Stdlib.Filename.concat path git_dir
      else git_dir
    in
    Stdlib.Sys.file_exists (Stdlib.Filename.concat git_dir "rebase-merge")
    || Stdlib.Sys.file_exists (Stdlib.Filename.concat git_dir "rebase-apply")

let find_for_branch ~process_mgr ~repo_root branch =
  let pairs = try list_with_branches ~process_mgr ~repo_root with _ -> [] in
  List.find_map pairs ~f:(fun (path, b) ->
      if Types.Branch.equal b branch then Some path else None)

let exists t = Stdlib.Sys.file_exists t.path
let path t = t.path
let patch_id t = t.patch_id
let branch t = t.branch

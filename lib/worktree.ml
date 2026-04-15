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

let clean_git_env =
  Stdlib.Lazy.from_fun (fun () ->
      Unix.environment () |> Array.to_list
      |> List.filter ~f:(fun s ->
          (not (String.is_prefix s ~prefix:"GIT_DIR="))
          && (not (String.is_prefix s ~prefix:"GIT_WORK_TREE="))
          && not (String.is_prefix s ~prefix:"GIT_INDEX_FILE="))
      |> Array.of_list)

let ref_exists ~process_mgr ~repo_root ref_path =
  let buf = Buffer.create 16 in
  match
    Eio.Process.run process_mgr
      ~env:(Stdlib.Lazy.force clean_git_env)
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
    Eio.Process.run process_mgr
      ~env:(Stdlib.Lazy.force clean_git_env)
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
    Eio.Process.run process_mgr
      ~env:(Stdlib.Lazy.force clean_git_env)
      ~stdout:(Eio.Flow.buffer_sink buf)
      ~stderr:(Eio.Flow.buffer_sink stderr_buf)
      [ "git"; "-C"; main_root; "rev-parse"; "--abbrev-ref"; "HEAD" ]
  with
  | () ->
      let current = String.strip (Buffer.contents buf) in
      String.equal current (Types.Branch.to_string branch)
  | exception e when has_cancellation e -> raise e
  | exception _ -> false

let run_git ~process_mgr args =
  let stderr_buf = Buffer.create 128 in
  match
    Eio.Process.run process_mgr
      ~env:(Stdlib.Lazy.force clean_git_env)
      ~stdout:(Eio.Flow.buffer_sink (Buffer.create 64))
      ~stderr:(Eio.Flow.buffer_sink stderr_buf)
      args
  with
  | () -> ()
  | exception e when has_cancellation e -> raise e
  | exception _ ->
      let stderr = String.strip (Buffer.contents stderr_buf) in
      let cmd = String.concat ~sep:" " args in
      failwith (Printf.sprintf "git command failed: %s\nstderr: %s" cmd stderr)

let add_worktree_for_existing_branch ~process_mgr ~repo_root ~path ~branch_str =
  run_git ~process_mgr
    [ "git"; "-C"; repo_root; "worktree"; "add"; path; branch_str ]

(** Collect all path prefixes of a branch name. For ["a/b/c"] returns
    [["a"; "a/b"]]. Used to detect case-insensitive ref collisions on macOS: a
    branch [Foo] stored as [refs/heads/Foo] blocks creation of [foo/bar] (which
    needs [refs/heads/foo/] as a directory). *)
let branch_prefixes branch_str =
  let parts = String.split branch_str ~on:'/' in
  let rec build acc prefix = function
    | [] | [ _ ] -> List.rev acc
    | seg :: rest ->
        let prefix =
          if String.is_empty prefix then seg else prefix ^ "/" ^ seg
        in
        build (prefix :: acc) prefix rest
  in
  build [] "" parts

(** Pure: find the first existing branch that case-insensitively collides with
    [branch_str] via the file-vs-directory ref storage on macOS. Checks both
    directions: existing branch equals a prefix of the new name (e.g. [Foo] vs
    [foo/bar]) and existing branch has the new name as a prefix (e.g. [Foo/bar]
    vs [foo]). Returns [Some colliding_branch] or [None]. *)
let find_ci_ref_collision ~existing_branches branch_str =
  let branch_lc = String.lowercase branch_str in
  let prefixes = branch_prefixes branch_str in
  match
    List.find_map prefixes ~f:(fun pfx ->
        let lower_pfx = String.lowercase pfx in
        List.find existing_branches ~f:(fun b ->
            String.equal (String.lowercase b) lower_pfx))
  with
  | Some _ as collision -> collision
  | None ->
      List.find existing_branches ~f:(fun b ->
          String.is_prefix (String.lowercase b) ~prefix:(branch_lc ^ "/"))

let check_case_insensitive_ref_collision ~process_mgr ~repo_root branch_str =
  let buf = Buffer.create 512 in
  let existing_branches =
    match
      Eio.Process.run process_mgr
        ~env:(Stdlib.Lazy.force clean_git_env)
        ~stdout:(Eio.Flow.buffer_sink buf)
        ~stderr:(Eio.Flow.buffer_sink (Buffer.create 16))
        [
          "git";
          "-C";
          repo_root;
          "for-each-ref";
          "--format=%(refname:short)";
          "refs/heads/";
        ]
    with
    | () -> String.split_lines (Buffer.contents buf)
    | exception e when has_cancellation e -> raise e
    | exception _ ->
        Eio.traceln
          "warning: git for-each-ref failed; case-insensitive ref collision \
           check skipped for %s"
          branch_str;
        []
  in
  match find_ci_ref_collision ~existing_branches branch_str with
  | Some colliding ->
      failwith
        (Printf.sprintf
           "Cannot create branch %s: existing branch %s conflicts on \
            case-insensitive filesystem (macOS). Delete or rename the \
            conflicting branch with: git branch -D %s"
           branch_str colliding colliding)
  | None -> ()

let create ~process_mgr ~repo_root ~project_name ~patch_id ~branch ~base_ref =
  let path = worktree_dir ~project_name ~patch_id in
  let branch_str = Types.Branch.to_string branch in
  if Stdlib.Sys.file_exists path then { patch_id; branch; path }
  else if branch_exists ~process_mgr ~repo_root branch_str then (
    add_worktree_for_existing_branch ~process_mgr ~repo_root ~path ~branch_str;
    { patch_id; branch; path })
  else
    let start_point =
      if remote_branch_exists ~process_mgr ~repo_root branch_str then
        "origin/" ^ branch_str
      else base_ref
    in
    check_case_insensitive_ref_collision ~process_mgr ~repo_root branch_str;
    (match
       run_git ~process_mgr
         [
           "git";
           "-C";
           repo_root;
           "worktree";
           "add";
           "-b";
           branch_str;
           path;
           start_point;
         ]
     with
    | () -> ()
    | exception e when has_cancellation e -> raise e
    | exception _ when branch_exists ~process_mgr ~repo_root branch_str ->
        (* Branch was created (e.g. by a concurrent attempt) but worktree
           setup failed — retry without -b *)
        add_worktree_for_existing_branch ~process_mgr ~repo_root ~path
          ~branch_str);
    { patch_id; branch; path }

let remove ~process_mgr ~repo_root t =
  Eio.Process.run process_mgr
    ~env:(Stdlib.Lazy.force clean_git_env)
    [ "git"; "-C"; repo_root; "worktree"; "remove"; "--force"; t.path ]

let detect_branch ~process_mgr ~path =
  let buf = Buffer.create 128 in
  let path = normalize_path path in
  let stderr_buf = Buffer.create 64 in
  (match
     Eio.Process.run process_mgr
       ~env:(Stdlib.Lazy.force clean_git_env)
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
     Eio.Process.run process_mgr
       ~env:(Stdlib.Lazy.force clean_git_env)
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
  let env = Stdlib.Lazy.force clean_git_env in
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

(** Pure: classify a [git fetch origin] invocation from its exit code and
    stderr. Split out so the mapping from raw git output to fetch outcome can be
    property-tested independently of the subprocess and mutex. *)
let classify_fetch_result ~code ~stderr =
  if code = 0 then Result.Ok ()
  else
    Result.Error
      (Printf.sprintf "git fetch origin failed (exit %d): %s" code
         (String.strip stderr))

let fetch_origin ~fetch_lock ~process_mgr ~path =
  (* Serialize concurrent fetches across worktrees of the same repo. All
     worktrees share the main repo's ref store, so simultaneous
     [git fetch origin] processes race on the compare-and-swap update of
     [refs/remotes/origin/*], producing
     "cannot lock ref ...: is at X but expected Y" in the loser. The mutex
     eliminates that race by construction. *)
  Eio.Mutex.use_rw ~protect:true fetch_lock (fun () ->
      let code, _stdout, stderr =
        run_git_exit_code ~process_mgr [ "git"; "-C"; path; "fetch"; "origin" ]
      in
      classify_fetch_result ~code ~stderr)

let git_status ~process_mgr ~path =
  let code, stdout, _ =
    run_git_exit_code ~process_mgr [ "git"; "-C"; path; "status" ]
  in
  if code <> 0 then "" else String.strip stdout

let conflict_diff ~process_mgr ~path =
  let code, stdout, _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "diff"; "--diff-filter=U" ]
  in
  if code <> 0 then ""
  else
    let s = String.strip stdout in
    (* Truncate to avoid blowing up the prompt *)
    if String.length s > 4000 then String.prefix s 4000 ^ "\n[truncated]" else s

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

type push_result =
  | Push_ok
  | Push_up_to_date
  | Push_no_commits
  | Push_rejected
  | Push_error of string
[@@deriving show, eq, sexp_of, compare]

(** Parse a single porcelain status line from [git push --porcelain]. Format:
    [<flag>\t<from>:<to>\t<summary>]. Returns the flag character. *)
let parse_push_porcelain stdout =
  let lines =
    String.split_lines (String.strip stdout)
    |> List.filter ~f:(fun l ->
        let s = String.strip l in
        (not (String.is_empty s))
        && (not (String.is_prefix s ~prefix:"To "))
        && not (String.equal s "Done"))
  in
  match lines with
  | [] -> None
  | line :: _ -> (
      match String.lstrip line with
      | s when String.length s > 0 -> Some s.[0]
      | _ -> None)

(** Pure: parse [git rev-list --count base..HEAD] output into a commit count.
    Returns [None] on non-zero exit or unparseable stdout (treat as unknown —
    caller may decide to proceed with the push rather than erroneously skip). *)
let parse_commit_count ~code ~stdout =
  if code <> 0 then None else Stdlib.int_of_string_opt (String.strip stdout)

type push_gate = Proceed | Skip_no_commits [@@deriving show, eq, sexp_of]

(** Pure: given a commit-count result, decide whether to push. Zero commits
    ahead of base means a push would publish an empty ref that GitHub rejects on
    PR creation — skip. Unknown ([None]) defaults to [Proceed] so real failures
    surface via the push step rather than being masked by a silent skip. *)
let push_gate_from_count = function
  | Some 0 -> Skip_no_commits
  | None | Some _ -> Proceed

(** Pure: classify a [git push --porcelain --force-with-lease] invocation from
    its exit code + stdout + stderr. Split out so the mapping from raw git
    output to [push_result] can be property-tested independently of the shell.
*)
let classify_push_result ~code ~stdout ~stderr =
  if code = 0 then
    match parse_push_porcelain stdout with
    | Some '=' -> Push_up_to_date
    | _ -> Push_ok
  else
    match parse_push_porcelain stdout with
    | Some '!' -> Push_rejected
    | _ ->
        Push_error
          (Printf.sprintf "push failed (exit %d): %s" code (String.strip stderr))

(** Effectful: run [git rev-list --count base..HEAD] in the worktree. *)
let commits_ahead_of_base ~process_mgr ~path ~base =
  let base_str = Types.Branch.to_string base in
  let code, stdout, _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "rev-list"; "--count"; base_str ^ "..HEAD" ]
  in
  parse_commit_count ~code ~stdout

let force_push_with_lease ~process_mgr ~path ~branch ~base =
  let count = commits_ahead_of_base ~process_mgr ~path ~base in
  match push_gate_from_count count with
  | Skip_no_commits -> Push_no_commits
  | Proceed ->
      let branch_str = Types.Branch.to_string branch in
      let code, stdout, stderr =
        run_git_exit_code ~process_mgr
          [
            "git";
            "-C";
            path;
            "push";
            "--porcelain";
            "--force-with-lease";
            "origin";
            branch_str;
          ]
      in
      classify_push_result ~code ~stdout ~stderr

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

open Base

type t = { patch_id : Types.Patch_id.t; branch : Types.Branch.t; path : string }
[@@deriving show, eq, sexp_of, compare]

(* Pure parsers and decision functions live in [Worktree_parser] (lib_core/).
   This file is the effectful handler — git subprocess driver, FS operations,
   per-worktree mutex pool — that calls into the pure side. *)

let normalize_path path =
  Worktree_parser.normalize_path ~cwd:(Stdlib.Sys.getcwd ()) path

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

let clean_git_env = Stdlib.Lazy.from_fun Git_env.clean_env

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

let branch_prefixes = Worktree_parser.branch_prefixes
let find_ci_ref_collision = Worktree_parser.find_ci_ref_collision

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
  Worktree_parser.parse_porcelain ~cwd:(Stdlib.Sys.getcwd ()) ~repo_root raw

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

(* Type re-exports — pure definitions live in Worktree_parser. The manifest
   equations equate Worktree.X with Worktree_parser.X across the library
   boundary so callers that pattern-match on Worktree.Push_ok etc. compile
   unchanged. *)

type unique_commit = Worktree_parser.unique_commit = {
  sha : string;
  subject : string;
}
[@@deriving show, eq, sexp_of, compare]

type rebase_strategy = Worktree_parser.rebase_strategy = Onto | Plain
[@@deriving show, eq, sexp_of, compare]

type conflict_info = Worktree_parser.conflict_info = {
  target : string;
  old_base : string;
  unique_commits : unique_commit list;
  strategy : rebase_strategy;
  orig_head : string;
}
[@@deriving show, eq, sexp_of, compare]

type rebase_result = Worktree_parser.rebase_result =
  | Ok
  | Noop
  | Conflict of conflict_info
  | Error of string
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

(** Pure: does [subject] match the conventional "[<project>] Patch <N>:"
    commit-subject format for some [N] in [ancestor_ids]? The patch-id segment
    is terminated by the first character that satisfies [Char.is_whitespace] or
    equals [':'] (or end of string), so any [Patch_id.to_string] value that
    doesn't itself contain such a character round-trips correctly. Empty
    [project_name] or [ancestor_ids] always returns false: callers must supply
    the real project name for this match to be meaningful. *)
let is_ancestor_patch_subject = Worktree_parser.is_ancestor_patch_subject

(** Pure: parse [git log --format=%H %s] output into [unique_commit] records,
    dropping entries whose subject matches an ancestor patch. Preserves git's
    newest-first emission order. Returns both the list and the oldest SHA (the
    [~1] of which becomes the rebase's [--onto] anchor). The list and the SHA
    are produced together so callers cannot accidentally request one without the
    other and lose the recovery info needed by the conflict prompt. *)
let classify_unique_commits = Worktree_parser.classify_unique_commits

(** Pure: parse [git log --format=%H %s] output and return the oldest SHA whose
    subject is not [is_ancestor_patch_subject]. Back-compat wrapper around
    [classify_unique_commits] — preserved so existing call sites that need only
    the SHA do not need to handle the per-commit list. *)
let oldest_non_ancestor_commit = Worktree_parser.oldest_non_ancestor_commit

(** Pure: assemble a [conflict_info] from the contents of
    [.git/rebase-merge/onto], [.git/rebase-merge/upstream], and
    [.git/rebase-merge/orig-head] together with the
    [git log --format=%H %s <upstream>..<orig-head>] output. Used by the
    rebase-already-in-progress recovery path so the patch-agent prompt can
    surface the same recovery command as the fresh-rebase path. Returns [None]
    only when [onto] or [upstream] is blank — those are required to render the
    recovery command. An empty / all-filtered log degrades to an empty
    [unique_commits] list, NOT to [None]: the recovery command is fully
    determined by [target] and [old_base], so a restarted orchestrator should
    still surface it (the prompt renderer omits the commits header when the list
    is empty).

    [onto_contents] is the rebase destination SHA (first positional arg of
    [git rebase --onto X Y]); [upstream_contents] is the old-base SHA (second
    positional arg, the limit on what gets replayed). The recovery command needs
    the upstream as [old_base], not [onto]. *)
let parse_rebase_merge_state = Worktree_parser.parse_rebase_merge_state

(** Find the old base commit for [--onto] rebase by identifying which commits on
    our branch are unique (not in target). Uses patch-id matching via
    [git log --cherry-pick], and additionally strips commits whose subject
    matches [[<project>] Patch N:] for any transitive ancestor N. The subject
    fallback handles squash-merged ancestors whose patch-ids no longer match
    (squash collapses multiple commits into one with a fresh patch-id). Returns
    the parent of the oldest commit that survives both filters. *)
let find_old_base ~process_mgr ~path ~target ~project_name ~ancestor_ids =
  let code, stdout, stderr =
    run_git_exit_code ~process_mgr
      [
        "git";
        "-C";
        path;
        "log";
        "--cherry-pick";
        "--right-only";
        "--no-merges";
        (* Defeat log.showSignature=true in the user's gitconfig — otherwise
           [gpg: Signature made ...] lines would appear in the output and be
           misinterpreted by [classify_unique_commits] as SHAs. *)
        "--no-show-signature";
        "--format=%H %s";
        Printf.sprintf "%s...HEAD" target;
      ]
  in
  if code <> 0 then
    Result.Error
      (Printf.sprintf "log cherry-pick failed (exit %d): %s" code
         (String.strip stderr))
  else
    match classify_unique_commits ~project_name ~ancestor_ids stdout with
    | Result.Error msg -> Result.Error msg
    | Result.Ok (commits, oldest_sha) ->
        let code, stdout, stderr =
          run_git_exit_code ~process_mgr
            [ "git"; "-C"; path; "rev-parse"; Printf.sprintf "%s~1" oldest_sha ]
        in
        if code <> 0 then
          Result.Error
            (Printf.sprintf "rev-parse oldest~1 failed (exit %d): %s" code
               (String.strip stderr))
        else Result.Ok (String.strip stdout, commits)

let classify_fetch_result = Worktree_parser.classify_fetch_result

let fetch_origin ~fetch_lock ~process_mgr ~path =
  (* Serialize concurrent fetches across worktrees of the same repo. All
     worktrees share the main repo's ref store, so simultaneous
     [git fetch origin] processes race on the compare-and-swap update of
     [refs/remotes/origin/*], producing
     "cannot lock ref ...: is at X but expected Y" in the loser. The mutex
     eliminates that race by construction. *)
  Eio.Mutex.use_ro fetch_lock (fun () ->
      try
        let code, _stdout, stderr =
          run_git_exit_code ~process_mgr
            [ "git"; "-C"; path; "fetch"; "origin" ]
        in
        classify_fetch_result ~code ~stderr
      with
      | exn when has_cancellation exn -> raise exn
      | exn ->
          Result.Error
            (Printf.sprintf "git fetch origin crashed: %s" (Exn.to_string exn)))

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

(** Effectful: read the resolved SHA of [ref] from the worktree at [path]. Used
    by the runner fiber to record [branch_rebased_onto_sha] after a successful
    rebase, so the next rebase can pass it as [prev_base_sha] and trim commits
    absorbed into a squash-merge on origin. Returns [None] on any error (missing
    ref, git not runnable, etc.) — callers treat that as "no information" and
    fall back to the legacy plain-rebase path. *)
let read_branch_sha ~process_mgr ~path ~ref_name =
  let code, stdout, _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "rev-parse"; "--verify"; ref_name ]
  in
  if code = 0 then
    let s = String.strip stdout in
    if String.is_empty s then None else Some s
  else None

let rebase_onto ?(prev_base_sha = None) ~process_mgr ~path ~target ~project_name
    ~ancestor_ids () =
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
    (* Capture the pre-rebase HEAD before either find_old_base's [git rev-parse]
       calls or the rebase itself can move it. Best-effort: an empty string is
       a valid value for [conflict_info.orig_head] (the prompt skips emergency
       recovery when empty). *)
    let orig_head =
      let code, stdout, _ =
        run_git_exit_code ~process_mgr
          [ "git"; "-C"; path; "rev-parse"; "HEAD" ]
      in
      if code = 0 then String.strip stdout else ""
    in
    match
      find_old_base ~process_mgr ~path ~target ~project_name ~ancestor_ids
    with
    | Result.Error msg ->
        (* If we can't find unique commits, decide what upstream to pass to
           [git rebase]:
           - [prev_base_sha = Some sha]: do [git rebase --onto target sha]. This
             is the patch-6 case — the orchestrator recorded the SHA the
             previous base resolved to, and the patch's own commits live in
             [sha..HEAD]. Drops squash-merged-equivalent dep commits.
           - [None]: legacy plain [git rebase target] — replays everything in
             [target..HEAD] including possibly-stale dep commits, preserved
             for back-compat with agents whose [branch_rebased_onto_sha] was
             never recorded. *)
        let upstream =
          Rebase_decision.upstream ~prev_base_sha ~fallback:target
        in
        let rebase_args =
          if String.equal upstream target then
            [ "git"; "-C"; path; "rebase"; target ]
          else [ "git"; "-C"; path; "rebase"; "--onto"; target; upstream ]
        in
        let rebase_code, _, rebase_stderr =
          run_git_exit_code ~process_mgr rebase_args
        in
        if rebase_code = 0 then Ok
        else if rebase_code <> 1 then
          Error
            (Printf.sprintf "rebase failed (fallback, %s) (exit %d): %s" msg
               rebase_code
               (String.strip rebase_stderr))
        else
          (* Leave rebase in progress for agent to resolve. No commit list is
             available because [classify_unique_commits] failed; the prompt
             renders a Plain-strategy recovery section recommending plain
             [git rebase <target>] instead of [--onto]. *)
          Conflict
            {
              target;
              old_base = (if String.equal upstream target then "" else upstream);
              unique_commits = [];
              strategy = Plain;
              orig_head;
            }
    | Result.Ok (old_base, unique_commits) ->
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
          (* Leave rebase in progress for agent to resolve, with the recovery
             info threaded through so the prompt can render the exact --onto
             command the supervisor used. *)
          Conflict
            { target; old_base; unique_commits; strategy = Onto; orig_head }

type push_result = Worktree_parser.push_result =
  | Push_ok
  | Push_up_to_date
  | Push_no_commits
  | Push_rejected of Push_reject_classify.rejection
  | Push_worktree_missing
  | Push_error of string
[@@deriving show, eq, sexp_of, compare]

let parse_push_porcelain = Worktree_parser.parse_push_porcelain
let parse_commit_count = Worktree_parser.parse_commit_count

type push_gate = Worktree_parser.push_gate = Proceed | Skip_no_commits
[@@deriving show, eq, sexp_of]

let push_gate_from_count = Worktree_parser.push_gate_from_count
let classify_push_result = Worktree_parser.classify_push_result

(** Effectful: run [git rev-list --count base..HEAD] in the worktree. *)
let commits_ahead_of_base ~process_mgr ~path ~base =
  let base_str = Types.Branch.to_string base in
  let code, stdout, _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "rev-list"; "--count"; base_str ^ "..HEAD" ]
  in
  parse_commit_count ~code ~stdout

let force_push_with_lease ~process_mgr ~path ~branch ~base =
  (* If the worktree directory is gone (deleted out from under us mid-session),
     short-circuit before spawning git so the caller can route to the
     worktree-missing cleanup path. Without this, every git invocation below
     fails with "fatal: cannot change to '<path>': No such file or directory"
     and surfaces as a generic [Push_error] that triggers retry-push instead
     of reconstruction. *)
  if not (Stdlib.Sys.file_exists path) then Push_worktree_missing
  else
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

let read_file_opt path =
  try
    let ic = Stdlib.open_in path in
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_in_noerr ic)
      (fun () ->
        let len = Stdlib.in_channel_length ic in
        let buf = Bytes.create len in
        Stdlib.really_input ic buf 0 len;
        Some (Bytes.to_string buf))
  with _ -> None

(** Effectful: reconstruct [conflict_info] when a rebase is already in progress
    in the worktree (the orchestrator restarts mid-rebase, or a previous run
    left state behind). Reads [.git/rebase-merge/{onto,upstream,orig-head}] to
    recover the [--onto] destination, the upstream (old_base) limit, and the
    pre-rebase HEAD, runs [git log --format=%H %s upstream..orig-head] to
    enumerate the commits the rebase intends to replay, then delegates to the
    pure [parse_rebase_merge_state]. Returns [None] best-effort: any read or git
    failure degrades to a no-recovery-section prompt rather than blocking
    delivery. Only handles [.git/rebase-merge] (the merge-style rebase used by
    [rebase_onto]); a [.git/rebase-apply] state returns [None]. *)
let read_in_progress_conflict_info ~process_mgr ~path ~target ~project_name
    ~ancestor_ids =
  let target = Types.Branch.to_string target in
  let code, stdout, _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "rev-parse"; "--git-dir" ]
  in
  if code <> 0 then None
  else
    let git_dir = String.strip stdout in
    let git_dir =
      if Stdlib.Filename.is_relative git_dir then
        Stdlib.Filename.concat path git_dir
      else git_dir
    in
    let onto_path = Stdlib.Filename.concat git_dir "rebase-merge/onto" in
    let upstream_path =
      Stdlib.Filename.concat git_dir "rebase-merge/upstream"
    in
    let orig_head_path =
      Stdlib.Filename.concat git_dir "rebase-merge/orig-head"
    in
    match (read_file_opt onto_path, read_file_opt upstream_path) with
    | Some onto_contents, Some upstream_contents ->
        let onto = String.strip onto_contents in
        let upstream = String.strip upstream_contents in
        if String.is_empty onto || String.is_empty upstream then None
        else
          (* orig-head is best-effort: a missing or empty file degrades to
             "" (the prompt skips the [git reset --hard] block) instead of
             dropping the whole recovery section. The fresh-rebase path
             already treats orig_head this way when [git rev-parse HEAD]
             fails; mirror that here so a restarted orchestrator gives the
             same guidance. *)
          let orig_head_contents =
            Option.value (read_file_opt orig_head_path) ~default:""
          in
          let orig_head = String.strip orig_head_contents in
          (* Use upstream..orig-head, not onto..orig-head: the unique-commit
             range is bounded by the upstream (the rebase's "since" anchor),
             not by the destination. When orig-head is empty (best-effort
             miss), fall back to HEAD so [git log] still yields the rebase's
             intended replay set. *)
          let log_endpoint =
            if String.is_empty orig_head then "HEAD" else orig_head
          in
          let log_code, log_stdout, _ =
            run_git_exit_code ~process_mgr
              [
                "git";
                "-C";
                path;
                "log";
                "--no-merges";
                "--no-show-signature";
                "--format=%H %s";
                Printf.sprintf "%s..%s" upstream log_endpoint;
              ]
          in
          if log_code <> 0 then None
          else
            parse_rebase_merge_state ~onto_contents ~upstream_contents
              ~orig_head_contents ~log_format_h_s:log_stdout ~project_name
              ~ancestor_ids ~target
    | _ -> None

let find_for_branch ~process_mgr ~repo_root branch =
  let pairs = try list_with_branches ~process_mgr ~repo_root with _ -> [] in
  List.find_map pairs ~f:(fun (path, b) ->
      if Types.Branch.equal b branch then Some path else None)

(** Drop git's worktree-registry entries for directories that no longer exist on
    disk. Useful before [find_for_branch] / [create] when a previous worktree
    directory was deleted out-of-band ([rm -rf] or a leftover failed checkout):
    without this, [git worktree list] still reports the stale entry and
    [git worktree add] refuses to recreate the same path. *)
let prune_admin ~process_mgr ~repo_root =
  let _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; repo_root; "worktree"; "prune" ]
  in
  ()

let exists t = Stdlib.Sys.file_exists t.path
let path t = t.path
let patch_id t = t.patch_id
let branch t = t.branch

module type S = sig
  val resolve_main_root : unit -> string
  val is_checked_out_in_repo_root : Types.Branch.t -> bool
  val remote_branch_exists : string -> bool

  val create :
    project_name:string ->
    patch_id:Types.Patch_id.t ->
    branch:Types.Branch.t ->
    base_ref:string ->
    t

  val remove : t -> unit
  val detect_branch : path:string -> Types.Branch.t
  val list_with_branches : unit -> (string * Types.Branch.t) list
  val find_for_branch : Types.Branch.t -> string option
  val prune_admin : unit -> unit

  val run_hook :
    clock:_ Eio.Time.clock ->
    script:string ->
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    env:(string * string) list ->
    unit ->
    (unit, string) Result.t

  val fetch_origin :
    fetch_lock:Eio.Mutex.t -> path:string -> (unit, string) Result.t

  val git_status : path:string -> string
  val conflict_diff : path:string -> string

  val rebase_onto :
    ?prev_base_sha:string option ->
    path:string ->
    target:Types.Branch.t ->
    project_name:string ->
    ancestor_ids:Types.Patch_id.t list ->
    unit ->
    rebase_result

  val read_branch_sha : path:string -> ref_name:string -> string option
  (** Resolve [ref_name] to a SHA in the worktree at [path]. [None] on any error
      (missing ref, git failure). Used by the runner fiber to capture
      [branch_rebased_onto_sha] after a successful rebase. *)

  val read_in_progress_conflict_info :
    path:string ->
    target:Types.Branch.t ->
    project_name:string ->
    ancestor_ids:Types.Patch_id.t list ->
    conflict_info option

  val force_push_with_lease :
    path:string -> branch:Types.Branch.t -> base:Types.Branch.t -> push_result

  val rebase_in_progress : path:string -> bool
end

type client = (module S)

let make ~process_mgr ~repo_root =
  (module struct
    let resolve_main_root () = resolve_main_root ~process_mgr ~repo_root

    let is_checked_out_in_repo_root branch =
      is_checked_out_in_repo_root ~process_mgr ~repo_root branch

    let remote_branch_exists branch_str =
      remote_branch_exists ~process_mgr ~repo_root branch_str

    let create ~project_name ~patch_id ~branch ~base_ref =
      create ~process_mgr ~repo_root ~project_name ~patch_id ~branch ~base_ref

    let remove t = remove ~process_mgr ~repo_root t
    let detect_branch ~path = detect_branch ~process_mgr ~path
    let list_with_branches () = list_with_branches ~process_mgr ~repo_root
    let find_for_branch branch = find_for_branch ~process_mgr ~repo_root branch
    let prune_admin () = prune_admin ~process_mgr ~repo_root

    let run_hook ~clock ~script ~cwd ~env () =
      User_config.run_hook ~process_mgr ~clock ~script ~cwd ~env ()

    let fetch_origin ~fetch_lock ~path =
      fetch_origin ~fetch_lock ~process_mgr ~path

    let git_status ~path = git_status ~process_mgr ~path
    let conflict_diff ~path = conflict_diff ~process_mgr ~path

    let rebase_onto ?(prev_base_sha = None) ~path ~target ~project_name
        ~ancestor_ids () =
      rebase_onto ~prev_base_sha ~process_mgr ~path ~target ~project_name
        ~ancestor_ids ()

    let read_branch_sha ~path ~ref_name =
      read_branch_sha ~process_mgr ~path ~ref_name

    let read_in_progress_conflict_info ~path ~target ~project_name ~ancestor_ids
        =
      read_in_progress_conflict_info ~process_mgr ~path ~target ~project_name
        ~ancestor_ids

    let force_push_with_lease ~path ~branch ~base =
      force_push_with_lease ~process_mgr ~path ~branch ~base

    let rebase_in_progress ~path = rebase_in_progress ~process_mgr ~path
  end : S)

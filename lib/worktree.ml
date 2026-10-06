(* @archlint.module shell
   @archlint.domain worktree-parser *)

open Base

type t = {
  patch_id : Types.Patch_id.t;
  branch : Types.Branch.t;
  path : string;
  newly_created : bool;
}
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

let has_cancellation = Process_tree.has_cancellation
let is_transient_spawn_failure = Process_tree.is_transient_spawn_failure
let retry_transient_spawn = Process_tree.retry_transient_spawn

(* [Eio.Process.run] wrapper that retries transient spawn failures. Named
   parameter [mgr] (not [process_mgr]) so a textual rewrite of the call sites
   below doesn't recurse into this definition. *)
let process_run_retry mgr ?env ?stdout ?stderr args =
  retry_transient_spawn (fun () ->
      Eio.Process.run mgr ?env ?stdout ?stderr args)

let clean_git_env = Stdlib.Lazy.from_fun Git_env.clean_env

let ref_exists ~process_mgr ~repo_root ref_path =
  let buf = Buffer.create 16 in
  match
    process_run_retry process_mgr
      ~env:(Stdlib.Lazy.force clean_git_env)
      ~stdout:(Eio.Flow.buffer_sink buf)
      ~stderr:(Eio.Flow.buffer_sink (Buffer.create 16))
      [ "git"; "-C"; repo_root; "rev-parse"; "--verify"; ref_path ]
  with
  | () -> true
  | exception e when has_cancellation e -> raise e
  | exception _ -> false

let remote_branch_exists ~process_mgr ~repo_root branch_str =
  ref_exists ~process_mgr ~repo_root ("refs/remotes/origin/" ^ branch_str)

let resolve_main_root ~process_mgr ~repo_root =
  let buf = Buffer.create 128 in
  let stderr_buf = Buffer.create 64 in
  match
    process_run_retry process_mgr
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
    process_run_retry process_mgr
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

(* Run a git command and capture (exit_code, stdout, stderr) without raising.
   Defined here, ahead of its first use in [read_repo_ref_sha] and
   [compute_ancestry], so the worktree-creation planner inputs can be gathered
   before the type re-exports and the heavier rebase/push functions below. *)
let run_git_exit_code ~process_mgr args =
  let stdout_buf = Buffer.create 256 in
  let stderr_buf = Buffer.create 64 in
  let env = Stdlib.Lazy.force clean_git_env in
  (* [Eio.Process.await] reports the exit status but does NOT wait for the
     internal fibers that copy the child's stdout/stderr pipes into these
     buffers (only [Eio.Process.run] documents that it does). Reading
     [Buffer.contents] inside the switch — right after [await] — therefore
     races the copy fibers: under scheduling load the buffer can still be empty
     or partial when read. That manifested as a flaky [rev-parse --abbrev-ref
     HEAD] returning "" → [worktree_head_branch = None] → a spurious
     branch-switched push refusal. The switch only releases (and so joins the
     copy fibers) when its body returns, so read the buffers *after*
     [Eio.Switch.run] completes. *)
  let code =
    Eio.Switch.run @@ fun sw ->
    let child =
      retry_transient_spawn (fun () ->
          Eio.Process.spawn ~sw process_mgr ~env
            ~stdin:(Eio.Flow.string_source "")
            ~stdout:(Eio.Flow.buffer_sink stdout_buf)
            ~stderr:(Eio.Flow.buffer_sink stderr_buf)
            args)
    in
    match Eio.Process.await child with `Exited c -> c | `Signaled s -> 128 + s
  in
  (* Read the buffers only here, after [Eio.Switch.run] has returned: that join
     point is what guarantees the copy fibers have finished. Bind them outside
     the switch body explicitly so this stays obvious. *)
  let stdout = Buffer.contents stdout_buf in
  let stderr = Buffer.contents stderr_buf in
  (code, stdout, stderr)

(* Read a ref's SHA from the main repo (NOT a worktree). Returns [None] if the
   ref does not exist or git fails. Used by [Worktree.create] to feed inputs to
   [Start_point_plan.plan]. *)
let read_repo_ref_sha ~process_mgr ~repo_root ~ref_name =
  let code, stdout, _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; repo_root; "rev-parse"; "--verify"; ref_name ]
  in
  if code = 0 then
    let s = String.strip stdout in
    if String.is_empty s then None else Some s
  else None

(* Compute the ancestor relationship between [local] and [remote] using two
   [git merge-base --is-ancestor] probes. Returns [Unknown] if either probe
   fails. Pure inputs are SHAs already known to exist at the time of call —
   caller is expected to gather them via [read_repo_ref_sha] first. *)
let compute_repo_ancestry ~process_mgr ~repo_root ~local ~remote :
    Start_point_plan.ancestry =
  let is_ancestor a b =
    let code, _, _ =
      run_git_exit_code ~process_mgr
        [ "git"; "-C"; repo_root; "merge-base"; "--is-ancestor"; a; b ]
    in
    match code with 0 -> Some true | 1 -> Some false | _ -> None
  in
  match (is_ancestor local remote, is_ancestor remote local) with
  | Some true, Some true -> Start_point_plan.Equal
  | Some true, Some false -> Start_point_plan.Remote_ahead
  | Some false, Some true -> Start_point_plan.Local_ahead
  | Some false, Some false -> Start_point_plan.Diverged
  | _ -> Start_point_plan.Unknown

let local_changes_represented_remotely ~process_mgr ~repo_root ~local ~remote =
  let code, stdout, _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; repo_root; "cherry"; remote; local ]
  in
  if code <> 0 then false
  else
    let changes =
      String.split_lines stdout
      |> List.filter ~f:(fun line -> not (String.is_empty (String.strip line)))
    in
    (not (List.is_empty changes))
    && List.for_all changes ~f:(fun line -> Char.equal line.[0] '-')
    &&
    let tree_diff_code, _, _ =
      run_git_exit_code ~process_mgr
        [ "git"; "-C"; repo_root; "diff"; "--quiet"; local; remote; "--" ]
    in
    tree_diff_code = 0

(* Fetch a single branch from origin into the corresponding remote-tracking
   ref. Returns a typed [fetch_branch_result] so callers can distinguish
   the routine "brand-new branch — no upstream yet" case from genuine
   fetch failures (network, auth, ref-lock contention). The planner
   correctly handles [remote_ref = None] either way; the distinction is
   load-bearing only for log clarity.

   Operates on [repo_root]; worktrees share the ref store with the main repo,
   so this updates [refs/remotes/origin/<branch>] for all workers. The
   [fetch_lock] mutex must be the same one shared with [fetch_origin] above. *)
let fetch_origin_branch ~fetch_lock ~process_mgr ~repo_root ~branch_str =
  Eio.Mutex.use_ro fetch_lock (fun () ->
      try
        let code, _stdout, stderr =
          run_git_exit_code ~process_mgr
            [
              "git";
              "-C";
              repo_root;
              "fetch";
              "origin";
              "+refs/heads/" ^ branch_str ^ ":refs/remotes/origin/" ^ branch_str;
            ]
        in
        Worktree_parser.classify_fetch_branch_result ~code ~stderr
      with
      | exn when has_cancellation exn -> raise exn
      | exn ->
          Worktree_parser.Fetch_branch_error
            (Printf.sprintf "git fetch origin %s crashed: %s" branch_str
               (Exn.to_string exn)))

let branch_prefixes = Worktree_parser.branch_prefixes
let find_ci_ref_collision = Worktree_parser.find_ci_ref_collision

let check_case_insensitive_ref_collision ~process_mgr ~repo_root branch_str =
  let buf = Buffer.create 512 in
  let existing_branches =
    match
      process_run_retry process_mgr
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

(* Creation wiring separates start-point decisions from backend lifecycle effects. *)
type create_io = {
  worktree_exists : path:string -> bool;
      (** True only for a validated, ready checkout of the requested branch. *)
  check_ref_collision : branch_str:string -> unit;
      (** Case-insensitive ref-collision guard; raises to abort creation. *)
  read_ref : ref_name:string -> string option;
      (** Resolve a ref to its SHA, or [None] when the ref is absent. *)
  ancestry : local:string -> remote:string -> Start_point_plan.ancestry;
      (** Two-way ancestry between an existing local and remote SHA. *)
  local_changes_represented_remotely : local:string -> remote:string -> bool;
      (** Whether every local-only patch is represented remotely and the
          resulting trees match. *)
  execute_action :
    path:string ->
    branch_str:string ->
    expected_local:string option ->
    Start_point_plan.action ->
    bool;
      (** Realize the approved action; return true only for a new checkout. *)
}

(* Wiring of [create], parameterised over its effects. Given [io], it reads the
   local/remote refs, computes ancestry only when both sides exist, consults the
   pure {!Start_point_plan.plan}, and either executes the action or surfaces the
   refusal. [make] supplies validated backend effects; the split exists
   so the control flow (including the short-circuit and the refusal mapping) can
   be unit-tested without spawning git.

   [branch_checked_out_in_main_root] and [existing_worktree_path] are checked by
   [Worktree_setup.ensure_worktree] before this point — we pass [false]/[None]
   so the planner's totality contract is preserved without redoing the work. *)
let create_with_io ~io ~project_name ~patch_id ~branch ~base_ref :
    (t, Start_point_plan.refusal) Result.t =
  let path = worktree_dir ~project_name ~patch_id in
  let branch_str = Types.Branch.to_string branch in
  if io.worktree_exists ~path then
    Result.Ok { patch_id; branch; path; newly_created = false }
  else (
    io.check_ref_collision ~branch_str;
    let local_ref = io.read_ref ~ref_name:("refs/heads/" ^ branch_str) in
    let remote_ref =
      io.read_ref ~ref_name:("refs/remotes/origin/" ^ branch_str)
    in
    let ancestry =
      match (local_ref, remote_ref) with
      | Some l, Some r -> io.ancestry ~local:l ~remote:r
      | _ -> Start_point_plan.Unknown
    in
    let local_changes_represented_remotely =
      match (local_ref, remote_ref, ancestry) with
      | Some local, Some remote, (Diverged | Unknown) ->
          io.local_changes_represented_remotely ~local ~remote
      | ( None,
          (None | Some _),
          (Local_ahead | Remote_ahead | Equal | Diverged | Unknown) )
      | Some _, None, (Local_ahead | Remote_ahead | Equal | Diverged | Unknown)
      | Some _, Some _, (Local_ahead | Remote_ahead | Equal) ->
          false
    in
    let decision =
      Start_point_plan.plan ~local_ref ~remote_ref ~ancestry
        ~base_branch:base_ref ~local_changes_represented_remotely
        ~branch_checked_out_in_main_root:false ~existing_worktree_path:None
    in
    match decision with
    | Refuse refusal -> Result.Error refusal
    | Plan action ->
        let newly_created =
          io.execute_action ~path ~branch_str ~expected_local:local_ref action
        in
        Result.Ok { patch_id; branch; path; newly_created })

let detect_branch ~process_mgr ~path =
  let buf = Buffer.create 128 in
  let path = normalize_path path in
  let stderr_buf = Buffer.create 64 in
  (match
     process_run_retry process_mgr
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
  | Merge_conflict of string
  | Uncommitted_changes of string
  | Error of string
[@@deriving show, eq, sexp_of, compare]

(* [run_git_exit_code] is defined earlier in the file, just after [run_git],
   so the start-point planner helpers above ([read_repo_ref_sha],
   [compute_repo_ancestry], [fetch_origin_branch]) can call it. *)

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

type onto_anchor = Worktree_parser.onto_anchor =
  | Anchor of string
  | No_anchor of string
[@@deriving show, eq, sexp_of, compare]

let classify_onto_anchor = Worktree_parser.classify_onto_anchor

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
    | Result.Ok (commits, oldest_sha) -> (
        let code, stdout, stderr =
          run_git_exit_code ~process_mgr
            [ "git"; "-C"; path; "rev-parse"; Printf.sprintf "%s~1" oldest_sha ]
        in
        (* An empty anchor (oldest is a root commit, or the running git resolves
           [<root>~1] to "" with exit 0) must NOT reach [git rebase --onto] — it
           aborts there with "invalid upstream ''". Route both the failed and
           blank cases to [Error] so [rebase_onto] takes the plain/upstream
           fallback. See [Worktree_parser.classify_onto_anchor]. *)
        match Worktree_parser.classify_onto_anchor ~code ~stdout with
        | Worktree_parser.Anchor old_base -> Result.Ok (old_base, commits)
        | Worktree_parser.No_anchor reason ->
            Result.Error (Printf.sprintf "%s: %s" reason (String.strip stderr)))

let classify_fetch_result = Worktree_parser.classify_fetch_result

type fetch_branch_result = Worktree_parser.fetch_branch_result =
  | Fetch_branch_ok
  | Fetch_branch_no_remote_ref
  | Fetch_branch_error of string
[@@deriving show, eq, sexp_of, compare]

let classify_fetch_branch_result = Worktree_parser.classify_fetch_branch_result

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

let has_uncommitted_changes ~process_mgr ~path =
  let code, stdout, stderr =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "status"; "--porcelain=v1"; "--untracked-files=all" ]
  in
  if code = 0 then Result.Ok (not (String.is_empty stdout))
  else
    Result.Error
      (Printf.sprintf "git status failed in %s (exit %d): %s" path code
         (String.strip stderr))

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

let is_ancestor ~process_mgr ~path ~ancestor ~descendant =
  let code, _, _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "merge-base"; "--is-ancestor"; ancestor; descendant ]
  in
  code = 0

let git_dir_opt ~process_mgr ~path =
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
    Some git_dir

let rebase_in_progress_raw ~process_mgr ~path =
  match git_dir_opt ~process_mgr ~path with
  | None -> false
  | Some git_dir ->
      Stdlib.Sys.file_exists (Stdlib.Filename.concat git_dir "rebase-merge")
      || Stdlib.Sys.file_exists (Stdlib.Filename.concat git_dir "rebase-apply")

let classify_rebase_exit_1 ~process_mgr ~path ~stderr conflict =
  if rebase_in_progress_raw ~process_mgr ~path then Conflict conflict
  else
    Error
      (Printf.sprintf "rebase exited 1 without leaving a rebase in progress: %s"
         (String.strip stderr))

let rebase_onto ~upstream ~process_mgr ~path ~target ~project_name ~ancestor_ids
    () =
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
    let status_code, status_stdout, status_stderr =
      run_git_exit_code ~process_mgr
        [ "git"; "-C"; path; "status"; "--porcelain=v1" ]
    in
    match
      Worktree_parser.classify_rebase_worktree_status ~code:status_code
        ~stdout:status_stdout ~stderr:status_stderr
    with
    | Some result -> result
    | None -> (
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
            (* The cherry-pick / patch-id detection in [find_old_base] failed,
           so use the caller-supplied [upstream] (computed by the executor
           via [Rebase_decision.plan] from the agent's anchor history). If
           [upstream] equals [target], no usable anchor exists and we fall
           back to the 2-arg form; otherwise [git rebase --onto target
           upstream HEAD] replays exactly the patch's own commits past
           [upstream]. *)
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
              classify_rebase_exit_1 ~process_mgr ~path ~stderr:rebase_stderr
                {
                  target;
                  old_base =
                    (if String.equal upstream target then "" else upstream);
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
              classify_rebase_exit_1 ~process_mgr ~path ~stderr:rebase_stderr
                { target; old_base; unique_commits; strategy = Onto; orig_head }
        )

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

(* [commits_ahead_of_base] previously ran [git rev-list --count base..HEAD]
   in the worktree; that's the function that produced PR #315's HEAD-vs-branch
   gate mismatch. [gather_push_plan_inputs] below now measures ahead-of-base
   on the named branch (refs/heads/<branch>), not HEAD, so an agent that
   [git switch]ed mid-session cannot trip the gate with commits the push
   command would never upload. *)

(* Gather the inputs [Push_plan.plan] needs. Returns a tuple so the caller
   can pattern-match the planner output without re-reading git twice. *)
let gather_push_plan_inputs ~on_phase ~process_mgr ~path ~branch_str ~base_str =
  let worktree_path_exists = Stdlib.Sys.file_exists path in
  let worktree_head_branch =
    if not worktree_path_exists then None
    else
      let code, stdout, _ =
        run_git_exit_code ~process_mgr
          [ "git"; "-C"; path; "rev-parse"; "--abbrev-ref"; "HEAD" ]
      in
      if code = 0 then
        let s = String.strip stdout in
        if String.is_empty s || String.equal s "HEAD" then None else Some s
      else None
  in
  let read_sha ref_name =
    if not worktree_path_exists then None
    else
      let code, stdout, _ =
        run_git_exit_code ~process_mgr
          [ "git"; "-C"; path; "rev-parse"; "--verify"; ref_name ]
      in
      if code = 0 then
        let s = String.strip stdout in
        if String.is_empty s then None else Some s
      else None
  in
  let branch_ref_sha =
    match read_sha ("refs/heads/" ^ branch_str) with
    | Some _ as sha -> sha
    | None -> (
        match worktree_head_branch with
        | Some head when String.equal head branch_str -> read_sha branch_str
        | Some _ | None -> None)
  in
  let remote_observation =
    match read_sha ("refs/remotes/origin/" ^ branch_str) with
    | Some sha -> Result.Ok (Some sha)
    | None when not worktree_path_exists -> Result.Ok None
    | None -> (
        let remote_ref = "refs/heads/" ^ branch_str in
        on_phase "remote observation (git ls-remote)";
        let code, stdout, stderr =
          run_git_exit_code ~process_mgr
            [
              "git";
              "-C";
              path;
              "ls-remote";
              "--exit-code";
              "--refs";
              "origin";
              remote_ref;
            ]
        in
        if code = 2 then Result.Ok None
        else if code <> 0 then
          Result.Error ("Cannot observe remote branch: " ^ String.strip stderr)
        else
          match
            Worktree_parser.parse_ls_remote_sha ~ref_name:remote_ref stdout
          with
          | None -> Result.Error "Cannot decode observed remote branch SHA"
          | Some sha ->
              (* Fetch only this observation's objects, leaving shared tracking
                 refs and FETCH_HEAD alone. A later remote write still fails the
                 captured lease; a fetch failure grants no publication authority. *)
              on_phase "remote object fetch (git fetch)";
              let code, _, stderr =
                run_git_exit_code ~process_mgr
                  [
                    "git";
                    "-C";
                    path;
                    "fetch";
                    "--no-write-fetch-head";
                    "--refmap=";
                    "--no-tags";
                    "--no-prune";
                    "origin";
                    sha;
                  ]
              in
              if code = 0 then Result.Ok (Some sha)
              else
                Result.Error
                  ("Cannot fetch observed remote commit: " ^ String.strip stderr)
        )
  in
  Result.map remote_observation ~f:(fun remote_tracking_sha ->
      on_phase "publication planning";
      let ancestry : Push_plan.ancestry =
        if not worktree_path_exists then Push_plan.Unknown
        else
          match (branch_ref_sha, remote_tracking_sha) with
          | _, None -> Push_plan.No_remote_yet
          | None, Some _ -> Push_plan.Unknown
          | Some local, Some remote -> (
              let code, _, _ =
                run_git_exit_code ~process_mgr
                  [
                    "git";
                    "-C";
                    path;
                    "merge-base";
                    "--is-ancestor";
                    remote;
                    local;
                  ]
              in
              (* exit 0 = remote is ancestor of local (local includes remote);
             exit 1 only means remote is not an ancestor of local. Probe the
             inverse to distinguish local-behind from true divergence. *)
              match code with
              | 0 -> Push_plan.Local_includes_remote
              | 1 -> (
                  let inverse_code, _, _ =
                    run_git_exit_code ~process_mgr
                      [
                        "git";
                        "-C";
                        path;
                        "merge-base";
                        "--is-ancestor";
                        local;
                        remote;
                      ]
                  in
                  match inverse_code with
                  | 0 -> Push_plan.Local_missing_remote
                  | 1 -> Push_plan.Local_diverged_from_remote
                  | _ -> Push_plan.Unknown)
              | _ -> Push_plan.Unknown)
      in
      (* Only the captured current history can authorize a rewrite. Every remote
     commit must be reachable or patch-equivalent to a commit on that history;
     stale reflog tips are never evidence of incorporation. Keep merges in the
     result so unmatched merge resolutions fail closed as well. *)
      let remote_changes_included =
        match (branch_ref_sha, remote_tracking_sha, ancestry) with
        | Some _, Some _, Push_plan.Local_includes_remote -> true
        | Some local, Some remote, Push_plan.Local_diverged_from_remote ->
            let code, stdout, _ =
              run_git_exit_code ~process_mgr
                [
                  "git";
                  "-C";
                  path;
                  "rev-list";
                  "--cherry-pick";
                  "--right-only";
                  "--count";
                  local ^ "..." ^ remote;
                  "--";
                ]
            in
            Option.equal Int.equal
              (Worktree_parser.parse_commit_count ~code ~stdout)
              (Some 0)
        | ( _,
            _,
            ( Push_plan.Local_missing_remote | Push_plan.No_remote_yet
            | Push_plan.Unknown ) )
        | ( None,
            _,
            ( Push_plan.Local_includes_remote
            | Push_plan.Local_diverged_from_remote ) )
        | ( _,
            None,
            ( Push_plan.Local_includes_remote
            | Push_plan.Local_diverged_from_remote ) ) ->
            false
      in
      let commits_ahead_of_base =
        if not worktree_path_exists then None
        else
          (* Measure ahead-of-base on the captured commit, not worktree HEAD —
         so a concurrent checkout or branch update cannot authorize a different
         commit from the one this publication will upload. *)
          let code, stdout, _ =
            run_git_exit_code ~process_mgr
              [
                "git";
                "-C";
                path;
                "rev-list";
                "--count";
                base_str ^ ".."
                ^ Option.value branch_ref_sha
                    ~default:("refs/heads/" ^ branch_str);
              ]
          in
          Worktree_parser.parse_commit_count ~code ~stdout
      in
      ( worktree_path_exists,
        worktree_head_branch,
        branch_ref_sha,
        remote_tracking_sha,
        ancestry,
        remote_changes_included,
        commits_ahead_of_base ))

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

let force_push_with_lease_unbounded ~on_phase ~preserve_history ~process_mgr
    ~path ~branch ~base =
  let branch_str = Types.Branch.to_string branch in
  let base_str = Types.Branch.to_string base in
  match
    gather_push_plan_inputs ~on_phase ~process_mgr ~path ~branch_str ~base_str
  with
  | Error detail -> Push_error detail
  | Ok
      ( worktree_path_exists,
        worktree_head_branch,
        branch_ref_sha,
        remote_tracking_sha,
        ancestry,
        remote_changes_included,
        commits_ahead_of_base ) -> (
      let plan rewrite_authority =
        Push_plan.plan ~preserve_history ~expected_branch:branch_str
          ~worktree_path_exists ~worktree_head_branch ~branch_ref_sha
          ~remote_tracking_sha ~ancestry ~remote_changes_included
          ~rewrite_authority ~commits_ahead_of_base
      in
      let preliminary = plan None in
      let decision =
        match preliminary with
        | Refuse (Push_plan.Remote_not_integrated _) ->
            let rewrite_authority =
              match (branch_ref_sha, remote_tracking_sha, ancestry) with
              | ( Some local_sha,
                  Some remote_sha,
                  Push_plan.Local_diverged_from_remote )
                when (not preserve_history)
                     && (not remote_changes_included)
                     && not (rebase_in_progress_raw ~process_mgr ~path) -> (
                  let code, reflog, _ =
                    run_git_exit_code ~process_mgr
                      [
                        "git";
                        "-C";
                        path;
                        "rev-parse";
                        "--path-format=absolute";
                        "--git-path";
                        "logs/refs/heads/" ^ branch_str;
                      ]
                  in
                  match
                    if code = 0 then read_file_opt (String.strip reflog)
                    else None
                  with
                  | None -> None
                  | Some reflog ->
                      Rewrite_lineage.of_reflog ~branch:branch_str ~local_sha
                        ~remote_sha ~reflog
                        ~ancestor_oracle:(fun sha ~descendant ->
                          let code, _, _ =
                            run_git_exit_code ~process_mgr
                              [
                                "git";
                                "-C";
                                path;
                                "merge-base";
                                "--is-ancestor";
                                sha;
                                descendant;
                              ]
                          in
                          code = 0))
              | ( _,
                  _,
                  ( Push_plan.Local_includes_remote
                  | Push_plan.Local_missing_remote | Push_plan.No_remote_yet
                  | Push_plan.Unknown ) )
              | None, _, Push_plan.Local_diverged_from_remote
              | _, None, Push_plan.Local_diverged_from_remote
              | Some _, Some _, Push_plan.Local_diverged_from_remote ->
                  None
            in
            plan rewrite_authority
        | Push _
        | Refuse
            ( Push_plan.Worktree_missing | Push_plan.No_commits_ahead_of_base
            | Push_plan.Branch_ref_missing _ | Push_plan.Branch_switched _
            | Push_plan.Local_missing_remote_commits _
            | Push_plan.History_would_be_rewritten _ ) ->
            preliminary
      in
      match decision with
      | Refuse Push_plan.Worktree_missing -> Push_worktree_missing
      | Refuse Push_plan.No_commits_ahead_of_base -> Push_no_commits
      | Refuse
          (( Push_plan.Branch_ref_missing _ | Push_plan.Branch_switched _
           | Push_plan.Local_missing_remote_commits _
           | Push_plan.Remote_not_integrated _
           | Push_plan.History_would_be_rewritten _ ) as r) -> (
          match Push_plan.to_push_reject_classify_rejection r with
          | Some rej -> Push_rejected rej
          | None ->
              (* These refusals all map to Some _ per
             [to_push_reject_classify_rejection]; fall back conservatively. *)
              Push_error (Push_plan.short_label (Push_plan.Refuse r)))
      | Push action -> (
          let args =
            match action with
            | Push_plan.Force_push_with_lease { local_sha; remote_sha } ->
                [
                  "git";
                  "-C";
                  path;
                  "push";
                  "--porcelain";
                  "--force-with-lease=refs/heads/" ^ branch_str ^ ":"
                  ^ remote_sha;
                  "origin";
                  local_sha ^ ":refs/heads/" ^ branch_str;
                ]
            | Push_plan.Initial_push { local_sha } ->
                [
                  "git";
                  "-C";
                  path;
                  "push";
                  "--porcelain";
                  "--force-with-lease=refs/heads/" ^ branch_str ^ ":";
                  "origin";
                  local_sha ^ ":refs/heads/" ^ branch_str;
                ]
          in
          on_phase "git push";
          let code, stdout, stderr = run_git_exit_code ~process_mgr args in
          let result = classify_push_result ~code ~stdout ~stderr in
          match action with
          | Push_plan.Initial_push _
            when equal_push_result result Push_ok
                 || equal_push_result result Push_up_to_date ->
              (* -u cannot identify a local branch from an immutable SHA refspec.
             Set the named branch's upstream only after successful publication. *)
              on_phase "upstream configuration (git branch)";
              let code, _, stderr =
                run_git_exit_code ~process_mgr
                  [
                    "git";
                    "-C";
                    path;
                    "branch";
                    "--set-upstream-to=origin/" ^ branch_str;
                    branch_str;
                  ]
              in
              if code = 0 then result
              else
                Push_error
                  ("Published branch but failed to set upstream: " ^ stderr)
          | Push_plan.Initial_push _ | Push_plan.Force_push_with_lease _ ->
              result))

let default_push_timeout_seconds = 120.0

let force_push_with_lease ?(preserve_history = false)
    ?(timeout_seconds = default_push_timeout_seconds) ~clock ~process_mgr ~path
    ~branch ~base () =
  (* Keep a single deadline for the whole operation, including remote IO before
     the push. Cancellation reports the operation actually awaiting completion. *)
  let phase = ref "publication planning" in
  let on_phase current = phase := current in
  match
    Eio.Time.with_timeout clock timeout_seconds (fun () ->
        Ok
          (force_push_with_lease_unbounded ~on_phase ~preserve_history
             ~process_mgr ~path ~branch ~base))
  with
  | Ok result -> result
  | Error `Timeout ->
      Push_error
        (Printf.sprintf "Publication timed out after %.0fs during %s"
           timeout_seconds !phase)

let rebase_in_progress ~process_mgr ~path =
  rebase_in_progress_raw ~process_mgr ~path

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

let path t = t.path
let patch_id t = t.patch_id
let branch t = t.branch
let newly_created t = t.newly_created

type integration_result =
  | Integrated of string
  | Integration_conflict of string
  | Integration_error of string

(* Caller owns the root-write lock. No checked-out branch is reset or rewritten. *)
let integrate ~process_mgr ~clock ~repo_root ~root_path ~root_branch
    ~descendant_branch ~head_sha =
  let git path args =
    Process_tree.run ~process_mgr ~clock
      ~env:(Stdlib.Lazy.force clean_git_env)
      ([ "git"; "-C"; path ] @ args)
  in
  let checked path args =
    match git path args with
    | 0, out, _ -> Result.Ok (String.strip out)
    | _, _, err -> Result.Error err
  in
  let fail msg = Integration_error msg in
  let root = Types.Branch.to_string root_branch in
  let child = Types.Branch.to_string descendant_branch in
  let remote_root = "refs/remotes/origin/" ^ root in
  let remote_child = "refs/remotes/origin/" ^ child in
  let run () =
    match
      checked repo_root
        [
          "fetch";
          "origin";
          root ^ ":" ^ remote_root;
          child ^ ":" ^ remote_child;
        ]
    with
    | Result.Error e -> fail e
    | Result.Ok _ -> (
        match
          ( checked repo_root [ "rev-parse"; "--verify"; remote_child ],
            checked repo_root [ "rev-parse"; "--verify"; remote_root ] )
        with
        | Result.Ok observed, Result.Ok tip when String.equal observed head_sha
          -> (
            let sync sha =
              match
                ( checked root_path [ "status"; "--porcelain" ],
                  checked root_path [ "symbolic-ref"; "--short"; "HEAD" ],
                  checked root_path [ "rev-parse"; "HEAD" ] )
              with
              | Result.Ok "", Result.Ok branch, Result.Ok local
                when String.equal branch root -> (
                  if
                    not
                      (is_ancestor ~process_mgr ~path:repo_root ~ancestor:local
                         ~descendant:sha)
                  then
                    fail
                      "Root checkout diverged; refusing to discard local \
                       commits"
                  else
                    match checked root_path [ "merge"; "--ff-only"; sha ] with
                    | Result.Ok _ -> Integrated sha
                    | Result.Error e -> fail e)
              | Result.Ok _, Result.Ok _, Result.Ok _ ->
                  fail "Root checkout is dirty or on an unexpected branch"
              | Result.Error e, _, _
              | _, Result.Error e, _
              | _, _, Result.Error e ->
                  fail e
            in
            if
              is_ancestor ~process_mgr ~path:repo_root ~ancestor:head_sha
                ~descendant:tip
            then sync tip
            else
              (* Check the managed checkout before publishing, including crash recovery. *)
              match
                ( checked root_path [ "status"; "--porcelain" ],
                  checked root_path [ "symbolic-ref"; "--short"; "HEAD" ],
                  checked root_path [ "rev-parse"; "HEAD" ] )
              with
              | Result.Ok "", Result.Ok branch, Result.Ok local
                when String.equal branch root
                     && is_ancestor ~process_mgr ~path:repo_root ~ancestor:local
                          ~descendant:tip ->
                  let tmp = Stdlib.Filename.temp_file "onton-integration-" "" in
                  Unix.unlink tmp;
                  Stdlib.Fun.protect
                    ~finally:(fun () ->
                      Eio.Cancel.protect (fun () ->
                          (* The add subprocess tree has been reaped before this
                             finalizer runs. A cancelled checkout may leave
                             Git's initialization lock; this private temporary
                             worktree is safe to remove even when locked. *)
                          let _ =
                            git repo_root
                              [
                                "worktree"; "remove"; "--force"; "--force"; tmp;
                              ]
                          in
                          ()))
                    (fun () ->
                      match
                        checked repo_root
                          [ "worktree"; "add"; "--detach"; tmp; tip ]
                      with
                      | Result.Error e -> fail e
                      | Result.Ok _ -> (
                          match
                            git tmp
                              [ "merge"; "--no-ff"; "--no-edit"; head_sha ]
                          with
                          | 0, _, _ -> (
                              match checked tmp [ "rev-parse"; "HEAD" ] with
                              | Result.Error e -> fail e
                              | Result.Ok sha -> (
                                  match
                                    checked tmp
                                      [
                                        "push";
                                        "origin";
                                        "HEAD:refs/heads/" ^ root;
                                      ]
                                  with
                                  | Result.Error e -> fail e
                                  | Result.Ok _ -> sync sha))
                          | _, _, e -> (
                              match
                                checked tmp
                                  [ "diff"; "--name-only"; "--diff-filter=U" ]
                              with
                              | Result.Ok files when not (String.is_empty files)
                                ->
                                  Integration_conflict files
                              | Result.Ok _ | Result.Error _ -> fail e)))
              | Result.Ok _, Result.Ok _, Result.Ok _ ->
                  fail "Root checkout is dirty, switched, or diverged"
              | Result.Error e, _, _
              | _, Result.Error e, _
              | _, _, Result.Error e ->
                  fail e)
        | Result.Ok _, Result.Ok _ ->
            fail "Descendant head changed since its checks were observed"
        | Result.Error e, _ | _, Result.Error e -> fail e)
  in
  match Eio.Time.with_timeout clock 120. (fun () -> Result.Ok (run ())) with
  | Result.Ok r -> r
  | Result.Error `Timeout -> fail "Git integration timed out"

let merge_preserving ~process_mgr ~path ~target : rebase_result =
  let pending = read_branch_sha ~process_mgr ~path ~ref_name:"MERGE_HEAD" in
  match pending with
  (* A retry may find partially staged repair work. Keep its original merge
     target and hand the pending merge back to the agent without aborting it. *)
  | Some sha -> Merge_conflict sha
  | None -> (
      match has_uncommitted_changes ~process_mgr ~path with
      | Result.Error e -> (Error e : rebase_result)
      | Result.Ok true -> Uncommitted_changes "Root checkout has local changes"
      | Result.Ok false -> (
          let head = read_branch_sha ~process_mgr ~path ~ref_name:"HEAD" in
          let code, stdout, stderr =
            run_git_exit_code ~process_mgr
              [
                "git";
                "-C";
                path;
                "merge";
                "--no-ff";
                "--no-edit";
                Types.Branch.to_string target;
              ]
          in
          if code = 0 then
            if
              Option.equal String.equal head
                (read_branch_sha ~process_mgr ~path ~ref_name:"HEAD")
            then Noop
            else Ok
          else
            let merge_head =
              read_branch_sha ~process_mgr ~path ~ref_name:"MERGE_HEAD"
            in
            let diff_code, unmerged_paths, diff_stderr =
              run_git_exit_code ~process_mgr
                [ "git"; "-C"; path; "diff"; "--name-only"; "--diff-filter=U" ]
            in
            match
              Worktree_parser.classify_merge_failure ~code ~stdout ~stderr
                ~merge_head
                ~unmerged_paths:(if diff_code = 0 then unmerged_paths else "")
            with
            | `Conflict sha -> Merge_conflict sha
            | `Error message ->
                (* A failed index probe cannot establish that aborting is safe.
                   Preserve the pending merge for the next retry's entry check. *)
                if diff_code <> 0 then
                  let preserved_merge =
                    match merge_head with
                    | None -> ""
                    | Some sha ->
                        Printf.sprintf
                          "\n\
                           Pending root merge preserved (MERGE_HEAD %s); the \
                           next retry will route to merge repair."
                          sha
                  in
                  Error
                    (Printf.sprintf
                       "%s\nUnmerged-index probe failed (exit %d): %s%s" message
                       diff_code (String.strip diff_stderr) preserved_merge)
                else if Option.is_none merge_head then Error message
                else
                  let abort_code, _, abort_err =
                    run_git_exit_code ~process_mgr
                      [ "git"; "-C"; path; "merge"; "--abort" ]
                  in
                  if abort_code = 0 then Error message
                  else
                    Error (message ^ "\nRoot merge abort failed: " ^ abort_err))
      )

let commit_gameplan ~clock ~process_mgr ~path ~publication ~message :
    (unit, string) Result.t =
  let relative_path = Gameplan_publication.path publication in
  let content = Gameplan_publication.content publication in
  let git args =
    let code, stdout, stderr =
      run_git_exit_code ~process_mgr ([ "git"; "-C"; path ] @ args)
    in
    if code <> 0 then
      failwith
        (Printf.sprintf "git %s exited %d: %s"
           (String.concat ~sep:" " args)
           code stderr);
    stdout
  in
  try
    Eio.Time.with_timeout_exn clock 120. (fun () ->
        (* This checkout is supervisor-owned. Refuse unrelated staged or dirty
           files rather than including or discarding them in a generated commit. *)
        List.iter
          [
            [ "diff"; "--name-only"; "-z" ];
            [ "diff"; "--cached"; "--name-only"; "-z" ];
            [ "ls-files"; "--others"; "--exclude-standard"; "-z" ];
          ]
          ~f:(fun args ->
            git args |> String.split ~on:'\000'
            |> List.iter ~f:(fun entry ->
                if
                  not (String.is_empty entry || String.equal entry relative_path)
                then
                  failwith
                    ("Unrelated worktree change prevents gameplan publication: "
                   ^ entry)));
        let components = String.split relative_path ~on:'/' in
        let rec prepare parent = function
          | [] -> failwith "Empty publication path"
          | [ filename ] -> Stdlib.Filename.concat parent filename
          | dir :: rest ->
              let next = Stdlib.Filename.concat parent dir in
              (match Unix.lstat next with
              | stat when Poly.equal stat.Unix.st_kind Unix.S_DIR -> ()
              | _ ->
                  failwith
                    ("Publication directory is not a real directory: " ^ next)
              | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                  Unix.mkdir next 0o755);
              prepare next rest
        in
        let destination = prepare path components in
        (match Unix.lstat destination with
        | stat when Poly.equal stat.Unix.st_kind Unix.S_REG ->
            let existing =
              Stdlib.In_channel.with_open_bin destination
                Stdlib.In_channel.input_all
            in
            if not (String.equal existing content) then
              failwith
                ("Gameplan destination already contains different content: "
               ^ relative_path)
        | _ ->
            failwith
              ("Gameplan destination is not a regular file: " ^ relative_path)
        | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
            Stdlib.Out_channel.with_open_bin destination (fun out ->
                Stdlib.Out_channel.output_string out content));
        ignore (git [ "add"; "--"; relative_path ]);
        if
          not
            (String.is_empty
               (git [ "diff"; "--cached"; "--name-only"; "--"; relative_path ]))
        then (
          let before = String.strip (git [ "rev-parse"; "HEAD" ]) in
          ignore
            (git [ "commit"; "--only"; "-m"; message; "--"; relative_path ]);
          let committed_paths =
            git [ "diff"; "--name-only"; "-z"; before; "HEAD" ]
            |> String.split ~on:'\000'
            |> List.filter ~f:(fun path -> not (String.is_empty path))
          in
          if not (List.equal String.equal committed_paths [ relative_path ])
          then
            failwith
              "Commit hook changed files outside the gameplan; publication was \
               not pushed");
        let committed = git [ "show"; "HEAD:" ^ relative_path ] in
        if not (String.equal committed content) then
          failwith "Commit hook changed the supplied gameplan";
        Result.Ok ())
  with
  | exn when has_cancellation exn -> raise exn
  | exn -> Result.Error (Stdlib.Printexc.to_string exn)

module type S = sig
  val resolve_main_root : unit -> string
  val is_checked_out_in_repo_root : Types.Branch.t -> bool
  val remote_branch_exists : string -> bool

  val create :
    project_name:string ->
    patch_id:Types.Patch_id.t ->
    branch:Types.Branch.t ->
    base_ref:string ->
    (t, Start_point_plan.refusal) Result.t

  val fetch_origin_branch :
    fetch_lock:Eio.Mutex.t -> branch:string -> fetch_branch_result
  (** Fetch a single branch from origin into the corresponding remote-tracking
      ref. Returns [Fetch_branch_no_remote_ref] for the routine brand-new-branch
      case (no upstream yet — not a failure); [Fetch_branch_error msg] for real
      fetch failures. Caller in [Worktree_setup.ensure_worktree] runs this
      before [create] so the planner sees a fresh view of [origin/<branch>]. *)

  val remove : discard:bool -> t -> unit
  val detect_branch : path:string -> Types.Branch.t
  val list_with_branches : unit -> (string * Types.Branch.t) list
  val find_for_branch : Types.Branch.t -> string option
  val prune_stale_for_branch : Types.Branch.t -> unit

  val ensure_ready :
    path:string -> branch:Types.Branch.t -> (bool, string) Result.t

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
  val has_uncommitted_changes : path:string -> (bool, string) Result.t
  val conflict_diff : path:string -> string

  val rebase_onto :
    path:string ->
    target:Types.Branch.t ->
    upstream:string ->
    project_name:string ->
    ancestor_ids:Types.Patch_id.t list ->
    unit ->
    rebase_result

  val read_branch_sha : path:string -> ref_name:string -> string option
  (** Resolve [ref_name] to a SHA in the worktree at [path]. [None] on any error
      (missing ref, git failure). *)

  val is_ancestor : path:string -> ancestor:string -> descendant:string -> bool

  val read_in_progress_conflict_info :
    path:string ->
    target:Types.Branch.t ->
    project_name:string ->
    ancestor_ids:Types.Patch_id.t list ->
    conflict_info option

  val force_push_with_lease :
    path:string -> branch:Types.Branch.t -> base:Types.Branch.t -> push_result

  val integrate :
    root_path:string ->
    root_branch:Types.Branch.t ->
    descendant_branch:Types.Branch.t ->
    head_sha:string ->
    integration_result

  val commit_gameplan :
    path:string ->
    publication:Gameplan_publication.t ->
    message:string ->
    (unit, string) Result.t

  val rebase_in_progress : path:string -> bool
end

type client = (module S)

let make_with_protection ~protected_branch ~fs ~config ~clock ~process_mgr
    ~repo_root =
  let module B =
    (val Worktree_backend.make ~fs ~clock ~process_mgr ~repo_root ~config
           ~timeout_seconds:120.)
  in
  let ready ~path ~branch =
    Result.map (B.inspect ~path ~branch) ~f:Option.is_some
  in
  (module struct
    let resolve_main_root () = resolve_main_root ~process_mgr ~repo_root

    let is_checked_out_in_repo_root branch =
      is_checked_out_in_repo_root ~process_mgr ~repo_root branch

    let remote_branch_exists branch_str =
      remote_branch_exists ~process_mgr ~repo_root branch_str

    let create ~project_name ~patch_id ~branch ~base_ref =
      let io =
        {
          worktree_exists =
            (fun ~path ->
              match ready ~path ~branch with
              | Ok b -> b
              | Error msg -> failwith msg);
          check_ref_collision =
            (fun ~branch_str ->
              check_case_insensitive_ref_collision ~process_mgr ~repo_root
                branch_str);
          read_ref =
            (fun ~ref_name ->
              read_repo_ref_sha ~process_mgr ~repo_root ~ref_name);
          ancestry =
            (fun ~local ~remote ->
              compute_repo_ancestry ~process_mgr ~repo_root ~local ~remote);
          local_changes_represented_remotely =
            (fun ~local ~remote ->
              local_changes_represented_remotely ~process_mgr ~repo_root ~local
                ~remote);
          execute_action =
            (fun ~path ~branch_str:_ ~expected_local action ->
              let _, created =
                B.materialize ~path ~branch ~expected_local action
              in
              created);
        }
      in
      create_with_io ~io ~project_name ~patch_id ~branch ~base_ref

    let remove ~discard t =
      match B.inspect ~path:t.path ~branch:t.branch with
      | Ok (Some checkout) -> B.remove ~discard checkout
      | Ok None -> ()
      | Error msg -> failwith msg

    let detect_branch ~path = detect_branch ~process_mgr ~path
    let list_with_branches () = B.list ()

    let find_for_branch branch =
      List.find_map (B.list ()) ~f:(fun (path, b) ->
          if Types.Branch.equal branch b then Some path else None)

    let prune_stale_for_branch branch = B.prune_stale_for_branch branch
    let ensure_ready = ready

    let run_hook ~clock ~script ~cwd ~env () =
      User_config.run_hook ~process_mgr ~clock ~script ~cwd ~env ()

    let fetch_origin ~fetch_lock ~path =
      fetch_origin ~fetch_lock ~process_mgr ~path

    let fetch_origin_branch ~fetch_lock ~branch =
      fetch_origin_branch ~fetch_lock ~process_mgr ~repo_root ~branch_str:branch

    let git_status ~path = git_status ~process_mgr ~path

    let has_uncommitted_changes ~path =
      has_uncommitted_changes ~process_mgr ~path

    let conflict_diff ~path = conflict_diff ~process_mgr ~path

    let rebase_onto ~path ~target ~upstream ~project_name ~ancestor_ids () =
      if
        Option.value_map protected_branch ~default:false ~f:(fun b ->
            Types.Branch.equal b (detect_branch ~path))
      then merge_preserving ~process_mgr ~path ~target
      else
        rebase_onto ~upstream ~process_mgr ~path ~target ~project_name
          ~ancestor_ids ()

    let read_branch_sha ~path ~ref_name =
      read_branch_sha ~process_mgr ~path ~ref_name

    let is_ancestor ~path ~ancestor ~descendant =
      is_ancestor ~process_mgr ~path ~ancestor ~descendant

    let read_in_progress_conflict_info ~path ~target ~project_name ~ancestor_ids
        =
      read_in_progress_conflict_info ~process_mgr ~path ~target ~project_name
        ~ancestor_ids

    let force_push_with_lease ~path ~branch ~base =
      let preserve_history =
        Option.value_map protected_branch ~default:false
          ~f:(Types.Branch.equal branch)
      in
      force_push_with_lease ~preserve_history ~clock ~process_mgr ~path ~branch
        ~base ()

    let integrate ~root_path ~root_branch ~descendant_branch ~head_sha =
      integrate ~process_mgr ~clock ~repo_root ~root_path ~root_branch
        ~descendant_branch ~head_sha

    let commit_gameplan ~path ~publication ~message =
      commit_gameplan ~clock ~process_mgr ~path ~publication ~message

    let rebase_in_progress ~path = rebase_in_progress ~process_mgr ~path
  end : S)

let make ~fs ~config ~clock ~process_mgr ~repo_root =
  make_with_protection ~protected_branch:None ~fs ~config ~clock ~process_mgr
    ~repo_root

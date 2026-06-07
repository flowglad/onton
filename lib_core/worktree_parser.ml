(* @archlint.module core
   @archlint.domain worktree-parser *)

open Base

(** Pure parsers for git command output and pure decision functions used by
    [Worktree]. Effectful operations (process spawning, file I/O) live in
    [lib/worktree.ml]; this module owns the data types and the parse/classify
    functions that turn raw git output into typed values. *)

(** Normalize a filesystem path: resolve relative against [cwd], strip a
    trailing slash on multi-character paths, and collapse trailing ["/."]
    segments so ["foo/."] compares equal to ["foo"]. Caller threads in [cwd]
    explicitly so the function stays pure; the lib/ wrapper supplies
    [Stdlib.Sys.getcwd ()]. *)
let normalize_path ~cwd path =
  let p =
    if Stdlib.Filename.is_relative path then Stdlib.Filename.concat cwd path
    else path
  in
  let p =
    if String.length p > 1 && String.is_suffix p ~suffix:"/" then
      let stripped = String.rstrip p ~drop:(Char.equal '/') in
      if String.is_empty stripped then p else stripped
    else p
  in
  let rec strip_dot_suffix s =
    if String.length s > 2 && String.is_suffix s ~suffix:"/." then
      strip_dot_suffix (String.chop_suffix_exn s ~suffix:"/.")
    else s
  in
  strip_dot_suffix p

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

(** Find the first existing branch that case-insensitively collides with
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

(** Parse [git worktree list --porcelain] output. Pure given a pre-resolved
    [~repo_root] (absolute) and a [~cwd] used to resolve any relative paths in
    the porcelain output. Returns [(path, branch)] pairs, dropping detached-HEAD
    entries and the repo root itself. *)
let parse_porcelain ~cwd ~repo_root raw =
  let lines = String.split_lines raw in
  let repo_root = normalize_path ~cwd repo_root in
  let flush_entry acc p branch =
    let p = normalize_path ~cwd p in
    match branch with
    | None -> acc
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
              | _ -> None
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

type unique_commit = { sha : string; subject : string }
[@@deriving show, eq, sexp_of, compare]

type rebase_strategy = Onto | Plain [@@deriving show, eq, sexp_of, compare]

type conflict_info = {
  target : string;
  old_base : string;
  unique_commits : unique_commit list;
  strategy : rebase_strategy;
  orig_head : string;
}
[@@deriving show, eq, sexp_of, compare]

type rebase_result = Ok | Noop | Conflict of conflict_info | Error of string
[@@deriving show, eq, sexp_of, compare]

(** Recognize a commit subject as one of an ancestor patch's commits via the
    project-prefixed pattern [[<project>] Patch <id>:]. Used during rebase to
    drop ancestor commits before computing the [--onto] anchor. *)
let is_ancestor_patch_subject ~project_name ~ancestor_ids subject =
  if String.is_empty project_name || List.is_empty ancestor_ids then false
  else
    let prefix = Printf.sprintf "[%s] Patch " project_name in
    match String.chop_prefix subject ~prefix with
    | None -> false
    | Some rest ->
        let id_end =
          String.lfindi rest ~f:(fun _ c ->
              Char.is_whitespace c || Char.equal c ':')
        in
        let id_str =
          match id_end with
          | None -> rest
          | Some i -> String.sub rest ~pos:0 ~len:i
        in
        (not (String.is_empty id_str))
        && List.mem ancestor_ids
             (Types.Patch_id.of_string id_str)
             ~equal:Types.Patch_id.equal

(** Parse [git log --format=%H %s] output into [unique_commit] records, dropping
    entries whose subject matches an ancestor patch. Preserves git's
    newest-first emission order. *)
let classify_unique_commits ~project_name ~ancestor_ids log_output =
  let lines = String.split_lines log_output in
  let kept =
    List.filter_map lines ~f:(fun line ->
        let line = String.rstrip line ~drop:(Char.equal '\r') in
        if String.is_empty (String.strip line) then None
        else
          match String.lsplit2 line ~on:' ' with
          | None -> None
          | Some ("", _) -> None
          | Some (sha, subject) ->
              if is_ancestor_patch_subject ~project_name ~ancestor_ids subject
              then None
              else Some { sha; subject })
  in
  match List.last kept with
  | Some last -> Result.Ok (kept, last.sha)
  | None -> Result.Error "no unique commits found"

let oldest_non_ancestor_commit ~project_name ~ancestor_ids log_output =
  Result.map
    (classify_unique_commits ~project_name ~ancestor_ids log_output)
    ~f:snd

(** Assemble a [conflict_info] from the contents of [.git/rebase-merge/onto],
    [.git/rebase-merge/upstream], and [.git/rebase-merge/orig-head] together
    with [git log --format=%H %s <upstream>..<orig-head>]. Returns [None] only
    when [onto] or [upstream] is blank. *)
let parse_rebase_merge_state ~onto_contents ~upstream_contents
    ~orig_head_contents ~log_format_h_s ~project_name ~ancestor_ids ~target =
  let onto = String.strip onto_contents in
  let old_base = String.strip upstream_contents in
  let orig_head = String.strip orig_head_contents in
  if String.is_empty onto || String.is_empty old_base then None
  else
    let commits =
      match
        classify_unique_commits ~project_name ~ancestor_ids log_format_h_s
      with
      | Result.Ok (commits, _oldest_sha) -> commits
      | Result.Error _ -> []
    in
    Some
      { target; old_base; unique_commits = commits; strategy = Onto; orig_head }

(** Classify a [git fetch origin] invocation from its exit code and stderr. *)
let classify_fetch_result ~code ~stderr =
  if code = 0 then Result.Ok ()
  else
    Result.Error
      (Printf.sprintf "git fetch origin failed (exit %d): %s" code
         (String.strip stderr))

(** Outcome of a branch-scoped
    [git fetch origin <branch>:refs/remotes/origin/<branch>]. Distinguishes the
    routine "brand-new branch has no upstream yet" case from a real fetch
    failure (network, auth, ref-lock contention). The pre-create fetch in
    [Worktree_setup.ensure_worktree] always trips the no-upstream case on the
    very first creation of a patch worktree — it is the normal path, not an
    error, and callers log it calmly so it doesn't masquerade as a problem in
    the operator log. *)
type fetch_branch_result =
  | Fetch_branch_ok
  | Fetch_branch_no_remote_ref
  | Fetch_branch_error of string
[@@deriving show, eq, sexp_of, compare]

(** Pure classifier for branch-scoped fetches. The [no_remote_ref] case keys off
    git's canonical phrasing ["couldn't find remote ref"]; any other non-zero
    exit produces [Fetch_branch_error] with the exit code and stripped stderr
    embedded, mirroring [classify_fetch_result]. *)
let classify_fetch_branch_result ~code ~stderr =
  if code = 0 then Fetch_branch_ok
  else if String.is_substring stderr ~substring:"couldn't find remote ref" then
    Fetch_branch_no_remote_ref
  else
    Fetch_branch_error
      (Printf.sprintf "git fetch origin failed (exit %d): %s" code
         (String.strip stderr))

type push_result =
  | Push_ok
  | Push_up_to_date
  | Push_no_commits
  | Push_rejected of Push_reject_classify.rejection
  | Push_worktree_missing
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

(** Parse [git rev-list --count base..HEAD] output into a commit count. *)
let parse_commit_count ~code ~stdout =
  if code <> 0 then None else Stdlib.int_of_string_opt (String.strip stdout)

type push_gate = Proceed | Skip_no_commits [@@deriving show, eq, sexp_of]

(** Given a commit-count result, decide whether to push. Zero commits ahead of
    base means a push would publish an empty ref that GitHub rejects on PR
    creation — skip. Unknown ([None]) defaults to [Proceed] so real failures
    surface via the push step. *)
let push_gate_from_count = function
  | Some 0 -> Skip_no_commits
  | None | Some _ -> Proceed

(** Classify a [git push --porcelain --force-with-lease --force-if-includes]
    invocation from its exit code + stdout + stderr. *)
let classify_push_result ~code ~stdout ~stderr =
  if code = 0 then
    match parse_push_porcelain stdout with
    | Some '=' -> Push_up_to_date
    | _ -> Push_ok
  else
    match parse_push_porcelain stdout with
    | Some '!' -> Push_rejected (Push_reject_classify.classify ~stderr ~stdout)
    | _ ->
        Push_error
          (Printf.sprintf "push failed (exit %d): %s" code (String.strip stderr))

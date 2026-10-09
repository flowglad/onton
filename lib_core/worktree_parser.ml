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

(* Git's unmerged index and MERGE_HEAD identify a content conflict; an exit
   code or human-readable diagnostic alone also includes command/hook errors. *)
let classify_merge_failure ~code ~stdout ~stderr ~merge_head ~unmerged_paths =
  match merge_head with
  | Some sha
    when (not (String.is_empty (String.strip sha)))
         && not (String.is_empty (String.strip unmerged_paths)) ->
      `Conflict (String.strip sha)
  | _ ->
      let detail =
        List.filter
          [ String.strip stdout; String.strip stderr ]
          ~f:(fun s -> not (String.is_empty s))
        |> String.concat ~sep:"\n"
      in
      `Error (Printf.sprintf "Root merge failed (exit %d): %s" code detail)

(** Match a scoped dependency subject for the reconciliation owner's best-effort
    replay-boundary inference. This predicate grants no provenance. *)
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

(** Decode a single exact [ls-remote --refs] result. Reject malformed hashes,
    extra refs, and mismatched names rather than granting remote authority. *)
let parse_ls_remote_sha ~ref_name stdout =
  match String.split_lines stdout with
  | [ line ] -> (
      match String.split line ~on:'\t' with
      | [ sha; name ]
        when String.equal name ref_name
             && (String.length sha = 40 || String.length sha = 64)
             && String.for_all sha ~f:(function
               | '0' .. '9' | 'a' .. 'f' -> true
               | _ -> false) ->
          Some sha
      | _ -> None)
  | _ -> None

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
  | Push_rejected of Push_reject_classify.rejection
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
    let rejection = Push_reject_classify.classify ~stderr ~stdout in
    match parse_push_porcelain stdout with
    | Some '!' -> Push_rejected rejection
    | _ when Push_reject_classify.equal_rejection rejection Permission_denied ->
        Push_rejected rejection
    | _ ->
        Push_error
          (Printf.sprintf "push failed (exit %d): %s" code (String.strip stderr))

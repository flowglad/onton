(* @archlint.module shell
   @archlint.domain github-target *)

open Base
open Onton_core

(** Run a subprocess, capture stdout, and guarantee the pipe is closed on any
    exception path. Returns the exit status and captured output, or [None] if
    opening the pipe itself raised. *)
let read_process_capture open_ic =
  match open_ic () with
  | exception _ -> None
  | ic ->
      let status = ref None in
      Stdlib.Fun.protect
        ~finally:(fun () ->
          if Option.is_none !status then
            try ignore (Unix.close_process_in ic) with _ -> ())
        (fun () ->
          let buf = Buffer.create 128 in
          (try
             while true do
               Buffer.add_char buf (Stdlib.input_char ic)
             done
           with End_of_file -> ());
          let s = Unix.close_process_in ic in
          status := Some s;
          Some (s, Buffer.contents buf))

type process_capture = {
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let read_channel_all ic =
  let buf = Buffer.create 128 in
  (try
     while true do
       Buffer.add_char buf (Stdlib.input_char ic)
     done
   with End_of_file -> ());
  Buffer.contents buf

let close_fd_noerr fd = try Unix.close fd with _ -> ()
let unlink_noerr path = try Stdlib.Sys.remove path with _ -> ()
let iter_option opt ~f = match opt with Some x -> f x | None -> ()

let read_file_noerr path =
  match Stdlib.open_in_bin path with
  | exception _ -> ""
  | ic ->
      Stdlib.Fun.protect
        ~finally:(fun () -> Stdlib.close_in_noerr ic)
        (fun () -> read_channel_all ic)

let format_process_failure { stdout; stderr; _ } =
  let detail =
    [ stderr; stdout ]
    |> List.map ~f:(fun s -> String.strip s)
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> String.concat ~sep:"\n"
  in
  if String.is_empty detail then "no output" else detail

(** Spawn [git] (not [git -C ...]) with a clean environment, capture stdout and
    stderr. Used for [git clone] before any working tree exists. [?extra_env] is
    appended to the clean env (use to set, e.g., a custom [GIT_SSH_COMMAND] for
    one-off probes that must not prompt or hang). *)
let run_git_no_cwd ?(extra_env = []) args =
  let argv = Array.of_list ("git" :: args) in
  let env =
    let base = Git_env.clean_env () in
    if List.is_empty extra_env then base
    else Array.append base (Array.of_list extra_env)
  in
  match Stdlib.Filename.temp_file "onton-git-stderr-" ".log" with
  | exception _ -> None
  | err_path -> (
      let stdin_fd = ref None in
      let stdout_rd = ref None in
      let stdout_wr = ref None in
      let stdout_ic = ref None in
      let err_fd = ref None in
      let pid = ref None in
      match
        Stdlib.Fun.protect
          ~finally:(fun () ->
            iter_option !stdout_ic ~f:Stdlib.close_in_noerr;
            iter_option !stdout_rd ~f:close_fd_noerr;
            iter_option !stdout_wr ~f:close_fd_noerr;
            iter_option !stdin_fd ~f:close_fd_noerr;
            iter_option !err_fd ~f:close_fd_noerr;
            iter_option !pid ~f:(fun p ->
                try ignore (Unix.waitpid [] p) with _ -> ());
            unlink_noerr err_path)
          (fun () ->
            let stdin = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
            stdin_fd := Some stdin;
            let rd, wr = Unix.pipe () in
            stdout_rd := Some rd;
            stdout_wr := Some wr;
            let err =
              Unix.openfile err_path
                [ Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CREAT ]
                0o600
            in
            err_fd := Some err;
            let child = Unix.create_process_env "git" argv env stdin wr err in
            pid := Some child;
            close_fd_noerr stdin;
            stdin_fd := None;
            close_fd_noerr wr;
            stdout_wr := None;
            close_fd_noerr err;
            err_fd := None;
            let ic = Unix.in_channel_of_descr rd in
            stdout_rd := None;
            stdout_ic := Some ic;
            let stdout = read_channel_all ic in
            Stdlib.close_in_noerr ic;
            stdout_ic := None;
            let _, status = Unix.waitpid [] child in
            pid := None;
            let stderr = read_file_noerr err_path in
            Some { status; stdout; stderr })
      with
      | result -> result
      | exception _ -> None)

(** Read all [remote.<name>.url] entries from a clone at [path]. Best effort:
    returns the empty list on any IO error or if [path] is not a git checkout.
    Used to inspect an existing managed clone's [origin] so we don't flip
    transports mid-life. *)
let read_remote_urls ~path =
  let git_dir = Stdlib.Filename.concat path ".git" in
  let is_git =
    try
      Stdlib.Sys.file_exists git_dir
      && (Stdlib.Sys.is_directory git_dir
         (* Worktrees have a .git file pointing at gitdir, not a directory.
            We can still ask git itself via remote -v but only when the .git
            entry exists in some form. *)
         ||
           try
             let st = Unix.lstat git_dir in
             match st.Unix.st_kind with
             | Unix.S_REG -> true
             | Unix.S_DIR | Unix.S_LNK | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO
             | Unix.S_SOCK ->
                 false
           with _ -> false)
    with _ -> false
  in
  if not is_git then []
  else
    match
      read_process_capture (fun () ->
          Unix.open_process_args_in "git"
            [| "git"; "-C"; path; "remote"; "-v" |])
    with
    | Some (Unix.WEXITED 0, out) ->
        String.split_lines out
        |> List.filter_map ~f:(fun line ->
            (* Each line is "<name>\t<url> (fetch|push)". Keep the URL field. *)
            match String.split line ~on:'\t' with
            | _ :: rest :: _ -> (
                match String.split rest ~on:' ' with
                | url :: _ -> Some url
                | [] -> None)
            | _ -> None)
        |> List.dedup_and_sort ~compare:String.compare
    | Some (Unix.WEXITED _, _)
    | Some (Unix.WSIGNALED _, _)
    | Some (Unix.WSTOPPED _, _)
    | None ->
        []

(** Probe whether SSH is usable for [owner/repo] from the current machine. Runs
    [git ls-remote -h git@github.com:owner/repo.git] with [BatchMode=yes] and
    [ConnectTimeout=5], so a missing or passphrase-locked SSH key, an
    unauthorized key, or a blocked outbound port all produce a clean [false]
    without prompting or hanging. Exit code 0 means SSH can both authenticate to
    GitHub and read this specific repo — which is also the condition for SSH
    push to work. *)
let probe_ssh_available ~owner ~repo =
  let url = Github_target.clone_url ~scheme:Github_target.Ssh ~owner ~repo in
  let extra_env =
    [ "GIT_SSH_COMMAND=ssh -o BatchMode=yes -o ConnectTimeout=5" ]
  in
  match run_git_no_cwd ~extra_env [ "ls-remote"; "-h"; url ] with
  | Some { status = Unix.WEXITED 0; _ } -> true
  | Some { status = Unix.WEXITED _; _ }
  | Some { status = Unix.WSIGNALED _; _ }
  | Some { status = Unix.WSTOPPED _; _ }
  | None ->
      false

(** Clone [owner/repo] from GitHub into [target_dir] (which must not exist yet).
    Authentication piggybacks on [Git_env.clean_env]'s [GIT_ASKPASS] helper
    (HTTPS) or the user's ssh-agent (SSH). For private repos with HTTPS,
    [Git_env.set_github_token] must have been called first. Uses
    [--filter=blob:none] for a partial clone — only fetched objects are
    downloaded lazily, which is the right tradeoff for onton's worktree-per-
    patch usage pattern. *)
let clone_managed_repo ~scheme ~owner ~repo ~target_dir =
  Project_store.ensure_dir (Stdlib.Filename.dirname target_dir);
  let url = Github_target.clone_url ~scheme ~owner ~repo in
  match run_git_no_cwd [ "clone"; "--filter=blob:none"; url; target_dir ] with
  | Some { status = Unix.WEXITED 0; _ } -> Ok ()
  | Some ({ status = Unix.WEXITED _; _ } as cap)
  | Some ({ status = Unix.WSIGNALED _; _ } as cap)
  | Some ({ status = Unix.WSTOPPED _; _ } as cap) ->
      Error
        (Printf.sprintf "git clone %s/%s failed: %s" owner repo
           (format_process_failure cap))
  | None -> Error (Printf.sprintf "git clone %s/%s: could not spawn" owner repo)

(** Resolve the transport scheme for the managed clone. Pure decision lives in
    [Github_target.resolve_scheme]; here we supply the effectful input (an SSH
    reachability probe) when no [override] is provided. The probe is skipped
    when the caller already has an override — either a [--clone-scheme] flag or
    a previously-persisted [url_scheme] from [config.json] — since the answer is
    fixed. An informational stderr line announces the auto-detected choice so
    the user can see why their managed clone landed on SSH (or fell back to
    HTTPS). *)
let resolve_clone_scheme ?(override : Github_target.url_scheme option = None)
    ~owner ~repo () =
  let ssh_available =
    match override with
    | Some _ -> false
    | None -> probe_ssh_available ~owner ~repo
  in
  let scheme = Github_target.resolve_scheme ~override ~ssh_available in
  (match (override, scheme) with
  | None, Github_target.Ssh ->
      Stdlib.Printf.eprintf
        "onton: SSH probe to git@github.com succeeded — cloning managed repo \
         via SSH\n\
         %!"
  | None, Github_target.Https ->
      Stdlib.Printf.eprintf
        "onton: SSH probe to git@github.com unavailable — cloning managed repo \
         via HTTPS\n\
         %!"
  | Some _, _ -> ());
  scheme

let string_of_url_scheme = function
  | Github_target.Https -> "https"
  | Github_target.Ssh -> "ssh"

let url_scheme_of_string = function
  | "ssh" -> Some Github_target.Ssh
  | "https" -> Some Github_target.Https
  | _ -> None

(** Ensure the onton-managed checkout for [project_name] is present and
    up-to-date. Clones if absent, fetches if present. Authenticates via
    [Git_env]'s configured token (HTTPS) or the user's ssh-agent (SSH).

    [?clone_scheme]: explicit override (e.g. from a [--clone-scheme] flag or a
    [url_scheme] field already in [config.json]). If [None], auto-detect by
    probing SSH reachability to GitHub — see {!resolve_clone_scheme}. The
    resolved scheme is returned together with the repo root so the caller can
    persist it back to [config.json] on subsequent runs. *)
let ensure_managed_repo ?(clone_scheme = None) ~project_name ~token ~owner ~repo
    () =
  Git_env.set_github_token token;
  let repo_root = Project_store.managed_repo_dir project_name in
  let git_dir = Stdlib.Filename.concat repo_root ".git" in
  let repo_root_exists = Stdlib.Sys.file_exists repo_root in
  let has_git_dir =
    try Stdlib.Sys.file_exists git_dir && Stdlib.Sys.is_directory git_dir
    with _ -> false
  in
  let repo_root_is_dir =
    try Stdlib.Sys.is_directory repo_root with _ -> false
  in
  let repo_root_emptiness =
    if repo_root_exists && repo_root_is_dir then
      try Ok (Array.length (Stdlib.Sys.readdir repo_root) = 0)
      with exn ->
        Error
          (Printf.sprintf
             "Managed checkout path %s exists but could not be inspected: %s"
             repo_root (Base.Exn.to_string exn))
    else Ok false
  in
  (* The SSH probe is only run on the clone path. When the managed clone
     already exists on disk, the on-disk [origin] URL is the source of
     truth, so probing would do a 5s network call whose result we'd
     immediately discard — and the accompanying "cloning managed repo via X"
     log line would be a lie. *)
  let clone_with_probe ~target_dir =
    let scheme = resolve_clone_scheme ~override:clone_scheme ~owner ~repo () in
    match clone_managed_repo ~scheme ~owner ~repo ~target_dir with
    | Ok () -> Ok (repo_root, scheme)
    | Error msg -> Error msg
  in
  if repo_root_exists && has_git_dir then (
    let module Repo = (val Repo_git.make ~repo_root) in
    (* If the managed clone already exists, the existing [origin] URL is
       authoritative — we don't change transport mid-life. Reflect that in
       the returned scheme so callers persist the matching value. The
       fallback when no parseable URL is found prefers the caller's override
       (a [--clone-scheme] flag or persisted scheme), defaulting to HTTPS. *)
    let existing_scheme =
      match read_remote_urls ~path:repo_root with
      | urls ->
          List.find_map urls ~f:Github_target.scheme_of_url
          |> Option.value
               ~default:(Option.value clone_scheme ~default:Github_target.Https)
    in
    match Repo.fetch_managed_repo () with
    | Ok () -> Ok (repo_root, existing_scheme)
    | Error msg ->
        (* Log to stderr but don't abort — the user may be offline and the
           local clone may already have everything they need. *)
        Stdlib.Printf.eprintf
          "onton: warning: %s (continuing with existing local clone)\n%!" msg;
        Ok (repo_root, existing_scheme))
  else if repo_root_exists && not repo_root_is_dir then
    Error
      (Printf.sprintf
         "Managed checkout path %s exists but is not a directory; remove it \
          and retry"
         repo_root)
  else if repo_root_exists then
    match repo_root_emptiness with
    | Error msg -> Error msg
    | Ok false ->
        Error
          (Printf.sprintf
             "Managed checkout path %s exists but is not a git checkout and is \
              not empty; remove it and retry"
             repo_root)
    | Ok true -> clone_with_probe ~target_dir:repo_root
  else clone_with_probe ~target_dir:repo_root

(** Resolve GitHub token: check GITHUB_TOKEN env var, then try [gh auth token].
    Uses argv (no shell). *)
let infer_github_token () =
  match Stdlib.Sys.getenv_opt "GITHUB_TOKEN" with
  | Some t when not (String.is_empty (String.strip t)) -> String.strip t
  | _ -> (
      try
        match
          read_process_capture (fun () ->
              Unix.open_process_args_in "gh" [| "gh"; "auth"; "token" |])
        with
        | Some (Unix.WEXITED 0, out) ->
            let t = String.strip out in
            if String.is_empty t then "" else t
        | Some (Unix.WEXITED _, _)
        | Some (Unix.WSIGNALED _, _)
        | Some (Unix.WSTOPPED _, _)
        | None ->
            ""
      with _ -> "")

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
    stderr. Used for [git clone] before any working tree exists. *)
let run_git_no_cwd args =
  let argv = Array.of_list ("git" :: args) in
  let env = Git_env.clean_env () in
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
              Unix.openfile err_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
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

(** Clone [owner/repo] from GitHub into [target_dir] (which must not exist yet).
    Authentication piggybacks on [Git_env.clean_env]'s [GIT_ASKPASS] helper, so
    [Git_env.set_github_token] must have been called first if the repo is
    private. Uses [--filter=blob:none] for a partial clone — only fetched
    objects are downloaded lazily, which is the right tradeoff for onton's
    worktree-per-patch usage pattern. *)
let clone_managed_repo ~owner ~repo ~target_dir =
  Project_store.ensure_dir (Stdlib.Filename.dirname target_dir);
  let url = Github_target.clone_url ~owner ~repo in
  match run_git_no_cwd [ "clone"; "--filter=blob:none"; url; target_dir ] with
  | Some { status = Unix.WEXITED 0; _ } -> Ok ()
  | Some ({ status = Unix.WEXITED _; _ } as cap)
  | Some ({ status = Unix.WSIGNALED _; _ } as cap)
  | Some ({ status = Unix.WSTOPPED _; _ } as cap) ->
      Error
        (Printf.sprintf "git clone %s/%s failed: %s" owner repo
           (format_process_failure cap))
  | None -> Error (Printf.sprintf "git clone %s/%s: could not spawn" owner repo)

(** Ensure the onton-managed checkout for [project_name] is present and
    up-to-date. Clones if absent, fetches if present. Authenticates via
    [Git_env]'s configured token. *)
let ensure_managed_repo ~project_name ~token ~owner ~repo =
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
  let repo_root_is_empty =
    try Array.length (Stdlib.Sys.readdir repo_root) = 0 with _ -> false
  in
  if repo_root_exists && has_git_dir then (
    let module Repo = (val Repo_git.make ~repo_root) in
    match Repo.fetch_managed_repo () with
    | Ok () -> Ok repo_root
    | Error msg ->
        (* Log to stderr but don't abort — the user may be offline and the
           local clone may already have everything they need. *)
        Stdlib.Printf.eprintf
          "onton: warning: %s (continuing with existing local clone)\n%!" msg;
        Ok repo_root)
  else if repo_root_exists && not repo_root_is_dir then
    Error
      (Printf.sprintf
         "Managed checkout path %s exists but is not a directory; remove it \
          and retry"
         repo_root)
  else if repo_root_exists && not repo_root_is_empty then
    Error
      (Printf.sprintf
         "Managed checkout path %s exists but is not a git checkout and is not \
          empty; remove it and retry"
         repo_root)
  else
    match clone_managed_repo ~owner ~repo ~target_dir:repo_root with
    | Ok () -> Ok repo_root
    | Error msg -> Error msg

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

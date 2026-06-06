(* @archlint.module shell
   @archlint.domain orchestrator *)

open Onton_core

type process_capture = {
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let read_channel_all ic =
  let buf = Buffer.create 128 in
  (try
     while true do
       Buffer.add_char buf (input_char ic)
     done
   with End_of_file -> ());
  Buffer.contents buf

(** Run [git -C repo_root ...] using argv rather than the shell, capture stdout
    and stderr, and close all process pipes on exception paths. *)
let run_git_capture ~repo_root args =
  let argv = Array.of_list ("git" :: "-C" :: repo_root :: args) in
  let env = Git_env.clean_env () in
  match Unix.open_process_args_full "git" argv env with
  | exception _ -> None
  | in_ch, out_ch, err_ch ->
      let status = ref None in
      Stdlib.Fun.protect
        ~finally:(fun () ->
          if Base.Option.is_none !status then
            try ignore (Unix.close_process_full (in_ch, out_ch, err_ch))
            with _ -> ())
        (fun () ->
          close_out_noerr out_ch;
          let stdout = read_channel_all in_ch in
          let stderr = read_channel_all err_ch in
          let s = Unix.close_process_full (in_ch, out_ch, err_ch) in
          status := Some s;
          Some { status = s; stdout; stderr })

let git_stdout ~repo_root args =
  match run_git_capture ~repo_root args with
  | Some { status = Unix.WEXITED 0; stdout; _ } ->
      Some (Base.String.strip stdout)
  | Some { status = Unix.WEXITED _; _ }
  | Some { status = Unix.WSIGNALED _; _ }
  | Some { status = Unix.WSTOPPED _; _ }
  | None ->
      None

let git_success ~repo_root args =
  Base.Option.is_some (git_stdout ~repo_root args)

let format_git_failure { stdout; stderr; _ } =
  let detail =
    [ stderr; stdout ]
    |> Base.List.map ~f:(fun s -> Base.String.strip s)
    |> Base.List.filter ~f:(fun s -> not (Base.String.is_empty s))
    |> Stdlib.String.concat "\n"
  in
  if Base.String.is_empty detail then "no output" else detail

module type S = sig
  val infer_owner_repo : unit -> (string * string) option
  val fetch_managed_repo : unit -> (unit, string) Result.t
  val infer_default_branch : unit -> Types.Branch.t

  val validate_branch_resolves :
    main_branch:Types.Branch.t -> (unit, string) Result.t
end

type client = (module S)

let make ~repo_root =
  (module struct
    (** Infer the forge owner/repo from [git remote get-url origin] in
        [repo_root]. Delegates URL parsing to
        {!Onton_core.Github_target.infer_owner_repo_from_url} so the regex that
        pins the host stays in the forge module. *)
    let infer_owner_repo () =
      try
        match run_git_capture ~repo_root [ "remote"; "get-url"; "origin" ] with
        | Some { status = Unix.WEXITED 0; stdout; _ } ->
            Github_target.infer_owner_repo_from_url stdout
        | Some { status = Unix.WEXITED _; _ }
        | Some { status = Unix.WSIGNALED _; _ }
        | Some { status = Unix.WSTOPPED _; _ }
        | None ->
            None
      with _ -> None

    (** Fetch the [origin] remote in an existing onton-managed repo. Best-effort
        -- a network-down resume should not fail the whole session. *)
    let fetch_managed_repo () =
      match run_git_capture ~repo_root [ "fetch"; "--prune"; "origin" ] with
      | Some { status = Unix.WEXITED 0; _ } -> Ok ()
      | Some ({ status = Unix.WEXITED _; _ } as cap)
      | Some ({ status = Unix.WSIGNALED _; _ } as cap)
      | Some ({ status = Unix.WSTOPPED _; _ } as cap) ->
          Error
            (Printf.sprintf "git fetch in %s failed: %s" repo_root
               (format_git_failure cap))
      | None ->
          Error (Printf.sprintf "git fetch in %s: could not spawn" repo_root)

    (** Detect the default branch of a git repository. Tries: 1.
        [git symbolic-ref refs/remotes/origin/HEAD], verifying the target
        tracking ref actually resolves (origin/HEAD is set at clone time and is
        NOT refreshed by [git fetch] -- a stale value can point at a branch that
        has since been renamed or deleted upstream). 2.
        [git rev-parse --verify refs/heads/main] -> "main" 3.
        [git rev-parse --verify refs/heads/master] -> "master" 4. Fallback:
        "main" *)
    let infer_default_branch () =
      let resolves ref_name =
        git_success ~repo_root [ "rev-parse"; "--verify"; ref_name ]
      in
      let head_probes () =
        if resolves "refs/heads/main" then Types.Branch.of_string "main"
        else if resolves "refs/heads/master" then
          Types.Branch.of_string "master"
        else Types.Branch.of_string "main"
      in
      match
        git_stdout ~repo_root [ "symbolic-ref"; "refs/remotes/origin/HEAD" ]
      with
      | Some ref_path ->
          let prefix = "refs/remotes/origin/" in
          let candidate =
            if Base.String.is_prefix ref_path ~prefix then
              Base.String.chop_prefix_exn ref_path ~prefix
            else ref_path
          in
          if resolves (prefix ^ candidate) then Types.Branch.of_string candidate
          else head_probes ()
      | None -> head_probes ()

    (** Verify that the configured [main_branch] resolves as
        [refs/remotes/origin/<branch>] in the local clone. If the local tracking
        ref is missing, attempt one [git fetch origin] and re-check. *)
    let validate_branch_resolves ~main_branch =
      let branch_str = Types.Branch.to_string main_branch in
      let ref_name = "refs/remotes/origin/" ^ branch_str in
      let resolves () =
        git_success ~repo_root [ "rev-parse"; "--verify"; ref_name ]
      in
      if resolves () then Ok ()
      else
        let () =
          Printf.eprintf "onton: fetching origin to verify branch...\n%!"
        in
        match run_git_capture ~repo_root [ "fetch"; "origin"; "--quiet" ] with
        | Some { status = Unix.WEXITED 0; _ } ->
            if resolves () then Ok ()
            else
              Error
                (Printf.sprintf
                   "configured main branch %S does not resolve as origin/%s in \
                    %s\n\
                   \  (the branch may have been renamed or deleted upstream, \
                    or the local clone has not fetched it).\n\
                   \  Refresh the local default and retry:\n\
                   \    git -C %s remote set-head origin -a\n\
                   \    git -C %s fetch --prune\n\
                   \  Or override at launch with --main-branch <name>."
                   branch_str branch_str repo_root repo_root repo_root)
        | Some ({ status = Unix.WEXITED _; _ } as failed)
        | Some ({ status = Unix.WSIGNALED _; _ } as failed)
        | Some ({ status = Unix.WSTOPPED _; _ } as failed) ->
            Error
              (Printf.sprintf
                 "configured main branch %S does not resolve as origin/%s in \
                  %s, and git fetch origin failed:\n\
                 \  %s\n\
                 \  Refresh the local default and retry:\n\
                 \    git -C %s remote set-head origin -a\n\
                 \    git -C %s fetch --prune\n\
                 \  Or override at launch with --main-branch <name>."
                 branch_str branch_str repo_root
                 (format_git_failure failed)
                 repo_root repo_root)
        | None ->
            Error
              (Printf.sprintf
                 "configured main branch %S does not resolve as origin/%s in \
                  %s, and git fetch origin could not be started.\n\
                 \  Refresh the local default and retry:\n\
                 \    git -C %s remote set-head origin -a\n\
                 \    git -C %s fetch --prune\n\
                 \  Or override at launch with --main-branch <name>."
                 branch_str branch_str repo_root repo_root repo_root)
  end : S)

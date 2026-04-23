open Base

(** Run [git -C path rev-parse --path-format=absolute --git-common-dir] and
    return the parent directory of the reported common dir — i.e. the main
    working tree. Returns [None] if [path] is not inside a git repository or the
    command fails for any reason. *)
let resolve_main_working_tree path =
  let env = Unix.environment () in
  let argv =
    [|
      "git";
      "-C";
      path;
      "rev-parse";
      "--path-format=absolute";
      "--git-common-dir";
    |]
  in
  match Unix.open_process_args_full "git" argv env with
  | exception _ -> None
  | in_ch, out_ch, err_ch -> (
      let buf = Buffer.create 128 in
      let status = ref None in
      Stdlib.Fun.protect
        ~finally:(fun () ->
          if Option.is_none !status then
            try ignore (Unix.close_process_full (in_ch, out_ch, err_ch))
            with _ -> ())
        (fun () ->
          (try
             while true do
               Buffer.add_char buf (Stdlib.input_char in_ch)
             done
           with End_of_file -> ());
          (* Drain stderr to keep it off the terminal (git prints
             "not a git repository" when [path] is outside any repo). *)
          (try
             while true do
               ignore (Stdlib.input_char err_ch)
             done
           with End_of_file -> ());
          status := Some (Unix.close_process_full (in_ch, out_ch, err_ch)));
      match !status with
      | Some (Unix.WEXITED 0) ->
          let common_dir = String.strip (Buffer.contents buf) in
          if String.is_empty common_dir then None
          else
            let parent = Stdlib.Filename.dirname common_dir in
            (* [--path-format=absolute] requires git >= 2.31; older git
               silently ignores the flag and returns a path relative to the
               worktree's CWD. Treat that as failure rather than resolving
               against the worktree and re-introducing the very bug this
               module is meant to fix. *)
            if Stdlib.Filename.is_relative parent then None else Some parent
      | Some (Unix.WEXITED _)
      | Some (Unix.WSIGNALED _)
      | Some (Unix.WSTOPPED _)
      | None ->
          None)

let normalize rr =
  let absolute =
    if Stdlib.Filename.is_relative rr then
      Stdlib.Filename.concat (Stdlib.Sys.getcwd ()) rr
    else rr
  in
  let normalized = Worktree.normalize_path absolute in
  match resolve_main_working_tree normalized with
  | Some main -> Worktree.normalize_path main
  | None -> normalized

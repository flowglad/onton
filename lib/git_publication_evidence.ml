(* @archlint.module shell
   @archlint.domain rewrite-lineage *)

open Base

exception Probe_failed of string

let rewrite_authority ~git ~branch ~local_sha ~remote_sha =
  let checked args =
    let code, stdout, stderr = git args in
    if code = 0 then stdout else raise (Probe_failed stderr)
  in
  let ancestor sha ~descendant =
    let code, _, stderr =
      git [ "merge-base"; "--is-ancestor"; sha; descendant ]
    in
    match code with 0 -> true | 1 -> false | _ -> raise (Probe_failed stderr)
  in
  let changed ?(deletions_only = false) before after =
    checked
      [
        "diff";
        "--no-ext-diff";
        "--no-textconv";
        "--no-renames";
        "--ignore-submodules=none";
        "--name-only";
        "-z";
        (if deletions_only then "--diff-filter=D" else "--diff-filter=ACDMRTUXB");
        before;
        after;
        "--";
      ]
  in
  try
    let path =
      checked
        [
          "rev-parse";
          "--path-format=absolute";
          "--git-path";
          "logs/refs/heads/" ^ branch;
        ]
      |> String.strip
    in
    let reflog =
      try
        let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
        let ic = Unix.in_channel_of_descr fd in
        Some
          (Stdlib.Fun.protect
             ~finally:(fun () -> Stdlib.close_in_noerr ic)
             (fun () -> Stdlib.In_channel.input_all ic))
      with Unix.Unix_error (Unix.ENOENT, _, _) -> None
    in
    Ok
      (Option.bind reflog ~f:(fun reflog ->
           Rewrite_lineage.of_reflog ~branch ~local_sha ~remote_sha ~reflog
             ~ancestor_oracle:ancestor
             ~content_oracle:(fun ~remote_sha ~target ~result_sha ->
               let bases =
                 let code, stdout, stderr =
                   git [ "merge-base"; "--all"; remote_sha; target ]
                 in
                 match code with
                 | 0 -> String.split_lines stdout
                 | 1 -> []
                 | _ -> raise (Probe_failed stderr)
               in
               match bases with
               | [ common ] when not (String.is_empty common) ->
                   Rewrite_lineage.changes_preserved
                     ~remote_changed_paths:(changed common remote_sha)
                     ~local_changed_paths:(changed remote_sha result_sha)
                     ~target_deleted_paths:
                       (changed ~deletions_only:true common target)
                     ~local_deleted_paths:
                       (changed ~deletions_only:true remote_sha result_sha)
               | _ -> false)))
  with
  | Probe_failed reason -> Error reason
  | Unix.Unix_error (error, call, path) ->
      Error (Printf.sprintf "%s %s: %s" call path (Unix.error_message error))
  | Sys_error reason -> Error reason

open Base

type t = { on_worktree_create : string option }

let config_dir ~github_owner ~github_repo =
  let home = Stdlib.Sys.getenv "HOME" in
  Stdlib.Filename.concat
    (Stdlib.Filename.concat
       (Stdlib.Filename.concat home ".config/onton")
       github_owner)
    github_repo

let load ~github_owner ~github_repo =
  let dir = config_dir ~github_owner ~github_repo in
  let script_path = Stdlib.Filename.concat dir "on_worktree_create" in
  let on_worktree_create =
    if Stdlib.Sys.file_exists script_path then Some script_path else None
  in
  { on_worktree_create }

let run_hook ~process_mgr ~script ~cwd ~env =
  let stderr_buf = Buffer.create 256 in
  try
    let env_array =
      Array.of_list (List.map env ~f:(fun (k, v) -> Printf.sprintf "%s=%s" k v))
    in
    let inherited = Unix.environment () |> Array.to_list in
    let merged =
      Array.of_list (List.append inherited (Array.to_list env_array))
    in
    Eio.Process.run process_mgr ~env:merged ~cwd
      ~stderr:(Eio.Flow.buffer_sink stderr_buf)
      [ script ];
    Ok ()
  with exn ->
    let stderr = String.strip (Buffer.contents stderr_buf) in
    let msg = Stdlib.Printexc.to_string exn in
    if String.is_empty stderr then Error msg
    else Error (Printf.sprintf "%s\nstderr: %s" msg stderr)

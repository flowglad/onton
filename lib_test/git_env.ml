open Base

let clean_env = Onton.Git_env.clean_env

let read_all fd =
  let buf = Buffer.create 256 in
  let bytes = Bytes.create 4096 in
  let rec loop () =
    match Unix.read fd bytes 0 (Bytes.length bytes) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes buf bytes ~pos:0 ~len:n;
        loop ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ();
  Buffer.contents buf

let run_git ~cwd args =
  let argv = Array.of_list ("git" :: "-C" :: cwd :: args) in
  let env = clean_env () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  let devnull =
    match Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 with
    | fd -> fd
    | exception exn ->
        (try Unix.close stderr_r with _ -> ());
        (try Unix.close stderr_w with _ -> ());
        raise exn
  in
  let stderr_buf, status =
    Stdlib.Fun.protect
      ~finally:(fun () -> try Unix.close stderr_r with _ -> ())
      (fun () ->
        let pid =
          Stdlib.Fun.protect
            ~finally:(fun () ->
              (try Unix.close stderr_w with _ -> ());
              try Unix.close devnull with _ -> ())
            (fun () ->
              Unix.create_process_env "git" argv env devnull devnull stderr_w)
        in
        let stderr_buf = read_all stderr_r in
        let rec waitpid () =
          match Unix.waitpid [] pid with
          | result -> result
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid ()
        in
        let _, status = waitpid () in
        (stderr_buf, status))
  in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      Stdlib.failwith
        (Printf.sprintf "git %s in %s: exit %d\nstderr: %s"
           (String.concat ~sep:" " args)
           cwd n (String.strip stderr_buf))
  | Unix.WSIGNALED n ->
      Stdlib.failwith
        (Printf.sprintf "git %s in %s: signaled %d\nstderr: %s"
           (String.concat ~sep:" " args)
           cwd n (String.strip stderr_buf))
  | Unix.WSTOPPED n ->
      Stdlib.failwith
        (Printf.sprintf "git %s in %s: stopped %d\nstderr: %s"
           (String.concat ~sep:" " args)
           cwd n (String.strip stderr_buf))

(* [git -C cwd args] returning trimmed stdout, spawned with {!clean_env} so no
   inherited [GIT_*] var (e.g. a pre-commit hook's [GIT_DIR]/[GIT_INDEX_FILE])
   can redirect it off [cwd]. Raises on nonzero exit, reporting stderr. *)
let git_capture ~cwd args =
  let argv = Array.of_list ("git" :: "-C" :: cwd :: args) in
  let env = clean_env () in
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  let devnull =
    match Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 with
    | fd -> fd
    | exception exn ->
        List.iter
          ~f:(fun fd -> try Unix.close fd with _ -> ())
          [ stdout_r; stdout_w; stderr_r; stderr_w ];
        raise exn
  in
  let out_buf, err_buf, status =
    Stdlib.Fun.protect
      ~finally:(fun () ->
        (try Unix.close stdout_r with _ -> ());
        try Unix.close stderr_r with _ -> ())
      (fun () ->
        let pid =
          Stdlib.Fun.protect
            ~finally:(fun () ->
              (try Unix.close stdout_w with _ -> ());
              (try Unix.close stderr_w with _ -> ());
              try Unix.close devnull with _ -> ())
            (fun () ->
              Unix.create_process_env "git" argv env devnull stdout_w stderr_w)
        in
        (* Drain stdout (then stderr) before reaping. git writes its small
           output and exits, closing both ends, so this cannot deadlock. *)
        let out_buf = read_all stdout_r in
        let err_buf = read_all stderr_r in
        let rec waitpid () =
          match Unix.waitpid [] pid with
          | result -> result
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid ()
        in
        let _, status = waitpid () in
        (out_buf, err_buf, status))
  in
  match status with
  | Unix.WEXITED 0 -> String.strip out_buf
  | Unix.WEXITED n ->
      Stdlib.failwith
        (Printf.sprintf "git %s in %s: exit %d\nstderr: %s"
           (String.concat ~sep:" " args)
           cwd n (String.strip err_buf))
  | Unix.WSIGNALED n ->
      Stdlib.failwith
        (Printf.sprintf "git %s in %s: signaled %d"
           (String.concat ~sep:" " args)
           cwd n)
  | Unix.WSTOPPED n ->
      Stdlib.failwith
        (Printf.sprintf "git %s in %s: stopped %d"
           (String.concat ~sep:" " args)
           cwd n)

(* Like {!run_git} but returns the process exit code instead of raising on a
   nonzero status (still raises if the process is signaled). For git queries
   whose exit code is the answer, e.g. [merge-base --is-ancestor]. Output is
   discarded. Spawned with {!clean_env}. *)
let git_exit_code ~cwd args =
  let argv = Array.of_list ("git" :: "-C" :: cwd :: args) in
  let env = clean_env () in
  let devnull_in = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let devnull_out = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  let pid =
    Stdlib.Fun.protect
      ~finally:(fun () ->
        (try Unix.close devnull_in with _ -> ());
        try Unix.close devnull_out with _ -> ())
      (fun () ->
        Unix.create_process_env "git" argv env devnull_in devnull_out
          devnull_out)
  in
  let rec waitpid () =
    match Unix.waitpid [] pid with
    | result -> result
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid ()
  in
  match snd (waitpid ()) with
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n | Unix.WSTOPPED n ->
      Stdlib.failwith
        (Printf.sprintf "git %s in %s: terminated by signal %d"
           (String.concat ~sep:" " args)
           cwd n)

(* Run a shell command in [dir] with the scrubbed git environment. Covers the
   non-git shell steps in integration fixtures (e.g. [echo base > README.md],
   [git clone ...]) without letting an inherited [GIT_*] var leak into any git
   the command invokes. stdout/stderr pass through; raises on nonzero exit. *)
let sh ~dir cmd =
  let full = Printf.sprintf "cd %s && %s" (Stdlib.Filename.quote dir) cmd in
  let argv = [| "/bin/sh"; "-c"; full |] in
  let env = clean_env () in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let pid =
    Stdlib.Fun.protect
      ~finally:(fun () -> try Unix.close devnull with _ -> ())
      (fun () ->
        Unix.create_process_env "/bin/sh" argv env devnull Unix.stdout
          Unix.stderr)
  in
  let rec waitpid () =
    match Unix.waitpid [] pid with
    | result -> result
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid ()
  in
  match snd (waitpid ()) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      Stdlib.failwith (Printf.sprintf "command failed (exit %d): %s" n full)
  | Unix.WSIGNALED n | Unix.WSTOPPED n ->
      Stdlib.failwith (Printf.sprintf "command killed (signal %d): %s" n full)

let init_repo dir =
  run_git ~cwd:dir [ "init"; "-q"; "-b"; "main" ];
  run_git ~cwd:dir [ "config"; "user.email"; "test@example.com" ];
  run_git ~cwd:dir [ "config"; "user.name"; "test" ];
  run_git ~cwd:dir [ "config"; "commit.gpgsign"; "false" ]

let rm_rf dir =
  let cmd = Printf.sprintf "rm -rf %s" (Stdlib.Filename.quote dir) in
  ignore (Stdlib.Sys.command cmd : int)

let with_temp_repo f =
  let dir = Stdlib.Filename.temp_dir "onton-test-repo-" "" in
  Stdlib.Fun.protect
    ~finally:(fun () -> rm_rf dir)
    (fun () ->
      init_repo dir;
      f dir)

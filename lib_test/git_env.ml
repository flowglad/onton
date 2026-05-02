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

let init_repo dir =
  run_git ~cwd:dir [ "init"; "-q"; "-b"; "main" ];
  run_git ~cwd:dir [ "config"; "user.email"; "test@example.com" ];
  run_git ~cwd:dir [ "config"; "user.name"; "test" ]

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

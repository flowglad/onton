(** Tests for [Onton.Project_lock]: per-project advisory lock using
    [Unix.lockf]. Verifies contention, stale reclamation, release, and
    re-acquire semantics without spinning up Eio. *)

open Onton

let ( // ) = Filename.concat

let mktempdir prefix =
  let base = Filename.get_temp_dir_name () in
  let rec loop n =
    let dir = base // Printf.sprintf "%s-%d-%d" prefix (Unix.getpid ()) n in
    try
      Unix.mkdir dir 0o755;
      dir
    with Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n + 1)
  in
  loop 0

let rm_rf dir =
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)))

let fail_if cond msg = if cond then failwith msg

let must_ok = function
  | Ok v -> v
  | Error (Project_lock.Held_by { pid; _ }) ->
      failwith (Printf.sprintf "expected Ok, got Held_by pid=%d" pid)
  | Error (Project_lock.Io_error s) ->
      failwith (Printf.sprintf "expected Ok, got Io_error %s" s)

let must_held_by = function
  | Error (Project_lock.Held_by { pid; _ }) -> pid
  | Ok _ -> failwith "expected Held_by, got Ok"
  | Error (Project_lock.Io_error s) ->
      failwith (Printf.sprintf "expected Held_by, got Io_error %s" s)

let on_stale_noop _pid = ()

let test_happy_path () =
  let dir = mktempdir "onton-lock-happy" in
  Fun.protect
    ~finally:(fun () -> rm_rf dir)
    (fun () ->
      let lock =
        must_ok (Project_lock.acquire ~project_dir:dir ~on_stale:on_stale_noop)
      in
      let path = dir // "onton.lock" in
      fail_if (not (Sys.file_exists path)) "lock file not created";
      let ic = open_in path in
      let content = In_channel.input_all ic in
      close_in ic;
      let pid = int_of_string (String.trim content) in
      fail_if
        (pid <> Unix.getpid ())
        (Printf.sprintf "PID mismatch: %d vs %d" pid (Unix.getpid ()));
      Project_lock.release lock)

let test_release_is_idempotent () =
  let dir = mktempdir "onton-lock-release" in
  Fun.protect
    ~finally:(fun () -> rm_rf dir)
    (fun () ->
      let lock =
        must_ok (Project_lock.acquire ~project_dir:dir ~on_stale:on_stale_noop)
      in
      Project_lock.release lock;
      Project_lock.release lock;
      (* After release, re-acquisition must succeed. *)
      let lock2 =
        must_ok (Project_lock.acquire ~project_dir:dir ~on_stale:on_stale_noop)
      in
      Project_lock.release lock2)

(** Fork a child that acquires the lock and pauses. Wait until the PID file is
    populated, then the parent attempts to acquire and must see
    [Held_by child_pid]. *)
let test_contention () =
  let dir = mktempdir "onton-lock-contention" in
  Fun.protect
    ~finally:(fun () -> rm_rf dir)
    (fun () ->
      let sync_fd, child_resume = Unix.pipe () in
      match Unix.fork () with
      | 0 ->
          Unix.close child_resume;
          let lock =
            match
              Project_lock.acquire ~project_dir:dir ~on_stale:on_stale_noop
            with
            | Ok l -> l
            | Error _ -> exit 2
          in
          (* Signal ready by writing the PID file (acquire already did).
             Block until parent closes [child_resume]. *)
          let buf = Bytes.create 1 in
          (try
             let _ = Unix.read sync_fd buf 0 1 in
             ()
           with _ -> ());
          Project_lock.release lock;
          exit 0
      | child_pid ->
          Unix.close sync_fd;
          (* Wait for child's lock file to appear with a non-empty PID. *)
          let lock_path = dir // "onton.lock" in
          let deadline = Unix.gettimeofday () +. 5.0 in
          let rec wait () =
            let populated =
              Sys.file_exists lock_path
              &&
                try
                  let ic = open_in lock_path in
                  let s = In_channel.input_all ic in
                  close_in ic;
                  String.length (String.trim s) > 0
                with _ -> false
            in
            if populated then ()
            else if Unix.gettimeofday () > deadline then
              failwith "child never populated lock file"
            else (
              ignore (Unix.select [] [] [] 0.01);
              wait ())
          in
          wait ();
          let held_pid =
            must_held_by
              (Project_lock.acquire ~project_dir:dir ~on_stale:on_stale_noop)
          in
          fail_if (held_pid <> child_pid)
            (Printf.sprintf "held-by pid mismatch: %d vs child %d" held_pid
               child_pid);
          (* Release child. *)
          Unix.close child_resume;
          let _, _ = Unix.waitpid [] child_pid in
          (* Now the parent can acquire. *)
          let lock =
            must_ok
              (Project_lock.acquire ~project_dir:dir ~on_stale:on_stale_noop)
          in
          Project_lock.release lock)

(** Crash recovery: if the previous holder died (SIGKILL), the kernel releases
    its F_TLOCK automatically — so a fresh [acquire] just succeeds. This
    verifies the practical "onton crashed, user restarts" case. The explicit
    stale-PID path in [project_lock.ml] is a defensive safety net for rarer
    cases (FD inherited across fork, dying holder with FDs still open
    elsewhere); those are difficult to reach from userspace and aren't tested.
*)
let test_crash_recovery () =
  let dir = mktempdir "onton-lock-crash" in
  Fun.protect
    ~finally:(fun () -> rm_rf dir)
    (fun () ->
      match Unix.fork () with
      | 0 ->
          let _ =
            match
              Project_lock.acquire ~project_dir:dir ~on_stale:on_stale_noop
            with
            | Ok l -> l
            | Error _ -> exit 2
          in
          (* Hang until parent kills us; never release. *)
          let rec spin () =
            ignore (Unix.select [] [] [] 10.0);
            spin ()
          in
          spin ()
      | child_pid ->
          let lock_path = dir // "onton.lock" in
          let deadline = Unix.gettimeofday () +. 5.0 in
          let rec wait () =
            let populated =
              Sys.file_exists lock_path
              &&
                try
                  let ic = open_in lock_path in
                  let s = In_channel.input_all ic in
                  close_in ic;
                  String.length (String.trim s) > 0
                with _ -> false
            in
            if populated then ()
            else if Unix.gettimeofday () > deadline then
              failwith "child never populated lock file"
            else (
              ignore (Unix.select [] [] [] 0.01);
              wait ())
          in
          wait ();
          (* Simulate crash: SIGKILL. Kernel releases the lock when the FD
             is auto-closed on process death. *)
          Unix.kill child_pid Sys.sigkill;
          let _, _ = Unix.waitpid [] child_pid in
          let lock =
            must_ok
              (Project_lock.acquire ~project_dir:dir ~on_stale:on_stale_noop)
          in
          Project_lock.release lock)

let () =
  test_happy_path ();
  test_release_is_idempotent ();
  test_contention ();
  test_crash_recovery ();
  print_endline "test_project_lock: OK"

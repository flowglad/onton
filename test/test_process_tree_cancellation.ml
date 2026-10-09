(* @archlint.module test
   @archlint.domain worktree *)

open Onton
module Git = Onton_test_support.Git_env

let check message condition = if not condition then failwith message

let scenario env ~streaming graceful =
  Git.with_temp_repo (fun dir ->
      let path name = Filename.concat dir name in
      let root_pid = path "root-pid" and child_pid = path "child-pid" in
      let cleaned = path "cleaned" in
      let write name contents =
        let out = open_out (path name) in
        Fun.protect
          ~finally:(fun () -> close_out out)
          (fun () -> output_string out contents)
      in
      write "child.sh"
        ("trap '' TERM INT HUP\necho $$ > " ^ Filename.quote child_pid
       ^ "\nwhile :; do sleep 30; done\n");
      write "root.sh"
        ((if graceful then
            "trap 'touch " ^ Filename.quote cleaned ^ "; exit 0' TERM\n"
          else "trap '' TERM INT HUP\n")
        ^ "echo $$ > " ^ Filename.quote root_pid ^ "\n/bin/sh "
        ^ Filename.quote (path "child.sh")
        ^ " &\nwait\n");
      let clock = Eio.Stdenv.clock env in
      let cancelled = ref false in
      Eio.Time.with_timeout_exn clock 10. (fun () ->
          Eio.Fiber.first
            (fun () ->
              try
                if streaming then
                  ignore
                    (Llm_backend.spawn_and_stream
                       ~process_mgr:(Eio.Stdenv.process_mgr env)
                       ~clock ~timeout:60. ~cwd:(Eio.Stdenv.cwd env)
                       ~env:(Unix.environment ())
                       ~setsid_exec:(Some (Sys.getenv "ONTON_SETSID_EXEC"))
                       ~args:[ "/bin/sh"; path "root.sh" ]
                       ~session_uuid:None
                       ~patch_id:
                         (Onton_core.Types.Patch_id.of_string "cancel-stream")
                       ~process_line:(fun _ -> [])
                       ~on_event:(fun _ -> ()))
                else
                  ignore
                    (Process_tree.run
                       ~process_mgr:(Eio.Stdenv.process_mgr env)
                       ~clock ~env:(Unix.environment ())
                       [ "/bin/sh"; path "root.sh" ]);
                failwith "command completed before cancellation"
              with exn when Process_tree.has_cancellation exn ->
                cancelled := true;
                raise exn)
            (fun () ->
              while
                not
                  (Sys.file_exists child_pid
                  && (Unix.stat child_pid).Unix.st_size > 0)
              do
                Eio.Time.sleep clock 0.001
              done));
      check "process cancellation propagates" !cancelled;
      check "graceful root runs its cleanup handler"
        (Sys.file_exists cleaned = graceful);
      List.iter
        (fun file ->
          let pid =
            In_channel.with_open_bin file In_channel.input_all
            |> String.trim |> int_of_string
          in
          check "root and TERM-ignoring descendant are reaped"
            (match Unix.kill pid 0 with
            | () -> false
            | exception Unix.Unix_error (Unix.ESRCH, _, _) -> true))
        [ root_pid; child_pid ])

(* A successful/failed command may exit before its filters. Cleanup must retain
   the command's exit status and reap descendants before returning either one. *)
let exited_leader env code =
  Git.with_temp_repo (fun dir ->
      let child_pid = Filename.concat dir "orphan-pid" in
      let script =
        "sh -c "
        ^ Filename.quote
            ("trap '' TERM INT HUP; echo $$ > " ^ Filename.quote child_pid
           ^ "; while :; do sleep 30; done")
        ^ " &\nwhile [ ! -s " ^ Filename.quote child_pid
        ^ " ]; do sleep 0.01; done\nprintf command-output\nexit "
        ^ string_of_int code
      in
      let actual, stdout, stderr =
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
            Process_tree.run
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~env:(Unix.environment ())
              [ "/bin/sh"; "-c"; script ])
      in
      check "cleanup preserves command status" (actual = code);
      check "cleanup preserves command output" (stdout = "command-output");
      check ("supervisor failed: " ^ stderr) (stderr = "");
      let pid =
        In_channel.with_open_bin child_pid In_channel.input_all
        |> String.trim |> int_of_string
      in
      check "orphan is reaped before command completion"
        (match Unix.kill pid 0 with
        | () -> false
        | exception Unix.Unix_error (Unix.ESRCH, _, _) -> true))

let lifecycle_get = function
  | Ok value -> value
  | Error error -> failwith (Project_lifecycle.error_message error)

let with_registration f =
  let registration =
    lifecycle_get (Project_lifecycle.acquire_registration ())
  in
  Fun.protect
    ~finally:(fun () -> Project_lifecycle.release_registration registration)
    (fun () -> f registration)

let with_writer f =
  let with_one project_name f =
    let writer =
      with_registration (fun registration ->
          lifecycle_get
            (Project_lifecycle.acquire_writer registration ~project_name))
    in
    Fun.protect ~finally:(fun () -> Project_lifecycle.release_use writer) f
  in
  with_one "crash-owner" (fun () -> with_one "crash-dependent" f)

let with_data_dir directory f =
  let previous = Sys.getenv_opt "ONTON_DATA_DIR" in
  Unix.putenv "ONTON_DATA_DIR" directory;
  Fun.protect
    ~finally:(fun () ->
      match previous with
      | Some value -> Unix.putenv "ONTON_DATA_DIR" value
      | None -> Unix.unsetenv "ONTON_DATA_DIR")
    f

let check_draining () =
  let busy name release = function
    | Error (Project_lifecycle.Busy _) -> ()
    | Error (Project_lifecycle.Io_error reason) -> failwith reason
    | Ok lease ->
        release lease;
        failwith (name ^ " admitted while old command was alive")
  in
  with_registration (fun registration ->
      List.iter
        (fun project_name ->
          busy "writer" Project_lifecycle.release_use
            (Project_lifecycle.acquire_writer registration ~project_name);
          busy "reader" Project_lifecycle.release_use
            (Project_lifecycle.acquire_use registration ~project_name);
          busy "retirement" Project_lifecycle.release_retirement
            (Project_lifecycle.acquire_retirement registration ~project_name))
        [ "crash-owner"; "crash-dependent" ];
      let unrelated =
        lifecycle_get
          (Project_lifecycle.acquire_writer registration
             ~project_name:"unrelated")
      in
      Project_lifecycle.release_use unrelated)

let parent_death env mode =
  Git.with_temp_repo (fun dir ->
      Git.run_git ~cwd:dir [ "commit"; "--allow-empty"; "-qm"; "base" ];
      with_data_dir (Filename.concat dir "data") (fun () ->
          let path name = Filename.concat dir name in
          let files =
            List.map path [ "root-pid"; "child-pid"; "supervisor-pid" ]
          in
          let script = path "root.sh" in
          let write text =
            Out_channel.with_open_bin script (fun out -> output_string out text)
          in
          write
            ("#!/bin/sh\n"
           ^ "[ \"$1\" = \"--json\" ] && [ \"$2\" = \"doctor\" ] && { echo \
              '{\"identity\":\"simgit\",\"version\":\"0.3.0\"}'; exit 0; }\n"
           ^ "[ \"$1\" = \"list\" ] && { echo '[]'; exit 0; }\n"
           ^ "trap '' TERM INT HUP\necho $$ > "
            ^ Filename.quote (path "root-pid")
            ^ "\necho $PPID > "
            ^ Filename.quote (path "supervisor-pid")
            ^ "\n/bin/sh -c "
            ^ Filename.quote
                ("trap '' TERM INT HUP; echo $$ > "
                ^ Filename.quote (path "child-pid")
                ^ "; exec sleep 30")
            ^ " >/dev/null 2>&1 &\nwait\n");
          Unix.chmod script 0o700;
          let read_pid file =
            In_channel.with_open_bin file In_channel.input_all
            |> String.trim |> int_of_string
          in
          let gone file =
            match Unix.kill (read_pid file) 0 with
            | () -> false
            | exception Unix.Unix_error (Unix.ESRCH, _, _) -> true
          in
          let ready file =
            Sys.file_exists file && (Unix.stat file).Unix.st_size > 0
          in
          let clock = Eio.Stdenv.clock env in
          Eio.Switch.run (fun sw ->
              let caller =
                Eio.Process.spawn ~sw
                  (Eio.Stdenv.process_mgr env)
                  ~stdin:(Eio.Flow.string_source "")
                  ~stdout:(Eio.Flow.buffer_sink (Buffer.create 32))
                  ~stderr:(Eio.Flow.buffer_sink (Buffer.create 32))
                  [ Sys.executable_name; "--crash-" ^ mode ^ "-client"; script ]
              in
              let reaped = ref false in
              Fun.protect
                ~finally:(fun () ->
                  if not !reaped then (
                    (try Eio.Process.signal caller Sys.sigkill with _ -> ());
                    ignore (Eio.Process.await caller));
                  List.iter
                    (fun file ->
                      if ready file then
                        try
                          let pid = read_pid file in
                          if pid > 0 then Unix.kill pid Sys.sigkill
                        with _ -> ())
                    files)
                (fun () ->
                  Eio.Time.with_timeout_exn clock 10. (fun () ->
                      while not (List.for_all ready files) do
                        Eio.Time.sleep clock 0.001
                      done);
                  (* Freeze cleanup to expose the lease handoff without timing a
                 short grace window. The command and descendant remain live. *)
                  Unix.kill (read_pid (path "supervisor-pid")) Sys.sigstop;
                  Eio.Process.signal caller Sys.sigkill;
                  ignore (Eio.Process.await caller);
                  reaped := true;
                  check_draining ();
                  Unix.kill (read_pid (path "supervisor-pid")) Sys.sigcont;
                  let cleaned =
                    Eio.Time.with_timeout clock 5. (fun () ->
                        while not (List.for_all gone files) do
                          Eio.Time.sleep clock 0.01
                        done;
                        Ok ())
                  in
                  check
                    (mode ^ " process tree survived abrupt caller death")
                    (cleaned = Ok ());
                  with_writer (fun () -> ())))))

let stale_parent env =
  Git.with_temp_repo (fun dir ->
      let marker = Filename.concat dir "executed" in
      let helper = Sys.getenv "ONTON_SETSID_EXEC" in
      let error = Buffer.create 32 in
      Eio.Switch.run (fun sw ->
          let child =
            Eio.Process.spawn ~sw
              (Eio.Stdenv.process_mgr env)
              ~stderr:(Eio.Flow.buffer_sink error)
              [
                helper;
                "--supervise-parent";
                string_of_int (Unix.getpid () + 1);
                "/bin/sh";
                "-c";
                "touch " ^ Filename.quote marker;
              ]
          in
          check "stale parent is refused before command dispatch"
            (Eio.Process.await child = `Exited 125));
      check "refused parent cannot execute a command"
        (not (Sys.file_exists marker));
      check "stale-parent refusal retains diagnostics" (Buffer.length error > 0))

let short_commands env =
  (* Concurrent rapid exits exercise the observation/reap boundary under PID
     churn. A completed command with no descendants must never signal a later
     process which happens to reuse its old group ID. *)
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
      Eio.Fiber.List.iter ~max_fibers:8
        (fun index ->
          let code = index mod 7 in
          let actual, stdout, stderr =
            Process_tree.run
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~env:(Unix.environment ())
              [ "/bin/sh"; "-c"; "exit " ^ string_of_int code ]
          in
          check
            ("short-command supervisor failed: " ^ stderr)
            (actual = code && stdout = "" && stderr = ""))
        (List.init 256 Fun.id))

let native_status env =
  List.iter
    (fun (script, expected) ->
      let status, _, _ =
        Process_tree.run_status
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~env:(Unix.environment ())
          [ "/bin/sh"; "-c"; script ]
      in
      check "supervision preserves native exit/signal status" (status = expected))
    [
      ("exit 23", `Exited 23);
      ("kill -TERM $$", `Signaled Sys.sigterm);
      ("kill -KILL $$", `Signaled Sys.sigkill);
    ]

let synchronous_capture () =
  (* Run before Eio_main.run: startup capture must not need an Eio scheduler.
     Bound the pipe-pressure fixture so a regression is reported, not hung. *)
  let previous_signal =
    Sys.signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> failwith "synchronous capture timed out"))
  in
  let previous_timer =
    Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.; it_value = 10. }
  in
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.setitimer Unix.ITIMER_REAL previous_timer);
      Sys.set_signal Sys.sigalrm previous_signal)
    (fun () ->
      let run script =
        Process_tree.run_sync ~env:(Unix.environment ())
          [ "/bin/sh"; "-c"; script ]
      in
      let status, stdout, stderr =
        run "head -c 262144 /dev/zero >&2; head -c 262144 /dev/zero; exit 23"
      in
      check "synchronous capture retains exit status" (status = Unix.WEXITED 23);
      check "both full pipes are drained without truncation"
        (String.length stdout = 262144 && String.length stderr = 262144);
      List.iter
        (fun (name, signal) ->
          let status, _, _ = run ("kill -" ^ name ^ " $$") in
          check "synchronous capture retains native signal status"
            (status = Unix.WSIGNALED signal))
        [ ("TERM", Sys.sigterm); ("KILL", Sys.sigkill) ];
      let status, stdout, _ = run "cat; printf stdin-closed" in
      check "synchronous commands receive stdin EOF"
        (status = Unix.WEXITED 0 && stdout = "stdin-closed"))

let missing_default env =
  Git.with_temp_repo (fun dir ->
      let marker = Filename.concat dir "dispatched" in
      let previous = Sys.getenv_opt "ONTON_SETSID_EXEC" in
      Unix.putenv "ONTON_SETSID_EXEC" "";
      Fun.protect
        ~finally:(fun () ->
          match previous with
          | Some path -> Unix.putenv "ONTON_SETSID_EXEC" path
          | None -> Unix.unsetenv "ONTON_SETSID_EXEC")
        (fun () ->
          let refused =
            try
              let result =
                Llm_backend.spawn_and_stream
                  ~process_mgr:(Eio.Stdenv.process_mgr env)
                  ~clock:(Eio.Stdenv.clock env) ~timeout:5.
                  ~cwd:(Eio.Stdenv.cwd env) ~env:(Unix.environment ())
                  ~setsid_exec:None ~args:[ "touch"; marker ] ~session_uuid:None
                  ~patch_id:
                    (Onton_core.Types.Patch_id.of_string "missing-supervisor")
                  ~process_line:(fun _ -> [])
                  ~on_event:(fun _ -> ())
              in
              result.Llm_backend.exit_code <> 0
            with exn -> not (Process_tree.has_cancellation exn)
          in
          check "missing default supervisor is refused" refused;
          check "missing default supervisor cannot dispatch a command"
            (not (Sys.file_exists marker));
          let status, _, stderr =
            Process_tree.run_sync ~env:(Unix.environment ()) [ "touch"; marker ]
          in
          check "missing synchronous supervisor is refused"
            (status = Unix.WEXITED 127 && stderr <> "");
          check "missing synchronous supervisor cannot dispatch"
            (not (Sys.file_exists marker))))

let run_crash_client env mode script =
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let repo = Filename.dirname script in
  let fake_git () =
    Unix.symlink script (Filename.concat repo "git");
    Unix.putenv "PATH"
      (repo ^ ":" ^ Option.value (Sys.getenv_opt "PATH") ~default:"")
  in
  match mode with
  | "stream" | "stream-default" ->
      ignore
        (Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout:60.
           ~cwd:(Eio.Stdenv.cwd env) ~env:(Unix.environment ())
           ~setsid_exec:
             (if mode = "stream" then Some (Sys.getenv "ONTON_SETSID_EXEC")
              else None)
           ~args:[ "/bin/sh"; script ] ~session_uuid:None
           ~patch_id:(Onton_core.Types.Patch_id.of_string "crash-stream")
           ~process_line:(fun _ -> [])
           ~on_event:(fun _ -> ()))
  | "command" ->
      ignore
        (Process_tree.run ~process_mgr ~clock ~env:(Unix.environment ())
           [ "/bin/sh"; script ])
  | "backend" ->
      let config =
        match
          Onton_core.Worktree_lifecycle.configure ~backend:"simgit"
            ~executable:(Some script)
        with
        | Ok config -> config
        | Error reason -> failwith reason
      in
      let module B =
        (val Worktree_backend.make ~fs:(Eio.Stdenv.fs env) ~clock ~process_mgr
               ~repo_root:repo ~config ~timeout_seconds:60.)
      in
      ignore
        (B.materialize
           ~path:(Filename.concat repo "checkout")
           ~branch:(Onton_core.Types.Branch.of_string "new")
           ~expected_local:None
           (Onton_core.Start_point_plan.Create_new_branch_from_base
              { base_branch = "HEAD" }))
  | "fetch" ->
      fake_git ();
      ignore
        (Worktree.fetch_origin ~fetch_lock:(Eio.Mutex.create ()) ~process_mgr
           ~path:repo)
  | "repo-fetch" ->
      fake_git ();
      let module R = (val Repo_git.make ~repo_root:repo) in
      ignore (R.fetch_managed_repo ())
  | "repo-root" ->
      fake_git ();
      ignore (Repo_root.normalize repo)
  | "clone" ->
      fake_git ();
      ignore
        (Managed_repo.ensure_managed_repo
           ~clone_scheme:(Some Onton_core.Github_target.Https)
           ~project_name:"crash-owner" ~token:"" ~owner:"test" ~repo:"test" ())
  | "sourcehut" ->
      fake_git ();
      let branch = Onton_core.Types.Branch.of_string in
      let id = Onton_core.Types.Pr_number.of_int 1 in
      let module F =
        (val Sourcehut.make_with_builds
               ~read_builds:(fun () -> Ok [])
               ~net:(Eio.Stdenv.net env) ~clock ~process_mgr ~token:""
               ~owner:"test" ~repo:"test" ~repo_root:repo
               ~main_branch:(branch "main")
               ~changes:[ (Some id, branch "feature", branch "main") ])
      in
      ignore (F.pr_state id)
  | _ -> failwith ("unknown crash mode: " ^ mode)

let modes =
  [
    "command";
    "stream";
    "stream-default";
    "backend";
    "fetch";
    "sourcehut";
    "repo-fetch";
    "repo-root";
    "clone";
  ]

let () =
  if Array.length Sys.argv = 3 then (
    if Sys.argv.(1) = "--test-parent" then (
      Eio_main.run (fun env -> parent_death env Sys.argv.(2));
      exit 0);
    List.iter
      (fun mode ->
        if Sys.argv.(1) = "--crash-" ^ mode ^ "-client" then (
          with_writer (fun () ->
              Eio_main.run (fun env -> run_crash_client env mode Sys.argv.(2)));
          exit 0))
      modes);
  synchronous_capture ();
  Eio_main.run (fun env ->
      native_status env;
      missing_default env;
      stale_parent env;
      List.iter (parent_death env) modes;
      List.iter
        (fun streaming -> List.iter (scenario env ~streaming) [ true; false ])
        [ true; false ];
      List.iter (exited_leader env) [ 0; 23 ];
      short_commands env);
  print_endline
    "process-tree graceful cleanup, orphan reaping and rapid exits: OK"

(* @archlint.module test
   @archlint.domain user-config *)

open Base
open Onton

(** Write [body] to a fresh executable script in a tmp dir and return its
    absolute path plus the dir. Caller is responsible for cleanup. *)
let make_script body =
  let dir = Stdlib.Filename.temp_dir "onton_user_config_" "" in
  let path = Stdlib.Filename.concat dir "hook" in
  let oc = Stdlib.open_out path in
  Stdlib.output_string oc body;
  Stdlib.close_out oc;
  Unix.chmod path 0o755;
  (dir, path)

let assert_contains ~label ~needle haystack =
  if not (String.is_substring haystack ~substring:needle) then
    failwith
      (Printf.sprintf "%s: expected output to contain %S, got:\n%s" label needle
         haystack)

let assert_not_contains ~label ~needle haystack =
  if String.is_substring haystack ~substring:needle then
    failwith
      (Printf.sprintf "%s: expected output NOT to contain %S, got:\n%s" label
         needle haystack)

let hook_tree env mode =
  let ending =
    match mode with 0 -> "exit 0\n" | 1 -> "exit 7\n" | _ -> "wait\n"
  in
  let dir, script =
    make_script
      ({|#!/bin/sh
echo $$ > root-pid
/bin/sh -c 'trap "" TERM INT HUP; echo $$ > child-pid; exec sleep 30' >/dev/null 2>&1 &
while [ ! -s child-pid ]; do sleep 0.01; done
echo ready-out
echo ready-err >&2
|}
     ^ ending)
  in
  let root_pid = Stdlib.Filename.concat dir "root-pid" in
  let child_pid = Stdlib.Filename.concat dir "child-pid" in
  let read_pid path =
    Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all
    |> String.strip |> Int.of_string
  in
  let clock = Eio.Stdenv.clock env in
  let run timeout =
    User_config.run_hook
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~clock ~script
      ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
      ~env:[] ~timeout ()
  in
  Stdlib.Fun.protect
    ~finally:(fun () ->
      List.iter [ child_pid; root_pid ] ~f:(fun path ->
          if Stdlib.Sys.file_exists path then
            try Unix.kill (read_pid path) Stdlib.Sys.sigkill
            with Unix.Unix_error (Unix.ESRCH, _, _) -> ()))
    (fun () ->
      (if mode = 3 then (
         let cancelled = ref false in
         Eio.Time.with_timeout_exn clock 10. (fun () ->
             Eio.Fiber.first
               (fun () ->
                 try
                   ignore (run 20.);
                   failwith "hook returned before cancellation"
                 with exn when Process_tree.has_cancellation exn ->
                   cancelled := true;
                   raise exn)
               (fun () ->
                 while
                   not
                     (Stdlib.Sys.file_exists child_pid
                     && (Unix.stat child_pid).Unix.st_size > 0)
                 do
                   Eio.Time.sleep clock 0.001
                 done));
         if not !cancelled then failwith "hook cancellation did not propagate")
       else
         match run (if mode = 2 then 2. else 10.) with
         | Ok () when mode = 0 -> ()
         | Ok () -> failwith "failed or timed-out hook reported success"
         | Error message when mode = 0 -> failwith message
         | Error message ->
             assert_contains ~label:"tree-hook status"
               ~needle:(if mode = 2 then "timed out" else "code 7")
               message;
             assert_contains ~label:"tree-hook stdout" ~needle:"ready-out"
               message;
             assert_contains ~label:"tree-hook stderr" ~needle:"ready-err"
               message);
      List.iter [ root_pid; child_pid ] ~f:(fun path ->
          if not (Stdlib.Sys.file_exists path) then
            failwith "hook did not use its requested working directory";
          match Unix.kill (read_pid path) 0 with
          | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ()
          | () ->
              failwith
                (Printf.sprintf "hook mode %d returned with a live descendant"
                   mode)))

let () =
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let fs = Eio.Stdenv.fs env in
  List.iter [ 0; 1; 2; 3 ] ~f:(hook_tree env);

  (let previous = Stdlib.Sys.getenv "ONTON_SETSID_EXEC" in
   let original_cwd = Stdlib.Sys.getcwd () in
   let supervisor =
     if Stdlib.Filename.is_relative previous then
       Stdlib.Filename.concat original_cwd previous
     else previous
   in
   let dir, script =
     make_script "#!/bin/sh\nprintf correct-directory > proof\n"
   in
   let checkout = Stdlib.Filename.concat dir "checkout" in
   Unix.mkdir checkout 0o700;
   Unix.symlink supervisor (Stdlib.Filename.concat dir "supervisor");
   Stdlib.Fun.protect
     ~finally:(fun () ->
       Unix.chdir original_cwd;
       Unix.putenv "ONTON_SETSID_EXEC" previous)
     (fun () ->
       Unix.chdir dir;
       Unix.putenv "ONTON_SETSID_EXEC" "./supervisor";
       (match
          User_config.run_hook ~process_mgr ~clock ~script
            ~cwd:Eio.Path.(fs / checkout)
            ~env:[] ()
        with
       | Ok () -> ()
       | Error reason -> failwith reason);
       if not (Stdlib.Sys.file_exists (Stdlib.Filename.concat checkout "proof"))
       then failwith "relative supervisor changed the hook working directory"));

  (let previous = Stdlib.Sys.getenv "ONTON_SETSID_EXEC" in
   let dir, script = make_script "#!/bin/sh\ntouch executed\n" in
   Stdlib.Fun.protect
     ~finally:(fun () -> Unix.putenv "ONTON_SETSID_EXEC" previous)
     (fun () ->
       Unix.putenv "ONTON_SETSID_EXEC" (Stdlib.Filename.concat dir "missing");
       (match
          User_config.run_hook ~process_mgr ~clock ~script
            ~cwd:Eio.Path.(fs / dir)
            ~env:[] ()
        with
       | Ok () -> failwith "missing supervisor authorized a hook"
       | Error reason ->
           assert_contains ~label:"missing supervisor"
             ~needle:"Process-tree supervisor unavailable" reason);
       if Stdlib.Sys.file_exists (Stdlib.Filename.concat dir "executed") then
         failwith "hook executed without process supervision"));

  (* ── Success: exit 0, stdout ignored by caller ────────────────────── *)
  (let dir, script = make_script "#!/bin/sh\necho hello\n" in
   let cwd = Eio.Path.(fs / dir) in
   match User_config.run_hook ~process_mgr ~clock ~script ~cwd ~env:[] () with
   | Ok () -> ()
   | Error msg -> failwith (Printf.sprintf "expected Ok, got Error: %s" msg));

  (* ── Failure: hook writes a diagnostic to STDOUT (the real-world case
        we hit — `echo ERROR:...; exit 1` — where previously only stderr
        was captured and the error reason was silently dropped). ───── *)
  (let dir, script =
     make_script "#!/bin/sh\necho 'ERROR: no opam switch' >&1\nexit 1\n"
   in
   let cwd = Eio.Path.(fs / dir) in
   match User_config.run_hook ~process_mgr ~clock ~script ~cwd ~env:[] () with
   | Ok () -> failwith "expected Error when hook exits 1"
   | Error msg ->
       assert_contains ~label:"stdout-on-failure" ~needle:"stdout:" msg;
       assert_contains ~label:"stdout-on-failure"
         ~needle:"ERROR: no opam switch" msg;
       assert_not_contains ~label:"stdout-on-failure" ~needle:"stderr:" msg);

  (* ── Failure: hook writes to STDERR only. ─────────────────────────── *)
  (let dir, script = make_script "#!/bin/sh\necho 'boom' >&2\nexit 2\n" in
   let cwd = Eio.Path.(fs / dir) in
   match User_config.run_hook ~process_mgr ~clock ~script ~cwd ~env:[] () with
   | Ok () -> failwith "expected Error when hook exits 2"
   | Error msg ->
       assert_contains ~label:"stderr-only" ~needle:"stderr:" msg;
       assert_contains ~label:"stderr-only" ~needle:"boom" msg;
       assert_not_contains ~label:"stderr-only" ~needle:"stdout:" msg);

  (* ── Failure: both streams populated — both must appear. ──────────── *)
  (let dir, script =
     make_script "#!/bin/sh\necho out-line\necho err-line >&2\nexit 3\n"
   in
   let cwd = Eio.Path.(fs / dir) in
   match User_config.run_hook ~process_mgr ~clock ~script ~cwd ~env:[] () with
   | Ok () -> failwith "expected Error when hook exits 3"
   | Error msg ->
       assert_contains ~label:"both-streams" ~needle:"stdout:" msg;
       assert_contains ~label:"both-streams" ~needle:"out-line" msg;
       assert_contains ~label:"both-streams" ~needle:"stderr:" msg;
       assert_contains ~label:"both-streams" ~needle:"err-line" msg);

  (* ── Env vars are passed through to the hook. ─────────────────────── *)
  (let dir, script =
     make_script
       "#!/bin/sh\n\
        if [ \"$ONTON_PATCH_ID\" != \"42\" ]; then\n\
       \  echo \"missing ONTON_PATCH_ID (got '$ONTON_PATCH_ID')\" >&2\n\
       \  exit 1\n\
        fi\n"
   in
   let cwd = Eio.Path.(fs / dir) in
   let env = [ ("ONTON_PATCH_ID", "42") ] in
   match User_config.run_hook ~process_mgr ~clock ~script ~cwd ~env () with
   | Ok () -> ()
   | Error msg -> failwith (Printf.sprintf "expected Ok, got Error: %s" msg));

  (* Timeout diagnostics and bounded cleanup for a hook without descendants. *)
  (let dir, script = make_script "#!/bin/sh\nexec sleep 5\n" in
   let cwd = Eio.Path.(fs / dir) in
   let t0 = Unix.gettimeofday () in
   (match
      User_config.run_hook ~process_mgr ~clock ~script ~cwd ~env:[] ~timeout:0.3
        ()
    with
   | Ok () -> failwith "expected timeout Error"
   | Error msg -> assert_contains ~label:"timeout" ~needle:"timed out" msg);
   let elapsed = Unix.gettimeofday () -. t0 in
   if Float.(elapsed > 3.0) then
     failwith
       (Printf.sprintf "timeout took %.1fs — SIGKILL likely didn't fire in time"
          elapsed));

  (* ── FD cap: the [ulimit -n N] prefix must reach the child so a runaway
        hook can't exhaust the shared FD table. The hook echoes its own
        [ulimit -n] then exits non-zero so the captured stdout surfaces via
        the Error message.

        [wrap_with_ulimit] clamps the requested limit against the test
        runner's own soft cap, so if the CI runner is already at [ulimit -Sn]
        < 128 (exactly the scenario this PR is meant to help with), the child
        sees the clamped value, not 128. Expect [min fd_limit parent_soft]. *)
  (let dir, script =
     make_script
       "#!/bin/sh\nlimit=$(ulimit -n)\necho \"limit=$limit\"\nexit 1\n"
   in
   let cwd = Eio.Path.(fs / dir) in
   let parent_soft =
     let ic = Unix.open_process_in "ulimit -Sn" in
     let s = String.strip (Stdlib.input_line ic) in
     ignore (Unix.close_process_in ic);
     if String.equal s "unlimited" then Int.max_value else Int.of_string s
   in
   let fd_limit = 128 in
   let expected = min fd_limit parent_soft in
   match
     User_config.run_hook ~process_mgr ~clock ~script ~cwd ~env:[] ~fd_limit ()
   with
   | Ok () -> failwith "expected Error (exit 1)"
   | Error msg ->
       assert_contains ~label:"fd-cap"
         ~needle:(Printf.sprintf "limit=%d" expected)
         msg);

  Stdlib.print_endline "test_user_config: OK"

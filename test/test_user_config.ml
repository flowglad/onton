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

let () =
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let fs = Eio.Stdenv.fs env in

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

  (* ── Timeout: hook that runs longer than [~timeout] must be killed
        with SIGKILL and return an Error whose message names the timeout.

        NOTE: SIGKILL is only delivered to the [child] PID we spawned
        (the outer [/bin/sh -c] that exec'd the hook). Grandchild processes
        the hook itself forked are not killed by this signal — mitigating
        that requires setpgid + killing the whole group, which is a
        follow-up. For this test we exec [sleep] directly so no grandchild
        is created. *)
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
        the Error message. ────────────────────────────────────────────── *)
  (let dir, script =
     make_script
       "#!/bin/sh\nlimit=$(ulimit -n)\necho \"limit=$limit\"\nexit 1\n"
   in
   let cwd = Eio.Path.(fs / dir) in
   match
     User_config.run_hook ~process_mgr ~clock ~script ~cwd ~env:[] ~fd_limit:128
       ()
   with
   | Ok () -> failwith "expected Error (exit 1)"
   | Error msg -> assert_contains ~label:"fd-cap" ~needle:"limit=128" msg);

  Stdlib.print_endline "test_user_config: OK"

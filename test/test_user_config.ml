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
  let fs = Eio.Stdenv.fs env in

  (* ── Success: exit 0, stdout ignored by caller ────────────────────── *)
  (let dir, script = make_script "#!/bin/sh\necho hello\n" in
   let cwd = Eio.Path.(fs / dir) in
   match User_config.run_hook ~process_mgr ~script ~cwd ~env:[] with
   | Ok () -> ()
   | Error msg -> failwith (Printf.sprintf "expected Ok, got Error: %s" msg));

  (* ── Failure: hook writes a diagnostic to STDOUT (the real-world case
        we hit — `echo ERROR:...; exit 1` — where previously only stderr
        was captured and the error reason was silently dropped). ───── *)
  (let dir, script =
     make_script "#!/bin/sh\necho 'ERROR: no opam switch' >&1\nexit 1\n"
   in
   let cwd = Eio.Path.(fs / dir) in
   match User_config.run_hook ~process_mgr ~script ~cwd ~env:[] with
   | Ok () -> failwith "expected Error when hook exits 1"
   | Error msg ->
       assert_contains ~label:"stdout-on-failure" ~needle:"stdout:" msg;
       assert_contains ~label:"stdout-on-failure"
         ~needle:"ERROR: no opam switch" msg;
       assert_not_contains ~label:"stdout-on-failure" ~needle:"stderr:" msg);

  (* ── Failure: hook writes to STDERR only. ─────────────────────────── *)
  (let dir, script = make_script "#!/bin/sh\necho 'boom' >&2\nexit 2\n" in
   let cwd = Eio.Path.(fs / dir) in
   match User_config.run_hook ~process_mgr ~script ~cwd ~env:[] with
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
   match User_config.run_hook ~process_mgr ~script ~cwd ~env:[] with
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
   match User_config.run_hook ~process_mgr ~script ~cwd ~env with
   | Ok () -> ()
   | Error msg -> failwith (Printf.sprintf "expected Ok, got Error: %s" msg));

  Stdlib.print_endline "test_user_config: OK"

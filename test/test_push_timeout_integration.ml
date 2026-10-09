(* @archlint.module test
   @archlint.domain push-plan *)

open Base
open Onton
open Onton_core
module Git_env = Onton_test_support.Git_env

let write_script path contents =
  let oc = Stdlib.open_out path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () -> Stdlib.output_string oc ("#!/bin/sh\nset -eu\n" ^ contents));
  Unix.chmod path 0o755

let scenario env stage expected_phase =
  Git_env.with_temp_repo @@ fun repo ->
  let git = Git_env.run_git ~cwd:repo in
  let capture = Git_env.git_capture ~cwd:repo in
  git [ "init"; "-q"; "--bare"; "origin.git" ];
  git [ "remote"; "add"; "origin"; Stdlib.Filename.concat repo "origin.git" ];
  git [ "commit"; "-q"; "--allow-empty"; "-m"; "base" ];
  git [ "checkout"; "-q"; "-b"; "feat" ];
  Git_env.sh ~dir:repo "echo feature > feature.txt";
  git [ "add"; "feature.txt" ];
  git [ "commit"; "-q"; "-m"; "feature" ];
  git [ "push"; "-q"; "-u"; "origin"; "main"; "feat" ];
  Git_env.sh ~dir:repo "echo extra > extra.txt";
  git [ "add"; "extra.txt" ];
  git [ "commit"; "-q"; "-m"; "extra" ];
  if String.equal stage "fetch" then (
    (* An unknown remote object ensures fetch needs the transport rather than
       satisfying the request entirely from the local object database. *)
    git [ "clone"; "-q"; "--branch"; "feat"; "origin.git"; "writer" ];
    let writer_git =
      Git_env.run_git ~cwd:(Stdlib.Filename.concat repo "writer")
    in
    writer_git [ "config"; "user.email"; "writer@example.com" ];
    writer_git [ "config"; "user.name"; "Writer" ];
    writer_git [ "commit"; "-q"; "--amend"; "-m"; "remote feature" ];
    writer_git [ "push"; "-q"; "--force"; "origin"; "feat" ]);
  let old_remote = capture [ "--git-dir=origin.git"; "rev-parse"; "feat" ] in
  let marker = Stdlib.Filename.concat repo "blocked.pid" in
  let push_started = Stdlib.Filename.concat repo "push-started" in
  let block =
    Printf.sprintf "echo $$ > %s.tmp\nmv %s.tmp %s\nexec sleep 30\n"
      (Stdlib.Filename.quote marker)
      (Stdlib.Filename.quote marker)
      (Stdlib.Filename.quote marker)
  in
  let receive_pack = Stdlib.Filename.concat repo "receive-pack" in
  write_script receive_pack
    (Printf.sprintf "touch %s\n%s\nexec git-receive-pack \"$@\"\n"
       (Stdlib.Filename.quote push_started)
       (if String.equal stage "push" then block else ""));
  git [ "config"; "remote.origin.receivepack"; receive_pack ];
  if not (String.equal stage "push") then (
    git [ "update-ref"; "-d"; "refs/remotes/origin/feat" ];
    let upload_pack = Stdlib.Filename.concat repo "upload-pack" in
    let pass = Stdlib.Filename.concat repo "observed" in
    write_script upload_pack
      ((if String.equal stage "fetch" then
          Printf.sprintf
            "if [ ! -e %s ]; then\n\
            \  touch %s\n\
            \  exec git-upload-pack \"$@\"\n\
             fi\n"
            (Stdlib.Filename.quote pass)
            (Stdlib.Filename.quote pass)
        else "")
      ^ block);
    git [ "config"; "remote.origin.uploadpack"; upload_pack ]);
  let clock = Eio_mock.Clock.make () in
  let publish () =
    Worktree.force_push_with_lease ~timeout_seconds:5.0
      ~clock:(clock :> float Eio.Time.clock_ty Eio.Time.clock)
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~path:repo
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
      ()
  in
  let cleanup () =
    (* Git may leave its transport child behind when cancelled. The
       fixture records the exec'd sleeper's PID so cleanup is deterministic. *)
    if Stdlib.Sys.file_exists marker then
      let ic = Stdlib.open_in marker in
      let pid =
        Stdlib.Fun.protect
          ~finally:(fun () -> Stdlib.close_in_noerr ic)
          (fun () -> Stdlib.int_of_string_opt (Stdlib.input_line ic))
      in
      Option.iter pid ~f:(fun pid ->
          Eio.Cancel.protect (fun () ->
              (try Unix.kill pid Stdlib.Sys.sigkill
               with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
              let still_exists () =
                match Unix.kill pid 0 with
                | () -> true
                | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false
              in
              let clock = Eio.Stdenv.clock env in
              match
                Eio.Time.with_timeout clock 5.0 (fun () ->
                    while still_exists () do
                      Eio.Time.sleep clock 0.01
                    done;
                    Ok ())
              with
              | Ok () -> ()
              | Error `Timeout ->
                  failwith
                    (Printf.sprintf
                       "%s: blocked descendant %d still exists after cleanup"
                       stage pid)))
  in
  let outcome =
    Stdlib.Fun.protect ~finally:cleanup (fun () ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10.0 (fun () ->
            let _, outcome =
              Eio.Fiber.pair
                (fun () ->
                  (* Expire the publication deadline only after real Git has
                     reached the selected phase, independent of machine speed. *)
                  while not (Stdlib.Sys.file_exists marker) do
                    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01
                  done;
                  Eio_mock.Clock.set_time clock 5.0)
                publish
            in
            outcome))
  in
  let expected = "Publication timed out after 5s during " ^ expected_phase in
  (match outcome with
  | Worktree.Push_error detail when String.equal detail expected -> ()
  | other ->
      failwith
        (Printf.sprintf "%s: expected %s, got %s" stage expected
           (Worktree.show_push_result other)));
  if Bool.(Stdlib.Sys.file_exists push_started <> String.equal stage "push")
  then failwith (stage ^ ": unexpected push invocation");
  let remote = capture [ "--git-dir=origin.git"; "rev-parse"; "feat" ] in
  if not (String.equal remote old_remote) then
    failwith (stage ^ ": timeout changed remote history");
  git [ "config"; "--unset"; "remote.origin.receivepack" ];
  if not (String.equal stage "push") then
    git [ "config"; "--unset"; "remote.origin.uploadpack" ];
  let retried =
    Worktree.force_push_with_lease ~clock:(Eio.Stdenv.clock env)
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~path:repo
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
      ()
  in
  if not (Worktree.equal_push_result retried Worktree.Push_ok) then
    failwith
      (Printf.sprintf "%s: retry failed: %s" stage
         (Worktree.show_push_result retried));
  Stdlib.print_endline (stage ^ " timeout and retry: OK")

let () =
  Eio_main.run @@ fun env ->
  List.iter
    [
      ("observation", "remote observation (git ls-remote)");
      ("fetch", "remote object fetch (git fetch)");
      ("push", "git push");
    ]
    ~f:(fun (stage, expected_phase) -> scenario env stage expected_phase)

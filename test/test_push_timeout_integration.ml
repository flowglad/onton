(* @archlint.module test
   @archlint.domain push-plan *)

open Base
open Onton
open Onton_core
module Git_env = Onton_test_support.Git_env
module B = Branch_reconcile
module E = Branch_reconcile_executor

let write_script path contents =
  let oc = Stdlib.open_out path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () -> Stdlib.output_string oc ("#!/bin/sh\nset -eu\n" ^ contents));
  Unix.chmod path 0o755

(* Phase entry and cancellation have different budgets. In particular, a
   completed publication must not leave the marker waiter running forever. *)
let await_phase ~clock ~timeout ~finished ~started ~describe =
  match
    Eio.Time.with_timeout clock timeout (fun () ->
        while not (started ()) do
          if !finished then
            failwith
              ("publication completed before blocked phase: " ^ describe ());
          Eio.Time.sleep clock 0.01
        done;
        Ok ())
  with
  | Ok () -> ()
  | Error `Timeout -> failwith ("phase entry timed out: " ^ describe ())

let expire_phase ~real_clock ~mock_clock ~timeout ~finished ~describe =
  match
    Eio.Time.with_timeout real_clock timeout (fun () ->
        let elapsed = ref 120.0 in
        while not !finished do
          Eio_mock.Clock.set_time mock_clock !elapsed;
          Eio.Time.sleep real_clock 0.01;
          elapsed := !elapsed +. 0.1
        done;
        Ok ())
  with
  | Ok () -> ()
  | Error `Timeout ->
      failwith ("blocked command cancellation timed out: " ^ describe ())

let check_phase_waiters env =
  let clock = Eio.Stdenv.clock env in
  let expect_failure prefix f =
    match f () with
    | () -> failwith ("expected waiter failure: " ^ prefix)
    | exception Failure message when String.is_prefix message ~prefix -> ()
  in
  expect_failure "publication completed before blocked phase" (fun () ->
      await_phase ~clock ~timeout:1.0 ~finished:(ref true)
        ~started:(fun () -> false)
        ~describe:(fun () -> "early completion"));
  expect_failure "phase entry timed out" (fun () ->
      await_phase ~clock ~timeout:0.02 ~finished:(ref false)
        ~started:(fun () -> false)
        ~describe:(fun () -> "missing marker"));
  expect_failure "blocked command cancellation timed out" (fun () ->
      expire_phase ~real_clock:clock ~mock_clock:(Eio_mock.Clock.make ())
        ~timeout:0.02 ~finished:(ref false) ~describe:(fun () ->
          "stuck cancellation"));
  await_phase ~clock ~timeout:1.0 ~finished:(ref false)
    ~started:(fun () -> true)
    ~describe:(fun () -> "already entered");
  Stdlib.print_endline
    "phase waiter early-completion and watchdog regressions: OK"

let scenario env stage =
  Git_env.with_temp_repo @@ fun repo ->
  let git = Git_env.run_git ~cwd:repo in
  let capture = Git_env.git_capture ~cwd:repo in
  git [ "init"; "-q"; "--bare"; ".git/timeout-origin.git" ];
  git
    [
      "remote";
      "add";
      "origin";
      Stdlib.Filename.concat repo ".git/timeout-origin.git";
    ];
  git [ "commit"; "-q"; "--allow-empty"; "-m"; "base" ];
  let materialized =
    match B.Commit.make (capture [ "rev-parse"; "HEAD" ]) with
    | Some sha -> sha
    | None -> assert false
  in
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
    git
      [
        "clone";
        "-q";
        "--branch";
        "feat";
        ".git/timeout-origin.git";
        ".git/timeout-writer";
      ];
    let writer_git =
      Git_env.run_git ~cwd:(Stdlib.Filename.concat repo ".git/timeout-writer")
    in
    writer_git [ "config"; "user.email"; "writer@example.com" ];
    writer_git [ "config"; "user.name"; "Writer" ];
    writer_git [ "commit"; "-q"; "--amend"; "-m"; "remote feature" ];
    writer_git [ "push"; "-q"; "--force"; "origin"; "feat" ]);
  let old_remote =
    capture [ "--git-dir=.git/timeout-origin.git"; "rev-parse"; "feat" ]
  in
  let marker = Stdlib.Filename.concat repo ".git/blocked.pid" in
  let push_started = Stdlib.Filename.concat repo ".git/push-started" in
  let block =
    Printf.sprintf "echo $$ > %s.tmp\nmv %s.tmp %s\nexec sleep 30\n"
      (Stdlib.Filename.quote marker)
      (Stdlib.Filename.quote marker)
      (Stdlib.Filename.quote marker)
  in
  let receive_pack = Stdlib.Filename.concat repo ".git/timeout-receive-pack" in
  write_script receive_pack
    (Printf.sprintf "touch %s\n%s\nexec git-receive-pack \"$@\"\n"
       (Stdlib.Filename.quote push_started)
       (if String.equal stage "push" then block else ""));
  git [ "config"; "remote.origin.receivepack"; receive_pack ];
  let upload_pack = Stdlib.Filename.concat repo ".git/timeout-upload-pack" in
  if not (String.equal stage "push") then (
    git [ "update-ref"; "-d"; "refs/remotes/origin/feat" ];
    write_script upload_pack block);
  let clock = Eio_mock.Clock.make () in
  let real =
    E.make_io
      ~clock:(clock :> float Eio.Time.clock_ty Eio.Time.clock)
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~path:repo
  in
  let last_command = ref "not started" in
  let last_result = ref "no result" in
  let describe () =
    Printf.sprintf "%s; last command: %s; last result: %s" stage !last_command
      !last_result
  in
  let io =
    E.
      {
        git =
          (fun args ->
            last_command := String.concat ~sep:" " args;
            last_result := "running";
            let result =
              match args with
              | command :: rest
                when String.equal command
                       (if String.equal stage "observation" then "ls-remote"
                        else "fetch")
                     && not (String.equal stage "push") ->
                  real.git (command :: ("--upload-pack=" ^ upload_pack) :: rest)
              | "push" :: rest when String.equal stage "push" ->
                  real.git ("push" :: ("--receive-pack=" ^ receive_pack) :: rest)
              | _ -> real.git args
            in
            let code, _, stderr = result in
            last_result := Printf.sprintf "exit %d: %s" code stderr;
            result);
      }
  in
  let intent =
    B.
      {
        base = "main";
        policy = Rewrite;
        purpose = Publish_session "timeout-fixture";
      }
  in
  let repair_token = ref None in
  let rec drive io remaining state =
    if remaining = 0 then failwith "publication did not terminate";
    let state =
      match B.decode (B.yojson_of_t state) with
      | Ok restored -> restored
      | Error reason -> failwith reason
    in
    match (B.operation state, B.pending state) with
    | Some operation, Some command ->
        let result =
          E.execute ~io ~prefix:"refs/onton/reconcile/timeout-test"
            ~branch:"feat" ~operation command
        in
        let next, effects =
          B.step state (B.Result { token = command.token; at = 100.; result })
        in
        List.iter effects ~f:(function
          | B.Repair token -> repair_token := Some token
          | B.Execute _ | B.Start_repair _ | B.Completed _ -> ());
        drive io (remaining - 1) next
    | None, _ | _, None -> state
  in
  let publish () =
    drive io 100
      (fst
         (B.step
            (fst (B.step B.empty (B.Materialized (B.New_branch materialized))))
            (B.Request intent)))
  in
  let cleanup () =
    (* Git may leave its transport or hook child behind when cancelled. The
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
        let finished = ref false in
        let real_clock = Eio.Stdenv.clock env in
        let _, outcome =
          Eio.Fiber.pair
            (fun () ->
              (* Allow setup under parallel suite load without charging it to
                 the cancellation watchdog. The marker proves the real Git
                 transport has entered the selected blocked phase. *)
              await_phase ~clock:real_clock ~timeout:60.0 ~finished
                ~started:(fun () -> Stdlib.Sys.file_exists marker)
                ~describe;
              expire_phase ~real_clock ~mock_clock:clock ~timeout:10.0 ~finished
                ~describe)
            (fun () ->
              Stdlib.Fun.protect ~finally:(fun () -> finished := true) publish)
        in
        outcome)
  in
  (match B.phase outcome with
  | Some (B.Waiting { reason; _ })
    when String.is_substring reason ~substring:"timeout" ->
      ()
  | None
  | Some
      ( B.Preparing | Integrating | Publishing | Confirming | Waiting _
      | Recovering | Settled | Intervention _ | Repairing _ ) ->
      failwith (stage ^ ": command timeout did not produce a durable retry"));
  (match B.operation outcome with
  | Some operation when Option.is_none operation.repair -> ()
  | Some _ | None -> failwith "infrastructure timeout consumed repair budget");
  if Bool.(Stdlib.Sys.file_exists push_started <> String.equal stage "push")
  then failwith (stage ^ ": unexpected push invocation");
  let remote =
    capture [ "--git-dir=.git/timeout-origin.git"; "rev-parse"; "feat" ]
  in
  if not (String.equal remote old_remote) then
    failwith (stage ^ ": timeout changed remote history");
  git [ "config"; "--unset"; "remote.origin.receivepack" ];
  let real =
    E.make_io ~clock:(Eio.Stdenv.clock env)
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~path:repo
  in
  let retried =
    match
      Eio.Time.with_timeout (Eio.Stdenv.clock env) 60.0 (fun () ->
          Ok (drive real 100 (fst (B.step outcome B.Recover))))
    with
    | Ok retried -> retried
    | Error `Timeout ->
        failwith (stage ^ ": real-clock publication retry timed out")
  in
  if Option.is_some !repair_token then
    failwith "transport recovery bypassed deterministic remote integration";
  List.iter
    [ ("feature.txt", "feature"); ("extra.txt", "extra") ]
    ~f:(fun (file, expected) ->
      let actual =
        capture [ "--git-dir=.git/timeout-origin.git"; "show"; "feat:" ^ file ]
      in
      if not (String.equal actual expected) then
        failwith (stage ^ ": resumed publication lost " ^ file));
  if not (Option.equal B.equal_phase (B.phase retried) (Some B.Settled)) then
    failwith (stage ^ ": resumed publication did not settle");
  if
    not
      (String.equal
         (capture [ "--git-dir=.git/timeout-origin.git"; "rev-parse"; "feat" ])
         (capture [ "rev-parse"; "HEAD" ]))
  then failwith (stage ^ ": retry published the wrong revision");
  Stdlib.print_endline (stage ^ " timeout and retry: OK")

let () =
  Eio_main.run @@ fun env ->
  check_phase_waiters env;
  List.iter [ "observation"; "fetch"; "push" ] ~f:(fun stage ->
      Onton_test_support.Branch_reconcile_git_fixture.timed
        ~name:("push-timeout/" ^ stage) (fun () -> scenario env stage))

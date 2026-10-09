(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
module B = Branch_reconcile
module E = Branch_reconcile_executor
module Git = Onton_test_support.Git_env

let check label condition = if not condition then failwith label
let get = function Ok value -> value | Error reason -> failwith reason
let restart state = get (B.decode (B.yojson_of_t state))

let commit dir message =
  Git.run_git ~cwd:dir [ "commit"; "--allow-empty"; "-qm"; message ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let prefix = "refs/onton/reconcile/legacy-verification"

let drive ?(on_effects = fun _ -> ()) io ~cut ~after ~before_command initial =
  let rec loop index now state =
    if index > 20 then
      failwith
        ("verification did not settle: "
        ^ Yojson.Safe.to_string (B.yojson_of_t state));
    let state = restart state in
    match (B.operation state, B.pending state) with
    | Some operation, Some command ->
        before_command command;
        let state, command, result =
          if index = cut then (
            if after then
              ignore (E.execute ~io ~prefix ~branch:"patch" ~operation command);
            let state, _ = B.step (restart state) B.Recover in
            let operation = Option.get (B.operation state) in
            let command = Option.get (B.pending state) in
            let result =
              E.execute ~io ~prefix ~branch:"patch" ~operation command
            in
            (state, command, result))
          else
            ( state,
              command,
              E.execute ~io ~prefix ~branch:"patch" ~operation command )
        in
        let state, effects =
          B.step state (B.Result { token = command.B.token; at = now; result })
        in
        on_effects effects;
        loop (index + 1) now state
    | Some operation, None -> (
        match operation.B.phase with
        | B.Waiting { until; reason } ->
            Printf.eprintf "verification probe retry: %s\n%!" reason;
            let now = until +. 0.1 in
            loop (index + 1) now (fst (B.step state (B.Tick now)))
        | B.Preparing | B.Integrating | B.Repairing _ | B.Publishing
        | B.Confirming | B.Recovering | B.Settled | B.Intervention _ ->
            restart state)
    | None, _ -> failwith "missing verification operation"
  in
  loop 0 0. initial

let resume_reconciliation io state =
  let state =
    Onton_core_test_support.Publication_fixture.exhausted_diagnosis state
  in
  let requested, effects =
    B.step (restart state)
      B.(
        Request
          {
            base = "main";
            policy = Preserve_ancestry;
            purpose = Reconcile_request "repair-legacy-publication";
          })
  in
  check "new intent alone does not authorize legacy mutation" (effects = []);
  let resumed, _ = B.step (restart requested) B.Resume in
  let final =
    drive io ~cut:0 ~after:true ~before_command:(fun _ -> ()) resumed
  in
  check "explicit reconciliation resolves failed legacy verification"
    (B.phase final = Some B.Settled);
  final

let () =
  Eio_main.run @@ fun env ->
  let make_io dir =
    E.make_io
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~clock:(Eio.Stdenv.clock env) ~path:dir
  in
  List.iter
    (fun scenario ->
      Git.with_temp_repo (fun remote ->
          let base = commit remote "base" in
          Git.with_temp_repo (fun dir ->
              Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
              Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
              Git.run_git ~cwd:dir [ "checkout"; "-qb"; "patch"; "origin/main" ];
              let source = commit dir "feature" in
              if scenario <> "missing" then
                Git.run_git ~cwd:dir
                  [ "push"; "-q"; "origin"; "HEAD:refs/heads/patch" ];
              if scenario = "different" then
                Git.run_git ~cwd:remote
                  [ "update-ref"; "refs/heads/patch"; base ];
              let retired_remote =
                if scenario = "retired-remote" then (
                  Git.run_git ~cwd:remote
                    [ "checkout"; "-qb"; "other-work"; base ];
                  let revision = commit remote "independent remote work" in
                  Git.run_git ~cwd:remote
                    [ "update-ref"; "refs/heads/patch"; revision ];
                  Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
                  Some revision)
                else None
              in
              if scenario = "contained" then
                Git.run_git ~cwd:remote
                  [ "update-ref"; "refs/heads/main"; source ];
              if scenario = "dirty" then (
                let out = open_out (Filename.concat dir "uncommitted") in
                output_string out "retain me";
                close_out out);
              let expected_remote =
                Git.git_capture ~cwd:dir
                  [ "ls-remote"; "origin"; "refs/heads/patch" ]
              in
              let config =
                Git.git_capture ~cwd:dir [ "config"; "--local"; "--list" ]
              in
              let raw = make_io dir in
              let mutated = ref false in
              let io =
                E.
                  {
                    git =
                      (fun args ->
                        if
                          List.exists
                            (fun verb -> List.mem verb args)
                            [
                              "push";
                              "rebase";
                              "merge";
                              "reset";
                              "checkout";
                              "commit";
                            ]
                        then (
                          mutated := true;
                          failwith "verification attempted Git mutation");
                        raw.git args);
                  }
              in
              List.iter
                (fun cut ->
                  List.iter
                    (fun after ->
                      let initial =
                        B.import_legacy_publication B.empty ~base:"missing-base"
                      in
                      let final =
                        drive io ~cut ~after
                          ~before_command:(fun _ -> ())
                          initial
                      in
                      let expected =
                        scenario = "matched" || scenario = "contained"
                      in
                      if not (Bool.equal (B.publications final <> []) expected)
                      then
                        failwith
                          (Printf.sprintf
                             "confirmation scenario=%s cut=%d after=%b state=%s"
                             scenario cut after
                             (Yojson.Safe.to_string (B.yojson_of_t final)));
                      check
                        "verification settles or requests observation-only \
                         diagnosis"
                        (if expected then B.phase final = Some B.Settled
                         else
                           Onton_core_test_support.Publication_fixture
                           .is_diagnosis final);
                      check "verification never mutates checkout or pushes"
                        (not !mutated);
                      check "checkout retained"
                        (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                        = source);
                      check "remote retained"
                        (Git.git_capture ~cwd:dir
                           [ "ls-remote"; "origin"; "refs/heads/patch" ]
                        = expected_remote);
                      check "tracking configuration retained"
                        (Git.git_capture ~cwd:dir
                           [ "config"; "--local"; "--list" ]
                        = config);
                      if scenario = "dirty" then
                        check "dirty file retained"
                          (Sys.file_exists (Filename.concat dir "uncommitted")))
                    [ false; true ])
                [ 0; 1; 2 ];
              if scenario = "matched" then (
                let failed_probe = ref false in
                let retry_io =
                  E.
                    {
                      git =
                        (fun args ->
                          if (not !failed_probe) && List.mem "ls-remote" args
                          then (
                            failed_probe := true;
                            (128, "", "temporary verification probe outage"))
                          else io.git args);
                    }
                in
                let retried =
                  drive retry_io ~cut:(-1) ~after:false
                    ~before_command:(fun _ -> ())
                    (B.import_legacy_publication B.empty ~base:"main")
                in
                check "failed probe resumes through owner backoff"
                  (!failed_probe && B.phase retried = Some B.Settled);
                let changed = ref false in
                let final =
                  drive io ~cut:(-1) ~after:false
                    ~before_command:(fun command ->
                      if
                        B.equal_command_kind command.B.kind
                          (B.Confirm (Option.get (B.Commit.make source)))
                        && not !changed
                      then (
                        changed := true;
                        ignore (commit dir "external work")))
                    (B.import_legacy_publication B.empty ~base:"main")
                in
                check "late checkout change prevents receipt"
                  (B.publications final = []);
                let external_head =
                  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                in
                check "external commit retained" (external_head <> source);
                ignore (resume_reconciliation raw final);
                let published =
                  Git.git_capture ~cwd:remote
                    [ "rev-parse"; "refs/heads/patch" ]
                in
                Git.run_git ~cwd:dir
                  [ "merge-base"; "--is-ancestor"; source; published ];
                Git.run_git ~cwd:dir
                  [ "merge-base"; "--is-ancestor"; external_head; published ]);
              if scenario = "missing" || scenario = "different" then (
                let stopped =
                  drive io ~cut:(-1) ~after:false
                    ~before_command:(fun _ -> ())
                    (B.import_legacy_publication B.empty ~base:"main")
                in
                check "unconfirmed legacy publication requests diagnosis"
                  (Onton_core_test_support.Publication_fixture.is_diagnosis
                     stopped);
                ignore (resume_reconciliation raw stopped);
                let published =
                  Git.git_capture ~cwd:remote
                    [ "rev-parse"; "refs/heads/patch" ]
                in
                Git.run_git ~cwd:dir
                  [ "merge-base"; "--is-ancestor"; source; published ];
                Git.run_git ~cwd:dir
                  [ "merge-base"; "--is-ancestor"; base; published ]);
              (match retired_remote with
              | None -> ()
              | Some retained ->
                  let stopped =
                    drive io ~cut:(-1) ~after:false
                      ~before_command:(fun _ -> ())
                      (B.import_legacy_publication B.empty ~base:"main")
                  in
                  check "independent remote requires diagnosis"
                    (Onton_core_test_support.Publication_fixture.is_diagnosis
                       stopped);
                  let stopped =
                    Onton_core_test_support.Publication_fixture
                    .exhausted_diagnosis stopped
                  in
                  Git.run_git ~cwd:remote
                    [ "update-ref"; "refs/heads/patch"; base ];
                  let requested, _ =
                    B.step (restart stopped)
                      B.(
                        Request
                          {
                            base = "main";
                            policy = Preserve_ancestry;
                            purpose =
                              Reconcile_request "retain-disappeared-remote";
                          })
                  in
                  let resumed, _ = B.step (restart requested) B.Resume in
                  let offered = ref None in
                  let repairing =
                    drive
                      ~on_effects:
                        (List.iter (function
                          | B.Repair token -> offered := Some token
                          | B.Start_repair _ | B.Execute _ | B.Completed _ -> ()))
                      raw ~cut:0 ~after:true
                      ~before_command:(fun _ -> ())
                      resumed
                  in
                  check
                    "lost remote evidence cannot be bypassed by current lease"
                    (Git.git_capture ~cwd:remote
                       [ "rev-parse"; "refs/heads/patch" ]
                    = base);
                  let operation = Option.get (B.operation repairing) in
                  check "retained independent work reaches history recovery"
                    (match operation.B.repair with
                    | Some { B.mode = B.History_recovery _; _ } -> true
                    | Some { B.mode = B.Content_repair | B.Diagnosis _; _ }
                    | None ->
                        false);
                  let retained_commit = Option.get (B.Commit.make retained) in
                  check "old remote obligation survives restart"
                    (List.mem retained_commit operation.B.recovery_revisions);
                  let refs =
                    Git.git_capture ~cwd:dir
                      [ "for-each-ref"; "--format=%(objectname)"; prefix ]
                  in
                  check "old remote work is pinned independently of remote ref"
                    (List.mem retained (String.split_on_char '\n' refs));
                  let token = Option.get !offered in
                  let claimed, effects =
                    B.step (restart repairing) (B.Repair_started token)
                  in
                  check "history repair is durably claimable"
                    (List.exists
                       (function
                         | B.Start_repair current -> B.equal_token token current
                         | B.Execute _ | B.Repair _ | B.Completed _ -> false)
                       effects);
                  Git.run_git ~cwd:dir [ "merge"; "--no-edit"; retained ];
                  let completed, _ =
                    B.step (restart claimed)
                      (B.Repair_completed { token; at = 100. })
                  in
                  let final =
                    drive raw ~cut:0 ~after:true
                      ~before_command:(fun _ -> ())
                      completed
                  in
                  check "preserving repair settles after verification"
                    (B.phase final = Some B.Settled);
                  let published =
                    Git.git_capture ~cwd:remote
                      [ "rev-parse"; "refs/heads/patch" ]
                  in
                  List.iter
                    (fun revision ->
                      Git.run_git ~cwd:dir
                        [ "merge-base"; "--is-ancestor"; revision; published ])
                    [ source; retained; base ]);
              let initial = B.import_legacy_publication B.empty ~base:"main" in
              let operation = Option.get (B.operation initial) in
              let publishing =
                Onton_core_test_support.Publication_fixture.publishing
                  ~candidate:source
                  ~step:(fun state event -> fst (B.step state event))
                  ~state:Fun.id B.empty
              in
              let command = Option.get (B.pending publishing) in
              let called = ref false in
              let forbidden_io =
                E.
                  {
                    git =
                      (fun _ ->
                        called := true;
                        failwith "unexpected I/O");
                  }
              in
              let result =
                E.execute ~io:forbidden_io ~prefix ~branch:"patch" ~operation
                  command
              in
              check "forged verification push rejected before I/O"
                ((not !called)
                && B.equal_result result
                     (B.Needs_diagnosis "command_not_allowed_for_purpose")))))
    [
      "matched"; "contained"; "missing"; "different"; "dirty"; "retired-remote";
    ];
  print_endline
    "legacy publication verification, restarts and mutation refusal: OK"

let () =
  Eio_main.run @@ fun env ->
  List.iter
    (fun existing ->
      Git.with_temp_repo (fun remote ->
          ignore (commit remote "base");
          Git.with_temp_repo (fun managed ->
              Git.run_git ~cwd:managed [ "remote"; "add"; "origin"; remote ];
              Git.run_git ~cwd:managed [ "fetch"; "-q"; "origin" ];
              let path = Filename.concat managed "linked" in
              if existing then (
                Git.run_git ~cwd:managed
                  [ "worktree"; "add"; "-qb"; "patch"; path; "origin/main" ];
                Git.run_git ~cwd:path
                  [ "push"; "-q"; "origin"; "HEAD:refs/heads/patch" ]);
              let module Real =
                (val Worktree.make ~fs:(Eio.Stdenv.fs env)
                       ~clock:(Eio.Stdenv.clock env)
                       ~process_mgr:(Eio.Stdenv.process_mgr env)
                       ~config:Worktree_lifecycle.git ~repo_root:managed)
              in
              let unwanted = ref false in
              let forbidden () =
                unwanted := true;
                failwith "verification entered checkout mutation path"
              in
              let module W = struct
                include Real

                let ensure_ready ~path:_ ~branch:_ = forbidden ()

                let create ~project_name:_ ~patch_id:_ ~branch:_ ~base_ref:_ =
                  forbidden ()

                let prune_stale_for_branch _ = forbidden ()
                let run_hook ~clock:_ ~script:_ ~cwd:_ ~env:_ () = forbidden ()
              end in
              let patch =
                List.hd (Onton_test_support.Test_generators.mk_linear_patches 1)
              in
              let patch =
                {
                  patch with
                  Types.Patch.branch = Types.Branch.of_string "patch";
                }
              in
              let patch_id = patch.Types.Patch.id in
              let gameplan =
                Onton_test_support.Test_generators.make_test_gameplan [ patch ]
              in
              let module Env = struct
                let runtime =
                  Runtime.create ~gameplan
                    ~main_branch:(Types.Branch.of_string "main")
                    ()

                let clock = Eio.Stdenv.clock env
                let fs = Eio.Stdenv.fs env
                let worktree_mutex = Eio.Mutex.create ()
                let hook_mutex = Eio.Mutex.create ()
                let fetch_mutex = Eio.Mutex.create ()
                let project_name = "legacy-verification-test"

                let user_config =
                  { User_config.on_worktree_create = Some "false" }
              end in
              Runtime.update_orchestrator Env.runtime (fun orch ->
                  Orchestrator.set_worktree_path orch patch_id
                    (if existing then Filename.concat managed "stale-path"
                     else path));
              let module WS = Worktree_setup.Make (W) (Env) in
              let checkpoint =
                Filename.concat managed "legacy-checkpoint.json"
              in
              let persist = Persistence.save_snapshot ~path:checkpoint in
              let outcome =
                Branch_reconcile_runner.run ~runtime:Env.runtime ~persist
                  ~patch_id
                  ~now:(fun () -> 1.)
                  ~execute:(WS.execute_reconciliation ~patch_id)
                  B.(
                    Request
                      {
                        base = "main";
                        policy = Preserve_ancestry;
                        purpose = Verify_publication;
                      })
              in
              check "wrapper never provisions, repairs metadata or runs hooks"
                (not !unwanted);
              check "verification wrapper outcome"
                (if existing then outcome = Branch_reconcile_runner.Idle
                 else
                   match outcome with
                   | Branch_reconcile_runner.Repair_needed _ -> true
                   | Branch_reconcile_runner.Idle
                   | Branch_reconcile_runner.Waiting
                   | Branch_reconcile_runner.Intervention _
                   | Branch_reconcile_runner.Checkpoint_failed _ ->
                       false);
              if existing then
                check "registered checkout replaces stale snapshot path"
                  (Runtime.read Env.runtime (fun snap ->
                       match
                         (Orchestrator.agent snap.Runtime.orchestrator patch_id)
                           .Patch_agent.worktree_path
                       with
                       | Some actual ->
                           Unix.realpath actual = Unix.realpath path
                       | None -> false));
              check "missing checkout is not created"
                (Bool.equal (Sys.file_exists path) existing);
              if not existing then (
                let saved = get (Persistence.load ~path:checkpoint) in
                Git.run_git ~cwd:managed
                  [ "worktree"; "add"; "-qb"; "patch"; path; "origin/main" ];
                let feature = commit path "restored feature" in
                Git.run_git ~cwd:path
                  [ "push"; "-q"; "origin"; "HEAD:refs/heads/patch" ];
                let module Restored = struct
                  include Env

                  let runtime =
                    Runtime.create ~gameplan
                      ~main_branch:(Types.Branch.of_string "main")
                      ~snapshot:saved ()
                end in
                let module Verification = Worktree_setup.Make (W) (Restored) in
                let resumed =
                  Branch_reconcile_runner.run ~runtime:Restored.runtime ~persist
                    ~patch_id
                    ~now:(fun () -> 2.)
                    ~execute:(Verification.execute_reconciliation ~patch_id)
                    B.Recover
                in
                check "restored checkout leaves legacy intervention"
                  (resumed = Branch_reconcile_runner.Idle && not !unwanted);
                check
                  "resumed verification leaves the external commit untouched"
                  (Git.git_capture ~cwd:path [ "rev-parse"; "HEAD" ] = feature);
                let upstream = commit remote "advance after legacy recovery" in
                let module Normal = Worktree_setup.Make (Real) (Restored) in
                let request =
                  B.(
                    Request
                      {
                        base = "main";
                        policy = Preserve_ancestry;
                        purpose = Reconcile_request "after-legacy-recovery";
                      })
                in
                let normal =
                  Branch_reconcile_runner.run ~runtime:Restored.runtime ~persist
                    ~patch_id
                    ~now:(fun () -> 3.)
                    ~execute:(Normal.execute_reconciliation ~patch_id)
                    request
                in
                check
                  "normal reconciliation remains live after legacy verification"
                  (normal = Branch_reconcile_runner.Idle);
                let head = Git.git_capture ~cwd:path [ "rev-parse"; "HEAD" ] in
                Git.run_git ~cwd:path
                  [ "merge-base"; "--is-ancestor"; feature; head ];
                Git.run_git ~cwd:path
                  [ "merge-base"; "--is-ancestor"; upstream; head ];
                check "normal reconciliation publishes the preserved result"
                  (Git.git_capture ~cwd:remote
                     [ "rev-parse"; "refs/heads/patch" ]
                  = head);
                let snapshot = get (Persistence.load ~path:checkpoint) in
                let owner =
                  (Orchestrator.agent snapshot.Runtime.orchestrator patch_id)
                    .Patch_agent.branch_reconcile
                in
                check "normal intent replaces legacy verification durably"
                  (B.phase owner = Some B.Settled
                  &&
                  match B.operation owner with
                  | Some operation ->
                      operation.B.intent.B.purpose
                      = B.Reconcile_request "after-legacy-recovery"
                  | None -> false)))))
    [ false; true ];
  print_endline
    "legacy verification bypasses checkout creation, cleanup and hooks: OK"

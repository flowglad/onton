(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
module B = Branch_reconcile
module E = Branch_reconcile_executor
module R = Branch_reconcile_runner
module Git = Onton_test_support.Git_env

let get = function Ok v -> v | Error e -> failwith e
let check msg b = if not b then failwith msg

let commit dir file content message =
  let oc = open_out_bin (Filename.concat dir file) in
  output_string oc content;
  close_out oc;
  Git.run_git ~cwd:dir [ "add"; file ];
  Git.run_git ~cwd:dir [ "commit"; "-q"; "-m"; message ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let sha text =
  match B.Commit.make text with
  | Some sha -> sha
  | None -> failwith "invalid fixture SHA"

let id = Types.Patch_id.of_string "1"

let gameplan =
  (get
     (Gameplan_parser.parse_string
        "projectName: demo\n\
         owner: owner\n\
         repo: repo\n\
         problemStatement: test\n\
         solutionSummary: test\n\
         patches:\n\
        \  - number: 1\n\
        \    title: patch\n\
        \    description: patch\n\
        \    dependsOn: []\n\
         dependencyGraph:\n\
        \  - patch: 1\n\
        \    dependsOn: []\n"))
    .Gameplan_parser.gameplan

let make_runtime () =
  Runtime.create ~gameplan ~main_branch:(Types.Branch.of_string "main") ()

let state runtime =
  Runtime.read runtime (fun s ->
      (Orchestrator.agent s.Runtime.orchestrator id)
        .Patch_agent.branch_reconcile)

let prefix = "refs/onton/reconcile/test/patch"
let intent = B.{ base = "main"; policy = Rewrite; purpose = Reconcile_base }

let execute io ~operation command =
  E.execute ~io ~prefix ~branch:"patch" ~operation command

let run runtime persist io event =
  R.run ~runtime ~persist ~patch_id:id
    ~now:(fun () -> 100.)
    ~execute:(execute io) event

let persist path = Persistence.save_snapshot ~path

let prepare remote dir =
  Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
  Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
  Git.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "patch"; "origin/main" ]

let write dir file bytes =
  let channel = open_out_bin (Filename.concat dir file) in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel bytes)

let require_turn = function
  | R.Repair_needed token -> token
  | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _ ->
      failwith "recovery did not offer an agent turn"

let publication_recovery env ~persistent ~timeout ~local_hook =
  Git.with_temp_repo (fun remote ->
      let base = commit remote "base" "base\n" "base" in
      Git.with_temp_repo (fun dir ->
          prepare remote dir;
          let original =
            commit dir "validation" "rejected\n" "patch requiring validation"
          in
          if local_hook then (
            write dir ".git/hooks/pre-push"
              "#!/bin/sh\n\
               if [ \"$(cat validation)\" != approved ]; then echo 'validation \
               must be approved' >&2; exit 1; fi\n";
            Unix.chmod (Filename.concat dir ".git/hooks/pre-push") 0o755);
          if (not timeout) && not local_hook then (
            write remote ".git/hooks/pre-receive"
              "#!/bin/sh\n\
               while read old new ref; do\n\
              \  value=$(git show \"$new:validation\")\n\
              \  if [ \"$value\" != approved ]; then echo 'validation must be \
               approved' >&2; exit 1; fi\n\
               done\n";
            Unix.chmod (Filename.concat remote ".git/hooks/pre-receive") 0o755;
            Git.run_git ~cwd:dir
              [
                "config";
                "remote.origin.receivepack";
                "git -c core.hooksPath="
                ^ Filename.quote (remote ^ "/.git/hooks")
                ^ " receive-pack";
              ]);
          let io =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let pushes = ref 0 in
          let actual_push = ref (not timeout) in
          let constrained =
            E.
              {
                git =
                  (fun args ->
                    match args with
                    | "push" :: _ ->
                        incr pushes;
                        if !actual_push then io.git args
                        else
                          ( 124,
                            "",
                            "Git command outcome uncertain after timeout" )
                    | [] | _ :: _ -> io.git args);
              }
          in
          let runtime = make_runtime () in
          Runtime.update_orchestrator runtime (fun orch ->
              Orchestrator.set_worktree_path orch id dir);
          let checkpoint = Filename.temp_file "publication-recovery" ".json" in
          Fun.protect
            ~finally:(fun () -> Sys.remove checkpoint)
            (fun () ->
              ignore
                (run runtime (persist checkpoint) constrained
                   (B.Materialized (B.New_branch (sha base))));
              let outcome =
                run runtime (persist checkpoint) constrained
                  (B.Request
                     {
                       intent with
                       purpose = B.Publish_session "validation";
                       policy = B.Preserve_ancestry;
                     })
              in
              if local_hook then (
                check "Onton publication bypasses local pre-push hooks"
                  (outcome = R.Idle && !pushes = 1);
                check "hook bypass publishes original content without a repair"
                  (Git.git_capture ~cwd:remote [ "show"; "patch:validation" ]
                  = "rejected"))
              else
                let outcome =
                  if timeout then (
                    check "first failed push backs off"
                      (outcome = R.Waiting && !pushes = 1);
                    run runtime (persist checkpoint) constrained (B.Tick 1000.))
                  else outcome
                in
                ignore (require_turn outcome);
                let snapshot = get (Persistence.load ~path:checkpoint) in
                let runtime =
                  Runtime.create ~gameplan
                    ~main_branch:(Types.Branch.of_string "main")
                    ~snapshot ()
                in
                let token =
                  require_turn
                    (run runtime (persist checkpoint) constrained B.Recover)
                in
                let turns = ref 0 in
                let perform ~agent:_ ~(turn : B.repair_turn) =
                  incr turns;
                  check "publication rejection receives publication repair task"
                    (match turn.B.mode with
                    | B.History_recovery { task = B.Repair_publication; _ } ->
                        true
                    | B.Diagnosis _ | B.Content_repair
                    | B.History_recovery
                        {
                          task = B.Finish_local_work | B.Reconstruct_history;
                          _;
                        } ->
                        false);
                  actual_push := true;
                  if persistent then
                    Git.run_git ~cwd:dir
                      [ "commit"; "--allow-empty"; "-qm"; "Repair attempt" ]
                  else
                    ignore
                      (commit dir "validation" "approved\n"
                         "Repair publication validation");
                  B.Repair_completed { token = turn.B.token; at = 1001. }
                in
                let repair token =
                  R.run_repair ~runtime ~persist:(persist checkpoint)
                    ~patch_id:id
                    ~with_capacity:(fun f -> f ())
                    ~now:(fun () -> 1001.)
                    ~execute:(fun ~agent:_ ~operation command ->
                      execute constrained ~operation command)
                    ~perform token
                in
                let outcome = repair token in
                if persistent then (
                  let outcome = repair (require_turn outcome) in
                  check "persistent rejection holds only after two agent turns"
                    (match outcome with
                    | R.Intervention _ -> !turns = 2 && !pushes = 3
                    | R.Idle | R.Waiting | R.Repair_needed _
                    | R.Checkpoint_failed _ ->
                        false);
                  check "failed publication retains original work"
                    (let code, _, _ =
                       io.git
                         [ "merge-base"; "--is-ancestor"; original; "HEAD" ]
                     in
                     code = 0))
                else (
                  check "local publication repair reaches verified receipt"
                    (outcome = R.Idle && !turns = 1
                    && B.phase (state runtime) = Some B.Settled);
                  check "published content passes remote validation"
                    (Git.git_capture ~cwd:remote [ "show"; "patch:validation" ]
                    = "approved");
                  check "bounded deterministic push attempts"
                    (!pushes = if timeout then 3 else 2)))))

let checkout_recovery env detached =
  Git.with_temp_repo (fun remote ->
      let base = commit remote "base" "base\n" "base" in
      Git.with_temp_repo (fun dir ->
          prepare remote dir;
          ignore (commit dir "patch-work" "patch\n" "patch work");
          Git.run_git ~cwd:dir
            (if detached then [ "checkout"; "--detach"; "-q" ]
             else [ "checkout"; "-qb"; "outside" ]);
          let observed =
            commit dir "interrupted" "retained\n" "interrupted work"
          in
          let io =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let runtime = make_runtime () in
          Runtime.update_orchestrator runtime (fun orch ->
              Orchestrator.set_worktree_path orch id dir);
          let persist _ = Ok () in
          ignore
            (run runtime persist io (B.Materialized (B.New_branch (sha base))));
          let token =
            require_turn (run runtime persist io (B.Request intent))
          in
          let outcome =
            R.run_repair ~runtime ~persist ~patch_id:id
              ~with_capacity:(fun f -> f ())
              ~now:(fun () -> 100.)
              ~execute:(fun ~agent:_ ~operation command ->
                execute io ~operation command)
              ~perform:(fun ~agent:_ ~(turn : B.repair_turn) ->
                Git.run_git ~cwd:dir [ "checkout"; "-q"; "patch" ];
                (* Retaining the unexpected checkout does not authorize adding
                   its commits to the managed patch. Restore the captured branch. *)
                B.Repair_completed { token = turn.token; at = 100. })
              token
          in
          check "valid unexpected checkout reaches owned recovery"
            (outcome = R.Idle && B.phase (state runtime) = Some B.Settled);
          check
            "unexpected checkout is retained without entering the patch diff"
            (List.mem (sha observed) (B.required_revisions (state runtime))
            && Git.git_exit_code ~cwd:remote
                 [ "cat-file"; "-e"; "patch:interrupted" ]
               <> 0)))

let () =
  Eio_main.run @@ fun env ->
  List.iter
    (fun timeout ->
      Git.with_temp_repo (fun remote ->
          let base = commit remote "base" "base\n" "base" in
          Git.with_temp_repo (fun dir ->
              prepare remote dir;
              ignore (commit dir "test-a" "first\n" "first patch commit");
              ignore (commit dir "test-b" "second\n" "second patch commit");
              ignore (commit remote "upstream" "upstream\n" "base advancement");
              Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
              let io =
                E.make_io
                  ~process_mgr:(Eio.Stdenv.process_mgr env)
                  ~clock:(Eio.Stdenv.clock env) ~path:dir
              in
              let attempts = ref 0 in
              let constrained =
                E.
                  {
                    git =
                      (fun args ->
                        if
                          args
                          = [ "-c"; "core.editor=true"; "merge"; "--continue" ]
                        then (
                          incr attempts;
                          ( 124,
                            "",
                            "Git command outcome uncertain after timeout" ))
                        else io.git args);
                  }
              in
              let runtime = make_runtime () in
              Runtime.update_orchestrator runtime (fun orch ->
                  Orchestrator.set_worktree_path orch id dir);
              let checkpoint = Filename.temp_file "recovery-progress" ".json" in
              Fun.protect
                ~finally:(fun () -> Sys.remove checkpoint)
                (fun () ->
                  ignore
                    (run runtime (persist checkpoint) io
                       (B.Materialized (B.New_branch (sha base))));
                  if timeout then
                    Git.run_git ~cwd:dir
                      [ "merge"; "--no-commit"; "--no-ff"; "origin/main" ]
                  else (
                    write dir "test-a" "staged fix\n";
                    Git.run_git ~cwd:dir [ "add"; "test-a" ];
                    write dir "test-b" "unstaged fix\n";
                    write dir "test-c" "untracked fix\n");
                  let result =
                    run runtime (persist checkpoint) constrained
                      (B.Request { intent with policy = B.Preserve_ancestry })
                  in
                  if timeout then
                    check "first mutation timeout waits" (result = R.Waiting)
                  else ignore (require_turn result);
                  let snapshot = get (Persistence.load ~path:checkpoint) in
                  let runtime =
                    Runtime.create ~gameplan
                      ~main_branch:(Types.Branch.of_string "main")
                      ~snapshot ()
                  in
                  let guidance =
                    "Continue the changes, run the validation, and commit \
                     unstaged files"
                  in
                  Runtime.update_orchestrator runtime (fun orch ->
                      let orch =
                        Orchestrator.reset_intervention_state orch id
                      in
                      Orchestrator.send_human_message orch id guidance);
                  let actions =
                    Runtime.read runtime (fun snap ->
                        Patch_controller.plan_actions snap.Runtime.orchestrator
                          ~patches:gameplan.Types.Gameplan.patches)
                  in
                  check
                    "scheduler keeps recovery runnable after restart, bump and \
                     human guidance"
                    (List.exists
                       (fun action ->
                         Orchestrator.equal_action action
                           (Orchestrator.Reconcile_branch
                              (id, (Option.get (B.operation (state runtime))).id)))
                       actions);
                  let token =
                    require_turn
                      (run runtime (persist checkpoint) constrained
                         (if timeout then B.Tick 1000. else B.Recover))
                  in
                  if timeout then
                    check "two deterministic attempts before recovery"
                      (!attempts = 2);
                  let turns = ref 0 in
                  let outcome =
                    R.run_repair ~runtime ~persist:(persist checkpoint)
                      ~patch_id:id
                      ~with_capacity:(fun f -> f ())
                      ~now:(fun () -> 1001.)
                      ~execute:(fun ~agent:_ ~operation command ->
                        execute constrained ~operation command)
                      ~perform:(fun ~agent ~turn ->
                        incr turns;
                        check "queued guidance reaches recovery context"
                          (Base.String.is_substring
                             (B.recovery_prompt ~context:"Patch task context"
                                ~guidance:agent.Patch_agent.human_messages turn)
                             ~substring:guidance);
                        if timeout then
                          Git.run_git ~cwd:dir
                            [ "-c"; "core.editor=true"; "merge"; "--continue" ]
                        else (
                          check "dirty task is finish work before integration"
                            (match turn.B.mode with
                            | B.History_recovery
                                { task = B.Finish_local_work; _ } ->
                                true
                            | B.History_recovery
                                {
                                  task =
                                    B.Reconstruct_history | B.Repair_publication;
                                  _;
                                }
                            | B.Content_repair | B.Diagnosis _ ->
                                false);
                          Git.run_git ~cwd:dir
                            [ "add"; "test-a"; "test-b"; "test-c" ];
                          Git.run_git ~cwd:dir
                            [ "commit"; "-qm"; "Finish interrupted tests" ]);
                        B.Repair_completed { token = turn.B.token; at = 1002. })
                      token
                  in
                  check "one recovery turn reaches verified publication"
                    (outcome = R.Idle && !turns = 1
                    && B.phase (state runtime) = Some B.Settled);
                  if timeout then
                    check "validation was not replayed after agent recovery"
                      (!attempts = 2);
                  List.iter
                    (fun (file, expected) ->
                      check
                        ("published work survives: " ^ file)
                        (Git.git_capture ~cwd:remote [ "show"; "patch:" ^ file ]
                        = expected))
                    (if timeout then
                       [
                         ("test-a", "first");
                         ("test-b", "second");
                         ("upstream", "upstream");
                       ]
                     else
                       [
                         ("test-a", "staged fix");
                         ("test-b", "unstaged fix");
                         ("test-c", "untracked fix");
                         ("upstream", "upstream");
                       ])))))
    [ false; true ];
  checkout_recovery env false;
  checkout_recovery env true;
  publication_recovery env ~persistent:false ~timeout:false ~local_hook:false;
  publication_recovery env ~persistent:false ~timeout:false ~local_hook:true;
  publication_recovery env ~persistent:true ~timeout:false ~local_hook:false;
  publication_recovery env ~persistent:false ~timeout:true ~local_hook:false;
  print_endline
    "restart, dirty work, bounded validation and publication recovery: OK"

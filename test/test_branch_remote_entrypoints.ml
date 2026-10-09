(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
module B = Branch_reconcile
module E = Branch_reconcile_executor
module R = Branch_reconcile_runner
module Git = Onton_test_support.Git_env

exception Interrupted

type entrypoint =
  | Scheduled
  | Requested
  | Scoped
  | Contributor
  | Adopted_conflict

type interruption =
  | Never
  | Before_mutation
  | After_mutation
  | Before_publication
  | After_publication

let get = function Ok value -> value | Error reason -> failwith reason
let check reason condition = if not condition then failwith reason

let commit dir file contents =
  let channel = open_out_bin (Filename.concat dir file) in
  output_string channel contents;
  close_out channel;
  Git.run_git ~cwd:dir [ "add"; file ];
  Git.run_git ~cwd:dir [ "commit"; "-qm"; file ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let revision text =
  match B.Commit.make text with
  | Some revision -> revision
  | None -> failwith "invalid fixture commit"

let id = Types.Patch_id.of_string "1"

let gameplan =
  let plan =
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
  in
  {
    plan with
    patches =
      List.map
        (fun patch ->
          { patch with Types.Patch.branch = Types.Branch.of_string "patch" })
        plan.patches;
  }

let scenario env entrypoint policy interruption unrelated =
  Git.with_temp_repo (fun remote ->
      let original_base = commit remote "base.txt" "base\n" in
      Git.with_temp_repo (fun dir ->
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
          Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
          Git.run_git ~cwd:dir [ "checkout"; "-qb"; "patch"; "origin/main" ];
          let local = commit dir "local.txt" "local\n" in
          let local =
            if entrypoint = Adopted_conflict then
              commit dir "base.txt" "local side\n"
            else local
          in
          if unrelated then (
            Git.run_git ~cwd:remote [ "checkout"; "--orphan"; "patch" ];
            Git.run_git ~cwd:remote [ "rm"; "-rf"; "." ])
          else Git.run_git ~cwd:remote [ "checkout"; "-qb"; "patch"; "main" ];
          let incoming = commit remote "remote.txt" "remote\n" in
          Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
          if entrypoint = Adopted_conflict then
            ignore (commit remote "base.txt" "base side\n");
          let base = commit remote "advanced.txt" "advanced\n" in
          Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
          if entrypoint = Adopted_conflict then (
            check "fixture did not create an active merge conflict"
              (Git.git_exit_code ~cwd:dir
                 [ "merge"; "--no-edit"; "origin/main" ]
              = 1);
            let channel = open_out_bin (Filename.concat dir "base.txt") in
            output_string channel "resolved\n";
            close_out channel;
            Git.run_git ~cwd:dir [ "add"; "base.txt" ]);
          let purpose =
            match entrypoint with
            | Scheduled -> B.Reconcile_base
            | Requested -> B.Reconcile_request "conflict-observation"
            | Adopted_conflict -> B.Reconcile_request "adopted-conflict"
            | Scoped ->
                B.Reconcile_scoped
                  {
                    request = "retarget";
                    project = "demo";
                    ancestors = [ Types.Patch_id.of_string "2" ];
                  }
            | Contributor ->
                B.Integrate_revision
                  { contributor = "child"; revision = revision base }
          in
          let requested_policy =
            match entrypoint with
            | Contributor -> B.Preserve_ancestry
            | Scheduled | Requested | Scoped | Adopted_conflict -> policy
          in
          let policy =
            if entrypoint = Adopted_conflict then B.Preserve_ancestry
            else requested_policy
          in
          let intent =
            B.{ base = "main"; policy = requested_policy; purpose }
          in
          let runtime =
            ref
              (Runtime.create ~gameplan
                 ~main_branch:(Types.Branch.of_string "main")
                 ())
          in
          let checkpoint = Filename.concat dir "owner-snapshot" in
          (* Keep the snapshot outside the checkout so it cannot make Git dirty. *)
          let checkpoint =
            Filename.concat
              (Git.git_capture ~cwd:dir [ "rev-parse"; "--absolute-git-dir" ])
              (Filename.basename checkpoint)
          in
          let persist = Persistence.save_snapshot ~path:checkpoint in
          let state () =
            Runtime.read !runtime (fun snap ->
                (Orchestrator.agent snap.Runtime.orchestrator id)
                  .Patch_agent.branch_reconcile)
          in
          let io =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let prefix = B.recovery_prefix ~project:"demo" ~branch:"patch" in
          let stopped = ref false
          and mutations = ref 0
          and publications = ref 0 in
          let attempts = ref [] in
          let execute ~operation command =
            let remote_attempt =
              Option.is_some operation.B.remote_integration
              ||
              match operation.B.local_action with
              | B.Prepare_remote_replay _ -> true
              | B.Publish_source | B.Integrate_source _ -> false
            in
            if remote_attempt && not (List.mem operation.id !attempts) then
              attempts := operation.id :: !attempts;
            let mutation =
              remote_attempt
              &&
              match command.B.kind with
              | B.Checkout_remote _ -> false
              | B.Integrate _ -> true
              | B.Verify_scope _ | B.Observe | B.Pin _ | B.Commit_merge _
              | B.Plan_remote_replay _ | B.Inspect | B.Verify_recovery
              | B.Continue _ | B.Publish _ | B.Confirm _ ->
                  false
            in
            let publication =
              remote_attempt
              &&
              match command.B.kind with
              | B.Publish _ -> true
              | B.Verify_scope _ | B.Observe | B.Pin _ | B.Integrate _
              | B.Commit_merge _ | B.Plan_remote_replay _ | B.Checkout_remote _
              | B.Inspect | B.Verify_recovery | B.Continue _ | B.Confirm _ ->
                  false
            in
            let stop before =
              (not !stopped)
              &&
              match interruption with
              | Before_mutation -> before && mutation
              | After_mutation -> (not before) && mutation
              | Before_publication -> before && publication
              | After_publication -> (not before) && publication
              | Never -> false
            in
            let interrupt () =
              stopped := true;
              raise Interrupted
            in
            if stop true then interrupt ();
            if mutation then incr mutations;
            if publication then incr publications;
            let result =
              E.execute ~io ~prefix ~branch:"patch" ~operation command
            in
            if stop false then interrupt ();
            result
          in
          let run event =
            R.run ~runtime:!runtime ~persist ~patch_id:id
              ~now:(fun () -> 100.)
              ~execute event
          in
          ignore (run (B.Materialized (B.New_branch (revision original_base))));
          let outcome =
            try run (B.Request intent)
            with Interrupted ->
              let snapshot = get (Persistence.load ~path:checkpoint) in
              runtime :=
                Runtime.create ~gameplan
                  ~main_branch:(Types.Branch.of_string "main")
                  ~snapshot ();
              let op = Option.get (B.operation (state ())) in
              let capture = Option.get op.remote_integration in
              check "restart changed captured incoming revision"
                (capture.source_revision = revision incoming);
              check "restart invented a base boundary"
                (capture.base_branch = None);
              check "remote attempt replaced base receipt"
                (match entrypoint with
                | Contributor -> B.integrations (state ()) = []
                | Scheduled | Requested | Scoped | Adopted_conflict -> (
                    match B.integrations (state ()) with
                    | [ receipt ] ->
                        receipt.capture.source_revision = revision local
                        && receipt.capture.target_revision = revision base
                    | _ -> false));
              run B.Recover
          in
          let settle = function
            | R.Idle -> ()
            | R.Waiting | R.Repair_needed _ | R.Intervention _
            | R.Checkpoint_failed _ ->
                failwith (Yojson.Safe.to_string (B.yojson_of_t (state ())))
          in
          let calls = ref 0 in
          if unrelated then
            match outcome with
            | R.Repair_needed token ->
                let active_token = ref token in
                let backend =
                  Llm_backend.
                    {
                      name = "remote-entrypoint-recovery";
                      run_streaming =
                        (fun ~project_name:_
                          ~cwd:_
                          ~patch_id:_
                          ~prompt
                          ~resume_session:_
                          ~session_uuid:_
                          ~complexity:_
                          ~on_event:_
                        ->
                          incr calls;
                          let saved = get (Persistence.load ~path:checkpoint) in
                          let owner =
                            (Orchestrator.agent saved.Runtime.orchestrator id)
                              .Patch_agent.branch_reconcile
                          in
                          (match
                             B.repair_turn owner ~branch:"patch" !active_token
                           with
                          | Some { mode = B.History_recovery _; _ } -> ()
                          | Some { mode = B.Content_repair | B.Diagnosis _; _ }
                          | None ->
                              failwith "history repair was not durably claimed");
                          check
                            "repair prompt omitted attempted remote integration"
                            (Base.String.is_substring prompt
                               ~substring:
                                 ("Pending remote contribution: " ^ incoming));
                          Git.run_git ~cwd:dir
                            [
                              "merge";
                              "--allow-unrelated-histories";
                              "--no-edit";
                              incoming;
                            ];
                          {
                            exit_code = 0;
                            stdout = "";
                            stderr = "";
                            got_events = true;
                            saw_final_result = true;
                            timed_out = false;
                          });
                    }
                in
                let repair () =
                  R.run_repair ~runtime:!runtime ~persist ~patch_id:id
                    ~with_capacity:(fun f -> f ())
                    ~now:(fun () -> 100.)
                    ~execute:(fun ~agent:_ ~operation command ->
                      execute ~operation command)
                    ~perform:(fun ~agent:_ ~turn ->
                      active_token := turn.B.token;
                      Branch_repair_session.run
                        ~on_event:(fun _ -> ())
                        ~context:"" ~guidance:[] ~backend
                        ~cwd:Eio.Path.(Eio.Stdenv.fs env / dir)
                        ~project_name:"demo" ~patch_id:id ~complexity:None ~turn
                        ~read_head:(fun () ->
                          Some
                            (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]))
                        ~now:(fun () -> 100.))
                    token
                in
                let rejected = repair () in
                check "an unrelated merge cannot pass scope verification"
                  (match rejected with
                  | R.Repair_needed _ -> true
                  | R.Idle | R.Waiting | R.Intervention _
                  | R.Checkpoint_failed _ ->
                      false);
                ignore (repair ());
                check "duplicate history repair reran" (!calls = 1);
                check
                  "unrelated ancestry cannot authorize a publication or receipt"
                  (!publications = 0
                  && B.publications (state ()) = []
                  && B.remote_integrations (state ()) = []);
                check "unrelated remote remains unchanged"
                  (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                  = incoming);
                check "unrelated remote is retained as evidence"
                  (List.mem (revision incoming)
                     (B.required_revisions (state ())))
            | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _ ->
                failwith "unrelated remote did not reach history recovery"
          else settle outcome;
          if not unrelated then (
            check "remote attempt restarted as a new operation"
              (List.length !attempts = 1);
            check "interruption was not exercised"
              (interruption = Never || !stopped);
            if not unrelated then
              check "remote mutation repeated" (!mutations = 1);
            check "publication repeated" (!publications = 1);
            List.iter
              (fun (file, contents) ->
                check
                  ("published tree lost " ^ file)
                  (Git.git_capture ~cwd:remote [ "show"; "patch:" ^ file ]
                  = contents))
              [
                ("local.txt", "local");
                ("advanced.txt", "advanced");
                ("remote.txt", "remote");
              ];
            let receipt =
              match B.remote_integrations (state ()) with
              | [ receipt ] -> receipt
              | _ -> failwith "missing or duplicate remote receipt"
            in
            check "remote receipt policy changed"
              (receipt.capture.integration_policy = policy);
            check "remote receipt captured another remote"
              (receipt.capture.source_revision = revision incoming);
            let preserved =
              B.Commit.to_string receipt.capture.target_revision
            in
            Git.run_git ~cwd:remote
              [ "merge-base"; "--is-ancestor"; preserved; "patch" ];
            Git.run_git ~cwd:remote
              [ "merge-base"; "--is-ancestor"; base; "patch" ];
            (match policy with
            | B.Rewrite -> ()
            | B.Preserve_ancestry ->
                List.iter
                  (fun before ->
                    Git.run_git ~cwd:remote
                      [ "merge-base"; "--is-ancestor"; before; "patch" ])
                  [ local; incoming ]);
            if entrypoint = Adopted_conflict then
              check "adopted staged resolution was lost"
                (Git.git_capture ~cwd:remote [ "show"; "patch:base.txt" ]
                = "resolved");
            let before = state () in
            settle (run (B.Request intent));
            check "settled entrypoint repeated reconciliation"
              (B.equal before (state ())))))

let () =
  Eio_main.run (fun env ->
      List.iter
        (fun policy ->
          List.iter
            (fun entrypoint -> scenario env entrypoint policy Never false)
            [ Scheduled; Requested; Scoped; Contributor; Adopted_conflict ];
          List.iter
            (fun interruption ->
              scenario env Scheduled policy interruption false)
            [
              Before_mutation;
              After_mutation;
              Before_publication;
              After_publication;
            ];
          scenario env Scheduled policy Never true)
        [ B.Rewrite; B.Preserve_ancestry ];
      print_endline
        "remote integration entrypoints, interruption and agent recovery: OK")

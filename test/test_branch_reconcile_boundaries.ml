(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
module B = Branch_reconcile
module E = Branch_reconcile_executor
module R = Branch_reconcile_runner
module Git = Onton_test_support.Git_env

exception Interrupted

let get = function Ok value -> value | Error reason -> failwith reason
let check label condition = if not condition then failwith label

let sha value =
  match B.Commit.make value with Some value -> value | None -> assert false

let id = Types.Patch_id.of_string "1"
let main = Types.Branch.of_string "main"

let gameplan =
  (get
     (Gameplan_parser.parse_string
        "projectName: boundary-test\n\
         owner: owner\n\
         repo: repo\n\
         patches:\n\
        \  - number: 1\n\
        \    title: patch\n\
        \    description: patch\n\
        \    dependsOn: []\n\
         dependencyGraph:\n\
        \  - patch: 1\n\
        \    dependsOn: []\n"))
    .Gameplan_parser.gameplan

let owner runtime =
  Runtime.read runtime (fun snapshot ->
      (Orchestrator.agent snapshot.Runtime.orchestrator id)
        .Patch_agent.branch_reconcile)

let commit ?shared dir file =
  let channel = open_out (Filename.concat dir file) in
  output_string channel (file ^ "\n");
  close_out channel;
  Git.run_git ~cwd:dir [ "add"; file ];
  Option.iter
    (fun content ->
      let channel = open_out (Filename.concat dir "base") in
      output_string channel content;
      close_out channel;
      Git.run_git ~cwd:dir [ "add"; "base" ])
    shared;
  Git.run_git ~cwd:dir [ "commit"; "-qm"; file ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

type boundary = Before | After | Refuse_result

let command_name = function
  | B.Observe -> "observe"
  | B.Pin _ -> "pin"
  | B.Integrate _ -> "integrate"
  | B.Publish _ -> "publish"
  | B.Confirm _ -> "confirm"
  | B.Inspect -> "inspect"
  | B.Continue _ -> "continue"
  | B.Verify_recovery -> "verify-recovery"
  | B.Plan_remote_replay _ -> "plan-remote"
  | B.Checkout_remote _ -> "checkout-remote"
  | B.Commit_merge _ -> "commit-merge"

let scenario env selected boundary =
  let merge =
    List.mem selected [ "merge-inspect"; "merge-continue"; "merge-completion" ]
  in
  let completion =
    selected = "repair-completion" || selected = "merge-completion"
  in
  let conflict =
    selected = "inspect" || selected = "continue" || completion || merge
  in
  let selected_command =
    match selected with
    | "merge-inspect" -> "inspect"
    | "merge-continue" -> "continue"
    | "replay-integrate" -> "integrate"
    | "replay-publish" -> "publish"
    | name -> name
  in
  let recovery = selected = "verify-recovery" in
  let remote_replay =
    List.mem selected
      [ "plan-remote"; "checkout-remote"; "replay-integrate"; "replay-publish" ]
  in
  Git.with_temp_repo (fun remote ->
      let base = commit remote "base" in
      Git.with_temp_repo (fun dir ->
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
          Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
          if recovery then
            Git.run_git ~cwd:dir
              [ "checkout"; "--orphan"; "patch"; "origin/main" ]
          else
            Git.run_git ~cwd:dir
              [ "checkout"; "-q"; "-b"; "patch"; "origin/main" ];
          ignore
            (commit
               ?shared:(if conflict then Some "local\n" else None)
               dir "work");
          let target =
            commit
              ?shared:(if conflict then Some "remote\n" else None)
              remote "upstream"
          in
          let source = Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ] in
          (* Keep checkpoints outside the checkout's untracked-file inventory. *)
          let snapshot_path =
            Filename.concat (Filename.concat dir ".git") "checkpoint.json"
          in
          let persist = Persistence.save_snapshot ~path:snapshot_path in
          let runtime = Runtime.create ~gameplan ~main_branch:main () in
          let raw =
            E.make_io
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~clock:(Eio.Stdenv.clock env) ~path:dir
          in
          let integrations = ref 0 and pushes = ref 0 in
          let continuations = ref 0 in
          let verifications = ref 0 in
          let first_candidate = ref None and checkouts = ref 0 in
          let io =
            E.
              {
                git =
                  (fun args ->
                    if List.mem "--continue" args then incr continuations;
                    (match args with
                    | "rebase" :: _ | "merge" :: _ -> incr integrations
                    | "push" :: _ ->
                        incr pushes;
                        if remote_replay && !pushes = 1 then (
                          first_candidate :=
                            Some
                              (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]);
                          Git.run_git ~cwd:remote
                            [ "checkout"; "-q"; "-b"; "patch"; base ];
                          ignore (commit remote "incoming");
                          Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ])
                    | "reset" :: _ -> incr checkouts
                    | _ -> ());
                    raw.git args);
              }
          in
          let execute ~operation command =
            (match command.B.kind with
            | B.Verify_recovery -> incr verifications
            | B.Observe | B.Pin _ | B.Integrate _ | B.Publish _ | B.Confirm _
            | B.Inspect | B.Continue _ | B.Commit_merge _
            | B.Plan_remote_replay _ | B.Checkout_remote _ ->
                ());
            E.execute ~io ~prefix:"refs/onton/reconcile/boundary-test"
              ~branch:"patch" ~operation command
          in
          let run runtime persist execute event =
            R.run ~runtime ~persist ~patch_id:id
              ~now:(fun () -> 100.)
              ~execute event
          in
          if not recovery then
            ignore
              (run runtime persist execute
                 (B.Materialized (B.New_branch (sha base))));
          let request =
            B.Request
              B.
                {
                  base = "main";
                  policy =
                    (if recovery || merge then Preserve_ancestry else Rewrite);
                  purpose = Reconcile_base;
                }
          in
          let event =
            if remote_replay then (
              check "lease race pauses before remote replay"
                (run runtime persist execute request = R.Waiting);
              B.Tick 1000.)
            else if (not conflict) && not recovery then request
            else
              let token =
                match run runtime persist execute request with
                | R.Repair_needed token -> token
                | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _
                  ->
                    failwith "fixture did not reach content repair"
              in
              let turn =
                match B.repair_turn (owner runtime) ~branch:"patch" token with
                | Some turn -> turn
                | None -> failwith "missing claimed repair"
              in
              let before =
                sha (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ])
              in
              (if recovery then (
                 check "unrelated histories require history recovery"
                   (match turn.B.mode with
                   | B.History_recovery _ -> true
                   | B.Content_repair -> false);
                 Git.run_git ~cwd:dir
                   [
                     "merge"; "--allow-unrelated-histories"; "--no-edit"; target;
                   ])
               else
                 let channel = open_out (Filename.concat dir "base") in
                 output_string channel "local\nremote\n";
                 close_out channel;
                 Git.run_git ~cwd:dir [ "add"; "base" ]);
              check "content repair stages without committing"
                (recovery
                || Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]
                   = B.Commit.to_string before);
              let after =
                sha (Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ])
              in
              B.repair_result ~turn ~at:100. ~before_head:(Some before)
                ~after_head:(Some after) ~timed_out:false ~final_result:true
                ~detail:"staged resolution"
          in
          let reached = ref false and refuse = ref false in
          let prior_verifications = !verifications in
          let failing_persist snapshot =
            if completion && not !reached then (
              reached := true;
              match boundary with
              | Before -> raise Interrupted
              | After ->
                  get (persist snapshot);
                  raise Interrupted
              | Refuse_result -> Error "injected result checkpoint failure")
            else if !refuse then (
              refuse := false;
              Error "injected result checkpoint failure")
            else persist snapshot
          in
          let interrupted_execute ~operation command =
            if command_name command.B.kind = selected_command && not !reached
            then (
              reached := true;
              match boundary with
              | Before -> raise Interrupted
              | After ->
                  ignore (execute ~operation command);
                  raise Interrupted
              | Refuse_result ->
                  let result = execute ~operation command in
                  refuse := true;
                  result)
            else execute ~operation command
          in
          (try
             let outcome =
               run runtime failing_persist interrupted_execute event
             in
             check "result checkpoint refusal is surfaced"
               (boundary = Refuse_result
               && outcome
                  = R.Checkpoint_failed "injected result checkpoint failure")
           with Interrupted ->
             check "only execution boundaries interrupt"
               (boundary <> Refuse_result));
          check ("selected boundary reached: " ^ selected) !reached;
          if recovery then
            check "unacknowledged verification cannot publish" (!pushes = 0);
          if completion then (
            check "completion checkpoint loss cannot continue or publish"
              (!continuations = 0 && !pushes = 0);
            check "completion checkpoint loss retains staged resolution"
              (Git.git_capture ~cwd:dir [ "show"; ":base" ] = "local\nremote"));
          let saved = get (Persistence.load ~path:snapshot_path) in
          let restored =
            Runtime.create ~gameplan ~main_branch:main ~snapshot:saved ()
          in
          check
            "checkpoint loss distinguishes refused writes from lost save \
             acknowledgement"
            (B.equal (owner runtime) (owner restored)
            = not (completion && boundary = After));
          check
            ("restart converges: " ^ selected)
            (run restored persist execute B.Recover = R.Idle);
          check "restart settles publication"
            (B.publication_status (owner restored) = `Published);
          List.iter
            (fun file ->
              check
                ("restart retains " ^ file)
                (Git.git_capture ~cwd:remote [ "show"; "patch:" ^ file ]
                = if conflict && file = "base" then "local\nremote" else file))
            [ "base"; "upstream"; "work" ];
          if recovery then (
            List.iter
              (fun revision ->
                check "verified recovery preserves both histories"
                  (Git.git_exit_code ~cwd:remote
                     [ "merge-base"; "--is-ancestor"; revision; "patch" ]
                  = 0))
              [ source; target ];
            check "lost verification is independently repeated after restart"
              (!verifications - prior_verifications
              = if boundary = Before then 1 else 2))
          else if remote_replay then (
            check "remote replay retains incoming work"
              (Git.git_capture ~cwd:remote [ "show"; "patch:incoming" ]
              = "incoming");
            check "remote work is replayed exactly once"
              (Git.git_capture ~cwd:remote
                 [ "log"; "--format=%s"; target ^ "..patch" ]
              = "incoming\nwork");
            let candidate =
              match !first_candidate with
              | Some value -> value
              | None -> failwith "missing captured candidate"
            in
            check "remote replay preserves the captured local candidate"
              (Git.git_exit_code ~cwd:remote
                 [ "merge-base"; "--is-ancestor"; candidate; "patch" ]
              = 0))
          else if merge then (
            let published =
              Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
            in
            List.iter
              (fun revision ->
                check "merge continuation preserves both parents"
                  (Git.git_exit_code ~cwd:remote
                     [ "merge-base"; "--is-ancestor"; revision; published ]
                  = 0))
              [ source; target ];
            check "merge continuation creates exactly one merge commit"
              (Git.git_capture ~cwd:remote
                 [ "rev-list"; "--count"; "--merges"; base ^ "..patch" ]
              = "1");
            check "merge continuation retains the captured parent order"
              (Git.git_capture ~cwd:remote
                 [ "show"; "-s"; "--format=%P"; "patch" ]
              = source ^ " " ^ target))
          else
            check "restart replays exactly the patch work"
              (Git.git_capture ~cwd:remote
                 [ "log"; "--format=%s"; target ^ "..patch" ]
              = "work");
          check "restart never repeats integration or publication"
            ((!integrations = if remote_replay then 2 else 1)
            && !pushes = if remote_replay then 2 else 1);
          check "remote replay checkout executes once"
            (!checkouts = if remote_replay then 1 else 0);
          check "restart continues a staged repair exactly once"
            (!continuations = if conflict then 1 else 0)))

let () =
  Eio_main.run (fun env ->
      List.iter
        (fun selected ->
          List.iter (scenario env selected) [ Before; After; Refuse_result ])
        [
          "observe";
          "pin";
          "integrate";
          "publish";
          "confirm";
          "inspect";
          "continue";
          "verify-recovery";
          "plan-remote";
          "checkout-remote";
          "repair-completion";
          "merge-inspect";
          "merge-continue";
          "merge-completion";
          "replay-integrate";
          "replay-publish";
        ]);
  print_endline "reconciliation command and result checkpoint boundaries: OK"

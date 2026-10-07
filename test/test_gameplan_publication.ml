(* @archlint.module test
   @archlint.domain orchestrator *)

open Onton
open Onton_core
open Types
module Git = Onton_test_support.Git_env

let get = function Ok value -> value | Error error -> failwith error
let id = Patch_id.of_string
let main = Branch.of_string "main"

let source =
  "# Keep this comment\n\
   projectName: demo\n\
   owner: owner\n\
   repo: repo\n\
   problemStatement: problem\n\
   solutionSummary: solution\n\
   patches:\n\
  \  - number: 1\n\
  \    title: First\n\
  \    description: First\n\
  \    dependsOn: []\n\
  \  - number: 2\n\
  \    title: Second\n\
  \    description: Second\n\
  \    dependsOn: [1]\n\
   dependencyGraph:\n\
  \  - patch: 1\n\
  \    dependsOn: []\n\
  \  - patch: 2\n\
  \    dependsOn: [1]\n"

let original =
  (get (Gameplan_parser.parse_string source)).Gameplan_parser.gameplan

let publication =
  get
    (Gameplan_publication.create ~directory:"/gameplans/" ~project_name:"demo"
       ~yaml:true ~content:source)

let gameplan = get (Gameplan.publish original publication)

let starts runtime =
  Runtime.read runtime (fun snap ->
      Patch_controller.plan_tick snap.Runtime.orchestrator ~project_name:"demo"
        ~gameplan:snap.Runtime.gameplan
      |> fun (_, _, actions) ->
      List.filter_map
        (function
          | Orchestrator.Start (pid, _) -> Some (Patch_id.to_string pid)
          | Orchestrator.Rebase _ | Orchestrator.Respond _
          | Orchestrator.Reconcile_branch _ ->
              None)
        actions)

let state_and_resume () =
  let runtime = Runtime.create ~gameplan ~main_branch:main () in
  assert (starts runtime = [ "0" ]);
  Runtime.update_orchestrator runtime (fun orch ->
      let orch =
        Orchestrator.set_pr_number orch (id "0") (Pr_number.of_int 10)
      in
      Orchestrator.set_pr_body_delivered orch (id "0") true);
  assert (starts runtime = []);
  Runtime.update_orchestrator runtime (fun orch ->
      Orchestrator.set_worktree_path orch (id "1") "/existing-checkout");
  assert (starts runtime = []);
  let snap = Runtime.read runtime Fun.id in
  let replace_field key value json =
    let fields = Option.value (Json.assoc json) ~default:[] in
    `Assoc ((key, value) :: List.filter (fun (field, _) -> field <> key) fields)
  in
  let json = Persistence.snapshot_to_yojson snap in
  let malformed_gameplan =
    Option.get (Json.field "gameplan" json)
    |> replace_field "publication"
         (`Assoc [ ("path", `String "../outside"); ("content", `String source) ])
  in
  assert (
    Result.is_error
      (Persistence.snapshot_of_yojson
         (replace_field "gameplan" malformed_gameplan json)));
  let restored =
    get (Persistence.snapshot_of_yojson (Persistence.snapshot_to_yojson snap))
  in
  let runtime =
    Runtime.create ~gameplan ~main_branch:main ~snapshot:restored ()
  in
  assert (starts runtime = []);
  assert (
    Gameplan_publication.equal publication
      (Option.get restored.Runtime.gameplan.Gameplan.publication));
  let prompt = Prompt.render_gameplan_layer ~project_name:"demo" gameplan in
  assert (
    Base.String.is_substring prompt ~substring:"`gameplans/demo/gameplan.yaml`");
  (* A queued Start and an existing checkout both remain behind the merge gate. *)
  Runtime.update_orchestrator runtime (fun orch ->
      Orchestrator.fire orch (Orchestrator.Start (id "1", main)));
  assert (
    not
      (Runtime.read runtime (fun snap ->
           (Orchestrator.agent snap.Runtime.orchestrator (id "1"))
             .Patch_agent.busy)));
  Runtime.update_orchestrator runtime (fun orch ->
      Orchestrator.mark_merged orch (id "0"));
  assert (starts runtime = [ "1" ]);
  let added =
    get
      (Runtime.add_patch runtime ~title:"Added" ~description:"Added"
         ~dependencies:[])
  in
  assert (List.mem (id "0") added.Patch.dependencies);
  (* The publication source survives save/load; repo changes cannot retarget it. *)
  Project_store.save_config ~project_name:"demo" ~github_owner:"owner"
    ~github_repo:"repo" ~backend:"codex" ~model:"model" ~main_branch:"main"
    ~poll_interval:1. ~repo_root:"repo" ~max_concurrency:1 ~max_ci_failures:3
    ~automerge_timeout:120. ~gameplan_publication:publication ();
  let config = get (Project_store.load_config ~project_name:"demo") in
  assert (
    Gameplan_publication.equal publication
      (Option.get config.Project_store.gameplan_publication));
  let config =
    get
      (Repo_config.parse_string ~known_backends:[]
         {|{"gameplan":{"directory":"/docs/plans/"}}|})
  in
  assert (config.Repo_config.gameplan_directory = Some "docs/plans")

let commit env ~yaml ~content =
  Git.with_temp_repo (fun path ->
      Git.run_git ~cwd:path [ "commit"; "--allow-empty"; "-m"; "base" ];
      let publication =
        get
          (Gameplan_publication.create ~directory:"gameplans"
             ~project_name:"demo" ~yaml ~content)
      in
      let run () =
        Worktree.commit_gameplan ~clock:(Eio.Stdenv.clock env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~path ~publication ~message:"[demo] Patch 0: Publish gameplan"
      in
      ignore (get (run ()));
      let destination =
        Filename.concat path (Gameplan_publication.path publication)
      in
      assert (
        In_channel.with_open_bin destination In_channel.input_all = content);
      assert (
        Git.git_capture ~cwd:path [ "diff"; "--name-only"; "HEAD~1"; "HEAD" ]
        = Gameplan_publication.path publication);
      let head = Git.git_capture ~cwd:path [ "rev-parse"; "HEAD" ] in
      ignore (get (run ()));
      assert (Git.git_capture ~cwd:path [ "rev-parse"; "HEAD" ] = head);
      Out_channel.with_open_bin destination (fun out ->
          Out_channel.output_string out "changed by user");
      assert (Result.is_error (run ()));
      assert (
        In_channel.with_open_bin destination In_channel.input_all
        = "changed by user"))

let refuses_symlink env () =
  Git.with_temp_repo (fun path ->
      Git.run_git ~cwd:path [ "commit"; "--allow-empty"; "-m"; "base" ];
      Unix.symlink "/tmp" (Filename.concat path "gameplans");
      assert (
        Result.is_error
          (Worktree.commit_gameplan ~clock:(Eio.Stdenv.clock env)
             ~process_mgr:(Eio.Stdenv.process_mgr env)
             ~path ~publication ~message:"publish")))

module Fake_forge : Forge.S with type error = string = struct
  type error = string

  let name = "test"
  let owner = "owner"
  let show_error error = error
  let poll_error _ = Poll_outcome.Transport_failed { msg = "unused" }
  let is_duplicate_change_error _ = false
  let is_permanent_error _ = false
  let is_merge_queue_required_error _ = false
  let supports_reviews = true
  let supports_pull_request_changes = true
  let supports_branch_changes = false

  type merge_result =
    | Merge_succeeded
    | Merge_queued of string
    | Merge_unconfirmed

  type enqueue_result =
    | Enqueued of Pr_state.merge_queue_entry
    | Already_enqueued of Pr_state.merge_queue_entry

  let change_url _ = None
  let branch_state ?known_state:_ _ = Error "unused"
  let pr_state _ = Error "unused"
  let merge_queue_removal_checks ~pr_number:_ = Error "unused"
  let check_failure_details ~check:_ = Error "unused"
  let rerun_failed_jobs_for_check ~check:_ = Error "unused"
  let list_prs ~branch:_ ?base:_ ~state:_ () = Ok []
  let update_pr_body ~pr_number:_ ~body:_ = Ok ()

  let reply_to_review_comment ~pr_number:_ ~comment_id:_ ~body:_ =
    Error "unused"

  let resolve_review_thread ~thread_id:_ = Error "unused"
  let viewer_login () = None

  let create_pull_request ~title ~head ~base ~body:_ ~draft =
    assert (title = "[demo] Patch 0: Publish gameplan");
    assert (head = Branch.of_string "demo/patch-0");
    assert (base = main);
    assert draft;
    Ok (Pr_number.of_int 10)

  let update_pr_base ~pr_number:_ ~base:_ = Ok ()
  let request_review ~pr_number:_ ~team_slug:_ = Error "unused"
  let set_draft ~pr_number:_ ~draft:_ = Ok ()
  let merge_pr ~pr_number:_ = Error "unused"
  let enqueue_pr ~pr_number:_ = Error "unused"
  let dequeue_pr ~pr_number:_ = Error "unused"
  let check_repo_access () = Ok ()
end

let runner_without_backend env ~feature ~retry =
  Git.with_temp_repo (fun path ->
      let origin = Filename.temp_dir "onton-publication-origin-" "" in
      Fun.protect
        ~finally:(fun () -> Git.sh ~dir:"/" ("rm -rf " ^ Filename.quote origin))
        (fun () ->
          Git.run_git ~cwd:origin [ "init"; "--bare"; "-b"; "main" ];
          Git.run_git ~cwd:path [ "commit"; "--allow-empty"; "-m"; "base" ];
          Git.run_git ~cwd:path [ "remote"; "add"; "origin"; origin ];
          Git.run_git ~cwd:path [ "push"; "origin"; "main" ];
          Git.run_git ~cwd:path [ "switch"; "-c"; "demo/patch-0" ];
          let clock = Eio.Stdenv.clock env in
          let fs = Eio.Stdenv.fs env in
          let process_mgr = Eio.Stdenv.process_mgr env in
          let module Real_worktree =
            (val Worktree.make ~fs ~config:Worktree_lifecycle.git ~clock
                   ~process_mgr ~repo_root:path)
          in
          let publications = ref [] in
          let published = ref None in
          let failed_base_push = ref false in
          let module W : Worktree.S = struct
            include Real_worktree

            let force_push_with_lease ~path:_ ~branch:_ ~base:_ =
              failwith "Gameplan publication must use durable reconciliation"

            let reconcile ~path ~project_name ~branch ~operation command =
              let fail_push =
                match command.Branch_reconcile.kind with
                | Branch_reconcile.Publish { candidate; expected } ->
                    let snapshot =
                      get
                        (Persistence.load
                           ~path:(Project_store.snapshot_path project_name))
                    in
                    let agent =
                      Orchestrator.agent snapshot.Runtime.orchestrator (id "0")
                    in
                    let completion =
                      Option.get agent.Patch_agent.session_completion
                    in
                    assert (
                      completion.Session_result.result
                      = Session_result.Session_ok);
                    (match
                       operation.Branch_reconcile.intent
                         .Branch_reconcile.purpose
                     with
                    | Branch_reconcile.Publish_revision _
                    | Branch_reconcile.Publish_session _ ->
                        assert (
                          completion.Session_result.head
                          = Some (Branch_reconcile.Commit.to_string candidate))
                    | Branch_reconcile.Reconcile_base
                    | Branch_reconcile.Reconcile_request _
                    | Branch_reconcile.Integrate_revision _ ->
                        let receipt =
                          List.hd
                            (Branch_reconcile.integrations
                               agent.Patch_agent.branch_reconcile)
                        in
                        assert (
                          receipt.Branch_reconcile.integrated_revision
                          = candidate);
                        assert (
                          agent.Patch_agent.branch_rebased_onto_sha
                          = Some
                              (Branch_reconcile.Commit.to_string
                                 receipt.Branch_reconcile.capture
                                   .Branch_reconcile.target_revision)));
                    assert (
                      Branch_reconcile.pending
                        agent.Patch_agent.branch_reconcile
                      = Some command);
                    assert (agent.Patch_agent.push_failure_count = 0);
                    assert (expected = !published);
                    let first = !publications = [] in
                    publications := candidate :: !publications;
                    let base_failure =
                      match
                        operation.Branch_reconcile.intent
                          .Branch_reconcile.purpose
                      with
                      | Branch_reconcile.Reconcile_base
                      | Branch_reconcile.Reconcile_request _
                      | Branch_reconcile.Integrate_revision _ ->
                          if retry && not !failed_base_push then (
                            failed_base_push := true;
                            true)
                          else false
                      | Branch_reconcile.Publish_revision _
                      | Branch_reconcile.Publish_session _ ->
                          false
                    in
                    (retry && first) || base_failure
                | Branch_reconcile.Commit_merge _
                | Branch_reconcile.Plan_remote_replay _
                | Branch_reconcile.Checkout_remote _ | Branch_reconcile.Observe
                | Branch_reconcile.Pin _ | Branch_reconcile.Integrate _
                | Branch_reconcile.Inspect | Branch_reconcile.Verify_recovery
                | Branch_reconcile.Continue _ | Branch_reconcile.Confirm _ ->
                    false
              in
              if fail_push then
                Branch_reconcile.Retryable
                  { reason = "transport unavailable"; retry_after = None }
              else
                let result =
                  Real_worktree.reconcile ~path ~project_name ~branch ~operation
                    command
                in
                (match command.Branch_reconcile.kind with
                | Branch_reconcile.Publish { candidate; _ } ->
                    if result = Branch_reconcile.Published then
                      published := Some candidate
                | Branch_reconcile.Commit_merge _
                | Branch_reconcile.Plan_remote_replay _
                | Branch_reconcile.Checkout_remote _ | Branch_reconcile.Observe
                | Branch_reconcile.Pin _ | Branch_reconcile.Integrate _
                | Branch_reconcile.Inspect | Branch_reconcile.Verify_recovery
                | Branch_reconcile.Continue _ | Branch_reconcile.Confirm _ ->
                    ());
                result

            let find_for_branch branch =
              if branch = Branch.of_string "demo/patch-0" then Some path
              else None

            let ensure_ready ~path:_ ~branch:_ = Ok true
          end in
          let runtime = Runtime.create ~gameplan ~main_branch:main () in
          if feature then
            Runtime.update_orchestrator runtime (fun orch ->
                Orchestrator.set_execution_mode orch
                  (get (Execution_mode.infer_gameplan gameplan)));
          let backend_calls = ref 0 in
          let module Env : Runner_fiber.Runner_env.S = struct
            let runtime = runtime
            let clock = clock
            let fs = fs
            let project_name = "demo"
            let user_config = User_config.{ on_worktree_create = None }
            let worktree_mutex = Eio.Mutex.create ()
            let hook_mutex = Eio.Mutex.create ()
            let fetch_mutex = Eio.Mutex.create ()
            let owner = "owner"
            let repo = "repo"
            let main_branch = main
            let max_concurrency = 1
            let automerge_timeout = 120.
            let review_team = None
            let findings_registry = Findings_registry.create ()
            let review_clients = []
            let transcripts = Hashtbl.create 4
            let transcript_updates = Hashtbl.create 4

            let event_log =
              Event_log.create
                ~path:
                  (Filename.concat
                     (Project_store.project_dir project_name)
                     "runner-events.jsonl")

            let pick_backend ~complexity:_ =
              incr backend_calls;
              failwith "Publication must never select an agent backend"

            let register_pr ~patch_id:_ ~pr_number:_ = ()
          end in
          let module Runner = Runner_fiber.Make (Fake_forge) (W) (Env) in
          let wait () =
            let rec loop () =
              let ready =
                Runtime.read runtime (fun snap ->
                    (Orchestrator.agent snap.Runtime.orchestrator (id "0"))
                      .Patch_agent.pr_body_delivered)
              in
              if ready then ()
              else (
                Eio.Time.sleep clock 0.02;
                loop ())
            in
            loop ()
          in
          Eio.Time.with_timeout_exn clock 25. (fun () ->
              Eio.Fiber.first (fun () -> Runner.run ()) wait);
          assert (!backend_calls = 0);
          assert (List.length !publications = if retry then 2 else 1);
          assert (
            List.for_all
              (Branch_reconcile.Commit.equal (List.hd !publications))
              !publications);
          assert (
            Git.git_capture ~cwd:path [ "rev-list"; "--count"; "main..HEAD" ]
            = "1");
          let final_agent =
            Runtime.read runtime (fun snap ->
                Orchestrator.agent snap.Runtime.orchestrator (id "0"))
          in
          assert (final_agent.Patch_agent.push_failure_count = 0);
          assert (
            Branch_reconcile.phase final_agent.Patch_agent.branch_reconcile
            = Some Branch_reconcile.Settled);
          assert (starts runtime = []);
          assert (
            Git.git_capture ~cwd:origin
              [
                "show"; "demo/patch-0:" ^ Gameplan_publication.path publication;
              ]
            = String.trim source);
          (* Later scheduling requests must observe a fresh base even after the
             preceding operation settled. A failed push retains the receipt and
             candidate checked by the executor wrapper above. *)
          let advance_and_rebase ?(kind = Operation_kind.Rebase) parent =
            let tree =
              Git.git_capture ~cwd:path [ "rev-parse"; parent ^ "^{tree}" ]
            in
            let target =
              Git.git_capture ~cwd:path
                [ "commit-tree"; tree; "-p"; parent; "-m"; "Advance base" ]
            in
            Git.run_git ~cwd:path
              [ "push"; "origin"; target ^ ":refs/heads/main" ];
            Runtime.update_orchestrator runtime (fun orch ->
                let orch =
                  if kind = Operation_kind.Merge_conflict then
                    Orchestrator.set_has_conflict orch (id "0")
                  else orch
                in
                Orchestrator.enqueue orch (id "0") kind);
            let rec wait_rebase () =
              let ready =
                Runtime.read runtime (fun snap ->
                    let agent =
                      Orchestrator.agent snap.Runtime.orchestrator (id "0")
                    in
                    agent.Patch_agent.branch_rebased_onto_sha = Some target
                    && Branch_reconcile.phase agent.Patch_agent.branch_reconcile
                       = Some Branch_reconcile.Settled)
              in
              if not ready then (
                Eio.Time.sleep clock 0.02;
                wait_rebase ())
            in
            Eio.Time.with_timeout_exn clock 25. (fun () ->
                Eio.Fiber.first (fun () -> Runner.run ()) wait_rebase);
            Git.run_git ~cwd:origin
              [ "merge-base"; "--is-ancestor"; target; "demo/patch-0" ];
            assert (
              Git.git_capture ~cwd:origin
                [
                  "show";
                  "demo/patch-0:" ^ Gameplan_publication.path publication;
                ]
              = String.trim source);
            assert (!backend_calls = 0);
            target
          in
          let original_base =
            Git.git_capture ~cwd:path [ "rev-parse"; "main" ]
          in
          let next_base = advance_and_rebase original_base in
          let third_base = advance_and_rebase next_base in
          ignore
            (advance_and_rebase ~kind:Operation_kind.Merge_conflict third_base);
          assert (List.length !publications = if retry then 6 else 4)))

let () =
  let data = Filename.temp_dir "onton-publication-" "" in
  let prior = Sys.getenv_opt "ONTON_DATA_DIR" in
  Unix.putenv "ONTON_DATA_DIR" data;
  Fun.protect
    ~finally:(fun () ->
      (match prior with
      | Some value -> Unix.putenv "ONTON_DATA_DIR" value
      | None -> Unix.unsetenv "ONTON_DATA_DIR");
      Git.sh ~dir:"/" ("rm -rf " ^ Filename.quote data))
    (fun () ->
      Eio_main.run (fun env ->
          state_and_resume ();
          commit env ~yaml:true ~content:source;
          commit env ~yaml:false
            ~content:"{\n  \"preserve\": \"formatting\"\n}\n";
          refuses_symlink env ();
          runner_without_backend env ~feature:false ~retry:false;
          runner_without_backend env ~feature:true ~retry:false;
          runner_without_backend env ~feature:false ~retry:true;
          runner_without_backend env ~feature:true ~retry:true));
  print_endline "test_gameplan_publication: OK"

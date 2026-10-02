(* @archlint.module test
   @archlint.domain session-driver *)
open Base
open Onton
open Onton_core.Types

let head = ref "base"
let pushes = ref 0
let base_head = ref "base"

module Fake_worktree : Worktree.S = struct
  let integrate ~root_path:_ ~root_branch:_ ~descendant_branch:_ ~head_sha:_ =
    Worktree.Integration_error "unsupported fake"

  let resolve_main_root () = assert false
  let is_checked_out_in_repo_root _ = assert false
  let remote_branch_exists _ = assert false
  let create ~project_name:_ ~patch_id:_ ~branch:_ ~base_ref:_ = assert false
  let remove ~discard:_ _ = assert false
  let detect_branch ~path:_ = assert false
  let list_with_branches () = assert false
  let find_for_branch _ = None
  let prune_stale_for_branch _ = assert false
  let ensure_ready ~path:_ ~branch:_ = Ok true
  let run_hook ~clock:_ ~script:_ ~cwd:_ ~env:_ () = assert false
  let fetch_origin ~fetch_lock:_ ~path:_ = assert false

  let fetch_origin_branch ~fetch_lock:_ ~branch:_ : Worktree.fetch_branch_result
      =
    assert false

  let git_status ~path:_ = assert false
  let has_uncommitted_changes ~path:_ = assert false
  let conflict_diff ~path:_ = assert false

  let rebase_onto ~path:_ ~target:_ ~upstream:_ ~project_name:_ ~ancestor_ids:_
      () =
    assert false

  let read_branch_sha ~path:_ ~ref_name =
    if String.is_suffix ref_name ~suffix:"/main" then Some !base_head
    else Some !head

  let is_ancestor ~path:_ ~ancestor ~descendant =
    String.equal ancestor "base" && String.equal descendant "advanced-base"

  let read_in_progress_conflict_info ~path:_ ~target:_ ~project_name:_
      ~ancestor_ids:_ =
    assert false

  let force_push_with_lease ~path:_ ~branch:_ ~base:_ =
    Int.incr pushes;
    if String.equal !head "base" then Worktree.Push_no_commits
    else Worktree.Push_ok

  let rebase_in_progress ~path:_ = assert false
end

let run_case ?(detect_pr = false) ?(advance_base = false) env ~content ~commit
    ~expect_opt_out =
  head := "base";
  base_head := if advance_base then "advanced-base" else "base";
  pushes := 0;
  let root = Stdlib.Filename.temp_dir "onton-wontdo-" "" in
  let old = Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" in
  Unix.putenv "ONTON_DATA_DIR" root;
  Exn.protect
    ~finally:(fun () ->
      (match old with
      | Some value -> Unix.putenv "ONTON_DATA_DIR" value
      | None -> Unix.unsetenv "ONTON_DATA_DIR");
      ignore (Stdlib.Sys.command ("rm -rf " ^ Stdlib.Filename.quote root)))
    ~f:(fun () ->
      let patch_id = Patch_id.of_string "p1" in
      let main = Branch.of_string "main" in
      let patch =
        Patch.
          {
            id = patch_id;
            title = "Test";
            description = "";
            branch = Branch.of_string "patch-1";
            dependencies = [];
            spec = "";
            acceptance_criteria = [];
            files = [];
            classification = "";
            changes = [];
            test_stubs_introduced = [];
            test_stubs_implemented = [];
            complexity = None;
            precedents = [];
            required_context = [];
          }
      in
      let gameplan =
        Gameplan.
          {
            project_name = "Wontdo test";
            repo_owner = "test";
            repo_name = "test";
            problem_statement = "";
            solution_summary = "";
            final_state_spec = "";
            patches = [ patch ];
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
            open_questions = [];
            functional_changes = [];
            context_resources = [];
            reachability_traces = [];
          }
      in
      let runtime = Runtime.create ~gameplan ~main_branch:main () in
      Runtime.update_orchestrator runtime (fun orch ->
          Orchestrator.fire orch (Orchestrator.Start (patch_id, main)));
      let module Env = struct
        let runtime = runtime
        let clock = Eio.Stdenv.clock env
        let fs = Eio.Stdenv.fs env
        let project_name = gameplan.project_name
        let owner = "test"
        let repo = "test"
        let transcripts = Stdlib.Hashtbl.create 1
        let transcript_updates = Stdlib.Hashtbl.create 1
        let event_log = Event_log.create ~path:"/dev/null"
        let user_config = { User_config.on_worktree_create = None }
        let worktree_mutex = Eio.Mutex.create ()
        let hook_mutex = Eio.Mutex.create ()
        let fetch_mutex = Eio.Mutex.create ()
      end in
      let module SD = Session_driver.Make (Fake_worktree) (Env) in
      let path =
        Project_store.wontdo_artifact_path ~project_name:gameplan.project_name
          ~patch_id
      in
      let backend =
        Llm_backend.
          {
            name = "test";
            run_streaming =
              (fun ~project_name:_
                ~cwd:_
                ~patch_id:_
                ~prompt
                ~resume_session:_
                ~session_uuid:_
                ~complexity:_
                ~on_event
              ->
                assert (String.is_substring prompt ~substring:path);
                assert (
                  String.is_substring prompt ~substring:"without committing");
                let oc = Stdlib.open_out_bin path in
                Stdlib.output_string oc content;
                Stdlib.close_out oc;
                if detect_pr then
                  on_event
                    (Stream_event.Text_delta
                       "https://github.com/test/test/pull/123\n");
                if commit then head := "commit";
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
      let agent =
        Runtime.read runtime (fun snap ->
            Orchestrator.agent snap.orchestrator patch_id)
      in
      let result =
        Telemetry_dispatch.with_sinks
          ~sinks:
            [
              Activity_log_sink.sink ~main_branch:main
                ~update:(Runtime.update_activity_log runtime)
                ();
            ]
          (fun () ->
            SD.run ~kind:None ~delivery_mode:Onton_core.Patch_decision.Start
              ~patch_id ~prompt:"Implement the patch" ~agent
              ~on_pr_detected:(fun pr_number ->
                Runtime.update_orchestrator runtime (fun orch ->
                    Orchestrator.set_pr_number orch patch_id pr_number))
              ~backend ~complexity:None)
      in
      let snapshot = Runtime.read runtime Fn.id in
      let after = Orchestrator.agent snapshot.orchestrator patch_id in
      if detect_pr then (
        assert (Onton_core.Patch_agent.has_pr after);
        assert (not (Onton_core.Patch_agent.needs_intervention after)));
      if expect_opt_out then (
        assert (Poly.equal result.disposition `Failed);
        assert (!pushes = 0);
        assert (not after.busy);
        assert (Onton_core.Patch_agent.needs_intervention after);
        assert (
          List.exists
            (Onton_core.Activity_log.recent_events snapshot.activity_log
               ~limit:100) ~f:(fun event ->
              String.is_substring event.message
                ~substring:(String.strip content)));
        let orch =
          Orchestrator.apply_start_outcome snapshot.orchestrator patch_id
            Orchestrator.Start_failed
        in
        assert (
          Onton_core.Patch_agent.needs_intervention
            (Orchestrator.agent orch patch_id)))
      else (
        assert (!pushes = 1);
        if commit then (
          assert (Poly.equal result.disposition `Ok);
          assert (after.no_commits_push_count = 0);
          assert (not (Onton_core.Patch_agent.needs_intervention after));
          assert (
            Option.equal String.equal after.expected_remote_head_oid
              (Some "commit")))))

let () =
  Eio_main.run (fun env ->
      run_case env ~content:"  This patch is unnecessary.\n" ~commit:false
        ~expect_opt_out:true;
      run_case env ~content:" \n\t " ~commit:false ~expect_opt_out:false;
      run_case env ~content:"Too late to opt out" ~commit:true
        ~expect_opt_out:false;
      run_case env ~detect_pr:true ~content:"A PR was detected during this turn"
        ~commit:false ~expect_opt_out:false;
      run_case env ~advance_base:true ~content:"Opt out after base advancement"
        ~commit:false ~expect_opt_out:true)

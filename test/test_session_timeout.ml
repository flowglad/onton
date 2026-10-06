(* @archlint.module test
   @archlint.domain session-driver *)
open Base
open Onton
open Onton_core.Types

let head = ref "base"
let pushes = ref 0
let base_head = ref "base"
let recovered_worktree = ref ""

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
  let find_for_branch _ = Some !recovered_worktree
  let prune_stale_for_branch _ = ()
  let ensure_ready ~path ~branch:_ = Ok (String.equal path !recovered_worktree)
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

  let commit_gameplan ~path:_ ~publication:_ ~message:_ = assert false
  let rebase_in_progress ~path:_ = assert false
end

let run_case env ~capture_session ~respond =
  let root = Stdlib.Filename.temp_dir "onton-session-timeout-" "" in
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
            project_name = "Timeout test";
            repo_owner = "test";
            repo_name = "test";
            problem_statement = "";
            architecture_design = None;
            solution_summary = "";
            final_state_spec = "";
            patches = [ patch ];
            operational_considerations = "";
            required_changes = "";
            ordering_constraints = [];
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
            open_questions = [];
            functional_changes = [];
            context_resources = [];
            publication = None;
            reachability_traces = [];
          }
      in
      let runtime = Runtime.create ~gameplan ~main_branch:main () in
      recovered_worktree := Stdlib.Filename.concat root "recovered";
      Runtime.update_orchestrator runtime (fun orch ->
          let orch =
            Orchestrator.set_worktree_path orch patch_id
              (Stdlib.Filename.concat root "obsolete")
          in
          Orchestrator.fire orch (Orchestrator.Start (patch_id, main)));
      if respond then
        Runtime.update_orchestrator runtime (fun orch ->
            let orch = Orchestrator.complete orch patch_id in
            let orch =
              Orchestrator.set_pr_number orch patch_id (Pr_number.of_int 123)
            in
            let orch =
              Orchestrator.enqueue orch patch_id Operation_kind.Human
            in
            Orchestrator.fire orch
              (Orchestrator.Respond (patch_id, Operation_kind.Human)));
      (* A fresh rescue attempt can time out too; its new thread must become
         resumable rather than leaving the agent stuck in Tried_fresh. *)
      Runtime.update_orchestrator runtime (fun orch ->
          let orch =
            Orchestrator.set_llm_session_id orch patch_id (Some "failed-thread")
          in
          let orch =
            Orchestrator.apply_session_result orch patch_id Session_no_commits
          in
          Orchestrator.set_session_failed orch patch_id);
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
      let attempt = ref 0 in
      let context_calls = ref 0 in
      let resumes = ref [] in
      let backend =
        Llm_backend.
          {
            name = "test";
            run_streaming =
              (fun ~project_name:_
                ~cwd:_
                ~patch_id:_
                ~prompt
                ~resume_session
                ~session_uuid:_
                ~complexity:_
                ~on_event
              ->
                Int.incr attempt;
                assert (!context_calls = if capture_session then 1 else !attempt);
                resumes := resume_session :: !resumes;
                assert (
                  String.is_prefix prompt
                    ~prefix:
                      (if Option.is_none resume_session then
                         "FULL PATCH CONTEXT\nContinue the patch"
                       else "Continue the patch"));
                assert (
                  Bool.equal
                    (String.is_substring prompt ~substring:"FULL PATCH CONTEXT")
                    (Option.is_none resume_session));
                if capture_session && !attempt = 1 then
                  on_event
                    (Stream_event.Session_init
                       {
                         session_id = "retained-thread";
                         api_key_source = None;
                         model = None;
                         claude_code_version = None;
                         permission_mode = None;
                       });
                on_event (Stream_event.Text_delta "Implementation progress");
                {
                  exit_code = 137;
                  stdout = "";
                  stderr = "";
                  got_events = true;
                  saw_final_result = false;
                  timed_out = true;
                });
          }
      in
      let snapshot_path = Stdlib.Filename.concat root "snapshot.json" in
      for _ = 1 to 4 do
        let agent =
          Runtime.read runtime (fun snap ->
              Orchestrator.agent snap.orchestrator patch_id)
        in
        let delivery_mode =
          if respond then Onton_core.Patch_decision.Respond
          else Onton_core.Patch_decision.Start
        in
        let result =
          SD.run ~kind:None ~delivery_mode ~patch_id
            ~prompt:
              (SD.create_prompt
                 ~context:(fun ~worktree_path ->
                   assert (String.equal worktree_path !recovered_worktree);
                   Int.incr context_calls;
                   "FULL PATCH CONTEXT\n")
                 ~turn:"Continue the patch")
            ~agent
            ~on_pr_detected:(fun _ -> ())
            ~backend ~complexity:None
        in
        assert (Poly.equal result.disposition `Failed);
        let snap = Runtime.read runtime Fn.id in
        let after = Orchestrator.agent snap.orchestrator patch_id in
        assert (not after.busy);
        assert (agent.no_commits_push_count = 1);
        assert (after.no_commits_push_count = agent.no_commits_push_count);
        assert (not (Onton_core.Patch_agent.needs_intervention after));
        assert (
          Onton_core.Patch_agent.equal_session_fallback after.session_fallback
            Fresh_available);
        assert (
          Option.equal String.equal after.llm_session_id
            (if capture_session then Some "retained-thread" else None));
        assert (Result.is_ok (Persistence.save ~path:snapshot_path snap));
        let loaded =
          Result.ok_or_failwith (Persistence.load ~path:snapshot_path)
        in
        let restored = Orchestrator.agent loaded.orchestrator patch_id in
        assert (
          Option.equal String.equal restored.llm_session_id after.llm_session_id);
        Runtime.update_orchestrator runtime (fun _ ->
            let orch =
              if respond then
                Orchestrator.enqueue loaded.orchestrator patch_id
                  Operation_kind.Human
              else loaded.orchestrator
            in
            Orchestrator.fire orch
              (if respond then
                 Orchestrator.Respond (patch_id, Operation_kind.Human)
               else Orchestrator.Start (patch_id, main)))
      done;
      assert (!attempt = 4);
      assert (
        Poly.equal (List.rev !resumes)
          (if capture_session then
             [
               None;
               Some "retained-thread";
               Some "retained-thread";
               Some "retained-thread";
             ]
           else [ None; None; None; None ]));
      let orch = Runtime.read runtime (fun snap -> snap.orchestrator) in
      let no_resume =
        Orchestrator.apply_session_result orch patch_id Session_no_resume
      in
      assert (
        Option.is_none (Orchestrator.agent no_resume patch_id).llm_session_id);
      let overflow =
        Orchestrator.apply_session_result orch patch_id
          Session_context_exhausted
      in
      assert (
        Option.is_none (Orchestrator.agent overflow patch_id).llm_session_id);
      assert (
        (Orchestrator.agent overflow patch_id).context_exhaustion_count = 1))

let () =
  Eio_main.run (fun env ->
      List.iter [ false; true ] ~f:(fun respond ->
          List.iter [ false; true ] ~f:(fun capture_session ->
              run_case env ~capture_session ~respond)))

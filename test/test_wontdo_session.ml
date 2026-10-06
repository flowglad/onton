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

  let commit_gameplan ~path:_ ~publication:_ ~message:_ = assert false
  let rebase_in_progress ~path:_ = assert false
end

let run_case ?(detect_pr = false) ?(advance_base = false)
    ?(delivery_mode = Onton_core.Patch_decision.Start) ?(kind = None)
    ?(branch_role = `Mainline) ?resume_session ?(prompt = "Implement the patch")
    env ~content ~commit ~expect_opt_out =
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
      let patches =
        match branch_role with
        | `Mainline | `Integration_root -> [ patch ]
        | `Feature_descendant ->
            let root_patch =
              {
                patch with
                id = Patch_id.of_string "root";
                branch = Branch.of_string "integration-root";
              }
            in
            [ root_patch; { patch with dependencies = [ root_patch.id ] } ]
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
            patches;
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
      Runtime.update_orchestrator runtime (fun orch ->
          let mode =
            match branch_role with
            | `Mainline -> Onton_core.Execution_mode.mainline
            | `Integration_root | `Feature_descendant -> (
                match Onton_core.Execution_mode.infer_gameplan gameplan with
                | Ok mode -> mode
                | Error message -> failwith message)
          in
          let orch = Orchestrator.set_execution_mode orch mode in
          let orch =
            Orchestrator.send_human_message orch patch_id "Original guidance"
          in
          let orch =
            Orchestrator.fire orch (Orchestrator.Start (patch_id, main))
          in
          let orch =
            match (delivery_mode, kind) with
            | Onton_core.Patch_decision.Respond, Some kind ->
                let orch = Orchestrator.complete orch patch_id in
                let orch =
                  match branch_role with
                  | `Feature_descendant ->
                      Orchestrator.mark_branch_published orch patch_id
                  | `Mainline | `Integration_root ->
                      Orchestrator.set_pr_number orch patch_id
                        (Pr_number.of_int 123)
                in
                Orchestrator.fire
                  (Orchestrator.enqueue orch patch_id kind)
                  (Orchestrator.Respond (patch_id, kind))
            | Onton_core.Patch_decision.Start, _
            | Onton_core.Patch_decision.Respond, None ->
                orch
          in
          Orchestrator.set_llm_session_id orch patch_id resume_session);
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
      let delivered = ref None in
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
                delivered := Some (prompt, resume_session);
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
      let start_with_human =
        Onton_core.Patch_decision.equal_delivery_mode delivery_mode Start
        && Option.equal Operation_kind.equal kind (Some Human)
      in
      let prompt =
        if start_with_human then
          match Onton_core.Patch_decision.start_delivery agent with
          | Start_with_human { messages } ->
              assert (List.equal String.equal messages [ "Original guidance" ]);
              prompt ^ "\n\n"
              ^ Prompt.render_human_message_prompt
                  ~project_name:gameplan.project_name messages
          | Start_initial -> failwith "Expected queued human guidance at start"
        else prompt
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
            SD.run ~kind ~delivery_mode ~patch_id
              ~prompt:
                (SD.create_prompt
                   ~context:(fun ~worktree_path:_ -> "")
                   ~turn:prompt)
              ~agent
              ~on_pr_detected:(fun pr_number ->
                Runtime.update_orchestrator runtime (fun orch ->
                    Orchestrator.set_pr_number orch patch_id pr_number))
              ~backend ~complexity:None)
      in
      let delivered_prompt, delivered_resume =
        match !delivered with
        | Some delivered -> delivered
        | None -> failwith "Backend was not invoked"
      in
      assert (Option.equal String.equal delivered_resume resume_session);
      if start_with_human then
        assert (
          String.is_substring delivered_prompt ~substring:"Original guidance");
      let plain_human_followup =
        Onton_core.Patch_decision.equal_delivery_mode delivery_mode Respond
        && Option.equal Operation_kind.equal kind (Some Human)
      in
      if plain_human_followup then (
        assert (String.equal delivered_prompt prompt);
        assert (Poly.equal result.disposition `Ok))
      else (
        assert (String.is_prefix delivered_prompt ~prefix:prompt);
        assert (String.is_substring delivered_prompt ~substring:path);
        assert (
          String.is_substring delivered_prompt ~substring:"without committing");
        match branch_role with
        | `Mainline -> ()
        | `Integration_root ->
            assert (
              String.is_substring delivered_prompt
                ~substring:"Integration root: preserve all published history.")
        | `Feature_descendant ->
            assert (
              String.is_substring delivered_prompt
                ~substring:"Feature branch construction:"));
      let snapshot = Runtime.read runtime Fn.id in
      let after = Orchestrator.agent snapshot.orchestrator patch_id in
      if detect_pr then (
        assert (Onton_core.Patch_agent.has_pr after);
        assert (not (Onton_core.Patch_agent.needs_intervention after)));
      if expect_opt_out then (
        assert (Poly.equal result.disposition `Failed);
        assert (!pushes = 0);
        assert (not after.busy);
        assert (List.is_empty after.inflight_human_messages);
        assert (List.is_empty after.human_messages);
        assert (List.is_empty after.queue);
        assert (Onton_core.Patch_agent.needs_intervention after);
        assert (
          Option.equal String.equal after.wontdo_reason
            (Some (String.strip content)));
        assert (
          Onton_core.Patch_agent.equal_session_fallback after.session_fallback
            Fresh_available);
        assert (after.start_attempts_without_pr = 0);
        assert (after.no_commits_push_count = 0);
        assert (not after.merged);
        assert (
          List.exists
            (Onton_core.Activity_log.recent_transitions snapshot.activity_log
               ~limit:100) ~f:(fun entry ->
              Onton_core.Display_status.equal entry.to_status Wontdo));
        let restored =
          match
            Persistence.snapshot_of_yojson
              (Persistence.snapshot_to_yojson snapshot)
          with
          | Ok snapshot -> snapshot
          | Error message -> failwith message
        in
        let restored_agent =
          Orchestrator.agent restored.orchestrator patch_id
        in
        assert (
          Option.equal String.equal restored_agent.wontdo_reason
            after.wontdo_reason);
        let stopped, effects, stopped_messages =
          Patch_controller.plan_tick_messages restored.orchestrator
            ~project_name:gameplan.project_name ~gameplan
        in
        assert (List.is_empty effects);
        assert (List.is_empty (Patch_controller.discovery_intents stopped));
        assert (List.is_empty stopped_messages);
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
            (Orchestrator.agent orch patch_id));
        let bumped = Orchestrator.reset_intervention_state orch patch_id in
        let bumped_agent = Orchestrator.agent bumped patch_id in
        assert (List.is_empty bumped_agent.human_messages);
        assert (List.is_empty bumped_agent.queue);
        assert (
          not
            (Onton_core.Patch_agent.needs_intervention
               (Orchestrator.agent bumped patch_id)));
        let reprompted =
          Orchestrator.send_human_message orch patch_id
            "Implement revised scope"
        in
        let reprompted_agent = Orchestrator.agent reprompted patch_id in
        assert (Option.is_none reprompted_agent.wontdo_reason);
        assert (not (Onton_core.Patch_agent.needs_intervention reprompted_agent));
        let ready, _, messages =
          Patch_controller.plan_tick_messages reprompted
            ~project_name:gameplan.project_name ~gameplan
        in
        let message =
          match messages with [ message ] -> message | _ -> assert false
        in
        Runtime.update_orchestrator runtime (fun _ ->
            let accepted, action =
              Orchestrator.accept_message ready message.message_id
            in
            assert (Option.is_some action);
            accepted);
        let resumed =
          Runtime.read runtime (fun snap ->
              Orchestrator.agent snap.orchestrator patch_id)
        in
        assert (
          Onton_core.Patch_decision.equal_start_delivery
            (Onton_core.Patch_decision.start_delivery resumed)
            (Start_with_human { messages = [ "Implement revised scope" ] }));
        let retry_backend =
          {
            backend with
            Llm_backend.run_streaming =
              (fun ~project_name:_
                ~cwd:_
                ~patch_id:_
                ~prompt:_
                ~resume_session:_
                ~session_uuid:_
                ~complexity:_
                ~on_event:_
              ->
                assert (not (Stdlib.Sys.file_exists path));
                head := "commit";
                {
                  Llm_backend.exit_code = 0;
                  stdout = "";
                  stderr = "";
                  got_events = true;
                  saw_final_result = true;
                  timed_out = false;
                });
          }
        in
        let retried =
          SD.run ~kind:(Some Human)
            ~delivery_mode:Onton_core.Patch_decision.Start ~patch_id
            ~prompt:
              (SD.create_prompt
                 ~context:(fun ~worktree_path:_ -> "FULL PATCH CONTEXT\n")
                 ~turn:"Implement revised scope")
            ~agent:resumed
            ~on_pr_detected:(fun _ -> assert false)
            ~backend:retry_backend ~complexity:None
        in
        assert (Poly.equal retried.disposition `Ok);
        assert (!pushes = 1);
        assert (
          Option.is_none
            (Runtime.read runtime (fun snap ->
                 (Orchestrator.agent snap.orchestrator patch_id).wontdo_reason))))
      else (
        assert (Option.is_none after.wontdo_reason);
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
      run_case env
        ~content:
          "  # WONTDO\n\nThis patch is unnecessary.\n\nPrerequisite missing.\n"
        ~commit:false ~expect_opt_out:true;
      run_case env ~content:" \n\t " ~commit:false ~expect_opt_out:false;
      run_case env ~content:"Too late to opt out" ~commit:true
        ~expect_opt_out:false;
      run_case env ~detect_pr:true ~content:"A PR was detected during this turn"
        ~commit:false ~expect_opt_out:false;
      run_case env ~advance_base:true ~content:"Opt out after base advancement"
        ~commit:false ~expect_opt_out:true;
      List.iter [ `Mainline; `Integration_root; `Feature_descendant ]
        ~f:(fun branch_role ->
          List.iter [ None; Some "existing-thread" ] ~f:(fun resume_session ->
              List.iter
                [
                  [ "Are both changes compatible with the current design?" ];
                  [ "Check compatibility."; "Explain any tradeoffs." ];
                ]
                ~f:(fun messages ->
                  let prompt =
                    Prompt.render_human_message_prompt
                      ~project_name:"Wontdo test" messages
                  in
                  run_case env ~branch_role ?resume_session ~prompt
                    ~delivery_mode:Onton_core.Patch_decision.Respond
                    ~kind:(Some Human) ~content:"" ~commit:false
                    ~expect_opt_out:false);
              run_case env ~branch_role ?resume_session
                ~delivery_mode:Onton_core.Patch_decision.Start
                ~kind:(Some Human) ~content:"" ~commit:true
                ~expect_opt_out:false;
              run_case env ~branch_role ?resume_session
                ~delivery_mode:Onton_core.Patch_decision.Respond ~kind:(Some Ci)
                ~content:"" ~commit:true ~expect_opt_out:false)))

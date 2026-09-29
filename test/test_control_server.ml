(* @archlint.module test
   @archlint.domain control-command *)

open Onton_core
open Types

let gameplan =
  {
    Gameplan.project_name = "control-test";
    repo_owner = "";
    repo_name = "";
    problem_statement = "";
    solution_summary = "";
    final_state_spec = "";
    patches = [];
    current_state_analysis = "";
    explicit_opinions = "";
    acceptance_criteria = [];
    open_questions = [];
    functional_changes = [];
    context_resources = [];
    reachability_traces = [];
  }

let () =
  let directory = Filename.temp_file "onton-control" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let path = Filename.concat directory "control.sock" in
  let snapshot_path = Filename.concat directory "snapshot.json" in
  let rec remove_tree path =
    if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
      Array.iter
        (fun entry -> remove_tree (Filename.concat path entry))
        (Sys.readdir path);
      Unix.rmdir path)
    else Sys.remove path
  in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      Eio_main.run @@ fun env ->
      let main_branch = Branch.of_string "main" in
      let runtime = Onton.Runtime.create ~gameplan ~main_branch () in
      let patch_id = Patch_id.of_string "branch-24" in
      Onton.Runtime.update_orchestrator runtime (fun orch ->
          Onton.Orchestrator.add_agent orch ~patch_id
            ~branch:(Branch.of_string "feature")
            ~base_branch:main_branch ~pr_number:(Pr_number.of_int 24));
      let net = Eio.Stdenv.net env in
      Eio.Fiber.any
        [
          Onton.Control_server.run ~net ~runtime ~snapshot_path ~path;
          (fun () ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
            let envelope ?(version = 1) id kind payload =
              `Assoc
                [
                  ("version", `Int version);
                  ("id", `String id);
                  ("type", `String kind);
                  ( "payload",
                    `Assoc (("patch_id", `String "branch-24") :: payload) );
                ]
            in
            let send command expected_id expected_status =
              Eio.Switch.run @@ fun sw ->
              let flow = Eio.Net.connect ~sw net (`Unix path) in
              Eio.Flow.copy_string (Yojson.Safe.to_string command ^ "\n") flow;
              let response =
                Eio.Buf_read.line (Eio.Buf_read.of_flow ~max_size:4096 flow)
                |> Yojson.Safe.from_string
              in
              assert (
                response
                = `Assoc
                    [
                      ("id", `String expected_id);
                      ("status", `String expected_status);
                    ])
            in
            let send_automerge id enabled expected_status =
              send
                (envelope id "set_automerge" [ ("enabled", `Bool enabled) ])
                id expected_status
            in
            send_automerge "test-1" true "applied";
            send_automerge "test-1" true "already_applied";
            send_automerge "test-noop" true "already_applied";
            (match Onton.Persistence.load ~path:snapshot_path with
            | Ok saved ->
                assert
                  (Option.get
                     (Onton.Orchestrator.find_agent
                        saved.Onton.Runtime.orchestrator patch_id))
                    .Patch_agent.automerge_enabled;
                assert (
                  not
                    (List.mem "test-noop"
                       saved.Onton.Runtime.applied_control_ids))
            | Error msg -> failwith msg);
            assert
              (Option.get
                 (Onton.Orchestrator.find_agent
                    (Onton.Runtime.read runtime (fun snapshot ->
                         snapshot.Onton.Runtime.orchestrator))
                    patch_id))
                .Patch_agent.automerge_enabled;
            send_automerge "test-2" false "applied";
            send (envelope "bump-fresh" "bump" []) "bump-fresh" "not_applicable";
            send
              (envelope ~version:2 "bump-version" "bump" [])
              "" "invalid_command";
            Onton.Runtime.update_orchestrator runtime (fun orch ->
                let rec cap n orch =
                  if n = 0 then orch
                  else
                    cap (n - 1)
                      (Onton.Orchestrator.increment_ci_failure_count orch
                         patch_id)
                in
                cap Patch_agent.default_max_ci_failures orch);
            send (envelope "bump-1" "bump" []) "bump-1" "applied";
            send (envelope "bump-1" "bump" []) "bump-1" "already_applied";
            assert (
              (Onton.Runtime.read runtime (fun snapshot ->
                   Onton.Orchestrator.agent snapshot.Onton.Runtime.orchestrator
                     patch_id))
                .Patch_agent.ci_failure_count = 0);
            let human_message =
              envelope "message-1" "send_human_message"
                [ ("message", `String "Please investigate CI") ]
            in
            send human_message "message-1" "applied";
            send human_message "message-1" "already_applied";
            send
              (envelope ~version:2 "message-version" "send_human_message"
                 [ ("message", `String "Please investigate CI") ])
              "" "invalid_command";
            send
              (envelope "message-blank" "send_human_message"
                 [ ("message", `String "   ") ])
              "" "invalid_command";
            let agent =
              Onton.Runtime.read runtime (fun snapshot ->
                  Onton.Orchestrator.agent snapshot.Onton.Runtime.orchestrator
                    patch_id)
            in
            assert (List.length agent.Patch_agent.human_messages = 1);
            (match Onton.Persistence.load ~path:snapshot_path with
            | Ok saved ->
                assert (
                  List.mem "message-1" saved.Onton.Runtime.applied_control_ids);
                assert (
                  not
                    (List.mem "bump-fresh"
                       saved.Onton.Runtime.applied_control_ids))
            | Error msg -> failwith msg);
            let sidecar_path = Filename.concat directory "llm-session-ids" in
            Unix.mkdir sidecar_path 0o700;
            let blocker_path = Filename.concat sidecar_path "branch-24.txt" in
            Unix.mkdir blocker_path 0o700;
            assert (
              Result.is_error
                (Onton.Runtime.read runtime (fun snapshot ->
                     Onton.Persistence.save ~path:snapshot_path snapshot)));
            send_automerge "test-3" true "applied";
            (match
               Onton.Persistence.snapshot_of_yojson
                 (Yojson.Safe.from_file snapshot_path)
             with
            | Ok saved ->
                assert
                  (Option.get
                     (Onton.Orchestrator.find_agent
                        saved.Onton.Runtime.orchestrator patch_id))
                    .Patch_agent.automerge_enabled
            | Error msg -> failwith msg);
            assert
              (Option.get
                 (Onton.Orchestrator.find_agent
                    (Onton.Runtime.read runtime (fun snapshot ->
                         snapshot.Onton.Runtime.orchestrator))
                    patch_id))
                .Patch_agent.automerge_enabled;
            Unix.rmdir blocker_path;
            Unix.rmdir sidecar_path;
            send_automerge "test-4" false "applied";
            Sys.remove snapshot_path;
            Unix.mkdir snapshot_path 0o700;
            send_automerge "test-5" true "persistence_failed";
            assert (
              not
                (Option.get
                   (Onton.Orchestrator.find_agent
                      (Onton.Runtime.read runtime (fun snapshot ->
                           snapshot.Onton.Runtime.orchestrator))
                      patch_id))
                  .Patch_agent.automerge_enabled);
            Unix.rmdir snapshot_path;
            Onton.Runtime.update_orchestrator runtime (fun orch ->
                Onton.Orchestrator.mark_merged orch patch_id);
            send
              (envelope "message-merged" "send_human_message"
                 [ ("message", `String "too late") ])
              "message-merged" "not_applicable";
            let snapshot = Onton.Runtime.read runtime Fun.id in
            assert (
              List.length
                (Onton.Orchestrator.agent snapshot.Onton.Runtime.orchestrator
                   patch_id)
                  .Patch_agent.human_messages
              = 1);
            assert (
              not
                (List.mem "message-merged"
                   snapshot.Onton.Runtime.applied_control_ids)));
        ];
      assert (not (Sys.file_exists path));
      Unix.chmod directory 0o755;
      let rejected_shared_parent =
        match
          Onton.Control_server.run ~net ~runtime ~snapshot_path ~path ()
        with
        | () -> false
        | exception Invalid_argument _ -> true
      in
      assert rejected_shared_parent;
      Unix.chmod directory 0o700)

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
            let send id enabled expected_status =
              Eio.Switch.run @@ fun sw ->
              let flow = Eio.Net.connect ~sw net (`Unix path) in
              let command =
                Yojson.Safe.to_string
                  (`Assoc
                     [
                       ("version", `Int 1);
                       ("id", `String id);
                       ("type", `String "set_automerge");
                       ( "payload",
                         `Assoc
                           [
                             ("patch_id", `String "branch-24");
                             ("enabled", `Bool enabled);
                           ] );
                     ])
              in
              Eio.Flow.copy_string (command ^ "\n") flow;
              let response =
                Eio.Buf_read.line (Eio.Buf_read.of_flow ~max_size:4096 flow)
                |> Yojson.Safe.from_string
              in
              assert (
                response
                = `Assoc
                    [ ("id", `String id); ("status", `String expected_status) ])
            in
            send "test-1" true "applied";
            send "test-1" true "already_applied";
            (match Onton.Persistence.load ~path:snapshot_path with
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
            send "test-2" false "applied";
            Onton.Runtime.update_orchestrator runtime (fun orch ->
                let rec cap n orch =
                  if n = 0 then orch
                  else
                    cap (n - 1)
                      (Onton.Orchestrator.increment_ci_failure_count orch
                         patch_id)
                in
                cap Patch_agent.default_max_ci_failures orch);
            let bump_command =
              Control_command.Bump { id = "bump-1"; patch_id }
            in
            assert (
              String.equal
                (Onton.Control_server.execute runtime ~snapshot_path
                   bump_command)
                "applied");
            assert (
              String.equal
                (Onton.Control_server.execute runtime ~snapshot_path
                   bump_command)
                "already_applied");
            assert (
              (Onton.Runtime.read runtime (fun snapshot ->
                   Onton.Orchestrator.agent snapshot.Onton.Runtime.orchestrator
                     patch_id))
                .Patch_agent.ci_failure_count = 0);
            let human_command =
              Control_command.Send_human_message
                {
                  id = "message-1";
                  patch_id;
                  message = "Please investigate CI";
                }
            in
            assert (
              String.equal
                (Onton.Control_server.execute runtime ~snapshot_path
                   human_command)
                "applied");
            assert (
              String.equal
                (Onton.Control_server.execute runtime ~snapshot_path
                   human_command)
                "already_applied");
            let agent =
              Onton.Runtime.read runtime (fun snapshot ->
                  Onton.Orchestrator.agent snapshot.Onton.Runtime.orchestrator
                    patch_id)
            in
            assert (List.length agent.Patch_agent.human_messages = 1);
            (match Onton.Persistence.load ~path:snapshot_path with
            | Ok saved ->
                assert (
                  List.mem "message-1" saved.Onton.Runtime.applied_control_ids)
            | Error msg -> failwith msg);
            let sidecar_path = Filename.concat directory "llm-session-ids" in
            Unix.mkdir sidecar_path 0o700;
            let blocker_path = Filename.concat sidecar_path "branch-24.txt" in
            Unix.mkdir blocker_path 0o700;
            assert (
              Result.is_error
                (Onton.Runtime.read runtime (fun snapshot ->
                     Onton.Persistence.save ~path:snapshot_path snapshot)));
            send "test-3" true "applied";
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
            send "test-4" false "applied";
            Sys.remove snapshot_path;
            Unix.mkdir snapshot_path 0o700;
            send "test-5" true "persistence_failed";
            assert (
              not
                (Option.get
                   (Onton.Orchestrator.find_agent
                      (Onton.Runtime.read runtime (fun snapshot ->
                           snapshot.Onton.Runtime.orchestrator))
                      patch_id))
                  .Patch_agent.automerge_enabled);
            Unix.rmdir snapshot_path);
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

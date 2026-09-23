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
            let send enabled expected_status =
              Eio.Switch.run @@ fun sw ->
              let flow = Eio.Net.connect ~sw net (`Unix path) in
              let command =
                Yojson.Safe.to_string
                  (`Assoc
                     [
                       ("version", `Int 1);
                       ("id", `String "test-1");
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
                    [
                      ("id", `String "test-1");
                      ("status", `String expected_status);
                    ])
            in
            send true "applied";
            send true "already_applied";
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
            send false "applied");
        ])

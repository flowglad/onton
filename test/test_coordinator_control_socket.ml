(* @archlint.module test
   @archlint.domain coordinator *)
open Onton_core
open Onton
open Types

[@@@warning "-4-40-42"]

module F = Onton_test_support.Coordinator_store_fixture

let check yes why = if not yes then failwith why

let gameplan : Gameplan.t =
  {
    project_name = "control";
    repo_owner = "";
    repo_name = "";
    problem_statement = "";
    architecture_design = None;
    solution_summary = "";
    final_state_spec = "";
    patches = [];
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

let () =
  let directory = Filename.temp_file ~temp_dir:"/tmp" "onton-c-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let path = Filename.concat directory "control.sock" in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.unlink path with _ -> ());
      Unix.rmdir directory)
    (fun () ->
      Eio_main.run @@ fun env ->
      let fixture = F.fixture () in
      let store, state, next = F.setup fixture in
      ignore
        (F.committed
           (Coordinator_store.commit store ~expected_version:0L
              ~write_id:"initial"
              (Coordinator_store.initial state)));
      let client =
        Resource_client.create ~helper:"fixture"
          ~transport:(F.transport fixture)
      in
      let ended, finish = Eio.Promise.create () in
      let telemetry = ref [] in
      let net = Eio.Stdenv.net env and clock = Eio.Stdenv.clock env in
      Eio.Fiber.both
        (fun () ->
          Coordinator.serve ~net ~clock ~control_path:path
            ~write_status:(fun bytes ->
              telemetry := Yojson.Safe.from_string bytes :: !telemetry)
            {
              store;
              client;
              next_write_id = next;
              next_event =
                (fun () ->
                  Eio.Promise.await ended;
                  None);
              now = (fun () -> 0L);
              stream_timeout = 1.;
              attached = (fun ~resource_id:_ ~readable:_ -> ());
              gameplan;
              main_branch = Branch.of_string "main";
              stop_request = (fun ~resource_id:_ -> Error "unexpected stop");
              outcome = (fun _ -> ());
              blocked = failwith;
            })
        (fun () ->
          let rec wait n =
            if Sys.file_exists path then ()
            else if n = 0 then failwith "socket not bound"
            else (
              Eio.Time.sleep clock 0.01;
              wait (n - 1))
          in
          wait 200;
          check
            ((Unix.lstat path).Unix.st_perm land 0o077 = 0)
            "socket is not owner-only";
          let send id kind expected =
            Eio.Switch.run @@ fun sw ->
            let flow = Eio.Net.connect ~sw net (`Unix path) in
            let frame =
              Yojson.Safe.to_string
                (`Assoc
                   [
                     ("version", `Int 3);
                     ("id", `String id);
                     ("run_id", `String "r");
                     ("config_id", `String "cfg");
                     ("kind", `String kind);
                     ("patch_id", `Null);
                     ("payload", `String "");
                   ])
              ^ "\n"
            in
            Eio.Flow.copy_string frame flow;
            let bytes =
              Eio.Buf_read.line (Eio.Buf_read.of_flow ~max_size:4096 flow)
            in
            check
              (Yojson.Safe.from_string bytes
              = `Assoc [ ("status", `String expected) ])
              "control response"
          in
          send "sleep" "sleep" "accepted";
          send "sleep" "sleep" "already_applied";
          send "sleep" "cancel" "rejected";
          check
            (List.exists
               (function
                 | `Assoc fields ->
                     List.assoc_opt "sleep_ready" fields = Some (`Bool true)
                 | _ -> false)
               !telemetry)
            "committed sleep-ready generation not emitted";
          send "wake" "wake" "accepted";
          fixture.reject_write <- true;
          send "uncertain" "sleep" "not_accepted";
          Eio.Promise.resolve finish ());
      check (not (Sys.file_exists path)) "socket leaked after loop exit";
      check
        (List.for_all
           (fun json ->
             let bytes = Yojson.Safe.to_string json in
             (not (Base.String.is_substring bytes ~substring:"capsule"))
             && not (Base.String.is_substring bytes ~substring:"payload"))
           !telemetry)
        "authority leaked in status");
  print_endline
    "coordinator socket: same-UID intake, serialized durable responses, \
     sleep/wake telemetry, uncertain commit and shutdown"

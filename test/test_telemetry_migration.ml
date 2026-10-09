(* @archlint.module test
   @archlint.domain failure-subkind *)

open Base
open Onton_core

let fail msg = Stdlib.failwith msg

let temp_path name =
  let path = Stdlib.Filename.temp_file name ".jsonl" in
  Stdlib.Sys.remove path;
  path

let read_lines path =
  let ic = Stdlib.open_in path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_in_noerr ic)
    (fun () ->
      let rec loop acc =
        match Stdlib.input_line ic with
        | line -> loop (line :: acc)
        | exception End_of_file -> List.rev acc
      in
      loop [])

let json_member name json = Yojson.Safe.Util.member name json

let string_member name json =
  match json_member name json with
  | `String value -> value
  | _ -> fail ("missing string field " ^ name)

let expect_equal_string ~name expected actual =
  if not (String.equal expected actual) then
    fail (Printf.sprintf "%s: expected %S, got %S" name expected actual)

let expect_equal_json ~name expected actual =
  if not (Yojson.Safe.equal expected actual) then
    fail
      (Printf.sprintf "%s: expected %s, got %s" name
         (Yojson.Safe.to_string expected)
         (Yojson.Safe.to_string actual))

let test_event_log_complete () =
  let path = temp_path "onton-event-log" in
  let event_log = Onton.Event_log.create ~path in
  let patch_id = Types.Patch_id.of_string "patch-5" in
  Onton.Telemetry_dispatch.with_sink ~sink:(Onton.Event_log.sink event_log)
    (fun () ->
      Onton.Telemetry_dispatch.emit
        (Telemetry.Event.Complete
           {
             patch_id;
             session_uuid = Some "session-uuid";
             subkind = Failure_subkind.Other "test";
             payload =
               `Assoc
                 [
                   ("result", `String "Session_failed");
                   ("agent_before", `Assoc []);
                   ("agent_after", `Assoc []);
                 ];
           }));
  match read_lines path with
  | [ line ] -> (
      let json = Yojson.Safe.from_string line in
      expect_equal_string ~name:"kind" "complete" (string_member "kind" json);
      expect_equal_json ~name:"patch_id"
        (Types.Patch_id.yojson_of_t patch_id)
        (json_member "patch_id" json);
      expect_equal_string ~name:"result" "Session_failed"
        (string_member "result" json);
      expect_equal_string ~name:"onton_session_uuid" "session-uuid"
        (string_member "onton_session_uuid" json);
      (match json_member "ts" json with
      | `Null -> fail "missing field ts"
      | _ -> ());
      match json_member "subkind" json with
      | `Null -> fail "missing field subkind"
      | _ -> ())
  | lines ->
      fail
        (Printf.sprintf "expected one events.jsonl line, got %d"
           (List.length lines))

let test_activity_log_free_form () =
  let log = ref Activity_log.empty in
  let update f = log := f !log in
  let patch_id = Types.Patch_id.of_string "patch-5" in
  Onton.Telemetry_dispatch.with_sink
    ~sink:
      (Onton.Activity_log_sink.sink
         ~main_branch:(Types.Branch.of_string "main")
         ~update ())
    (fun () ->
      Onton.Telemetry_dispatch.emit
        (Telemetry.Event.Free_form
           {
             patch_id = Some patch_id;
             level = Telemetry.Event.Info;
             message = "hello";
           }));
  match Activity_log.recent_events !log ~limit:1 with
  | [ event ] ->
      expect_equal_string ~name:"message" "hello"
        event.Activity_log.Event.message
  | events ->
      fail
        (Printf.sprintf "expected one activity event, got %d"
           (List.length events))

let test_activity_log_stream_drops_untagged () =
  (* Raw child-process stdout/stderr lines (codex envelopes, "Tool Bash …", …)
     are emitted by Llm_backend for replay diagnostics. They must not reach the
     user-facing Activity pane. *)
  let log = ref Activity_log.empty in
  let update f = log := f !log in
  let patch_id = Types.Patch_id.of_string "patch-5" in
  Onton.Telemetry_dispatch.with_sink
    ~sink:
      (Onton.Activity_log_sink.sink
         ~main_branch:(Types.Branch.of_string "main")
         ~update ())
    (fun () ->
      Onton.Telemetry_dispatch.emit
        (Telemetry.Event.Stream
           {
             patch_id;
             session_uuid = Some "session-uuid";
             channel = `Stdout;
             raw = {|{"type":"turn.completed"}|};
           });
      Onton.Telemetry_dispatch.emit
        (Telemetry.Event.Stream
           {
             patch_id;
             session_uuid = Some "session-uuid";
             channel = `Stderr;
             raw = "Tool Bash — /bin/zsh -lc 'ls'";
           }));
  match Activity_log.recent_stream_entries !log ~limit:1 with
  | [] -> ()
  | entries ->
      fail
        (Printf.sprintf "expected zero stream entries, got %d"
           (List.length entries))

let test_activity_log_stream_accepts_tagged () =
  let log = ref Activity_log.empty in
  let update f = log := f !log in
  let patch_id = Types.Patch_id.of_string "patch-5" in
  let tagged =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("activity_log_kind", `String "finished");
           ("reason", `String "ended turn");
         ])
  in
  Onton.Telemetry_dispatch.with_sink
    ~sink:
      (Onton.Activity_log_sink.sink
         ~main_branch:(Types.Branch.of_string "main")
         ~update ())
    (fun () ->
      Onton.Telemetry_dispatch.emit
        (Telemetry.Event.Stream
           {
             patch_id;
             session_uuid = Some "session-uuid";
             channel = `Stdout;
             raw = tagged;
           }));
  match Activity_log.recent_stream_entries !log ~limit:1 with
  | [ entry ] -> (
      match entry.Activity_log.Stream_entry.kind with
      | Activity_log.Stream_entry.Finished "ended turn" -> ()
      | Activity_log.Stream_entry.Finished _
      | Activity_log.Stream_entry.Text_chunk _
      | Activity_log.Stream_entry.Tool_use _
      | Activity_log.Stream_entry.Stream_error _ ->
          fail
            (Printf.sprintf "unexpected stream entry %s"
               (Activity_log.Stream_entry.show entry)))
  | entries ->
      fail
        (Printf.sprintf "expected one stream entry, got %d"
           (List.length entries))

let agent_json ?(busy = false) patch_id =
  `Assoc
    [
      ("patch_id", Types.Patch_id.yojson_of_t patch_id);
      ("pr_status", `Assoc [ ("kind", `String "absent") ]);
      ("pr_number", `Null);
      ("busy", `Bool busy);
      ("merged", `Bool false);
      ("satisfies", `Bool false);
      ("base_branch", `Null);
      ("queue", `List []);
      ("current_op", `Null);
      ("session_fallback", `String "Fresh_available");
      ("ci_failure_count", `Int 0);
      ("start_attempts_without_pr", `Int 0);
      ("conflict_noop_count", `Int 0);
      ("no_commits_push_count", `Int 0);
      ("context_exhaustion_count", `Int 0);
      (* Retired counters in archived event payloads cannot recreate an
         independent publication intervention in the status projection. *)
      ("push_failure_count", `Int 999);
      ("rebase_failure_count", `Int 0);
      ("pr_body_artifact_miss_count", `Int 0);
    ]

let test_activity_log_records_status_transition () =
  let log = ref Activity_log.empty in
  let update f = log := f !log in
  let patch_id = Types.Patch_id.of_string "patch-5" in
  Onton.Telemetry_dispatch.with_sink
    ~sink:
      (Onton.Activity_log_sink.sink
         ~main_branch:(Types.Branch.of_string "main")
         ~update ())
    (fun () ->
      Onton.Telemetry_dispatch.emit
        (Telemetry.Event.Action
           {
             patch_id;
             session_uuid = None;
             payload =
               `Assoc
                 [
                   ("action", `String "Start");
                   ("agent_before", agent_json patch_id);
                   ("agent_after", agent_json ~busy:true patch_id);
                 ];
           }));
  match Activity_log.recent_transitions !log ~limit:1 with
  | [ transition ] ->
      if
        not
          (Display_status.equal
             transition.Activity_log.Transition_entry.from_status
             Display_status.Pending)
      then fail "expected transition from pending";
      if
        not
          (Display_status.equal
             transition.Activity_log.Transition_entry.to_status
             Display_status.Starting)
      then fail "expected transition to starting";
      expect_equal_string ~name:"action" "Start"
        transition.Activity_log.Transition_entry.action
  | transitions ->
      fail
        (Printf.sprintf "expected one transition, got %d"
           (List.length transitions))

let test_activity_log_projects_owner_intervention () =
  let module B = Branch_reconcile in
  let patch_id = Types.Patch_id.of_string "owner-status" in
  let requested, _ =
    B.step B.empty
      (B.Request
         B.
           {
             base = "main";
             policy = Rewrite;
             purpose = Provision_checkout "status";
           })
  in
  let token =
    match B.pending requested with
    | Some command -> command.B.token [@warning "-42"]
    | None -> fail "missing provisioning command"
  in
  let stopped, _ =
    B.step requested
      (B.Result { token; at = 100.; result = B.Permanent "permission_denied" })
  in
  let resumed, _ = B.step stopped B.Resume in
  let json owner =
    match agent_json patch_id with
    | `Assoc fields ->
        `Assoc (("branch_reconcile", B.yojson_of_t owner) :: fields)
    | _ -> fail "invalid agent fixture"
  in
  let log = ref Activity_log.empty in
  Onton.Telemetry_dispatch.with_sink
    ~sink:
      (Onton.Activity_log_sink.sink
         ~main_branch:(Types.Branch.of_string "main")
         ~update:(fun f -> log := f !log)
         ())
    (fun () ->
      List.iter
        [ (requested, stopped); (stopped, resumed) ]
        ~f:(fun (before, after) ->
          Onton.Telemetry_dispatch.emit
            (Telemetry.Event.Action
               {
                 patch_id;
                 session_uuid = None;
                 payload =
                   `Assoc
                     [
                       ("action", `String "Reconcile_branch");
                       ("agent_before", json before);
                       ("agent_after", json after);
                     ];
               })));
  let transitions = Activity_log.recent_transitions !log ~limit:10 in
  if List.length transitions <> 2 then
    fail "owner intervention/resume lost status transition";
  let observed from_status to_status =
    List.exists transitions ~f:(fun transition ->
        Display_status.equal
          transition.Activity_log.Transition_entry.from_status from_status
        && Display_status.equal
             transition.Activity_log.Transition_entry.to_status to_status)
  in
  if
    not
      (observed Display_status.Pending Display_status.Needs_help
      && observed Display_status.Needs_help Display_status.Pending)
  then fail "activity log disagrees with owner intervention/resume"

let test_pre_migration_events_jsonl_loads () =
  let line =
    {|{"ts":"2026-05-19T00:00:00Z","kind":"complete","patch_id":"patch-5","result":"Session_succeeded","agent_before":{},"agent_after":{}}|}
  in
  let path = temp_path "onton-old-event-log" in
  let oc = Stdlib.open_out path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out_noerr oc)
    (fun () ->
      Stdlib.output_string oc line;
      Stdlib.output_char oc '\n');
  let event_log = Onton.Event_log.create ~path in
  let patch_id = Types.Patch_id.of_string "patch-5" in
  Onton.Telemetry_dispatch.with_sink ~sink:(Onton.Event_log.sink event_log)
    (fun () ->
      Onton.Telemetry_dispatch.emit
        (Telemetry.Event.Complete
           {
             patch_id;
             session_uuid = None;
             subkind = Failure_subkind.Ok;
             payload =
               `Assoc
                 [
                   ("result", `String "Session_ok");
                   ("agent_before", `Assoc []);
                   ("agent_after", `Assoc []);
                 ];
           }));
  match read_lines path with
  | [ loaded; _new_line ] ->
      if not (String.equal loaded line) then
        fail
          (Printf.sprintf "legacy line changed: expected %S, got %S" line loaded);
      let json = Yojson.Safe.from_string loaded in
      expect_equal_string ~name:"old kind" "complete"
        (string_member "kind" json);
      expect_equal_string ~name:"old result" "Session_succeeded"
        (string_member "result" json)
  | lines ->
      fail
        (Printf.sprintf "expected legacy plus appended line, got %d lines"
           (List.length lines))

let[@warning "-42"] test_repair_session_identity () =
  let module B = Branch_reconcile in
  let open B in
  let source = Option.value_exn (Commit.make (String.make 40 'a')) in
  let target = Option.value_exn (Commit.make (String.make 40 'b')) in
  let state, _ =
    step empty
      (Request { base = "main"; policy = Rewrite; purpose = Reconcile_base })
  in
  let reply state result =
    let command = Option.value_exn (pending state) in
    step state (Result { token = command.token; at = 1.; result })
  in
  let state, _ =
    reply state
      (Observed
         {
           destination = Remote_id.of_destination "telemetry-origin";
           head = source;
           source;
           target;
           remote = Some source;
           boundary = Recorded source;
           topology = Equal;
           clean = true;
           sequencer = None;
           conflicts = 0;
           target_included = false;
           base_contains_source = false;
           completed_integration = false;
         })
  in
  let state, _ = reply state Pinned in
  let state, effects =
    reply state (Conflict { head = source; sequencer = "step"; conflicts = 1 })
  in
  let token =
    List.find_map_exn effects ~f:(function
      | Repair token -> Some token
      | Execute _ | Start_repair _ | Completed _ -> None)
  in
  let state, _ = step state (Repair_started token) in
  let state = Result.ok_or_failwith (decode (yojson_of_t state)) in
  let turn = Option.value_exn (repair_turn state ~branch:"patch" token) in
  Eio_main.run (fun env ->
      List.iter [ false; true ] ~f:(fun raises ->
          let path = temp_path "onton-repair-identity" in
          Stdlib.Fun.protect
            ~finally:(fun () -> Stdlib.Sys.remove path)
            (fun () ->
              let session = ref None in
              let backend =
                Onton.Llm_backend.
                  {
                    name = "fixture";
                    run_streaming =
                      (fun ~project_name:_
                        ~cwd:_
                        ~patch_id:_
                        ~prompt:_
                        ~resume_session:_
                        ~session_uuid
                        ~complexity:_
                        ~on_event:_
                      ->
                        session := Some session_uuid;
                        let entries =
                          List.map (read_lines path) ~f:(fun line ->
                              Yojson.Safe.from_string line)
                        in
                        if List.length entries <> 1 then
                          fail
                            "repair identity must be logged before backend \
                             dispatch";
                        if raises then fail "backend outage";
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
              Onton.Telemetry_dispatch.with_sink
                ~sink:(Onton.Event_log.sink (Onton.Event_log.create ~path))
                (fun () ->
                  ignore
                    (Onton.Branch_repair_session.run ~backend
                       ~cwd:(Eio.Stdenv.fs env) ~project_name:"identity"
                       ~patch_id:(Types.Patch_id.of_string "1")
                       ~complexity:None ~turn
                       ~read_head:(fun () -> Some (Commit.to_string source))
                       ~now:(fun () -> 2.)));
              let entry =
                Yojson.Safe.from_string (List.hd_exn (read_lines path))
              in
              expect_equal_string ~name:"repair event"
                "branch_repair_session_started"
                (string_member "kind" entry);
              expect_equal_string ~name:"backend session correlation"
                (Option.value_exn !session)
                (string_member "onton_session_uuid" entry);
              expect_equal_json ~name:"repair operation" (`Int token.operation)
                (json_member "operation_id" entry);
              expect_equal_json ~name:"repair command" (`Int token.command)
                (json_member "command_id" entry))))

let () =
  test_repair_session_identity ();
  test_event_log_complete ();
  test_activity_log_free_form ();
  test_activity_log_stream_drops_untagged ();
  test_activity_log_stream_accepts_tagged ();
  test_activity_log_records_status_transition ();
  test_activity_log_projects_owner_intervention ();
  test_pre_migration_events_jsonl_loads ()

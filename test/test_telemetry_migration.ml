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
    ~sink:(Onton.Activity_log_sink.sink ~update ()) (fun () ->
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

let test_activity_log_stream () =
  let log = ref Activity_log.empty in
  let update f = log := f !log in
  let patch_id = Types.Patch_id.of_string "patch-5" in
  Onton.Telemetry_dispatch.with_sink
    ~sink:(Onton.Activity_log_sink.sink ~update ()) (fun () ->
      Onton.Telemetry_dispatch.emit
        (Telemetry.Event.Stream
           {
             patch_id;
             session_uuid = Some "session-uuid";
             channel = `Stdout;
             raw = "chunk";
           }));
  match Activity_log.recent_stream_entries !log ~limit:1 with
  | [ entry ] -> (
      match entry.Activity_log.Stream_entry.kind with
      | Activity_log.Stream_entry.Text_chunk "chunk" -> ()
      | Activity_log.Stream_entry.Text_chunk _
      | Activity_log.Stream_entry.Tool_use _
      | Activity_log.Stream_entry.Finished _
      | Activity_log.Stream_entry.Stream_error _ ->
          fail
            (Printf.sprintf "unexpected stream entry %s"
               (Activity_log.Stream_entry.show entry)))
  | entries ->
      fail
        (Printf.sprintf "expected one stream entry, got %d"
           (List.length entries))

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

let () =
  test_event_log_complete ();
  test_activity_log_free_form ();
  test_activity_log_stream ();
  test_pre_migration_events_jsonl_loads ()

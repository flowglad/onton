(* @archlint.module test
   @archlint.domain failure-subkind *)

open Base
open Onton
open Onton_core

external unsetenv : string -> unit = "caml_onton_unsetenv"

let failures = ref 0

let fail name msg =
  Stdio.printf "FAIL: %s: %s\n" name msg;
  Int.incr failures

let expect name condition msg = if not condition then fail name msg

let read_file path =
  let ic = Stdlib.open_in_bin path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_in_noerr ic)
    (fun () ->
      let len = Stdlib.in_channel_length ic in
      Stdlib.really_input_string ic len)

let file_exists = Stdlib.Sys.file_exists

let temp_root () =
  let dir =
    Stdlib.Filename.concat
      (Stdlib.Filename.get_temp_dir_name ())
      (Printf.sprintf "onton-session-telemetry-%d-%06d" (Unix.getpid ())
         (Random.int 1_000_000))
  in
  Unix.mkdir dir 0o755;
  dir

let json_member name json =
  match json with
  | `Assoc fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None

let subkind_of_meta json =
  Option.bind (json_member "subkind" json) ~f:(fun value ->
      match Failure_subkind.t_of_yojson value with
      | subkind -> Some subkind
      | exception _ -> None)

let complete_entries path =
  read_file path |> String.split_lines
  |> List.filter_map ~f:(fun line ->
      match Yojson.Safe.from_string line with
      | json -> (
          match json_member "kind" json with
          | Some (`String "complete") -> Some json
          | Some _ | None -> None)
      | exception _ -> None)

let with_data_dir root f =
  let previous = Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" in
  Unix.putenv "ONTON_DATA_DIR" root;
  Exn.protect ~f ~finally:(fun () ->
      match previous with
      | Some value -> Unix.putenv "ONTON_DATA_DIR" value
      | None -> unsetenv "ONTON_DATA_DIR")

let run_case ~name ~subkind ~exit_code =
  let root = temp_root () in
  with_data_dir root @@ fun () ->
  let project_name = "Telemetry Test " ^ name in
  let patch_id = Types.Patch_id.of_string ("patch-" ^ name) in
  let session_uuid = "uuid-" ^ name in
  Project_store.ensure_dir (Project_store.project_dir project_name);
  let event_log =
    Event_log.create ~path:(Project_store.event_log_path project_name)
  in
  let artifact_sink =
    Session_artifacts.create ~project_name ~patch_id ~session_uuid
  in
  let meta =
    Session_meta.create ~onton_session_uuid:session_uuid
      ~claude_session_id:"claude-session"
      ~patch_id:(Types.Patch_id.to_string patch_id)
      ~started_at:1.0 ~ended_at:2.0 ~exit_code ~subkind ()
  in
  Telemetry_dispatch.with_sinks
    ~sinks:[ Event_log.sink event_log ]
    (fun () ->
      Telemetry_dispatch.register_sink artifact_sink;
      Telemetry_dispatch.emit
        (Telemetry.Event.Spawn_started
           {
             patch_id;
             session_uuid;
             prompt = "prompt";
             argv = [ "claude"; "--output-format"; "stream-json" ];
             env_redacted = [| "ANTHROPIC_API_KEY=<redacted>" |];
           });
      Telemetry_dispatch.emit
        (Telemetry.Event.Stream
           {
             patch_id;
             session_uuid = Some session_uuid;
             channel = `Stdout;
             raw = {|{"type":"result"}|};
           });
      Telemetry_dispatch.emit
        (Telemetry.Event.Stream
           {
             patch_id;
             session_uuid = Some session_uuid;
             channel = `Stderr;
             raw = "stderr before unregister";
           });
      Telemetry_dispatch.emit
        (Telemetry.Event.Spawn_finalized
           { patch_id; session_uuid; meta = Session_meta.yojson_of_t meta });
      Telemetry_dispatch.unregister_sink
        ~name:(Session_artifacts.sink_name ~session_uuid);
      Telemetry_dispatch.emit
        (Telemetry.Event.Stream
           {
             patch_id;
             session_uuid = Some session_uuid;
             channel = `Stderr;
             raw = "stderr after unregister";
           });
      Telemetry_dispatch.emit
        (Telemetry.Event.Complete
           {
             patch_id;
             session_uuid = Some session_uuid;
             subkind;
             payload = `Assoc [ ("result", `String name) ];
           }));
  let dir = Session_artifacts.artifact_dir ~project_name ~session_uuid in
  let meta_path = Session_artifacts.meta_path ~project_name ~session_uuid in
  List.iter
    [
      "prompt.txt";
      "argv.txt";
      "env.txt";
      "stdout.jsonl";
      "stderr.log";
      "meta.json";
    ] ~f:(fun filename ->
      expect name
        (file_exists (Stdlib.Filename.concat dir filename))
        (filename ^ " missing"));
  let meta_json = Yojson.Safe.from_file meta_path in
  expect name
    (match json_member "schema_version" meta_json with
    | Some (`Int 1) -> true
    | _ -> false)
    "meta schema_version was not 1";
  expect name
    (Option.equal Failure_subkind.equal
       (subkind_of_meta meta_json)
       (Some subkind))
    "meta subkind mismatch";
  expect name
    (match json_member "exit_code" meta_json with
    | Some (`Int actual) -> actual = exit_code
    | _ -> false)
    "meta exit_code mismatch";
  let stderr = read_file (Stdlib.Filename.concat dir "stderr.log") in
  expect name
    (String.is_substring stderr ~substring:"stderr before unregister")
    "registered stderr stream was not written";
  expect name
    (not (String.is_substring stderr ~substring:"stderr after unregister"))
    "unregistered artifact sink still consumed stream";
  let completes =
    complete_entries (Project_store.event_log_path project_name)
  in
  let complete = List.hd completes in
  expect name (Option.is_some complete) "complete entry missing";
  Option.iter complete ~f:(fun json ->
      expect name
        (match json_member "onton_session_uuid" json with
        | Some (`String uuid) -> String.equal uuid session_uuid
        | _ -> false)
        "complete uuid mismatch";
      expect name
        (match json_member "subkind" json with
        | Some value ->
            Failure_subkind.equal (Failure_subkind.t_of_yojson value) subkind
        | None -> false)
        "complete subkind mismatch")

let () =
  Random.self_init ();
  run_case ~name:"success" ~subkind:Failure_subkind.Ok ~exit_code:0;
  run_case ~name:"auth" ~subkind:Failure_subkind.Auth_unavailable ~exit_code:1;
  run_case ~name:"timeout" ~subkind:Failure_subkind.Timed_out ~exit_code:124;
  run_case ~name:"process" ~subkind:Failure_subkind.Process_error ~exit_code:1;
  if !failures > 0 then Stdlib.exit 1

open Base

type append_file = {
  path : string;
  mutable channel : Stdlib.out_channel option;
}

let make_append_file path = { path; channel = None }

let ensure_open file =
  match file.channel with
  | Some channel -> channel
  | None ->
      let channel =
        Stdlib.open_out_gen
          [ Open_creat; Open_text; Open_append ]
          0o644 file.path
      in
      file.channel <- Some channel;
      channel

let append_line file line =
  let channel = ensure_open file in
  Stdlib.output_string channel line;
  Stdlib.output_char channel '\n';
  Stdlib.flush channel

let close_file file =
  match file.channel with
  | None -> ()
  | Some channel ->
      file.channel <- None;
      Stdlib.close_out_noerr channel

let write_text_file ~path ~content =
  let oc = Stdlib.open_out_bin path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () ->
      (* These diagnostic sidecar files are best-effort and written directly.
         Unlike meta.json they are not crash-safe atomic writes, so a partial
         file after process or filesystem failure is possible. *)
      Stdlib.output_string oc content;
      Stdlib.flush oc)

let join_lines lines =
  match lines with [] -> "" | _ -> String.concat ~sep:"\n" lines ^ "\n"

let event_session_uuid = function
  | Telemetry.Event.Stream { session_uuid; _ } -> session_uuid
  | Spawn_started { session_uuid; _ } | Spawn_finalized { session_uuid; _ } ->
      Some session_uuid
  | Poll _ | Action _ | Complete _ | Free_form _ -> None

let sink_name ~session_uuid = "session_artifacts:" ^ session_uuid

let artifact_dir ~project_name ~session_uuid =
  Stdlib.Filename.concat (Project_store.sessions_dir project_name) session_uuid

let meta_path ~project_name ~session_uuid =
  Stdlib.Filename.concat (artifact_dir ~project_name ~session_uuid) "meta.json"

let create ~project_name ~patch_id:_ ~session_uuid =
  let artifact_dir = artifact_dir ~project_name ~session_uuid in
  Project_store.ensure_dir artifact_dir;
  let stdout_file =
    make_append_file (Stdlib.Filename.concat artifact_dir "stdout.jsonl")
  in
  let stderr_file =
    make_append_file (Stdlib.Filename.concat artifact_dir "stderr.log")
  in
  let meta_path = meta_path ~project_name ~session_uuid in
  let write_started ~prompt ~argv ~env_redacted =
    write_text_file
      ~path:(Stdlib.Filename.concat artifact_dir "prompt.txt")
      ~content:prompt;
    write_text_file
      ~path:(Stdlib.Filename.concat artifact_dir "argv.txt")
      ~content:(join_lines argv);
    write_text_file
      ~path:(Stdlib.Filename.concat artifact_dir "env.txt")
      ~content:(env_redacted |> Array.to_list |> join_lines)
  in
  let write_finalized ~meta =
    (match
       Persistence.write_file_atomically ~path:meta_path
         ~content:(Yojson.Safe.pretty_to_string meta)
     with
    | Ok () -> ()
    | Error msg ->
        Stdlib.prerr_endline
          ("session_artifacts: failed to write " ^ meta_path ^ ": " ^ msg));
    close_file stdout_file;
    close_file stderr_file
  in
  {
    Telemetry.Sink.name = sink_name ~session_uuid;
    interested_in =
      (fun event ->
        Option.value_map (event_session_uuid event) ~default:false
          ~f:(String.equal session_uuid));
    consume =
      (function
      | Telemetry.Event.Spawn_started
          { session_uuid = event_uuid; prompt; argv; env_redacted; _ }
        when String.equal event_uuid session_uuid ->
          write_started ~prompt ~argv ~env_redacted
      | Telemetry.Event.Stream
          { session_uuid = Some event_uuid; channel; raw; _ }
        when String.equal event_uuid session_uuid ->
          append_line
            (match channel with
            | `Stdout -> stdout_file
            | `Stderr -> stderr_file)
            raw
      | Telemetry.Event.Spawn_finalized { session_uuid = event_uuid; meta; _ }
        when String.equal event_uuid session_uuid ->
          write_finalized ~meta
      | Telemetry.Event.Spawn_started _ -> ()
      | Telemetry.Event.Stream _ -> ()
      | Telemetry.Event.Spawn_finalized _ -> ()
      | Telemetry.Event.Poll _ -> ()
      | Telemetry.Event.Action _ -> ()
      | Telemetry.Event.Complete _ -> ()
      | Telemetry.Event.Free_form _ -> ());
  }

let rm_rf path =
  try
    ignore
      (Stdlib.Sys.command
         (Printf.sprintf "rm -rf %s" (Stdlib.Filename.quote path)))
  with _ -> ()

let default_data_root () =
  match Stdlib.Sys.getenv_opt "XDG_DATA_HOME" with
  | Some xdg -> Stdlib.Filename.concat xdg "onton"
  | None ->
      Stdlib.Filename.concat
        (Stdlib.Filename.concat (Stdlib.Sys.getenv "HOME") ".local/share")
        "onton"

let with_temp_data_dir f =
  let old = Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" in
  let restore_path_when_unset =
    match old with Some _ -> None | None -> Some (default_data_root ())
  in
  let dir = Stdlib.Filename.temp_dir "onton-session-artifacts-" "" in
  Unix.putenv "ONTON_DATA_DIR" dir;
  Stdlib.Fun.protect
    ~finally:(fun () ->
      (match old with
      | Some value -> Unix.putenv "ONTON_DATA_DIR" value
      | None -> (
          match restore_path_when_unset with
          | None -> ()
          | Some path ->
              (* Tests do not have an unsetenv binding. Restore the resolved
                 default data root so later tests never inherit a deleted temp
                 directory through ONTON_DATA_DIR. *)
              Project_store.ensure_dir path;
              Unix.putenv "ONTON_DATA_DIR" path));
      rm_rf dir)
    (fun () -> f ())

let read_file path =
  let ic = Stdlib.open_in_bin path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_in_noerr ic)
    (fun () -> Stdlib.In_channel.input_all ic)

let spawn_started_event ~patch_id ~session_uuid ~env_redacted =
  Telemetry.Event.Spawn_started
    {
      patch_id;
      session_uuid;
      prompt = "prompt text";
      argv = [ "claude"; "--json" ];
      env_redacted;
    }

let spawn_finalized_event ~patch_id ~session_uuid =
  let meta =
    Session_meta.create ~onton_session_uuid:session_uuid
      ~claude_session_id:"claude-session"
      ~patch_id:(Types.Patch_id.to_string patch_id)
      ~started_at:1.0 ~ended_at:2.0 ~exit_code:0 ~subkind:Failure_subkind.Ok ()
  in
  Telemetry.Event.Spawn_finalized
    { patch_id; session_uuid; meta = Session_meta.yojson_of_t meta }

let%test "create+finalize writes meta.json with schema_version=1" =
  with_temp_data_dir @@ fun () ->
  let project_name = "Session Artifact Project" in
  let patch_id = Types.Patch_id.of_string "patch-4" in
  let session_uuid = "session-1" in
  let sink = create ~project_name ~patch_id ~session_uuid in
  sink.consume
    (spawn_started_event ~patch_id ~session_uuid ~env_redacted:[| "PATH=/bin" |]);
  sink.consume (spawn_finalized_event ~patch_id ~session_uuid);
  let meta_path =
    Stdlib.Filename.concat
      (Stdlib.Filename.concat
         (Project_store.sessions_dir project_name)
         session_uuid)
      "meta.json"
  in
  Stdlib.Sys.file_exists meta_path
  &&
  match
    Session_meta.t_of_yojson (Yojson.Safe.from_string (read_file meta_path))
  with
  | meta -> meta.Session_meta.schema_version = 1
  | exception _ -> false

let%test "Stream events append lines in order to stdout.jsonl" =
  with_temp_data_dir @@ fun () ->
  let project_name = "Session Artifact Streams" in
  let patch_id = Types.Patch_id.of_string "patch-4" in
  let session_uuid = "session-2" in
  let sink = create ~project_name ~patch_id ~session_uuid in
  sink.consume
    (Telemetry.Event.Stream
       {
         patch_id;
         session_uuid = Some session_uuid;
         channel = `Stdout;
         raw = "{\"a\":1}";
       });
  sink.consume
    (Telemetry.Event.Stream
       {
         patch_id;
         session_uuid = Some session_uuid;
         channel = `Stdout;
         raw = "{\"b\":2}";
       });
  sink.consume (spawn_finalized_event ~patch_id ~session_uuid);
  let stdout_path =
    Stdlib.Filename.concat
      (Stdlib.Filename.concat
         (Project_store.sessions_dir project_name)
         session_uuid)
      "stdout.jsonl"
  in
  String.equal (read_file stdout_path) "{\"a\":1}\n{\"b\":2}\n"

let%test "interested_in returns false for events with different session_uuid" =
  with_temp_data_dir @@ fun () ->
  let patch_id = Types.Patch_id.of_string "patch-4" in
  let sink =
    create ~project_name:"Project" ~patch_id ~session_uuid:"session-3"
  in
  (not
     (sink.interested_in
        (Telemetry.Event.Stream
           {
             patch_id;
             session_uuid = Some "other";
             channel = `Stdout;
             raw = "x";
           })))
  && (not
        (sink.interested_in
           (spawn_started_event ~patch_id ~session_uuid:"other"
              ~env_redacted:[| "PATH=/bin" |])))
  && not
       (sink.interested_in
          (spawn_finalized_event ~patch_id ~session_uuid:"other"))

let%test "env writer preserves pre-redacted entries" =
  with_temp_data_dir @@ fun () ->
  let project_name = "Session Artifact Env" in
  let patch_id = Types.Patch_id.of_string "patch-4" in
  let session_uuid = "session-4" in
  let sink = create ~project_name ~patch_id ~session_uuid in
  sink.consume
    (spawn_started_event ~patch_id ~session_uuid
       ~env_redacted:
         [|
           "ANTHROPIC_API_KEY=<REDACTED>";
           "SAFE_NAME=<REDACTED>";
           "NORMAL=value";
         |]);
  sink.consume (spawn_finalized_event ~patch_id ~session_uuid);
  let env_path =
    Stdlib.Filename.concat
      (Stdlib.Filename.concat
         (Project_store.sessions_dir project_name)
         session_uuid)
      "env.txt"
  in
  let env_text = read_file env_path in
  String.is_substring env_text ~substring:"ANTHROPIC_API_KEY=<REDACTED>"
  && String.is_substring env_text ~substring:"SAFE_NAME=<REDACTED>"
  && String.is_substring env_text ~substring:"NORMAL=value"

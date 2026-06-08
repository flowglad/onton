(* @archlint.module test
   @archlint.domain failure-subkind *)

open Base
open Onton
open Onton_core

let failures = ref 0

let fail name msg =
  Stdio.printf "FAIL: %s: %s\n" name msg;
  Int.incr failures

let expect name condition msg = if not condition then fail name msg

let temp_root () =
  let dir =
    Stdlib.Filename.concat
      (Stdlib.Filename.get_temp_dir_name ())
      (Printf.sprintf "onton-debug-upload-%d-%06d" (Unix.getpid ())
         (Random.int 1_000_000))
  in
  Unix.mkdir dir 0o755;
  dir

let write_file path contents =
  Project_store.ensure_dir (Stdlib.Filename.dirname path);
  let oc = Stdlib.open_out_bin path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out_noerr oc)
    (fun () -> Stdlib.output_string oc contents)

let json_member name = function
  | `Assoc fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None

let assoc_string_map = function
  | `Assoc fields ->
      List.filter_map fields ~f:(fun (name, value) ->
          match value with `String s -> Some (name, s) | _ -> None)
  | _ -> []

let bundle_files bundle_json =
  json_member "files" bundle_json
  |> Option.value ~default:(`Assoc [])
  |> assoc_string_map

let file_contents files path = List.Assoc.find files ~equal:String.equal path

let meta_json ~patch_id ~session_uuid ~subkind ~ended_at =
  `Assoc
    [
      ("schema_version", `Int 1);
      ("onton_session_uuid", `String session_uuid);
      ("patch_id", `String (Types.Patch_id.to_string patch_id));
      ("started_at", `Float 0.);
      ("ended_at", `String ended_at);
      ("exit_code", `Int 0);
      ("subkind", Failure_subkind.yojson_of_t subkind);
    ]
  |> Yojson.Safe.to_string ~std:true

let subkind_from_manifest manifest patch_id =
  let latest =
    json_member "latest_subkinds_by_patch" manifest
    |> Option.value ~default:`Null
  in
  match json_member patch_id latest with
  | Some value -> Failure_subkind.t_of_yojson value
  | None -> Failure_subkind.Other "missing"

let artifact_paths manifest =
  match json_member "artifact_paths" manifest with
  | Some (`List items) ->
      List.filter_map items ~f:(function `String s -> Some s | _ -> None)
  | _ -> []

let setup_project () =
  let root = temp_root () in
  Unix.putenv "ONTON_DATA_DIR" root;
  let project_name = "Debug Upload Test" in
  let project_dir = Project_store.project_dir project_name in
  Project_store.ensure_dir project_dir;
  write_file
    (Project_store.config_path project_name)
    {|{"project_name":"Debug Upload Test","github_token":"ghp_configsecret"}|};
  write_file
    (Project_store.snapshot_path project_name)
    "snapshot token xoxp-snapshot-secret";
  write_file
    (Project_store.event_log_path project_name)
    "event token github_pat_event_secret";
  write_file (Project_store.gameplan_path project_name) "gameplan";
  let sessions_dir = Project_store.sessions_dir project_name in
  write_file
    (Stdlib.Filename.concat
       (Stdlib.Filename.concat sessions_dir "uuid-new")
       "env.txt")
    "API_KEY=key-value\n\
     SESSION_TOKEN=token-value\n\
     APP_SECRET=secret-value\n\
     PATH=/bin";
  write_file
    (Stdlib.Filename.concat
       (Stdlib.Filename.concat sessions_dir "uuid-new")
       "stdout.jsonl")
    (String.concat ~sep:"\n"
       [
         "ghp_abc123";
         "gho_abc123";
         "ghu_abc123";
         "ghs_abc123";
         "github_pat_abc123";
         "glpat-abc123";
         "xoxb-abc123";
         "xapp-abc123";
         "sk-ant-abc123";
       ]);
  write_file
    (Stdlib.Filename.concat
       (Stdlib.Filename.concat
          (Stdlib.Filename.concat sessions_dir "uuid-new")
          "nested")
       "raw.log")
    "nested xoxp-nested-secret";
  write_file
    (Stdlib.Filename.concat
       (Stdlib.Filename.concat sessions_dir "uuid-old")
       "meta.json")
    (meta_json
       ~patch_id:(Types.Patch_id.of_string "patch-a")
       ~session_uuid:"uuid-old" ~subkind:Failure_subkind.Auth_unavailable
       ~ended_at:"2026-05-19T10:00:00Z");
  write_file
    (Stdlib.Filename.concat
       (Stdlib.Filename.concat sessions_dir "uuid-new")
       "meta.json")
    (meta_json
       ~patch_id:(Types.Patch_id.of_string "patch-a")
       ~session_uuid:"uuid-new" ~subkind:Failure_subkind.Ok
       ~ended_at:"2026-05-19T11:00:00Z");
  write_file
    (Stdlib.Filename.concat
       (Stdlib.Filename.concat sessions_dir "uuid-auth")
       "meta.json")
    (meta_json
       ~patch_id:(Types.Patch_id.of_string "patch-b")
       ~session_uuid:"uuid-auth" ~subkind:Failure_subkind.Auth_unavailable
       ~ended_at:"2026-05-19T09:00:00Z");
  project_name

let () =
  Random.self_init ();
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"telemetry route selects interested sinks"
       ~count:100 QCheck2.Gen.bool (fun interested ->
         let sink =
           {
             Telemetry.Sink.name = "debug-upload-route-test";
             interested_in = (fun _ -> interested);
             consume = (fun _ -> ());
           }
         in
         let routed =
           Telemetry.route ~sinks:[ sink ]
             (Telemetry.Event.Free_form
                {
                  patch_id = None;
                  level = Telemetry.Event.Info;
                  message = "debug";
                })
         in
         Bool.equal (List.length routed = 1) interested));
  (* [yojson_of_t] then [t_of_yojson] round-trips every subkind, including the
     payload-carrying [Api_error] and [Other] constructors. [to_string] must be
     total. *)
  let gen_subkind : Failure_subkind.t QCheck2.Gen.t =
    QCheck2.Gen.oneof
      [
        QCheck2.Gen.return Failure_subkind.Ok;
        QCheck2.Gen.return Failure_subkind.Auth_unavailable;
        QCheck2.Gen.return Failure_subkind.Network_error;
        QCheck2.Gen.return Failure_subkind.Timed_out;
        QCheck2.Gen.return Failure_subkind.Context_exhausted;
        QCheck2.Gen.return Failure_subkind.No_session_to_resume;
        QCheck2.Gen.return Failure_subkind.Empty_response;
        QCheck2.Gen.return Failure_subkind.Process_error;
        QCheck2.Gen.map
          (fun status -> Failure_subkind.Api_error { status })
          (QCheck2.Gen.option (QCheck2.Gen.int_range 100 599));
        QCheck2.Gen.map
          (fun s -> Failure_subkind.Other s)
          (QCheck2.Gen.string_size (QCheck2.Gen.int_range 0 8));
      ]
  in
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"Failure_subkind t yojson round-trip" ~count:500
       gen_subkind (fun subkind ->
         let round_trip =
           Failure_subkind.t_of_yojson (Failure_subkind.yojson_of_t subkind)
         in
         Failure_subkind.equal round_trip subkind
         && String.length (Failure_subkind.to_string subkind) >= 0));
  (* [yojson_of_init_info] then [init_info_of_yojson] round-trips init_info. *)
  let gen_opt_string =
    QCheck2.Gen.option (QCheck2.Gen.string_size (QCheck2.Gen.int_range 0 8))
  in
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"Failure_subkind init_info yojson round-trip"
       ~count:300
       QCheck2.Gen.(triple gen_opt_string gen_opt_string gen_opt_string)
       (fun (api_key_source, model, claude_code_version) ->
         let init =
           { Failure_subkind.api_key_source; model; claude_code_version }
         in
         let round_trip =
           Failure_subkind.init_info_of_yojson
             (Failure_subkind.yojson_of_init_info init)
         in
         Failure_subkind.equal_init_info round_trip init));
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"failure subkind JSON surface is linked"
       QCheck2.Gen.unit (fun () ->
         ignore Failure_subkind.init_info_of_yojson;
         ignore Failure_subkind.t_of_yojson;
         ignore Failure_subkind.to_string;
         ignore Failure_subkind.yojson_of_init_info;
         ignore Failure_subkind.yojson_of_t;
         true));
  let project_name = setup_project () in
  let bundle =
    Debug_upload.build_bundle ~project_name ~version:"1.2.3-test"
    |> Yojson.Safe.from_string
  in
  let files = bundle_files bundle in
  let manifest =
    match file_contents files "manifest.json" with
    | Some raw -> Yojson.Safe.from_string raw
    | None -> `Null
  in
  expect "bundle embeds manifest.json at root"
    (Option.is_some (file_contents files "manifest.json"))
    "manifest.json missing";
  List.iter
    [
      "sessions/uuid-new/env.txt";
      "sessions/uuid-new/stdout.jsonl";
      "sessions/uuid-new/nested/raw.log";
      "sessions/uuid-new/meta.json";
      "sessions/uuid-old/meta.json";
      "sessions/uuid-auth/meta.json";
    ] ~f:(fun path ->
      expect "bundle walks sessions recursively"
        (Option.is_some (file_contents files path))
        (path ^ " missing from bundle"));
  let env_txt =
    Option.value (file_contents files "sessions/uuid-new/env.txt") ~default:""
  in
  expect "bundle redacts env values matching KEY|TOKEN|SECRET"
    (String.is_substring env_txt ~substring:"API_KEY=<REDACTED>"
    && String.is_substring env_txt ~substring:"SESSION_TOKEN=<REDACTED>"
    && String.is_substring env_txt ~substring:"APP_SECRET=<REDACTED>"
    && String.is_substring env_txt ~substring:"PATH=/bin"
    && (not (String.is_substring env_txt ~substring:"key-value"))
    && (not (String.is_substring env_txt ~substring:"token-value"))
    && not (String.is_substring env_txt ~substring:"secret-value"))
    "env values were not redacted correctly";
  let all_contents = String.concat ~sep:"\n" (List.map files ~f:snd) in
  List.iter
    [
      "ghp_abc123";
      "gho_abc123";
      "ghu_abc123";
      "ghs_abc123";
      "github_pat_abc123";
      "glpat-abc123";
      "xoxb-abc123";
      "xoxp-nested-secret";
      "xapp-abc123";
      "sk-ant-abc123";
      "ghp_configsecret";
      "github_pat_event_secret";
    ] ~f:(fun token ->
      expect "bundle redacts known token-format strings everywhere"
        (not (String.is_substring all_contents ~substring:token))
        ("token was not redacted: " ^ token));
  expect "bundle redacts known token-format strings everywhere"
    (String.is_substring all_contents ~substring:"<REDACTED>")
    "redaction marker missing";
  expect "manifest latest_subkinds_by_patch reflects newest meta.json"
    (Failure_subkind.equal
       (subkind_from_manifest manifest "patch-a")
       Failure_subkind.Ok
    && Failure_subkind.equal
         (subkind_from_manifest manifest "patch-b")
         Failure_subkind.Auth_unavailable
    &&
    let artifacts = artifact_paths manifest in
    List.mem artifacts "sessions/uuid-new/nested/raw.log" ~equal:String.equal)
    "manifest summary mismatch";
  if !failures > 0 then Stdlib.exit 1

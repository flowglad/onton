(* @archlint.module shell
   @archlint.domain pi-event-parser *)

open Base

(* Pure parser + CLI-arg builder live in [Pi_event_parser] (lib_core/). This
   file is the effectful streaming driver. *)

let build_args = Pi_event_parser.build_args
let parse_event = Pi_event_parser.parse_event
let auto_model = Pi_event_parser.auto_model

let extension_path ~executable =
  let is_file path =
    try Poly.equal (Unix.stat path).Unix.st_kind Unix.S_REG
    with Unix.Unix_error _ -> false
  in
  match Stdlib.Sys.getenv_opt "ONTON_PI_EXTENSION" with
  | Some path when not (String.is_empty path) ->
      if Stdlib.Filename.is_relative path then
        failwith "ONTON_PI_EXTENSION must be an absolute entrypoint";
      if is_file path then Unix.realpath path
      else failwith ("Pi extension not found: " ^ path)
  | _ ->
      let executable =
        Backend_preflight.find_executable executable
        |> Option.value ~default:executable
        |> Unix.realpath
      in
      let directory = Stdlib.Filename.dirname executable in
      let prefix =
        if String.equal (Stdlib.Filename.basename directory) "bin" then
          Stdlib.Filename.dirname directory
        else directory
      in
      let entrypoint =
        Stdlib.Filename.concat prefix "share/onton/pi-onton/index.ts"
      in
      let assets = Stdlib.Filename.dirname entrypoint in
      if
        List.for_all [ "index.ts"; "protocol.ts"; "files.ts" ] ~f:(fun name ->
            is_file (Stdlib.Filename.concat assets name))
      then entrypoint
      else
        failwith
          "Pi extension is missing from the Onton installation. Reinstall \
           Onton or set ONTON_PI_EXTENSION to the pi-onton entrypoint."

let run_streaming ~model ~effort ~process_mgr ~clock ~timeout ~setsid_exec
    ~project_name ~cwd ~patch_id ~prompt ~resume_session ~session_uuid
    ~complexity ~on_event =
  let model = Llm_backend.resolve_auto_model ~model ~complexity ~auto_model in
  let cwd_path = snd cwd in
  let patch_id_str = Types.Patch_id.to_string patch_id in
  let args =
    build_args ~model ~effort ~cwd_path ~patch_id:patch_id_str ~prompt
      ~resume_session
  in
  (* A per-execution envelope, rather than inferred paths or conversation state,
     keeps resumed agents attached to the current supervisor delivery. *)
  let session_dir =
    Session_artifacts.artifact_dir ~project_name ~session_uuid
  in
  Project_store.ensure_dir session_dir;
  let artifact_dir = Project_store.artifact_dir ~project_name ~patch_id in
  Project_store.ensure_dir artifact_dir;
  let context_path = Stdlib.Filename.concat session_dir "pi-context.json" in
  let context =
    `Assoc
      [
        ("version", `Int 1);
        ("project", `String project_name);
        ("patch_id", `String patch_id_str);
        ("session_uuid", `String session_uuid);
        ("prompt", `String prompt);
        ("artifacts_root", `String (Project_store.artifacts_root project_name));
        ("artifact_dir", `String artifact_dir);
        ( "gameplan_path",
          `String (Project_store.gameplan_artifact_path project_name) );
      ]
  in
  (match
     Persistence.write_file_atomically ~path:context_path
       ~content:(Yojson.Safe.to_string context)
   with
  | Ok () -> ()
  | Error message -> failwith ("Pi context: " ^ message));
  let extension_path = extension_path ~executable:Stdlib.Sys.executable_name in
  let args = args @ [ "--extension"; extension_path ] in
  let env =
    Spawn_env.merge_env ~base_env:(Unix.environment ())
      ~overrides:
        (("ONTON_PI_CONTEXT", context_path)
        :: Spawn_env.per_patch_env ~project_name ~patch_id)
  in
  let process_line line =
    let trimmed = String.strip line in
    if String.is_empty trimmed then [] else parse_event trimmed
  in
  Llm_backend.emit_spawn_started ~patch_id ~session_uuid ~prompt ~args ~env;
  Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~env
    ~setsid_exec ~args ~session_uuid:(Some session_uuid) ~patch_id ~process_line
    ~on_event

let create ~model ~effort ~process_mgr ~clock ~timeout ~setsid_exec :
    Llm_backend.t =
  {
    name = "Pi";
    run_streaming =
      (fun ~project_name
        ~cwd
        ~patch_id
        ~prompt
        ~resume_session
        ~session_uuid
        ~complexity
        ~on_event
      ->
        run_streaming ~model ~effort ~process_mgr ~clock ~timeout ~setsid_exec
          ~cwd ~project_name ~patch_id ~prompt ~resume_session ~session_uuid
          ~complexity ~on_event);
  }

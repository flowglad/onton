open Base

(* Pure parser + CLI-arg builder live in [Pi_event_parser] (lib_core/). This
   file is the effectful streaming driver. *)

let build_args = Pi_event_parser.build_args
let parse_event = Pi_event_parser.parse_event
let auto_model = Pi_event_parser.auto_model

let run_streaming ~model ~process_mgr ~clock ~timeout ~setsid_exec ~project_name
    ~cwd ~patch_id ~prompt ~resume_session ~session_uuid ~complexity ~on_event =
  let model = Llm_backend.resolve_auto_model ~model ~complexity ~auto_model in
  let cwd_path = snd cwd in
  let patch_id_str = Types.Patch_id.to_string patch_id in
  let args =
    build_args ~model ~cwd_path ~patch_id:patch_id_str ~prompt ~resume_session
  in
  let env =
    Spawn_env.merge_env ~base_env:(Unix.environment ())
      ~overrides:(Spawn_env.per_patch_env ~project_name ~patch_id)
  in
  let process_line line =
    let trimmed = String.strip line in
    if String.is_empty trimmed then [] else parse_event trimmed
  in
  Llm_backend.emit_spawn_started ~patch_id ~session_uuid ~prompt ~args ~env;
  Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~env
    ~setsid_exec ~args ~session_uuid:(Some session_uuid) ~patch_id ~process_line
    ~on_event

let create ~model ~process_mgr ~clock ~timeout ~setsid_exec : Llm_backend.t =
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
        run_streaming ~model ~process_mgr ~clock ~timeout ~setsid_exec ~cwd
          ~project_name ~patch_id ~prompt ~resume_session ~session_uuid
          ~complexity ~on_event);
  }

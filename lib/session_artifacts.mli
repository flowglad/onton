val create :
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  session_uuid:string ->
  Telemetry.Sink.t

val sink_name : session_uuid:string -> string
val artifact_dir : project_name:string -> session_uuid:string -> string
val meta_path : project_name:string -> session_uuid:string -> string

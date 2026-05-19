open Base

val create :
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  session_uuid:string ->
  Telemetry.Sink.t

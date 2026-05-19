open Base

type t = {
  schema_version : int; [@yojson.default 1]
  onton_session_uuid : string;
  claude_session_id : string option; [@yojson.option]
  patch_id : string;
  started_at : float;
  ended_at : float;
  exit_code : int;
  subkind : Failure_subkind.t;
  api_key_source : string option; [@yojson.option]
  model : string option; [@yojson.option]
  claude_code_version : string option; [@yojson.option]
}
[@@deriving yojson]

val create :
  onton_session_uuid:string ->
  ?claude_session_id:string ->
  patch_id:string ->
  started_at:float ->
  ended_at:float ->
  exit_code:int ->
  subkind:Failure_subkind.t ->
  ?api_key_source:string ->
  ?model:string ->
  ?claude_code_version:string ->
  unit ->
  t

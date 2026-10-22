(* @archlint.module interface
   @archlint.domain worker-checkpoint *)

type t
(** Versioned, patch-scoped execution state. Admission receipts are stored by
    the host beside these exact capsule bytes, never recursively inside them. *)

val decode : Yojson.Safe.t -> (t, string) result
val encode : t -> Yojson.Safe.t
val to_string : t -> string

val initial :
  run_id:string ->
  patch_id:Types.Patch_id.t ->
  workspace_id:string ->
  context:string ->
  agent:Patch_agent.t ->
  t

val validate_binding :
  t ->
  run_id:string ->
  patch_id:Types.Patch_id.t ->
  context:string ->
  state_version:int64 ->
  attention_id:string option ->
  gameplan:Types.Gameplan.t ->
  main_branch:Types.Branch.t ->
  (Patch_agent.t, string) result

val version : t -> int64
val workspace_id : t -> string
val context : t -> string
val attention_id : t -> string option
val latest_operation : t -> Worker_protocol.operation option

val terminal :
  t -> (Worker_protocol.result_status * string * string option) option

val status_name : Worker_protocol.result_status -> string

val advance :
  t ->
  version:int64 ->
  operation:Worker_protocol.operation ->
  guidance:string ->
  workspace_id:string option ->
  agent:Patch_agent.t ->
  terminal:(Worker_protocol.result_status * string * string option) option ->
  t

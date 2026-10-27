(* @archlint.module interface
   @archlint.domain worker-checkpoint *)

type t
(** Versioned, patch-scoped execution state. Admission receipts are stored by
    the host beside these exact capsule bytes, never recursively inside them. *)

(** This codec remains in [lib] because it reuses the effectful [Persistence]
    patch-agent snapshot codec. Moving it to [lib_core] would require moving
    that existing codec and its migration dependencies across the library
    boundary. Its decoder still performs no I/O. *)

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

val with_v3_context : t -> Worker_protocol_v3.execution_context -> t
val v3_context : t -> string option
val v3_input : t -> Worker_protocol_v3.input option
val with_v3_input : t -> Worker_protocol_v3.input -> t
val note : t -> Worker_protocol_v3.note option
val with_note : t -> Worker_protocol_v3.note -> t

type projection = private {
  patch_id : string;
  state_version : int64;
  selected_base : string;
  execution_mode : string;
  root_role : string;
  publication : string option;
  note_bytes : string option;
  terminal_status : Worker_protocol.result_status option;
}

val project :
  t ->
  run_id:string ->
  patch_id:Types.Patch_id.t ->
  context:string ->
  state_version:int64 ->
  attention_id:string option ->
  gameplan:Types.Gameplan.t ->
  main_branch:Types.Branch.t ->
  (projection, string) result
(** Validate the exact capsule binding and expose only execution facts; a
    previous assignment's notes cannot become this publication's notes. *)

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
  operation:Worker_protocol.operation ->
  guidance:string ->
  workspace_id:string option ->
  agent:Patch_agent.t ->
  terminal:(Worker_protocol.result_status * string * string option) option ->
  (t, string) result
(** Advances exactly one state version, rejecting counter exhaustion. *)

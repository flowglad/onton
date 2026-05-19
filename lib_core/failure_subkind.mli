open Base

type t =
  | Ok
  | Auth_unavailable
  | Api_error of { status : int option }
  | Network_error
  | Timed_out
  | No_session_to_resume
  | Empty_response
  | Process_error
  | Other of string
[@@deriving show, eq, sexp_of]

type init_info = {
  api_key_source : string option;
  model : string option;
  claude_code_version : string option;
}
[@@deriving show, eq]

val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val init_info_of_yojson : Yojson.Safe.t -> init_info
val yojson_of_init_info : init_info -> Yojson.Safe.t

val classify :
  classification:Run_classification.classification ->
  init:init_info ->
  text_tail:string ->
  stderr_tail:string ->
  t

val to_string : t -> string

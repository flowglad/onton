(* @archlint.module interface
   @archlint.domain patch-agent-rpc *)

open Base

type command =
  | Prompt of { request_id : string; content : string }
  | Abort of { request_id : string }
  | Status of { request_id : string }
  | Shutdown of { request_id : string }
[@@deriving show, eq, sexp_of, compare]

type usage = {
  input_tokens : int;
  output_tokens : int;
  cache_read_tokens : int;
  cache_write_tokens : int;
}
[@@deriving show, eq, sexp_of, compare]

type event =
  | Session_init of {
      session_id : string;
      model_id : string;
      provider : string;
    }
  | Turn_started of { turn_index : int }
  | Text_delta of { delta : string }
  | Tool_call of { name : string; input : string; call_id : string }
  | Done of { stop_reason : string; final_text : string; usage : usage option }
  | Error of { code : string; message : string }
[@@deriving show, eq, sexp_of, compare]

val serialize_command : command -> string
(** Serialize a single command as compact JSON plus one trailing ['\n']. The
    returned string never contains any other raw line breaks. *)

val parse_event : string -> (event, string) Result.t
(** Decode one compact JSON event line. Rejects input containing raw line breaks
    and never raises: malformed input returns [Error reason]. *)

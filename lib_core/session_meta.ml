(* @archlint.module core
   @archlint.domain session-meta *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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

let create ~onton_session_uuid ?claude_session_id ~patch_id ~started_at
    ~ended_at ~exit_code ~subkind ?api_key_source ?model ?claude_code_version ()
    =
  {
    schema_version = 1;
    onton_session_uuid;
    claude_session_id;
    patch_id;
    started_at;
    ended_at;
    exit_code;
    subkind;
    api_key_source;
    model;
    claude_code_version;
  }

let%test "yojson roundtrip preserves all fields" =
  let meta =
    create ~onton_session_uuid:"uuid-1" ~claude_session_id:"claude-1"
      ~patch_id:"patch-4" ~started_at:1.25 ~ended_at:2.5 ~exit_code:17
      ~subkind:(Failure_subkind.Api_error { status = Some 401 })
      ~api_key_source:"oauth" ~model:"sonnet" ~claude_code_version:"1.2.3" ()
  in
  match t_of_yojson (yojson_of_t meta) with
  | meta' -> Yojson.Safe.equal (yojson_of_t meta) (yojson_of_t meta')
  | exception _ -> false

(* @archlint.module interface
   @archlint.domain telemetry *)

module Event : sig
  type level = Trace | Debug | Info | Warning | Error
  [@@deriving show, eq, yojson]

  type t =
    | Poll of { patch_id : Types.Patch_id.t; payload : Yojson.Safe.t }
    | Action of {
        patch_id : Types.Patch_id.t;
        session_uuid : string option;
        payload : Yojson.Safe.t;
      }
    | Complete of {
        patch_id : Types.Patch_id.t;
        session_uuid : string option;
        subkind : Failure_subkind.t;
        payload : Yojson.Safe.t;
      }
    | Stream of {
        patch_id : Types.Patch_id.t;
        session_uuid : string option;
        channel : [ `Stdout | `Stderr ];
        raw : string;
      }
    | Spawn_started of {
        patch_id : Types.Patch_id.t;
        session_uuid : string;
        prompt : string;
        argv : string list;
        env_redacted : string array;
      }
    | Spawn_finalized of {
        patch_id : Types.Patch_id.t;
        session_uuid : string;
        meta : Yojson.Safe.t;
      }
    | Free_form of {
        patch_id : Types.Patch_id.t option;
        level : level;
        message : string;
      }
  [@@deriving show, yojson]
end

module Sink : sig
  type t = {
    name : string;
    interested_in : Event.t -> bool;
    consume : Event.t -> unit;
  }
end

val route : sinks:Sink.t list -> Event.t -> Sink.t list

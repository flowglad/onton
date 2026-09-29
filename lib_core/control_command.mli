(* @archlint.module interface
   @archlint.domain control-command *)

type t =
  | Set_automerge of {
      id : string;
      patch_id : Types.Patch_id.t;
      enabled : bool;
    }
  | Bump of { id : string; patch_id : Types.Patch_id.t }
  | Send_human_message of {
      id : string;
      patch_id : Types.Patch_id.t;
      message : string;
    }

val decode : Yojson.Safe.t -> (t, string) result
val id : t -> string
val max_retained_ids : int
val recent_ids : string list -> string list
val record_id : string -> string list -> string list

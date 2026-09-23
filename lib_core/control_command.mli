type t =
  | Set_automerge of {
      id : string;
      patch_id : Types.Patch_id.t;
      enabled : bool;
    }

val decode : Yojson.Safe.t -> (t, string) result
val id : t -> string

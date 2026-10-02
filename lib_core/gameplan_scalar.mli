(* @archlint.module interface
   @archlint.domain gameplan-document *)

type style = Plain | Quoted

val decode : style -> string -> (Yojson.Safe.t, string) result
(** Interpret a YAML scalar using JSON scalar types. [Quoted] covers quoted and
    block scalars and preserves their contents as strings. [Plain] recognizes
    nulls, booleans and JSON numbers; other values remain strings. Non-finite
    numbers return errors. Never raises on malformed input. *)

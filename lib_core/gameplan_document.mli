(* @archlint.module interface
   @archlint.domain gameplan-document *)

val of_string : string -> (Yojson.Safe.t, string) result
(** Decode JSON or one YAML document into the shared gameplan data model. YAML
    uses JSON scalar types, with empty scalars and [~] also meaning null. Quoted
    and block scalars remain strings. Mapping keys must be strings and unique.
    Tags, anchors, aliases and multiple documents return errors. Never raises on
    malformed input. *)

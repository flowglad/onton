(* @archlint.module interface
   @archlint.domain gameplan-parser *)

open Base

type t = {
  gameplan : Types.Gameplan.t;
  dependency_graph : Types.Patch_id.t list Map.M(Types.Patch_id).t;
}

val parse_json_string : string -> (t, string) Result.t
val parse_json_file : string -> (t, string) Result.t

val parse_string : string -> (t, string) Result.t
(** Parse a YAML or JSON gameplan through the same semantic validation. *)

val parse_file : string -> (t, string) Result.t
(** [parse_file path] parses a YAML or JSON gameplan file. [.json] files use
    strict JSON parsing; other filenames accept either format. *)

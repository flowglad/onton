open Base

type t = {
  gameplan : Types.Gameplan.t;
  dependency_graph : Types.Patch_id.t list Map.M(Types.Patch_id).t;
}

val parse_string : string -> (t, string) Result.t
val parse_file : string -> (t, string) Result.t

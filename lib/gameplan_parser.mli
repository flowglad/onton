open Base

type t = {
  project_name : string;
  problem_statement : string;
  solution_summary : string;
  patches : Types.Patch.t list;
  dependency_graph : Types.Patch_id.t list Map.M(Types.Patch_id).t;
}

val parse_string : string -> (t, string) Result.t
val parse_file : string -> (t, string) Result.t

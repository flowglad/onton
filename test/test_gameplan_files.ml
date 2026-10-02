(* @archlint.module test
   @archlint.domain gameplan-parser *)

open Onton_core

let () =
  if Array.length Sys.argv <> 3 then failwith "Expected YAML and JSON fixtures";
  match
    ( Gameplan_parser.parse_file Sys.argv.(1),
      Gameplan_parser.parse_file Sys.argv.(2) )
  with
  | Ok yaml, Ok json ->
      if
        not
          (Types.Gameplan.equal yaml.Gameplan_parser.gameplan
             json.Gameplan_parser.gameplan)
      then failwith "YAML and JSON fixtures produce different plans";
      if
        not
          (Base.Map.equal
             (Base.List.equal Types.Patch_id.equal)
             yaml.Gameplan_parser.dependency_graph
             json.Gameplan_parser.dependency_graph)
      then failwith "YAML and JSON fixtures produce different dependency graphs"
  | Error msg, _ | Ok _, Error msg -> failwith msg

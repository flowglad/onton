(* @archlint.module test
   @archlint.domain opencode-event-parser *)

open Onton_core

let opencode_normalizes_known_tool_names =
  QCheck2.Test.make ~name:"opencode known tool names normalize to display names"
    ~count:200
    QCheck2.Gen.(oneof_list [ "read"; "write"; "edit"; "bash"; "grep"; "glob" ])
    (fun tool ->
      let normalized = Opencode_event_parser.normalize_tool_name tool in
      String.length normalized >= String.length tool
      && Char.uppercase_ascii normalized.[0] = normalized.[0])

let opencode_parser_public_surface_is_linked =
  QCheck2.Test.make ~name:"opencode parser public surface is linked"
    QCheck2.Gen.unit (fun () ->
      ignore Opencode_event_parser.auto_model;
      ignore Opencode_event_parser.build_args;
      ignore Opencode_event_parser.normalize_input_json;
      ignore Opencode_event_parser.parse_event;
      true)

let () =
  QCheck2.Test.check_exn opencode_normalizes_known_tool_names;
  QCheck2.Test.check_exn opencode_parser_public_surface_is_linked

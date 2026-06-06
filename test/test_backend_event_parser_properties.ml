(* @archlint.module test
   @archlint.domain codex-event-parser *)

open Onton_core

let invalid_codex_json_is_ignored =
  QCheck2.Test.make ~name:"invalid codex JSON is ignored" ~count:200
    QCheck2.Gen.string_small (fun line ->
      let events, _ =
        Codex_event_parser.parse_event_with_cost_tracking ~model:None
          ~budget_cap_nano_usd:None ~cost_state:Codex_cost.initial_cost_state
          line
      in
      match Yojson.Safe.from_string line with
      | exception Yojson.Json_error _ -> events = []
      | _ -> true)

let public_codex_parser_surface_is_linked =
  QCheck2.Test.make ~name:"codex parser public surface is linked"
    QCheck2.Gen.unit (fun () ->
      ignore Codex_event_parser.auto_model;
      ignore Codex_event_parser.build_args;
      ignore Codex_event_parser.parse_event;
      true)

let () =
  QCheck2.Test.check_exn invalid_codex_json_is_ignored;
  QCheck2.Test.check_exn public_codex_parser_surface_is_linked

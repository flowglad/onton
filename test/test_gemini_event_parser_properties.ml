(* @archlint.module test
   @archlint.domain gemini-event-parser *)

open Onton_core

let gemini_build_args_preserve_prompt =
  QCheck2.Test.make ~name:"gemini args preserve prompt" ~count:200
    QCheck2.Gen.string_small (fun prompt ->
      List.mem prompt
        (Gemini_event_parser.build_args ~model:None ~prompt ~resume_session:None))

let gemini_parse_event_is_total =
  QCheck2.Test.make ~name:"gemini parse_event is total" ~count:200
    QCheck2.Gen.string_small (fun line ->
      let events = Gemini_event_parser.parse_event line in
      List.length events >= 0)

let gemini_auto_model_is_total =
  QCheck2.Test.make ~name:"gemini auto_model returns a model for any complexity"
    ~count:200
    QCheck2.Gen.(option (int_range (-5) 10))
    (fun complexity ->
      match Gemini_event_parser.auto_model ~complexity with
      | Some m -> String.length m > 0
      | None -> false)

let gemini_parser_public_surface_is_linked =
  QCheck2.Test.make ~name:"gemini parser public surface is linked"
    QCheck2.Gen.unit (fun () ->
      ignore Gemini_event_parser.auto_model;
      ignore Gemini_event_parser.parse_event;
      true)

let () =
  QCheck2.Test.check_exn gemini_build_args_preserve_prompt;
  QCheck2.Test.check_exn gemini_parse_event_is_total;
  QCheck2.Test.check_exn gemini_auto_model_is_total;
  QCheck2.Test.check_exn gemini_parser_public_surface_is_linked

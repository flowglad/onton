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

let opencode_parse_event_is_total =
  QCheck2.Test.make ~name:"opencode parse_event is total" ~count:200
    QCheck2.Gen.string_small (fun line ->
      let events = Opencode_event_parser.parse_event line in
      List.length events >= 0)

let opencode_build_args_preserve_prompt =
  QCheck2.Test.make ~name:"opencode build_args preserve prompt" ~count:200
    QCheck2.Gen.string_small (fun prompt ->
      let args =
        Opencode_event_parser.build_args ~model:None ~cwd_path:"/tmp/work"
          ~prompt ~resume_session:None
      in
      List.mem prompt args)

let opencode_auto_model_is_total =
  QCheck2.Test.make
    ~name:"opencode auto_model returns a model for any complexity" ~count:200
    QCheck2.Gen.(option (int_range (-5) 10))
    (fun complexity ->
      match Opencode_event_parser.auto_model ~complexity with
      | Some m -> String.length m > 0
      | None -> false)

(* [normalize_input_json] renames [filePath] -> [file_path] for read/write/edit
   tools and is the identity for other tools. Generating a value, we assert the
   value round-trips to itself for a non-rewriting tool. *)
let opencode_normalize_input_json_identity_for_unknown_tool =
  QCheck2.Test.make
    ~name:"opencode normalize_input_json is identity for non-file tools"
    ~count:200 QCheck2.Gen.string_small (fun v ->
      let json = `Assoc [ ("filePath", `String v) ] in
      let out = Opencode_event_parser.normalize_input_json ~tool:"bash" json in
      out = json)

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
  QCheck2.Test.check_exn opencode_parse_event_is_total;
  QCheck2.Test.check_exn opencode_build_args_preserve_prompt;
  QCheck2.Test.check_exn opencode_auto_model_is_total;
  QCheck2.Test.check_exn opencode_normalize_input_json_identity_for_unknown_tool;
  QCheck2.Test.check_exn opencode_parser_public_surface_is_linked

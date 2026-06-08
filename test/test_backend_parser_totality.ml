(* @archlint.module test
   @archlint.domain json *)

open Base
open Onton_core

(** Totality fuzz tests for every backend parser in onton_core.

    Each parser's [parse_event] (or [parse_event_with_cost_tracking]) takes a
    raw NDJSON line. Under the AGENTS.md contract, these are total: malformed or
    unexpected payloads must return [[]] / [None] rather than raise. The inline
    [%test] blocks in each parser cover the named branches; these properties
    cover the unbounded space of inputs that real CLI subprocesses can emit
    (truncation, garbage prefix, encoding glitches, schema drift). *)

(* ─────────────────────────────────────────────────────────────────────────
   Generators
   ───────────────────────────────────────────────────────────────────────── *)

let gen_string =
  let open QCheck2.Gen in
  oneof
    [
      string;
      pure "";
      pure "{}";
      pure "not json at all";
      pure "\x00\x01\x02";
      string_size (int_range 0 4096);
    ]

(* Arbitrary Yojson.Safe.t values, then serialize to a string. Covers
   non-object payloads (bare ints, lists, nested), missing keys, wrong-type
   members — the failure modes that real CLIs emit during version drift. *)
let gen_yojson : Yojson.Safe.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  sized
  @@ fix (fun self n ->
      if n <= 0 then
        oneof
          [
            pure `Null;
            map (fun b -> `Bool b) bool;
            map (fun i -> `Int i) nat_small;
            (* finite floats only — NaN/inf round-trip blows up Yojson.to_string *)
            map (fun i -> `Float (Float.of_int i /. 100.)) (int_range 0 10_000);
            map (fun s -> `String s) string;
          ]
      else
        oneof
          [
            self 0;
            map (fun xs -> `List xs) (list_size (int_range 0 4) (self (n / 2)));
            map
              (fun pairs -> `Assoc pairs)
              (list_size (int_range 0 4)
                 (pair (string_size (int_range 1 8)) (self (n / 2))));
          ])

let gen_json_line =
  let open QCheck2.Gen in
  oneof [ map (fun y -> Yojson.Safe.to_string y) gen_yojson; gen_string ]

(* ─────────────────────────────────────────────────────────────────────────
   The totality property — applied uniformly to each parser
   ───────────────────────────────────────────────────────────────────────── *)

let totality ~name f =
  QCheck2.Test.make ~name ~count:1000 gen_json_line (fun line ->
      try
        ignore (f line);
        true
      with _ -> false)

let strip_ansi_does_not_grow =
  QCheck2.Test.make ~name:"claude strip_ansi does not grow output" ~count:1000
    gen_string (fun line ->
      try
        String.length (Claude_event_parser.strip_ansi line)
        <= String.length line
      with _ -> false)

let strip_ansi_removes_stray_controls =
  QCheck2.Test.make ~name:"claude strip_ansi removes stray controls" ~count:1000
    gen_string (fun line ->
      try
        Claude_event_parser.strip_ansi line
        |> String.for_all ~f:(fun c ->
            match Char.to_int c with
            | 0x09 | 0x0a | 0x1b -> true
            | n -> n > 0x1f)
      with _ -> false)

let strip_ansi_known_sequences =
  QCheck2.Test.make ~name:"claude strip_ansi removes known ANSI sequences"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let raw =
        "\x1b[31mred\x1b[0m\r\n\x00plain\t\x1b]0;title\x07body\x1b(0x"
      in
      String.equal (Claude_event_parser.strip_ansi raw) "red\nplain\tbody")

let json_accessors_are_total =
  QCheck2.Test.make ~name:"Json accessors are total" ~count:1000 gen_yojson
    (fun json ->
      try
        ignore (Json.field "field" json);
        ignore (Json.string json);
        ignore (Json.int json);
        ignore (Json.bool json);
        ignore (Json.list json);
        ignore (Json.assoc json);
        ignore (Json.string_field "field" json);
        ignore (Json.int_field "field" json);
        ignore (Json.bool_field "field" json);
        ignore (Json.try_of_yojson (fun value -> value) json);
        true
      with _ -> false)

let gen_complexity =
  let open QCheck2.Gen in
  oneof [ return None; map (fun k -> Some k) (int_range (-5) 10) ]

(* [auto_model] always names a concrete model. *)
let claude_auto_model_is_total =
  QCheck2.Test.make ~name:"claude auto_model names a model for any complexity"
    ~count:200 gen_complexity (fun complexity ->
      match Claude_event_parser.auto_model ~complexity with
      | Some m -> String.length m > 0
      | None -> false)

(* [max_turns_for] is positive for any complexity. *)
let claude_max_turns_for_is_positive =
  QCheck2.Test.make ~name:"claude max_turns_for is positive" ~count:200
    gen_complexity (fun complexity ->
      Claude_event_parser.max_turns_for ~complexity > 0)

(* [model_args] emits ["--model"; m] for a non-empty model and [] otherwise. *)
let claude_model_args_round_trips =
  QCheck2.Test.make ~name:"claude model_args wraps non-empty model" ~count:200
    QCheck2.Gen.(option string_small)
    (fun model ->
      let args = Claude_event_parser.model_args model in
      match model with
      | Some m when not (String.is_empty m) ->
          List.equal String.equal args [ "--model"; m ]
      | _ -> List.is_empty args)

(* Build an env oracle from the generated [api_key]: it answers
   [ANTHROPIC_API_KEY] with the generated value (and [None] for everything
   else). [bare_args] emits [--bare] iff that value is non-blank. The constant
   oracles [no_env] and [with_api_key] anchor the two extremes. *)
let claude_bare_args_tracks_api_key =
  QCheck2.Test.make ~name:"claude bare_args tracks ANTHROPIC_API_KEY presence"
    ~count:200
    QCheck2.Gen.(option string_small)
    (fun api_key ->
      let oracle = function "ANTHROPIC_API_KEY" -> api_key | _ -> None in
      let args = Claude_event_parser.bare_args ~getenv_opt:oracle in
      let expected_bare =
        match api_key with
        | Some k -> not (String.is_empty (String.strip k))
        | None -> false
      in
      let got_bare = not (List.is_empty args) in
      Bool.equal got_bare expected_bare
      (* anchor with the constant oracles supplied by the parser *)
      && List.is_empty
           (Claude_event_parser.bare_args ~getenv_opt:Claude_event_parser.no_env)
      && List.equal String.equal
           (Claude_event_parser.bare_args
              ~getenv_opt:Claude_event_parser.with_api_key)
           [ "--bare" ])

(* [budget_cap_args] only emits a cap flag when [ONTON_BUDGET_CAP_USD] parses to
   a positive float. Feed the generated string through a custom oracle and check
   the membership of "--max-budget-usd" matches that predicate. [ignore_warn]
   absorbs the warning on unparseable input. *)
let claude_budget_cap_args_tracks_env =
  QCheck2.Test.make ~name:"claude budget_cap_args tracks ONTON_BUDGET_CAP_USD"
    ~count:200
    QCheck2.Gen.(
      oneof
        [
          map (fun f -> Float.to_string f) (float_range (-5.) 5.); string_small;
        ])
    (fun raw ->
      let oracle = function "ONTON_BUDGET_CAP_USD" -> Some raw | _ -> None in
      let args =
        Claude_event_parser.budget_cap_args ~getenv_opt:oracle
          ~warn:Claude_event_parser.ignore_warn ()
      in
      let expect_cap =
        match Float.of_string_opt (String.strip raw) with
        | Some cap -> Float.( > ) cap 0.
        | None -> false
      in
      let got_cap = List.mem args "--max-budget-usd" ~equal:String.equal in
      Bool.equal got_cap expect_cap
      (* anchor: [no_env] never produces a cap flag *)
      && List.is_empty
           (Claude_event_parser.budget_cap_args
              ~getenv_opt:Claude_event_parser.no_env
              ~warn:Claude_event_parser.ignore_warn ()))

(* [build_args] always threads the prompt and the resume session id through. *)
let claude_build_args_carry_prompt =
  QCheck2.Test.make ~name:"claude build_args carry prompt and resume" ~count:200
    QCheck2.Gen.(pair string_small (option string_small))
    (fun (prompt, resume_session) ->
      let args =
        Claude_event_parser.build_args ~getenv_opt:Claude_event_parser.no_env
          ~warn:Claude_event_parser.ignore_warn ~model:None ~complexity:None
          ~prompt ~resume_session
      in
      List.mem args prompt ~equal:String.equal
      &&
      match resume_session with
      | Some id -> List.mem args id ~equal:String.equal
      | None -> true)

(* [build_stream_args] threads the prompt through (with no minted session). *)
let claude_build_stream_args_carry_prompt =
  QCheck2.Test.make ~name:"claude build_stream_args carry prompt" ~count:200
    QCheck2.Gen.string_small (fun prompt ->
      let args =
        Claude_event_parser.build_stream_args
          ~getenv_opt:Claude_event_parser.no_env
          ~warn:Claude_event_parser.ignore_warn ~model:None ~complexity:None
          ~prompt ~minted_session_id:None ~resume_session:None
      in
      List.mem args prompt ~equal:String.equal)

(* [find_json_start] never grows the string and is idempotent. *)
let claude_find_json_start_idempotent =
  QCheck2.Test.make ~name:"claude find_json_start is idempotent" ~count:1000
    gen_string (fun s ->
      let once = Claude_event_parser.find_json_start s in
      String.length once <= String.length s
      && String.equal once (Claude_event_parser.find_json_start once))

(* [session_init_of_json] yields exactly one Session_init when session_id is
   present, and [] otherwise. *)
let claude_session_init_of_json_round_trips =
  QCheck2.Test.make ~name:"claude session_init_of_json keys on session_id"
    ~count:200 QCheck2.Gen.string_small (fun id ->
      let json = `Assoc [ ("session_id", `String id) ] in
      match Claude_event_parser.session_init_of_json json with
      | [ _ ] -> true
      | _ -> false)

(* [parse_stream_event] agrees with the head of [parse_stream_events]. *)
let claude_parse_stream_event_matches_head =
  QCheck2.Test.make
    ~name:"claude parse_stream_event matches parse_stream_events head"
    ~count:1000 gen_json_line (fun line ->
      match
        ( Claude_event_parser.parse_stream_event line,
          Claude_event_parser.parse_stream_events line )
      with
      | None, [] -> true
      | Some e, head :: _ -> Types.Stream_event.equal e head
      | _ -> false)

let claude_public_surface_is_linked =
  QCheck2.Test.make ~name:"claude parser public surface is linked"
    QCheck2.Gen.unit (fun () ->
      ignore Claude_event_parser.auto_model;
      ignore Claude_event_parser.bare_args;
      ignore Claude_event_parser.budget_cap_args;
      ignore Claude_event_parser.build_args;
      ignore Claude_event_parser.build_stream_args;
      ignore Claude_event_parser.find_json_start;
      ignore Claude_event_parser.ignore_warn;
      ignore Claude_event_parser.max_turns_for;
      ignore Claude_event_parser.model_args;
      ignore Claude_event_parser.no_env;
      ignore Claude_event_parser.parse_stream_event;
      ignore Claude_event_parser.parse_stream_events;
      ignore Claude_event_parser.session_init_of_json;
      ignore Claude_event_parser.with_api_key;
      true)

(* ─────────────────────────────────────────────────────────────────────────
   Tests
   ───────────────────────────────────────────────────────────────────────── *)

let () =
  let codex_with_state line =
    Codex_event_parser.parse_event_with_cost_tracking ~model:None
      ~budget_cap_nano_usd:None ~cost_state:Codex_cost.initial_cost_state line
  in
  let claude_event line = Claude_event_parser.parse_stream_event line in
  let claude_events line = Claude_event_parser.parse_stream_events line in
  let claude_strip line = Claude_event_parser.strip_ansi line in
  let claude_find_json line = Claude_event_parser.find_json_start line in
  let suite =
    [
      totality ~name:"codex parse_event total" Codex_event_parser.parse_event;
      totality ~name:"codex parse_event_with_cost_tracking total"
        codex_with_state;
      totality ~name:"gemini parse_event total" Gemini_event_parser.parse_event;
      totality ~name:"pi parse_event total" Pi_event_parser.parse_event;
      totality ~name:"opencode parse_event total"
        Opencode_event_parser.parse_event;
      totality ~name:"claude parse_stream_event total" claude_event;
      totality ~name:"claude parse_stream_events total" claude_events;
      totality ~name:"claude strip_ansi total" claude_strip;
      totality ~name:"claude find_json_start total" claude_find_json;
      strip_ansi_does_not_grow;
      strip_ansi_removes_stray_controls;
      strip_ansi_known_sequences;
      json_accessors_are_total;
      claude_auto_model_is_total;
      claude_max_turns_for_is_positive;
      claude_model_args_round_trips;
      claude_bare_args_tracks_api_key;
      claude_budget_cap_args_tracks_env;
      claude_build_args_carry_prompt;
      claude_build_stream_args_carry_prompt;
      claude_find_json_start_idempotent;
      claude_session_init_of_json_round_trips;
      claude_parse_stream_event_matches_head;
      claude_public_surface_is_linked;
    ]
  in
  let exit_code = QCheck_base_runner.run_tests ~verbose:true suite in
  if exit_code <> 0 then Stdlib.exit exit_code

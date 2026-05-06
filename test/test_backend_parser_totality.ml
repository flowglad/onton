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
    ]
  in
  let exit_code = QCheck_base_runner.run_tests ~verbose:true suite in
  if exit_code <> 0 then Stdlib.exit exit_code

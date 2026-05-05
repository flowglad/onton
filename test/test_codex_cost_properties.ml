open Base
open Onton

(** Property + interleaving tests for {!Codex_cost}.

    Expectations are derived from the Codex reference implementation in
    [codex-rs/exec/src/exec_events.rs::Usage] (the JSONL schema
    [codex exec --json] actually emits) and
    [codex-rs/codex-api/src/sse/responses.rs] (the upstream OpenAI Responses-API
    schema Codex consumes). Codex CLI emits the flat [reasoning_output_tokens]
    spelling; [Codex_cost] also accepts two legacy/defensive schemas that may
    appear in older or intermediate payloads. *)

(* ---------- Generators ---------- *)

(* Token counts: small ranges keep cumulative arithmetic well within int64
   without overflow concerns, while still covering the boundary cases. *)
let gen_token_count = QCheck2.Gen.int_range 0 1_000_000
let gen_negative_int = QCheck2.Gen.int_range (-100_000) (-1)

(* Generate a Codex-spec-shaped usage value (the alt-flat schema emitted by
   [codex exec --json]). [reasoning_output_tokens <= output_tokens] reflects
   the Responses API contract: reasoning is a subset of output. *)
let gen_codex_usage_json =
  let open QCheck2.Gen in
  let* input = gen_token_count in
  let* output = gen_token_count in
  let* cached = gen_token_count in
  let* reasoning = int_range 0 output in
  return
    (`Assoc
       [
         ("input_tokens", `Int input);
         ("cached_input_tokens", `Int cached);
         ("output_tokens", `Int output);
         ("reasoning_output_tokens", `Int reasoning);
       ])

(* Generate the legacy "Responses-API-shaped" usage (nested
   output_tokens_details.reasoning_tokens) we still accept defensively. *)
let gen_responses_api_usage_json =
  let open QCheck2.Gen in
  let* input = gen_token_count in
  let* output = gen_token_count in
  let* reasoning = int_range 0 output in
  return
    (`Assoc
       [
         ("input_tokens", `Int input);
         ("output_tokens", `Int output);
         ( "output_tokens_details",
           `Assoc [ ("reasoning_tokens", `Int reasoning) ] );
       ])

(* Recursive arbitrary Yojson generator (depth-bounded) used to prove
   [usage_of_yojson] is total — never raises regardless of input shape. *)
let rec gen_arbitrary_json depth =
  let open QCheck2.Gen in
  let leaves =
    [
      return `Null;
      map (fun b -> `Bool b) bool;
      map (fun i -> `Int i) (int_range (-1_000) 1_000);
      map (fun s -> `String s) (string_small_of printable);
      return (`Float 1.5);
    ]
  in
  if depth <= 0 then oneof leaves
  else
    let smaller = gen_arbitrary_json (depth - 1) in
    oneof
      (leaves
      @ [
          map (fun xs -> `List xs) (list_size (int_range 0 4) smaller);
          map
            (fun pairs -> `Assoc pairs)
            (list_size (int_range 0 4)
               (pair (string_small_of printable) smaller));
        ])

let gen_arbitrary_yojson = gen_arbitrary_json 3

let gen_pricing =
  let open QCheck2.Gen in
  let* in_rate = int_range 1 100_000_000 in
  let* out_rate = int_range 1 100_000_000 in
  return
    Codex_cost.
      {
        input_nano_usd_per_1k = Int64.of_int in_rate;
        output_nano_usd_per_1k = Int64.of_int out_rate;
      }

let gen_model_name =
  QCheck2.Gen.oneof_list [ Some "gpt-5.4-mini"; Some "gpt-5.4"; Some "gpt-5.5" ]

(* Cap is large enough that some interleavings stay under and others cross. *)
let gen_cap_nano_usd =
  let open QCheck2.Gen in
  oneof
    [ return None; map (fun n -> Some (Int64.of_int n)) (int_range 1 100_000) ]

(* ---------- Stream_event predicates (exhaustive — guards against drift) ---------- *)

let is_final_result : Types.Stream_event.t -> bool = function
  | Final_result _ -> true
  | Turn_started | Text_delta _ | Tool_use _ | Error _ | Session_init _ -> false

let is_error : Types.Stream_event.t -> bool = function
  | Error _ -> true
  | Turn_started | Text_delta _ | Tool_use _ | Final_result _ | Session_init _
    ->
      false

(* ---------- Properties: pricing math ---------- *)

let prop_cost_zero_tokens =
  QCheck2.Test.make ~name:"cost_nano_usd_for_tokens 0 r = 0"
    QCheck2.Gen.(int_range 0 1_000_000_000)
    (fun rate ->
      Int64.equal (Codex_cost.cost_nano_usd_for_tokens 0 (Int64.of_int rate)) 0L)

let prop_cost_negative_tokens_clamps =
  QCheck2.Test.make ~name:"cost_nano_usd_for_tokens negative tokens = 0"
    QCheck2.Gen.(pair gen_negative_int (int_range 1 1_000_000_000))
    (fun (tokens, rate) ->
      Int64.equal
        (Codex_cost.cost_nano_usd_for_tokens tokens (Int64.of_int rate))
        0L)

let prop_cost_monotone_in_tokens =
  QCheck2.Test.make ~name:"cost_nano_usd_for_tokens monotone non-decreasing"
    QCheck2.Gen.(triple gen_token_count gen_token_count (int_range 0 1_000_000))
    (fun (a, b, rate) ->
      let lo, hi = if a <= b then (a, b) else (b, a) in
      let r = Int64.of_int rate in
      Int64.(
        Codex_cost.cost_nano_usd_for_tokens lo r
        <= Codex_cost.cost_nano_usd_for_tokens hi r))

let prop_cost_of_usage_equals_input_plus_output =
  QCheck2.Test.make
    ~name:
      "cost_of_usage approximates input * r_in + output * r_out (within 1 \
       nano-USD truncation)"
    QCheck2.Gen.(pair gen_codex_usage_json gen_pricing)
    (fun (json, pricing) ->
      (* Reasoning tokens are priced at the same rate as visible output, so
         splitting [output_tokens] into [visible + reasoning] should yield
         the same cost as charging [output_tokens] uniformly. The split
         truncates twice (visible*r/1000 and reasoning*r/1000) where the
         unified form truncates once, so the result can under-count by at
         most 1 nano-USD per turn. *)
      let usage = Codex_cost.usage_of_yojson json in
      let actual = Codex_cost.cost_of_usage ~pricing usage in
      let unified =
        Int64.(
          Codex_cost.cost_nano_usd_for_tokens usage.input_tokens
            pricing.input_nano_usd_per_1k
          + Codex_cost.cost_nano_usd_for_tokens usage.output_tokens
              pricing.output_nano_usd_per_1k)
      in
      let drift = Int64.(unified - actual) in
      Int64.(drift >= 0L && drift <= 1L))

let prop_cost_of_usage_non_negative =
  QCheck2.Test.make ~name:"cost_of_usage >= 0 for any decoded JSON"
    QCheck2.Gen.(pair gen_arbitrary_yojson gen_pricing)
    (fun (json, pricing) ->
      let usage = Codex_cost.usage_of_yojson json in
      Int64.(Codex_cost.cost_of_usage ~pricing usage >= 0L))

(* ---------- Properties: usage decoding ---------- *)

let prop_usage_of_yojson_total =
  QCheck2.Test.make ~name:"usage_of_yojson is total — never raises" ~count:500
    gen_arbitrary_yojson (fun json ->
      try
        let _ = Codex_cost.usage_of_yojson json in
        true
      with _ -> false)

let prop_usage_of_yojson_non_negative_fields =
  QCheck2.Test.make
    ~name:"usage_of_yojson fields are non-negative for any input" ~count:500
    gen_arbitrary_yojson (fun json ->
      let u = Codex_cost.usage_of_yojson json in
      u.input_tokens >= 0 && u.output_tokens >= 0 && u.reasoning_tokens >= 0)

let prop_usage_codex_schema_decodes_alt_flat =
  QCheck2.Test.make
    ~name:
      "Codex CLI schema: reasoning_output_tokens decodes as reasoning_tokens"
    gen_codex_usage_json (fun json ->
      let u = Codex_cost.usage_of_yojson json in
      let open Yojson.Safe.Util in
      let expected =
        member "reasoning_output_tokens" json
        |> to_int_option |> Option.value ~default:0
      in
      Int.equal u.reasoning_tokens expected)

let prop_usage_responses_api_schema_decodes_nested =
  QCheck2.Test.make
    ~name:
      "Responses API schema: nested output_tokens_details.reasoning_tokens \
       decodes (defensive fallback)" gen_responses_api_usage_json (fun json ->
      let u = Codex_cost.usage_of_yojson json in
      let open Yojson.Safe.Util in
      let expected =
        member "output_tokens_details" json
        |> member "reasoning_tokens" |> to_int_option |> Option.value ~default:0
      in
      Int.equal u.reasoning_tokens expected)

let prop_usage_schema_priority_nested_wins_over_flat =
  QCheck2.Test.make
    ~name:"schema priority: nested.reasoning_tokens wins over flat siblings"
    QCheck2.Gen.(triple (int_range 1 100) (int_range 1 100) (int_range 1 100))
    (fun (nested, flat, alt_flat) ->
      let json =
        `Assoc
          [
            ("output_tokens", `Int 1000);
            ( "output_tokens_details",
              `Assoc [ ("reasoning_tokens", `Int nested) ] );
            ("reasoning_tokens", `Int flat);
            ("reasoning_output_tokens", `Int alt_flat);
          ]
      in
      Int.equal (Codex_cost.usage_of_yojson json).reasoning_tokens nested)

let prop_usage_schema_priority_flat_wins_over_alt =
  QCheck2.Test.make
    ~name:
      "schema priority: flat reasoning_tokens wins over reasoning_output_tokens"
    QCheck2.Gen.(pair (int_range 1 100) (int_range 1 100))
    (fun (flat, alt_flat) ->
      let json =
        `Assoc
          [
            ("output_tokens", `Int 1000);
            ("reasoning_tokens", `Int flat);
            ("reasoning_output_tokens", `Int alt_flat);
          ]
      in
      Int.equal (Codex_cost.usage_of_yojson json).reasoning_tokens flat)

(* ---------- Properties: cap decision ---------- *)

let prop_cap_none_always_within =
  QCheck2.Test.make ~name:"check_cap None is always Within_cap"
    QCheck2.Gen.(int_range 0 1_000_000_000)
    (fun delta ->
      let s =
        Codex_cost.advance Codex_cost.initial_cost_state (Int64.of_int delta)
      in
      Codex_cost.equal_cap_decision
        (Codex_cost.check_cap ~budget_cap_nano_usd:None s)
        Codex_cost.Within_cap)

let prop_cap_at_boundary_is_within =
  QCheck2.Test.make
    ~name:"check_cap at boundary (cumulative = cap) is Within_cap"
    QCheck2.Gen.(int_range 1 1_000_000_000)
    (fun cap ->
      let s =
        Codex_cost.advance Codex_cost.initial_cost_state (Int64.of_int cap)
      in
      Codex_cost.equal_cap_decision
        (Codex_cost.check_cap ~budget_cap_nano_usd:(Some (Int64.of_int cap)) s)
        Codex_cost.Within_cap)

let prop_cap_above_boundary_is_exceeded =
  QCheck2.Test.make ~name:"check_cap above boundary is Exceeded"
    QCheck2.Gen.(pair (int_range 1 1_000_000) (int_range 1 1_000_000))
    (fun (cap, over) ->
      let s =
        Codex_cost.advance Codex_cost.initial_cost_state
          (Int64.of_int (cap + over))
      in
      match
        Codex_cost.check_cap ~budget_cap_nano_usd:(Some (Int64.of_int cap)) s
      with
      | Exceeded { cumulative_nano_usd; cap_nano_usd } ->
          Int64.equal cumulative_nano_usd (Int64.of_int (cap + over))
          && Int64.equal cap_nano_usd (Int64.of_int cap)
      | Within_cap -> false)

(* ---------- Properties: advance ---------- *)

let prop_advance_monotone =
  QCheck2.Test.make ~name:"advance is monotone non-decreasing for any delta"
    QCheck2.Gen.(pair (int_range 0 1_000_000) (int_range (-100_000) 100_000))
    (fun (start, delta) ->
      let s =
        Codex_cost.advance Codex_cost.initial_cost_state (Int64.of_int start)
      in
      let s' = Codex_cost.advance s (Int64.of_int delta) in
      Int64.(s'.cumulative_nano_usd >= s.cumulative_nano_usd))

let prop_advance_negative_clamps =
  QCheck2.Test.make ~name:"advance negative delta is a no-op"
    QCheck2.Gen.(pair (int_range 0 1_000_000) gen_negative_int)
    (fun (start, delta) ->
      let s =
        Codex_cost.advance Codex_cost.initial_cost_state (Int64.of_int start)
      in
      let s' = Codex_cost.advance s (Int64.of_int delta) in
      Int64.equal s'.cumulative_nano_usd s.cumulative_nano_usd)

(* ---------- Properties: on_turn_completed (single step) ---------- *)

let prop_on_turn_emits_exactly_one_final_result =
  QCheck2.Test.make
    ~name:"on_turn_completed always emits exactly one Final_result"
    QCheck2.Gen.(triple gen_codex_usage_json gen_model_name gen_cap_nano_usd)
    (fun (usage_json, model, cap) ->
      let json = `Assoc [ ("usage", usage_json) ] in
      let { Codex_cost.events; _ } =
        Codex_cost.on_turn_completed ~model ~budget_cap_nano_usd:cap
          ~state:Codex_cost.initial_cost_state json
      in
      Int.equal (List.count events ~f:is_final_result) 1)

let prop_on_turn_state_monotone =
  QCheck2.Test.make
    ~name:"on_turn_completed produces monotone non-decreasing state"
    QCheck2.Gen.(triple gen_codex_usage_json gen_model_name gen_cap_nano_usd)
    (fun (usage_json, model, cap) ->
      let json = `Assoc [ ("usage", usage_json) ] in
      let prior = Codex_cost.advance Codex_cost.initial_cost_state 1_000L in
      let { Codex_cost.state; _ } =
        Codex_cost.on_turn_completed ~model ~budget_cap_nano_usd:cap
          ~state:prior json
      in
      Int64.(state.cumulative_nano_usd >= prior.cumulative_nano_usd))

let prop_on_turn_unknown_model_no_cost_change =
  QCheck2.Test.make
    ~name:"on_turn_completed with unknown model leaves state unchanged"
    QCheck2.Gen.(pair gen_codex_usage_json gen_cap_nano_usd)
    (fun (usage_json, cap) ->
      let json = `Assoc [ ("usage", usage_json) ] in
      let prior = Codex_cost.advance Codex_cost.initial_cost_state 42L in
      let { Codex_cost.state; _ } =
        Codex_cost.on_turn_completed ~model:None ~budget_cap_nano_usd:cap
          ~state:prior json
      in
      Int64.equal state.cumulative_nano_usd prior.cumulative_nano_usd)

let prop_on_turn_error_iff_exceeded =
  QCheck2.Test.make
    ~name:
      "on_turn_completed emits Error iff post-state cap-decision is Exceeded"
    QCheck2.Gen.(triple gen_codex_usage_json gen_model_name gen_cap_nano_usd)
    (fun (usage_json, model, cap) ->
      let json = `Assoc [ ("usage", usage_json) ] in
      let { Codex_cost.events; state } =
        Codex_cost.on_turn_completed ~model ~budget_cap_nano_usd:cap
          ~state:Codex_cost.initial_cost_state json
      in
      let has_error = List.exists events ~f:is_error in
      let exceeded =
        match Codex_cost.check_cap ~budget_cap_nano_usd:cap state with
        | Codex_cost.Exceeded _ -> true
        | Within_cap -> false
      in
      Bool.equal has_error exceeded)

let prop_on_turn_error_precedes_final_result =
  QCheck2.Test.make
    ~name:"on_turn_completed: when Error is emitted it precedes Final_result"
    QCheck2.Gen.(triple gen_codex_usage_json gen_model_name gen_cap_nano_usd)
    (fun (usage_json, model, cap) ->
      let json = `Assoc [ ("usage", usage_json) ] in
      let { Codex_cost.events; _ } =
        Codex_cost.on_turn_completed ~model ~budget_cap_nano_usd:cap
          ~state:Codex_cost.initial_cost_state json
      in
      match events with
      | [ ev ] -> is_final_result ev
      | [ a; b ] -> is_error a && is_final_result b
      | [] | _ :: _ :: _ :: _ -> false)

(* ---------- Interleaving properties (sequences of turn.completed) ---------- *)

let fold_turns ~model ~cap usages =
  List.fold usages ~init:(Codex_cost.initial_cost_state, [])
    ~f:(fun (state, all_events) usage_json ->
      let json = `Assoc [ ("usage", usage_json) ] in
      let { Codex_cost.events; state } =
        Codex_cost.on_turn_completed ~model ~budget_cap_nano_usd:cap ~state json
      in
      (state, all_events @ events))

let gen_usage_seq =
  QCheck2.Gen.(list_size (int_range 0 30) gen_codex_usage_json)

let prop_interleaving_cumulative_monotone =
  QCheck2.Test.make ~count:200
    ~name:"interleaving: cumulative state never decreases across a sequence"
    QCheck2.Gen.(triple gen_usage_seq gen_model_name gen_cap_nano_usd)
    (fun (usages, model, cap) ->
      let _, ok =
        List.fold usages ~init:(Codex_cost.initial_cost_state, true)
          ~f:(fun (state, ok) usage_json ->
            let json = `Assoc [ ("usage", usage_json) ] in
            let { Codex_cost.state = state'; _ } =
              Codex_cost.on_turn_completed ~model ~budget_cap_nano_usd:cap
                ~state json
            in
            ( state',
              ok
              && Int64.(state'.cumulative_nano_usd >= state.cumulative_nano_usd)
            ))
      in
      ok)

let prop_interleaving_cumulative_equals_sum =
  QCheck2.Test.make ~count:200
    ~name:"interleaving: cumulative = Σ per-turn cost"
    QCheck2.Gen.(pair gen_usage_seq gen_model_name)
    (fun (usages, model) ->
      let final_state, _ = fold_turns ~model ~cap:None usages in
      let expected =
        match Codex_cost.model_pricing model with
        | None -> 0L
        | Some pricing ->
            List.fold usages ~init:0L ~f:(fun acc usage_json ->
                let usage = Codex_cost.usage_of_yojson usage_json in
                Int64.(acc + Codex_cost.cost_of_usage ~pricing usage))
      in
      Int64.equal final_state.cumulative_nano_usd expected)

let prop_interleaving_final_result_count_equals_turn_count =
  QCheck2.Test.make ~count:200
    ~name:"interleaving: # Final_result events = # turns"
    QCheck2.Gen.(triple gen_usage_seq gen_model_name gen_cap_nano_usd)
    (fun (usages, model, cap) ->
      let _, events = fold_turns ~model ~cap usages in
      Int.equal (List.count events ~f:is_final_result) (List.length usages))

(* Once cumulative crosses the cap, every subsequent turn should also emit
   Error: the state stays Exceeded forever (cumulative is monotone, cap is
   constant). *)
let prop_interleaving_error_is_sticky =
  QCheck2.Test.make ~count:200
    ~name:
      "interleaving: once cap is exceeded, every later turn also emits Error"
    QCheck2.Gen.(pair gen_usage_seq gen_model_name)
    (fun (usages, model) ->
      (* Pick a cap small enough that some sequence will cross it but not so
         small that the very first turn always crosses (interesting cases
         have a transition mid-sequence). *)
      let cap = Some 10_000L in
      let _, events_per_turn =
        List.fold usages ~init:(Codex_cost.initial_cost_state, [])
          ~f:(fun (state, acc) usage_json ->
            let json = `Assoc [ ("usage", usage_json) ] in
            let { Codex_cost.events; state } =
              Codex_cost.on_turn_completed ~model ~budget_cap_nano_usd:cap
                ~state json
            in
            (state, acc @ [ events ]))
      in
      let has_error events = List.exists events ~f:is_error in
      let rec stickiness_ok seen_error = function
        | [] -> true
        | turn :: rest ->
            let now_error = has_error turn in
            if seen_error && not now_error then false
            else stickiness_ok (seen_error || now_error) rest
      in
      stickiness_ok false events_per_turn)

let prop_interleaving_state_independent_of_order =
  QCheck2.Test.make ~count:200
    ~name:
      "interleaving: cumulative state is invariant under permutation (cost is \
       commutative)"
    QCheck2.Gen.(pair gen_usage_seq gen_model_name)
    (fun (usages, model) ->
      let s1, _ = fold_turns ~model ~cap:None usages in
      let s2, _ = fold_turns ~model ~cap:None (List.rev usages) in
      Int64.equal s1.cumulative_nano_usd s2.cumulative_nano_usd)

(* ---------- Runner ---------- *)

let () =
  let suite =
    [
      prop_cost_zero_tokens;
      prop_cost_negative_tokens_clamps;
      prop_cost_monotone_in_tokens;
      prop_cost_of_usage_equals_input_plus_output;
      prop_cost_of_usage_non_negative;
      prop_usage_of_yojson_total;
      prop_usage_of_yojson_non_negative_fields;
      prop_usage_codex_schema_decodes_alt_flat;
      prop_usage_responses_api_schema_decodes_nested;
      prop_usage_schema_priority_nested_wins_over_flat;
      prop_usage_schema_priority_flat_wins_over_alt;
      prop_cap_none_always_within;
      prop_cap_at_boundary_is_within;
      prop_cap_above_boundary_is_exceeded;
      prop_advance_monotone;
      prop_advance_negative_clamps;
      prop_on_turn_emits_exactly_one_final_result;
      prop_on_turn_state_monotone;
      prop_on_turn_unknown_model_no_cost_change;
      prop_on_turn_error_iff_exceeded;
      prop_on_turn_error_precedes_final_result;
      prop_interleaving_cumulative_monotone;
      prop_interleaving_cumulative_equals_sum;
      prop_interleaving_final_result_count_equals_turn_count;
      prop_interleaving_error_is_sticky;
      prop_interleaving_state_independent_of_order;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

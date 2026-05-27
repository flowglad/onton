open Base

(** Pure cost-tracking decision logic. See [codex_cost.mli] for contract. *)

(* ---------- Cost state ---------- *)

type cost_state = { cumulative_nano_usd : int64 }
[@@deriving show, eq, sexp_of, compare]

let initial_cost_state = { cumulative_nano_usd = 0L }

let advance s delta =
  let delta = if Int64.(delta < 0L) then 0L else delta in
  { cumulative_nano_usd = Int64.(s.cumulative_nano_usd + delta) }

(* ---------- Usage decoding ---------- *)

type usage = { input_tokens : int; output_tokens : int; reasoning_tokens : int }
[@@deriving show, eq, sexp_of, compare]

let zero_usage = { input_tokens = 0; output_tokens = 0; reasoning_tokens = 0 }
let int_member name json = Json.int_field name json

(* Reasoning tokens are reported under three schemas. In priority order:
   1. nested [output_tokens_details.reasoning_tokens] (OpenAI Responses API
      shape, surfaces in older / passthrough payloads),
   2. flat [reasoning_tokens] (intermediate shape),
   3. flat [reasoning_output_tokens] (the shape [codex exec --json] actually
      emits today — see codex-rs/exec/src/exec_events.rs::Usage).
   Take the first that decodes as an int; fall through to zero. *)
let reasoning_tokens_of_usage usage =
  let from_nested =
    match Json.field "output_tokens_details" usage with
    | Some (`Assoc _ as details) -> int_member "reasoning_tokens" details
    | _ -> None
  in
  let from_flat = int_member "reasoning_tokens" usage in
  let from_alt_flat = int_member "reasoning_output_tokens" usage in
  Option.first_some from_nested from_flat
  |> (fun acc -> Option.first_some acc from_alt_flat)
  |> Option.value ~default:0 |> Int.max 0

let usage_of_yojson json =
  match json with
  | `Assoc _ ->
      let count name =
        int_member name json |> Option.value ~default:0 |> Int.max 0
      in
      {
        input_tokens = count "input_tokens";
        output_tokens = count "output_tokens";
        reasoning_tokens = reasoning_tokens_of_usage json;
      }
  | _ -> zero_usage

(* ---------- Pricing & cost ---------- *)

type model_pricing = {
  input_nano_usd_per_1k : int64;
  output_nano_usd_per_1k : int64;
}
[@@deriving show, eq, sexp_of, compare]

let model_pricing = function
  | Some "gpt-5.4-mini" ->
      Some
        {
          input_nano_usd_per_1k = 750_000L;
          output_nano_usd_per_1k = 4_500_000L;
        }
  | Some "gpt-5.4" ->
      Some
        {
          input_nano_usd_per_1k = 2_500_000L;
          output_nano_usd_per_1k = 15_000_000L;
        }
  | Some "gpt-5.5" ->
      Some
        {
          input_nano_usd_per_1k = 5_000_000L;
          output_nano_usd_per_1k = 30_000_000L;
        }
  | _ -> None

let cost_nano_usd_for_tokens tokens nano_usd_per_1k =
  let tokens = Int.max 0 tokens in
  Int64.(of_int tokens * nano_usd_per_1k / 1000L)

let cost_of_usage ~pricing usage =
  let visible_output_tokens =
    Int.max 0 (usage.output_tokens - usage.reasoning_tokens)
  in
  let input =
    cost_nano_usd_for_tokens usage.input_tokens pricing.input_nano_usd_per_1k
  in
  let visible =
    cost_nano_usd_for_tokens visible_output_tokens
      pricing.output_nano_usd_per_1k
  in
  let reasoning =
    cost_nano_usd_for_tokens usage.reasoning_tokens
      pricing.output_nano_usd_per_1k
  in
  Int64.(input + visible + reasoning)

let float_usd_of_nano_usd nano_usd = Int64.to_float nano_usd /. 1_000_000_000.0

(* ---------- Cap decision ---------- *)

type cap_decision =
  | Within_cap
  | Exceeded of { cumulative_nano_usd : int64; cap_nano_usd : int64 }
[@@deriving show, eq, sexp_of, compare]

let check_cap ~budget_cap_nano_usd state =
  match budget_cap_nano_usd with
  | None -> Within_cap
  | Some cap when Int64.(state.cumulative_nano_usd > cap) ->
      Exceeded
        { cumulative_nano_usd = state.cumulative_nano_usd; cap_nano_usd = cap }
  | Some _ -> Within_cap

(* ---------- Turn-completed decision ---------- *)

type turn_decision = { events : Types.Stream_event.t list; state : cost_state }
[@@deriving show, eq, sexp_of, compare]

let turn_cost_for_payload ~model json =
  match model_pricing model with
  | None -> 0L
  | Some pricing ->
      let usage =
        match Json.field "usage" json with Some u -> u | None -> `Null
      in
      cost_of_usage ~pricing (usage_of_yojson usage)

let final_result =
  Types.Stream_event.Final_result
    { text = ""; stop_reason = Types.Stop_reason.End_turn }

let on_turn_completed ~model ~budget_cap_nano_usd ~state json =
  let added = turn_cost_for_payload ~model json in
  let state = advance state added in
  let events =
    match check_cap ~budget_cap_nano_usd state with
    | Within_cap -> [ final_result ]
    | Exceeded { cumulative_nano_usd; cap_nano_usd } ->
        [
          Types.Stream_event.Error
            (Printf.sprintf
               "Codex budget cap exceeded: cumulative cost $%.4f > cap $%.4f"
               (float_usd_of_nano_usd cumulative_nano_usd)
               (float_usd_of_nano_usd cap_nano_usd));
          final_result;
        ]
  in
  { events; state }

(* ---------- Inline shape tests (property tests live in test/) ---------- *)

let%test "advance is monotone non-decreasing" =
  let s = advance initial_cost_state 100L in
  let s' = advance s (-500L) in
  Int64.equal s'.cumulative_nano_usd 100L

let%test "usage_of_yojson zero on null / non-object" =
  equal_usage (usage_of_yojson `Null) zero_usage
  && equal_usage (usage_of_yojson (`Int 7)) zero_usage
  && equal_usage (usage_of_yojson (`List [ `Int 1 ])) zero_usage

let%test "usage_of_yojson zero when usage is empty object" =
  equal_usage (usage_of_yojson (`Assoc [])) zero_usage

let%test "usage_of_yojson nested reasoning_tokens schema" =
  let j =
    `Assoc
      [
        ("input_tokens", `Int 1);
        ("output_tokens", `Int 2);
        ("output_tokens_details", `Assoc [ ("reasoning_tokens", `Int 3) ]);
      ]
  in
  equal_usage (usage_of_yojson j)
    { input_tokens = 1; output_tokens = 2; reasoning_tokens = 3 }

let%test "usage_of_yojson flat reasoning_tokens schema" =
  let j =
    `Assoc
      [
        ("input_tokens", `Int 4);
        ("output_tokens", `Int 5);
        ("reasoning_tokens", `Int 6);
      ]
  in
  equal_usage (usage_of_yojson j)
    { input_tokens = 4; output_tokens = 5; reasoning_tokens = 6 }

let%test "usage_of_yojson alt-flat reasoning_output_tokens schema" =
  let j =
    `Assoc
      [
        ("input_tokens", `Int 0);
        ("output_tokens", `Int 0);
        ("reasoning_output_tokens", `Int 11);
      ]
  in
  equal_usage (usage_of_yojson j)
    { input_tokens = 0; output_tokens = 0; reasoning_tokens = 11 }

let%test "usage_of_yojson nested missing falls through to flat" =
  let j =
    `Assoc
      [ ("output_tokens_details", `Assoc []); ("reasoning_tokens", `Int 9) ]
  in
  Int.equal (usage_of_yojson j).reasoning_tokens 9

let%test "usage_of_yojson nested null falls through to flat" =
  let j =
    `Assoc [ ("output_tokens_details", `Null); ("reasoning_tokens", `Int 7) ]
  in
  Int.equal (usage_of_yojson j).reasoning_tokens 7

let%test "usage_of_yojson clamps negatives" =
  let j =
    `Assoc
      [
        ("input_tokens", `Int (-5));
        ("output_tokens", `Int (-3));
        ("reasoning_tokens", `Int (-1));
      ]
  in
  equal_usage (usage_of_yojson j) zero_usage

let%test "check_cap None is always Within_cap" =
  equal_cap_decision
    (check_cap ~budget_cap_nano_usd:None
       (advance initial_cost_state 1_000_000L))
    Within_cap

let%test "check_cap at boundary is Within_cap (closed cap)" =
  equal_cap_decision
    (check_cap ~budget_cap_nano_usd:(Some 100L)
       (advance initial_cost_state 100L))
    Within_cap

let%test "check_cap above boundary is Exceeded" =
  match
    check_cap ~budget_cap_nano_usd:(Some 100L) (advance initial_cost_state 101L)
  with
  | Exceeded { cumulative_nano_usd; cap_nano_usd } ->
      Int64.equal cumulative_nano_usd 101L && Int64.equal cap_nano_usd 100L
  | Within_cap -> false

let%test "on_turn_completed unknown model leaves state untouched" =
  let { state; events } =
    on_turn_completed ~model:None ~budget_cap_nano_usd:None
      ~state:initial_cost_state
      (`Assoc
         [
           ( "usage",
             `Assoc [ ("input_tokens", `Int 100); ("output_tokens", `Int 200) ]
           );
         ])
  in
  Int64.equal state.cumulative_nano_usd 0L
  && List.equal Types.Stream_event.equal events [ final_result ]

let%test "on_turn_completed survives malformed payload" =
  let { state; events } =
    on_turn_completed ~model:(Some "gpt-5.5") ~budget_cap_nano_usd:None
      ~state:initial_cost_state `Null
  in
  Int64.equal state.cumulative_nano_usd 0L
  && List.equal Types.Stream_event.equal events [ final_result ]

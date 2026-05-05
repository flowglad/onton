(** Pure cost-tracking decision logic for the Codex backend.

    No I/O, no exceptions: every value-returning function in this module is
    total over its declared input domain — including ill-formed JSON shapes
    handed to {!usage_of_yojson} and {!on_turn_completed}. The effectful handler
    ({!Codex_backend.run_streaming}) reads stdout, accumulates {!cost_state}
    across lines, and emits the events this module decides.

    Organization:
    - {b State} — abstract [cost_state] threaded through the decision.
    - {b Usage decoding} — total Yojson decoder for token-usage payloads.
    - {b Pricing & cost} — model rate table and per-usage cost computation.
    - {b Cap decision} — pure check against a budget cap.
    - {b Turn decision} — orchestration of the above into a [turn_decision]. *)

(** {2 Cost state} *)

type cost_state = private { cumulative_nano_usd : int64 }
[@@deriving show, eq, sexp_of, compare]
(** Cumulative cost in nano-USD across all priced [turn.completed] events
    observed so far in a spawn. Private to enforce monotonicity at construction
    time: the only ways to obtain a value are {!initial_cost_state} (zero) and
    {!advance} (clamped non-negative). *)

val initial_cost_state : cost_state

val advance : cost_state -> int64 -> cost_state
(** [advance s delta] adds [max 0 delta] to the cumulative cost. Negative
    [delta] is treated as zero so the cumulative is always monotone
    non-decreasing. *)

(** {2 Usage decoding}

    Codex's [turn.completed] payload has a [usage] sub-object whose schema has
    drifted: token counts may be missing, [output_tokens_details] may be absent
    or null, and reasoning tokens have been spelled three different ways across
    versions ([usage.output_tokens_details.reasoning_tokens],
    [usage.reasoning_tokens], [usage.reasoning_output_tokens]). *)

type usage = { input_tokens : int; output_tokens : int; reasoning_tokens : int }
[@@deriving show, eq, sexp_of, compare]

val zero_usage : usage

val usage_of_yojson : Yojson.Safe.t -> usage
(** Total decoder. Returns {!zero_usage} for any non-object input, missing keys,
    or non-int values. All token counts are clamped to [>= 0].

    For [reasoning_tokens] the decoder tries three schemas in priority order and
    returns the first that yields an int:
    + nested: [output_tokens_details.reasoning_tokens]
    + flat: [reasoning_tokens]
    + alt: [reasoning_output_tokens] *)

(** {2 Pricing & cost} *)

type model_pricing = {
  input_nano_usd_per_1k : int64;
  output_nano_usd_per_1k : int64;
}
[@@deriving show, eq, sexp_of, compare]

val model_pricing : string option -> model_pricing option
(** Pricing table for known Codex model names. [None] for unknown / unset models
    — callers treat that as "cost tracking disabled". Keep in sync with
    {!Codex_backend.auto_model}. *)

val cost_nano_usd_for_tokens : int -> int64 -> int64
(** [cost_nano_usd_for_tokens tokens nano_usd_per_1k] =
    [max 0 tokens * rate / 1000]. Saturates negative tokens to zero so the
    result is always non-negative. *)

val cost_of_usage : pricing:model_pricing -> usage -> int64
(** Total cost (input + visible output + reasoning) at the given pricing.
    Visible output tokens = [max 0 (output_tokens - reasoning_tokens)] so
    reasoning tokens are not double-counted when they are reported as a subset
    of the output stream. *)

val float_usd_of_nano_usd : int64 -> float
(** Convert nano-USD to USD as a float, for human-facing strings. *)

(** {2 Budget cap decision} *)

type cap_decision =
  | Within_cap
  | Exceeded of { cumulative_nano_usd : int64; cap_nano_usd : int64 }
[@@deriving show, eq, sexp_of, compare]

val check_cap : budget_cap_nano_usd:int64 option -> cost_state -> cap_decision
(** [check_cap ~budget_cap_nano_usd s] returns [Within_cap] when no cap is set
    or [s.cumulative_nano_usd <= cap]; otherwise [Exceeded]. The cap-equal case
    is intentionally [Within_cap] so the cap is a closed upper bound rather than
    a strict one. *)

(** {2 Turn-completed decision} *)

type turn_decision = { events : Types.Stream_event.t list; state : cost_state }
[@@deriving show, eq, sexp_of, compare]

val on_turn_completed :
  model:string option ->
  budget_cap_nano_usd:int64 option ->
  state:cost_state ->
  Yojson.Safe.t ->
  turn_decision
(** Pure decision for a [turn.completed] payload. Always emits a [Final_result];
    additionally prepends an [Error] when the post-advance cumulative cost has
    crossed [budget_cap_nano_usd]. The returned [state] is the post-advance
    state regardless of cap status, so cumulative costs keep accruing after a
    breach (the handler reacts to the [Error] by self-killing the spawn).

    Total over arbitrary [Yojson.Safe.t]: ill-formed payloads are decoded as
    {!zero_usage} (zero cost), and unknown models leave the state unchanged. *)

val create :
  model:string option ->
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  setsid_exec:string option ->
  Llm_backend.t
(** Create an LLM backend that uses the Codex CLI. [model], when provided, is
    passed to [codex exec] via the [-m] flag; [None] (or empty) lets the Codex
    CLI pick its own default. [timeout] is the maximum session duration in
    seconds before the process is killed. See {!Claude_backend.create} for
    [setsid_exec] semantics. *)

val parse_event : string -> Types.Stream_event.t list
(** Parse a single NDJSON line from Codex's JSON output. Exposed for testing. *)

val auto_model : complexity:int option -> string option
(** Codex's complexity → model ladder used to resolve [--model auto]. Exposed so
    the TUI can display the concrete model name a patch will run under. *)

type cost_state = Codex_cost.cost_state
(** Re-export of {!Codex_cost.cost_state}. Cost-tracking decision logic lives in
    {!Codex_cost}; this module is the effectful handler that drives it. *)

val initial_cost_state : cost_state

val parse_event_with_cost_tracking :
  model:string option ->
  budget_cap_nano_usd:int64 option ->
  cost_state:cost_state ->
  string ->
  Types.Stream_event.t list * cost_state
(** Parse a single NDJSON line, threading [cost_state] through. Delegates the
    [turn.completed] cost/cap decision to {!Codex_cost.on_turn_completed} — this
    function is just a thin JSON-line dispatcher. Exposed for testing. *)

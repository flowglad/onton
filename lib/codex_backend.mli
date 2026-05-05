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

type cost_state = { cumulative_usd : float }

val initial_cost_state : cost_state

val parse_event_with_cost_tracking :
  model:string option ->
  budget_cap_usd:float option ->
  cost_state:cost_state ->
  string ->
  Types.Stream_event.t list * cost_state
(** Parse a single NDJSON line while accumulating per-spawn Codex token cost.
    [turn.completed] usage is priced using the pinned model's current rates;
    when [budget_cap_usd] is crossed, the returned events include
    [Stream_event.Error]. Exposed for testing. *)

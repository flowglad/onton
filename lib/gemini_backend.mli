val create :
  model:string option ->
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  setsid_exec:string option ->
  Llm_backend.t
(** Create an LLM backend that uses the Gemini CLI. [model], when provided, is
    passed via [-m]; [None] (or empty) lets the Gemini CLI pick its own default.
    [timeout] is the maximum session duration in seconds before the process is
    killed. See {!Claude_backend.create} for [setsid_exec] semantics. *)

val parse_event : string -> Types.Stream_event.t list
(** Parse a single NDJSON line from Gemini's stream-json output. Exposed for
    testing. *)

val auto_model : complexity:int option -> string option
(** Gemini's complexity → model ladder used to resolve [--model auto]. Exposed
    so the TUI can display the concrete model name a patch will run under. *)

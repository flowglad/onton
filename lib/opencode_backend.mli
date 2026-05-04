val create :
  model:string option ->
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  setsid_exec:string option ->
  Llm_backend.t
(** Create an LLM backend that uses the OpenCode CLI. [model], when provided, is
    passed via [-m]; [None] (or empty) lets OpenCode pick its own default.
    [timeout] is the maximum session duration in seconds before the process is
    killed. See {!Claude_backend.create} for [setsid_exec] semantics. *)

val parse_event : string -> Types.Stream_event.t list
(** Parse a single NDJSON line from OpenCode's JSON output. Exposed for testing.
*)

val create :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  Llm_backend.t
(** Create an LLM backend that uses the OpenCode CLI. [timeout] is the maximum
    session duration in seconds before the process is killed. *)

val parse_event : string -> Types.Stream_event.t list
(** Parse a single NDJSON line from OpenCode's JSON output. Exposed for testing.
*)

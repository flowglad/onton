val create :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  setsid_exec:string option ->
  Llm_backend.t
(** Create an LLM backend that uses the Gemini CLI. [timeout] is the maximum
    session duration in seconds before the process is killed. See
    {!Claude_backend.create} for [setsid_exec] semantics. *)

val parse_event : string -> Types.Stream_event.t list
(** Parse a single NDJSON line from Gemini's stream-json output. Exposed for
    testing. *)

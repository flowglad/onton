val create :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  Llm_backend.t
(** Create an LLM backend that uses the Codex CLI. [timeout] is the maximum
    session duration in seconds before the process is killed. *)

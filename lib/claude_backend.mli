val create :
  name:string ->
  model:string option ->
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  setsid_exec:string option ->
  Llm_backend.t
(** Create an LLM backend that uses the Claude CLI. [model], when provided, is
    passed via [--model]; [None] (or empty) lets the Claude CLI pick its own
    default. [timeout] is the maximum session duration in seconds before the
    process is killed. [setsid_exec], when [Some path], routes the subprocess
    through the [onton-setsid-exec] shim so it leads its own process group and
    the whole tree can be reaped on teardown. *)

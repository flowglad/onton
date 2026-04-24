val create :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  setsid_exec:string option ->
  Llm_backend.t
(** Create an LLM backend that uses the Claude CLI. [timeout] is the maximum
    session duration in seconds before the process is killed. [setsid_exec],
    when [Some path], routes the subprocess through the [onton-setsid-exec] shim
    so it leads its own process group and the whole tree can be reaped on
    teardown. *)

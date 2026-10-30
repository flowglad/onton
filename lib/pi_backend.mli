(* @archlint.module interface
   @archlint.domain pi-backend *)

val create :
  model:string option ->
  effort:string option ->
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  setsid_exec:string option ->
  Llm_backend.t
(** Create an LLM backend that uses the Pi CLI. [model], when provided, is
    passed via [--model]; [None] (or empty) lets the Pi CLI pick its own
    default. [effort] is passed via [--thinking]; [None] leaves Pi's thinking
    setting unchanged. [timeout] is the maximum idle duration in seconds without
    stdout or stderr output before the process is killed. See
    {!Claude_backend.create} for [setsid_exec] semantics.

    Every execution writes a versioned delivery envelope and exports its path as
    [ONTON_PI_CONTEXT] for the installed pi-onton extension, which is loaded
    automatically from [share/onton/pi-onton] relative to the resolved
    executable. [ONTON_PI_EXTENSION] overrides the installed entrypoint for
    development. *)

val parse_event : string -> Types.Stream_event.t list
(** Parse a single NDJSON line from Pi's JSON output. Exposed for testing. *)

val auto_model : complexity:int option -> string option
(** Pi's complexity → model ladder used to resolve [--model auto]. Exposed so
    the TUI can display the concrete model name a patch will run under. *)

val extension_path : executable:string -> string
(** Locate the installed extension for an executable, following symlinks.
    Supports prefix/bin and flat release archive layouts; a configured override
    takes precedence. Missing/incomplete installations fail with an actionable
    error before starting Pi. *)

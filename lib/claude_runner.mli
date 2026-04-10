open Base

(** Claude subprocess runner.

    Spawns and manages Claude CLI processes for patches. Each patch gets at most
    one Claude process (one fiber). The runner uses [--resume <session_id>] to
    resume a specific session by its ID.

    Unlike [--print] mode, we use [-p] which runs Claude in session-saving mode.
    This requires a PTY (allocated via [script -q /dev/null]) but enables
    session persistence. The session ID is captured from the [system/init]
    streaming event and stored for subsequent [--resume] calls.

    Design decision: one fiber per Claude process for natural backpressure —
    busy patches don't get new work. *)

val run :
  process_mgr:_ Eio.Process.mgr ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  patch_id:Types.Patch_id.t ->
  prompt:string ->
  resume_session:string option ->
  Llm_backend.result
(** Spawn a Claude CLI process for [patch_id] in directory [cwd].

    If [resume_session] is [Some id], the session is resumed with
    [--resume <id>]. Otherwise a new session is created.

    Returns a {!Llm_backend.result} with exit code and captured stdout/stderr.
*)

val run_streaming :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  patch_id:Types.Patch_id.t ->
  prompt:string ->
  resume_session:string option ->
  on_event:(Types.Stream_event.t -> unit) ->
  Llm_backend.result
(** Like {!run} but uses [--output-format stream-json]. Each NDJSON line is
    parsed into a {!Types.Stream_event.t} and passed to [on_event] as it
    arrives. The returned {!Llm_backend.result} has an empty [stdout] since
    output was consumed incrementally. The [on_event] callback will receive a
    {!Types.Stream_event.Session_init} with the session ID from the first
    streaming event. If [got_events] is [false] on return, the [--resume] likely
    failed to find the session. *)

val parse_stream_event : string -> Types.Stream_event.t option
(** Parse a single NDJSON line from Claude's stream-json output into a
    {!Types.Stream_event.t}. Returns [None] for unrecognized or malformed lines.
*)

val strip_ansi : string -> string
(** Strip ANSI escape sequences from PTY output. Exposed for testing. *)

val pty_wrap : string list -> string list
(** Wrap a command in [script -q /dev/null] for PTY allocation. Exposed for
    testing. *)

val build_args : prompt:string -> resume_session:string option -> string list
(** Build the CLI argument list for the Claude process. Exposed for testing. *)

val build_stream_args :
  prompt:string -> resume_session:string option -> string list
(** Build the CLI argument list for stream-json output mode. Exposed for
    testing. *)

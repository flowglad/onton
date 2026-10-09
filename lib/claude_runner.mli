(* @archlint.module interface
   @archlint.domain claude-runner *)

open Base

(** Claude subprocess runner.

    Spawns and manages Claude CLI processes for patches. Each patch gets at most
    one Claude process (one fiber). The runner uses [--resume <session_id>] to
    resume a specific session by its ID.

    Unlike [--print] mode, we use [-p] which runs Claude in session-saving mode
    so [--resume <session_id>] can rehydrate a prior turn. The session ID is
    captured from the [system/init] streaming event and stored for subsequent
    [--resume] calls.

    Design decision: one fiber per Claude process for natural backpressure —
    busy patches don't get new work. *)

val auto_model : complexity:int option -> string option
(** Map a 1/2/3 complexity to a Claude model alias. [None] complexity (and any
    out-of-range value) is treated as the strongest tier — be conservative.
    Returns [Some "haiku"] / [Some "sonnet"] / [Some "opus"] — Claude Code's
    stable aliases. *)

val run_streaming :
  model:string option ->
  effort:string option ->
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  setsid_exec:string option ->
  project_name:string ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  patch_id:Types.Patch_id.t ->
  prompt:string ->
  resume_session:string option ->
  session_uuid:string ->
  complexity:int option ->
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
(** Strip ANSI escape sequences and stray control characters from a line.
    Exposed for testing. *)

val build_args :
  getenv_opt:(string -> string option) ->
  model:string option ->
  effort:string option ->
  complexity:int option ->
  prompt:string ->
  resume_session:string option ->
  string list
(** Build the CLI argument list for the Claude process. Exposed for testing. *)

val build_stream_args :
  getenv_opt:(string -> string option) ->
  model:string option ->
  effort:string option ->
  complexity:int option ->
  prompt:string ->
  minted_session_id:string option ->
  resume_session:string option ->
  string list
(** Build the CLI argument list for stream-json output mode. Exposed for
    testing. [minted_session_id] and [resume_session] are mutually exclusive:
    fresh spawns pass the former, resumes pass the latter. *)

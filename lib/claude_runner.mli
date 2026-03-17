open Base

(** Claude subprocess runner.

    Spawns and manages Claude CLI processes for patches. Each patch gets at most
    one Claude process (one fiber). The runner uses [--continue] to resume the
    most recent session in the working directory (worktree).

    Unlike [--print] mode, we use [-p] which runs Claude in session-saving mode.
    This requires a PTY (allocated via [script -q /dev/null]) but enables
    [--continue] to find and resume prior sessions.

    Design decision: one fiber per Claude process for natural backpressure —
    busy patches don't get new work. *)

type result = {
  exit_code : int;
  stdout : string;
  stderr : string;
  got_events : bool;
}
[@@deriving show, eq, sexp_of, compare]

val run :
  process_mgr:_ Eio.Process.mgr ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  patch_id:Types.Patch_id.t ->
  prompt:string ->
  continue:bool ->
  result
(** Spawn a Claude CLI process for [patch_id] in directory [cwd].

    If [continue] is [true], the session is resumed with [--continue]. Otherwise
    a new session is created.

    Returns a {!result} with exit code and captured stdout/stderr. *)

val run_streaming :
  process_mgr:_ Eio.Process.mgr ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  patch_id:Types.Patch_id.t ->
  prompt:string ->
  continue:bool ->
  on_event:(Types.Stream_event.t -> unit) ->
  result
(** Like {!run} but uses [--output-format stream-json]. Each NDJSON line is
    parsed into a {!Types.Stream_event.t} and passed to [on_event] as it
    arrives. The returned {!result} has an empty [stdout] since output was
    consumed incrementally. If [got_events] is [false] on return, the
    [--continue] likely failed to find a session. *)

val parse_stream_event : string -> Types.Stream_event.t option
(** Parse a single NDJSON line from Claude's stream-json output into a
    {!Types.Stream_event.t}. Returns [None] for unrecognized or malformed lines.
*)

val strip_ansi : string -> string
(** Strip ANSI escape sequences from PTY output. Exposed for testing. *)

val pty_wrap : string list -> string list
(** Wrap a command in [script -q /dev/null] for PTY allocation. Exposed for
    testing. *)

val build_args : prompt:string -> continue:bool -> string list
(** Build the CLI argument list for the Claude process. Exposed for testing. *)

val build_stream_args : prompt:string -> continue:bool -> string list
(** Build the CLI argument list for stream-json output mode. Exposed for
    testing. *)

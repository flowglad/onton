open Base

(** Generic LLM backend interface.

    A backend wraps an LLM CLI's streaming invocation behind a common type. The
    process manager, clock, and timeout are captured at construction time so
    that the record type avoids Eio's unquantifiable row variables. *)

type result = {
  exit_code : int;
  stdout : string;
      (** Bounded raw stdout capture from streaming backends. This is retained
          for diagnostics when a backend exits cleanly without parseable events;
          it may be truncated. *)
  stderr : string;
  got_events : bool;
  saw_final_result : bool;
  timed_out : bool;
}
[@@deriving show, eq, sexp_of, compare]

val resolve_auto_model :
  model:string option ->
  complexity:int option ->
  auto_model:(complexity:int option -> string option) ->
  string option
(** Resolve the [--model] sentinel ["auto"] (case-insensitive) into a concrete
    model name using [auto_model] (each backend supplies its own complexity →
    model mapping). All other model values, including [None] and the empty
    string, pass through unchanged so the backend's own default still wins. *)

val redact_env : string array -> string array

val emit_spawn_started :
  patch_id:Types.Patch_id.t ->
  session_uuid:string option ->
  prompt:string ->
  args:string list ->
  env:string array ->
  unit

val spawn_and_stream :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  env:string array ->
  setsid_exec:string option ->
  args:string list ->
  session_uuid:string option ->
  patch_id:Types.Patch_id.t ->
  process_line:(string -> Types.Stream_event.t list) ->
  on_event:(Types.Stream_event.t -> unit) ->
  result
(** Spawn a subprocess, read NDJSON lines from stdout, and stream parsed events.
    Each stdout line is passed to [process_line] which returns events to forward
    to [on_event]. Handles pipe setup, stdin EOF, stderr capture, and exit code
    extraction. Stdout allows large single-line JSON events from CLIs such as
    Codex; stderr is capped and truncated. The process is killed after [timeout]
    seconds.

    When [setsid_exec] is supplied, [args] is prefixed with that path (a tiny
    OCaml shim that calls [setsid(2)] before exec'ing). The child then leads its
    own process group, and teardown sends [kill(2)] to the whole group so
    tool-call grandchildren (e.g. Bash-spawned shells) are reaped rather than
    reparented to PID 1. *)

type t = {
  name : string;
  run_streaming :
    project_name:string ->
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    patch_id:Types.Patch_id.t ->
    prompt:string ->
    resume_session:string option ->
    session_uuid:string option ->
    complexity:int option ->
    on_event:(Types.Stream_event.t -> unit) ->
    result;
}
(** [complexity] is the gameplan-author's 1/2/3 estimate for this patch. When
    the user passes [--model auto], each backend uses [complexity] to pick a
    backend-specific model: harder patches get stronger models. [None] means the
    gameplan didn't specify (legacy gameplans, ad-hoc operations) — the backend
    should treat that as the highest tier. *)

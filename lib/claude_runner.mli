open Base

(** Claude subprocess runner.

    Spawns and manages Claude CLI processes for patches. Each patch gets at most
    one Claude process (one fiber). The runner uses session resumption: if a
    patch already has a session ID, the process is resumed with [--resume].

    Design decision: one fiber per Claude process for natural backpressure —
    busy patches don't get new work. *)

type result = {
  session_id : Types.Session_id.t;
  exit_code : int;
  stdout : string;
  stderr : string;
}
[@@deriving show, eq, sexp_of, compare]

val run :
  process_mgr:_ Eio.Process.mgr ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  patch_id:Types.Patch_id.t ->
  prompt:string ->
  session_id:Types.Session_id.t option ->
  result
(** Spawn a Claude CLI process for [patch_id] in directory [cwd].

    If [session_id] is [Some id], the session is resumed with [--resume id].
    Otherwise a new session is created.

    Returns a {!result} with the session ID (parsed from Claude's output or
    generated), exit code, and captured stdout/stderr. *)

val generate_session_id : unit -> Types.Session_id.t
(** Generate a fresh session ID. *)

val build_args :
  prompt:string -> session_id:Types.Session_id.t option -> string list
(** Build the CLI argument list for the Claude process. Exposed for testing. *)

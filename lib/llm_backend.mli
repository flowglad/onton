open Base

(** Generic LLM backend interface.

    A backend wraps an LLM CLI's streaming invocation behind a common type. The
    process manager is captured at construction time so that the record type
    avoids Eio's unquantifiable row variables. *)

type result = {
  exit_code : int;
  stdout : string;
  stderr : string;
  got_events : bool;
}
[@@deriving show, eq, sexp_of, compare]

val spawn_and_stream :
  process_mgr:_ Eio.Process.mgr ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  args:string list ->
  process_line:(string -> Types.Stream_event.t list) ->
  on_event:(Types.Stream_event.t -> unit) ->
  result
(** Spawn a subprocess, read NDJSON lines from stdout, and stream parsed events.
    Each stdout line is passed to [process_line] which returns events to forward
    to [on_event]. Handles pipe setup, stdin EOF, stderr capture, and exit code
    extraction. *)

type t = {
  name : string;
  run_streaming :
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    patch_id:Types.Patch_id.t ->
    prompt:string ->
    continue:bool ->
    on_event:(Types.Stream_event.t -> unit) ->
    result;
}

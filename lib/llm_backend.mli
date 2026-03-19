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

type t = {
  run_streaming :
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    patch_id:Types.Patch_id.t ->
    prompt:string ->
    continue:bool ->
    on_event:(Types.Stream_event.t -> unit) ->
    result;
}

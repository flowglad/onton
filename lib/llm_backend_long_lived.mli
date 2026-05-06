open Base

(** Generic long-lived LLM backend interface.

    A long-lived backend owns a persistent subprocess across a patch lifetime.
    The gameplan-stable and patch-stable prompt layers are supplied once at
    {!start}; each subsequent {!prompt} sends only the turn-dynamic suffix. *)

type handle
(** Abstract session handle for a live long-lived backend process. *)

type result = Llm_backend.result [@@deriving show, eq, sexp_of, compare]

type t = {
  name : string;
  start :
    'tag 'clock.
    process_mgr:[> 'tag Eio.Process.mgr_ty ] Eio.Resource.t ->
    clock:'clock Eio.Time.clock ->
    timeout:float ->
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    env:string array ->
    project_name:string ->
    patch_id:Types.Patch_id.t ->
    gameplan:string ->
    patch:string ->
    resume_session:string option ->
    complexity:int option ->
    handle;
  prompt :
    'tag 'clock.
    handle ->
    process_mgr:[> 'tag Eio.Process.mgr_ty ] Eio.Resource.t ->
    clock:'clock Eio.Time.clock ->
    timeout:float ->
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    env:string array ->
    prompt:string ->
    on_event:(Types.Stream_event.t -> unit) ->
    result;
  abort : handle -> unit;
  shutdown : handle -> unit;
}
(** [complexity] is the gameplan-author's 1/2/3 estimate for this patch. When
    the user passes [--model auto], a long-lived backend can resolve the actual
    model at {!start} time from this value. [None] means the gameplan did not
    specify a complexity. *)

val placeholder : name:string -> t
(** Placeholder implementation for incremental integration. Every lifecycle
    function raises [Failure "not implemented; see patch 4"]. *)

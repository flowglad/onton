(* @archlint.module interface
   @archlint.domain branch-reconcile *)

val run :
  context:string ->
  guidance:string list ->
  backend:Llm_backend.t ->
  on_event:(Types.Stream_event.t -> unit) ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  complexity:int option ->
  resume_session:string option ->
  session_uuid:string ->
  turn:Branch_reconcile.repair_turn ->
  read_head:(unit -> string option) ->
  now:(unit -> float) ->
  Branch_reconcile.event
(** Run one claimed diagnostic, content-repair or history-recovery turn. Session
    identity and stream collection belong to the patch session driver; this
    adapter never selects or mints a separate conversation. Stream events reach
    the caller. Does not invoke ordinary session completion or publication.
    Cancellation propagates with the claimed repair checkpoint intact; the next
    dispatch must inspect Git. *)

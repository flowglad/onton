(* @archlint.module interface
   @archlint.domain branch-reconcile *)

val run :
  backend:Llm_backend.t ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  complexity:int option ->
  turn:Branch_reconcile.repair_turn ->
  read_head:(unit -> string option) ->
  now:(unit -> float) ->
  Branch_reconcile.event
(** Run one claimed content-repair or history-recovery turn. Does not invoke
    ordinary session completion or publication. Cancellation propagates with the
    claimed repair checkpoint intact; the next dispatch must inspect Git. *)

(* @archlint.module shell
   @archlint.domain worktree-parser *)

val run :
  process_mgr:_ Eio.Process.mgr ->
  clock:float Eio.Time.clock_ty Eio.Time.clock ->
  env:string array ->
  string list ->
  int * string * string
(** Capture a command through the installed onton-setsid-exec supervisor. On
    return or cancellation, its process group has terminated and been reaped
    before the supervisor is reaped. A missing supervisor fails closed. *)

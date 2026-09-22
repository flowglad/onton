(* @archlint.module interface
   @archlint.domain worktree-lifecycle *)

open Base

type checkout
(** Validated linked checkout. Ownership is persisted outside its working
    directory, so an interrupted mount or creation retains the owning backend.
*)

val path : checkout -> string
val owner : checkout -> Worktree_lifecycle.config

module type S = sig
  val inspect :
    path:string -> branch:Types.Branch.t -> (checkout option, string) Result.t
  (** Repairs recoverable state, then validates root, repository and branch.
      [Ok None] means absent; errors must never be treated as absence. *)

  val materialize :
    path:string ->
    branch:Types.Branch.t ->
    expected_local:string option ->
    Start_point_plan.action ->
    checkout * bool
  (** Executes an approved action. Failure/cancellation can leave a branch or
      partial checkout; inspect before retrying. Never rolls back branch history
      on retry. A failed remote reset restores the approved old ref with
      compare-and-swap, preserving concurrent ref changes. Once a failed simgit
      child is reaped and required ref rollback has completed, unlock and
      ordinary remove recover clean/absent targets; dirty files or cleanup
      failures retain a cleanup-pending record that inspection can resume across
      restarts. *)

  val list : unit -> (string * Types.Branch.t) list
  val remove : discard:bool -> checkout -> unit

  val prune_stale_for_branch : Types.Branch.t -> unit
  (** Removes a stale registration for the requested branch when its working
      directory is gone, using the recorded owning backend when present and
      native Git otherwise. Ownership metadata is removed with the registration.
      Unreadable ownership also falls back to native Git. Registrations for
      every other branch are left untouched; backend-command and
      metadata-deletion errors propagate. *)

  val reconcile : unit -> unit
end

val make :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:float Eio.Time.clock_ty Eio.Time.clock ->
  process_mgr:_ Eio.Process.mgr ->
  repo_root:string ->
  config:Worktree_lifecycle.config ->
  timeout_seconds:float ->
  (module S)
(** Preflights the configured executable. Commands use argv, a clean Git
    environment, closed stdin, and a bounded lifetime. *)

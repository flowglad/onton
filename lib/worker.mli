(* @archlint.module interface
   @archlint.domain worker-transport *)

type env = {
  input : Eio_unix.source_ty Eio.Resource.t;
  output : Eio_unix.sink_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Time.clock;
  process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  gameplan : Types.Gameplan.t;
  main_branch : Types.Branch.t;
  repo_root : string;
  project_name : string;
  backend : Llm_backend.t;
  observe_forge :
    branch:Types.Branch.t ->
    (Branch_reconcile.workspace_forge * Types.Pr_number.t option, string) result;
  ack_timeout : float;
}
(** Explicit patch worker environment. No local scheduler, poller, control
    socket, or periodic snapshot fiber is constructed by this entry point. *)

val run : env -> unit
(** Host loss or an unacknowledged checkpoint raises after cancelling active
    descendants. The spawning host must confirm termination before replacement.
*)

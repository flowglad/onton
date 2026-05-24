(** Shared run-level capabilities captured at functor instantiation time. *)
module type S = sig
  val runtime : Runtime.t
  val clock : float Eio.Time.clock_ty Eio.Time.clock
  val fs : Eio.Fs.dir_ty Eio.Path.t
  val project_name : string
  val user_config : User_config.t
  val worktree_mutex : Eio.Mutex.t
  val hook_mutex : Eio.Mutex.t

  val fetch_mutex : Eio.Mutex.t
  (** Single mutex shared across every fiber that issues [git fetch] against the
      managed repo's ref store. See [Run_env.S.fetch_mutex] in the
      implementation for the race it serializes. *)
end

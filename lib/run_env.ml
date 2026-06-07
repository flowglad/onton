(* @archlint.module interface
   @archlint.domain run-env *)

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
      managed repo's ref store (the managed repo root + every per-patch
      worktree). Git worktrees share the parent repo's [refs/remotes/origin/*]
      directory, so concurrent [git fetch] processes race on the
      compare-and-swap update and the loser fails with
      ["cannot lock ref ...: is at X but expected Y"]. Held by
      {!Worktree.fetch_origin} via [~fetch_lock]. *)
end

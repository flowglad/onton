(* @archlint.module shell
   @archlint.domain orchestrator *)

module type S = sig
  val persist : Runtime.snapshot -> (unit, string) result

  val record_session_id :
    patch_id:Types.Patch_id.t -> session_id:string -> (unit, string) result
end

module Local (Config : sig
  val project_name : string
end) : S = struct
  let snapshot_path = Project_store.snapshot_path Config.project_name

  let persist snapshot =
    try
      Project_store.ensure_dir (Stdlib.Filename.dirname snapshot_path);
      Persistence.save_snapshot ~path:snapshot_path snapshot
    with exn ->
      if Worktree.has_cancellation exn then raise exn
      else Error (Base.Exn.to_string exn)

  let record_session_id ~patch_id ~session_id =
    Persistence.record_session_id ~snapshot_path ~patch_id ~session_id
end

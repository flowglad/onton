(* @archlint.module exempt
   @archlint.exempt-reason effect-facade *)

module type S = sig
  val persist : Runtime.snapshot -> (unit, string) result

  val clear_session_id : patch_id:Types.Patch_id.t -> (unit, string) result
  (** Durably invalidate the rejected backend identity for this patch before
      returning [Ok]. Recovery must not restore it from either the snapshot or a
      sidecar. Local persistence uses a sidecar tombstone over the old snapshot;
      workers checkpoint the cleared identity through the host acknowledgement.
      A later accepted conversation may record a new identity. *)

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

  let clear_session_id ~patch_id =
    Persistence.clear_session_id ~snapshot_path ~patch_id

  let record_session_id ~patch_id ~session_id =
    Persistence.record_session_id ~snapshot_path ~patch_id ~session_id
end

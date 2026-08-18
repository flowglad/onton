(* @archlint.module interface
   @archlint.domain orchestrator *)

open Types

(** Pure reconciliation of the freshly loaded gameplan with state persisted in a
    runtime snapshot.

    Runtime-added planned patches use the reserved [addN] id namespace. They
    exist only in the snapshot, so blindly replacing the snapshot gameplan with
    the source gameplan on resume loses them while leaving their agents behind.
    This module preserves those patches and can reconstruct legacy snapshots
    that were already damaged by the old resume behavior. *)

type missing_patch = {
  patch_id : Patch_id.t;
  branch : Branch.t;
  dependencies : Patch_id.t list;
}
[@@deriving show, eq]

type repair =
  | Preserved_snapshot_patch of Patch_id.t
  | Reconstructed_missing_patch of Patch_id.t
[@@deriving show, eq]

type result = { gameplan : Gameplan.t; repairs : repair list }
[@@deriving show, eq]

val added_patch_event_message : Patch.t -> string
(** Stable activity-log encoding used by both the runtime patch producer and
    legacy resume reconstruction. *)

val reconcile :
  loaded:Gameplan.t ->
  persisted:Gameplan.t ->
  missing_patches:missing_patch list ->
  activity_log:Activity_log.t ->
  result
(** [reconcile] keeps [loaded] authoritative for its project metadata and all
    patch ids it contains, then appends runtime-added [addN] patches found only
    in [persisted].

    A [missing_patch] absent from both gameplans is reconstructed only when its
    id is a canonical runtime-added [addN] id and its branch matches
    {!Gameplan.branch_of_id}. The most recent matching "Added patch" activity
    event supplies its title and dependency list when available; otherwise a
    safe recovery description and the graph dependencies are used. Unknown and
    duplicate, self-referential, and cyclic dependencies are removed, so the
    returned gameplan is always referentially valid. Snapshot-only patches that
    collide with an existing branch are skipped.

    The function is total and idempotent. *)

(* @archlint.module interface
   @archlint.domain prune-decision *)

open Base
open Types

(** Pure decisions for pruning persisted project state. Handlers are responsible
    for reading snapshots and locks; this module only classifies already-decoded
    snapshot data. *)

type project_status = All_terminal | Not_terminal | No_patches
[@@deriving show, eq, sexp_of, compare]

type recovery_ref = private {
  name : string;
  revision : Branch_reconcile.Commit.t;
}
[@@deriving eq, compare, sexp_of]

type reclamation =
  | Retain of Branch_reconcile.Commit.t list
  | Reclaim of recovery_ref list
[@@deriving eq, compare, sexp_of]

val parse_recovery_refs : string -> (recovery_ref list, string) Result.t
(** Decode ref name, object ID, symbolic target and object type separated by NUL
    bytes. Only unique, direct commit refs in the recovery namespace are
    accepted; malformed, symbolic and non-commit entries are errors. *)

val repository_within_project : project_dir:string -> git_dir:string -> bool
(** Whether deleting the canonical project directory would remove the canonical
    common Git directory. Paths must already be absolute and normalized. *)

val recovery_dependencies :
  Patch_agent.t Map.M(Patch_id).t -> Branch_reconcile.Commit.t list
(** Revisions needed by every agent in a surviving project, including merged
    ancestors of live patches and compatibility anchor history during migration.
*)

val plan_reclamation :
  project:string ->
  protected_projects:string list ->
  inventory:recovery_ref list ->
  required:Branch_reconcile.Commit.t list ->
  reachability:(Branch_reconcile.Commit.t * recovery_ref list) list ->
  (reclamation, string) Result.t
(** Each required revision needs one successful [--contains] observation bound
    to the captured inventory. Keep a project if its refs are the only recovery
    refs retaining any surviving checkpoint revision. Only locked projects in
    [protected_projects] can supply independent anchors. Foreign recovery refs
    are never deletion candidates. The caller must hold the retained owners'
    locks through observation and deletion, and compare-and-delete captured
    tips. *)

val classify_snapshot :
  patches:Patch.t list ->
  agents:Patch_agent.t Map.M(Patch_id).t ->
  closed_patch_ids:Patch_id.t list ->
  project_status
(** Classify a decoded project snapshot for pruning.

    A project is [All_terminal] only when the gameplan has at least one patch
    and every gameplan patch has a corresponding agent that is either merged in
    the snapshot or whose PR was observed closed during the prune refresh.
    Missing agents and unsettled reconciliation owners count as [Not_terminal],
    including interventions that still retain recoverable work. *)

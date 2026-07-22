(* @archlint.module interface
   @archlint.domain prune-decision *)

open Base
open Types

(** Pure decisions for pruning persisted project state. Handlers are responsible
    for reading snapshots and locks; this module only classifies already-decoded
    snapshot data. *)

type project_status = All_terminal | Not_terminal | No_patches
[@@deriving show, eq, sexp_of, compare]

val classify_snapshot :
  patches:Patch.t list ->
  agents:Patch_agent.t Map.M(Patch_id).t ->
  closed_patch_ids:Patch_id.t list ->
  project_status
(** Classify a decoded project snapshot for pruning.

    A project is [All_terminal] only when the gameplan has at least one patch
    and every gameplan patch has a corresponding agent that is either merged in
    the snapshot or whose PR was observed closed during the prune refresh.
    Missing agents count as [Not_terminal]. *)

open Base
open Types

(** Pure decisions for pruning persisted project state. Handlers are responsible
    for reading snapshots and locks; this module only classifies already-decoded
    snapshot data. *)

type project_status = All_merged | Not_merged | No_patches
[@@deriving show, eq, sexp_of, compare]

val classify_snapshot :
  patches:Patch.t list ->
  agents:Patch_agent.t Map.M(Patch_id).t ->
  project_status
(** Classify a decoded project snapshot for pruning.

    A project is [All_merged] only when the gameplan has at least one patch and
    every gameplan patch has a corresponding merged agent. Missing agents count
    as [Not_merged]. *)

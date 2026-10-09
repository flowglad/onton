(* @archlint.module interface
   @archlint.domain execution-mode *)
open Types

type t [@@deriving sexp_of, show, eq]

val mainline : t
val root : t -> Patch_id.t option
val is_root : t -> Patch_id.t -> bool
val is_descendant : t -> Patch_id.t -> bool

val branch_only_published : t -> Patch_agent.t -> bool
(** Confirmed publication in a feature descendant workflow. Mainline and root
    patches still require PR creation after publication. *)

val infer : Graph.t -> (t, string) result
(** Validate a nonempty graph with a sole root reachable from every patch. *)

val restore : Graph.t -> Patch_id.t option -> (t, string) result
(** Missing persisted selection means mainline; a stored root must still match.
*)

val infer_gameplan : Gameplan.t -> (t, string) result
(** Infer the implementation root, excluding the deterministic publication
    prerequisite. Publication retains the ordinary PR lifecycle and targets
    main; implementation remains behind its merge-required dependencies. *)

val restore_gameplan : Gameplan.t -> Patch_id.t option -> (t, string) result
(** Restore the persisted implementation root, including publication metadata.
    Missing selection retains mainline behavior. *)

val terminal :
  t ->
  Patch_id.t ->
  branch_of:(Patch_id.t -> Branch.t) ->
  main:Branch.t ->
  Branch.t

(** Open dependencies stop at the integration boundary; the root remains a
    publication dependency in [deps_satisfied]. *)

val open_deps :
  t ->
  Graph.t ->
  Patch_id.t ->
  has_merged:(Patch_id.t -> bool) ->
  Patch_id.t list

val base :
  t ->
  Graph.t ->
  Patch_id.t ->
  has_merged:(Patch_id.t -> bool) ->
  branch_of:(Patch_id.t -> Branch.t) ->
  main:Branch.t ->
  Branch.t option

val deps_satisfied :
  t ->
  Graph.t ->
  Patch_id.t ->
  has_merged:(Patch_id.t -> bool) ->
  has_pr:(Patch_id.t -> bool) ->
  bool

val additions_allowed :
  t -> Graph.t -> construction_open:bool -> dependencies:Patch_id.t list -> bool

val descendants_complete :
  t -> Graph.t -> has_merged:(Patch_id.t -> bool) -> bool

val integration_ready :
  t ->
  construction_open:bool ->
  ignore_inflight:bool ->
  max_failures:int ->
  terminal:Branch.t ->
  Patch_agent.t ->
  bool

val root_ready :
  t ->
  Graph.t ->
  has_merged:(Patch_id.t -> bool) ->
  pending_integrations:bool ->
  Patch_agent.t ->
  bool

val validate_terminal :
  t ->
  branch_of:(Patch_id.t -> Branch.t) ->
  main:Branch.t ->
  (unit, string) result

val observation_pending :
  ?confirmed_remote_head:string -> t -> Patch_agent.t -> string option -> bool
(** Feature-mode publications require an observation of the exact expected
    commit. An intermediate head cannot settle a newer publication. *)

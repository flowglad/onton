open Base

(** Dependency graph over patches.

    Encodes the spec fragment: deps, depends-on, deps-satisfied, open-pr-deps,
    sole-open-dep, initial-base. *)

type t [@@deriving sexp_of]

val of_patches : Types.Patch.t list -> t
(** Build a graph from a list of patches. Patches not in the list have no deps
    (per spec: "Added patches have no deps. all p | ~in-gameplan p -> #(deps p)
    = 0"). *)

val deps : t -> Types.Patch_id.t -> Types.Patch_id.t list
(** [deps t p] returns the direct dependencies of [p]. Returns [[]] if [p] is
    not in the gameplan. *)

val depends_on : t -> Types.Patch_id.t -> dep:Types.Patch_id.t -> bool
(** [depends_on t p d] is true iff [d] is in [deps t p]. *)

val open_pr_deps :
  t ->
  Types.Patch_id.t ->
  has_merged:(Types.Patch_id.t -> bool) ->
  Types.Patch_id.t list
(** [open_pr_deps t p ~has_merged] returns the subset of [deps t p] whose
    patches have not yet merged. [has_merged] is a predicate over patch ids. *)

val deps_satisfied :
  t ->
  Types.Patch_id.t ->
  has_merged:(Types.Patch_id.t -> bool) ->
  has_pr:(Types.Patch_id.t -> bool) ->
  bool
(** [deps_satisfied t p ~has_merged ~has_pr] is true when:
    - [#(open_pr_deps t p) <= 1], AND
    - every dep has either merged or has an open PR. *)

val sole_open_dep :
  t ->
  Types.Patch_id.t ->
  has_merged:(Types.Patch_id.t -> bool) ->
  Types.Patch_id.t
(** [sole_open_dep t p ~has_merged] returns the single unmerged dependency when
    there is exactly one. Raises if there is not exactly one. *)

val initial_base :
  t ->
  Types.Patch_id.t ->
  has_merged:(Types.Patch_id.t -> bool) ->
  branch_of:(Types.Patch_id.t -> Types.Branch.t) ->
  main:Types.Branch.t ->
  Types.Branch.t
(** [initial_base t p ~has_merged ~branch_of ~main] computes the base branch for
    [p]:
    - If there are no open deps, returns [main].
    - If there is exactly one open dep, returns [branch_of (sole_open_dep ...)].
    - Raises [Invalid_argument] if more than one open dep exists. *)

val dependents : t -> Types.Patch_id.t -> Types.Patch_id.t list
(** [dependents t p] returns all patches that directly depend on [p]. *)

val all_patch_ids : t -> Types.Patch_id.t list
(** [all_patch_ids t] returns every patch id in the graph. *)

open Base
open Types

type start_mode = Naive | Greedy [@@deriving show, eq, sexp_of]

val start_mode_to_string : start_mode -> string
(** Stable CLI/config spelling for start scheduling modes. *)

val start_mode_of_string : string -> (start_mode, string) Result.t
(** Parse a CLI/config spelling for start scheduling modes. *)

val start_deps_satisfied :
  Graph.t ->
  Patch_id.t ->
  has_merged:(Patch_id.t -> bool) ->
  has_pr:(Patch_id.t -> bool) ->
  start_mode:start_mode ->
  bool
(** Whether [patch_id]'s dependencies allow starting under [start_mode].

    [Greedy] allows a patch when every dependency has either merged or has an
    open PR, and at most one dependency remains unmerged. [Naive] requires every
    direct dependency to have merged. *)

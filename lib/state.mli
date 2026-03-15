open Base
open Types

module Queue_key : sig
  type t = Patch_id.t * Operation_kind.t [@@deriving sexp_of, compare]

  include Comparator.S with type t := t
end

module Comment_patch_key : sig
  type t = Comment.t * Patch_id.t [@@deriving sexp_of, compare]

  include Comparator.S with type t := t
end

(** Per-patch mutable state context, corresponding to the PatchCtx spec context.
*)
module Patch_ctx : sig
  type t [@@deriving sexp_of]

  val empty : t
  val is_queued : t -> patch_id:Patch_id.t -> kind:Operation_kind.t -> bool

  val set_queued :
    t -> patch_id:Patch_id.t -> kind:Operation_kind.t -> value:bool -> t

  val is_busy : t -> patch_id:Patch_id.t -> bool
  val set_busy : t -> patch_id:Patch_id.t -> value:bool -> t
  val has_pr : t -> patch_id:Patch_id.t -> bool
  val set_has_pr : t -> patch_id:Patch_id.t -> value:bool -> t
  val has_session : t -> patch_id:Patch_id.t -> bool
  val set_has_session : t -> patch_id:Patch_id.t -> value:bool -> t
  val needs_intervention : t -> patch_id:Patch_id.t -> bool
  val set_needs_intervention : t -> patch_id:Patch_id.t -> value:bool -> t
  val ci_failure_count : t -> patch_id:Patch_id.t -> int
  val set_ci_failure_count : t -> patch_id:Patch_id.t -> count:int -> t
  val increment_ci_failure_count : t -> patch_id:Patch_id.t -> t
  val base_branch : t -> patch_id:Patch_id.t -> Branch.t option
  val set_base_branch : t -> patch_id:Patch_id.t -> branch:Branch.t -> t
  val known_patch_ids : t -> Patch_id.t list
end

(** Comment resolution state, corresponding to the Comments spec context. *)
module Comments : sig
  type t [@@deriving sexp_of]

  val empty : t
  val is_resolved : t -> comment:Comment.t -> bool
  val set_resolved : t -> comment:Comment.t -> value:bool -> t
  val is_pending : t -> comment:Comment.t -> patch_id:Patch_id.t -> bool

  val set_pending :
    t -> comment:Comment.t -> patch_id:Patch_id.t -> value:bool -> t
end

type t = { patch_ctx : Patch_ctx.t; comments : Comments.t } [@@deriving sexp_of]

val empty : t
val update_patch_ctx : t -> f:(Patch_ctx.t -> Patch_ctx.t) -> t
val update_comments : t -> f:(Comments.t -> Comments.t) -> t

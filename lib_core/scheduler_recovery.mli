(* @archlint.module interface
   @archlint.domain scheduler-recovery *)
open Base

type kind =
  | Forge_revision
  | Ci_evidence
  | Queue_removal
  | Native_stack
  | Pr_base
  | Pr_draft
  | Merge_attempt
[@@deriving eq, compare, sexp_of, yojson]

type scope = {
  head : string option;
  base : string option;
  pr : int option;
  publication : string option;
  queue : string option; [@yojson.default None]
}
[@@deriving eq, compare, sexp_of, yojson]

type ticket = private { id : int; kind : kind; scope : scope }
[@@deriving eq, compare, sexp_of]

type t [@@deriving eq, compare, sexp_of]

val empty : t
val grace : kind -> float
val label : kind -> string
val prompt : kind -> string

val observe : t -> now:float -> scope:scope -> blocked:kind list -> t
(** Stable waits have fixed deadlines. Two failed attempts also demand repair.
    Accepted repair completion gets a bounded observation window, then an
    explicit operator repair prompt. Changed scopes reject stale completions. *)

val pending : t -> ticket option
val attention : t -> string option
val failed : t -> now:float -> scope:scope -> kind:kind -> t
val is_running : t -> ticket -> bool
val claim : t -> ticket -> t

val complete : ?now:float -> ?scope:scope -> t -> ticket -> success:bool -> t
(** [scope] records the resulting scope of an intentionally revision-changing
    repair, after the caller has verified that the running turn produced it.
    Without [now], uses the last observation time; live completions must pass
    their actual completion time. *)

val recover : t -> running:ticket option -> t
(** Requeue a claimed repair whose execution owner was lost during restart. *)

val resolve : t -> kind -> t
val contains : t -> kind -> bool
val write_allowed : t -> kind -> bool
val resume : t -> t
val yojson_of_t : t -> Yojson.Safe.t

val decode : Yojson.Safe.t -> t
(** Malformed state resets to [empty], including all repair entries. This total
    compatibility decoder does not preserve partial or invalid authority. *)

val yojson_of_ticket : ticket -> Yojson.Safe.t
val decode_ticket : Yojson.Safe.t -> ticket option

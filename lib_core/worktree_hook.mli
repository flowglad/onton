(* @archlint.module interface
   @archlint.domain branch-reconcile *)

type request = { id : string; path : string; branch : string; script : string }
[@@deriving eq, compare, sexp_of, yojson]

type run = { request : request; attempt : int }
[@@deriving eq, compare, sexp_of]

type outcome = Succeeded | Failed of string
[@@deriving eq, compare, sexp_of, yojson]

type t [@@deriving eq, compare, sexp_of, yojson]

type event =
  | Plan of request
  | Started of run
  | Completed of { id : string; attempt : int; outcome : outcome }
[@@deriving eq, compare, sexp_of]

type decision = Ready | Run of run | Stop of string [@@deriving eq]

val empty : t
val valid : t -> bool

val decode : Yojson.Safe.t -> (t, string) result
(** Total checked decoder. Raw generated converters compose into the owner
    decoder, which also checks [valid]. *)

val next : t -> path:string -> branch:string -> decision
(** Only a planned, context-matching hook can run. A durable start without a
    completion is uncertain after interruption and cannot be repeated
    implicitly. *)

val step : t -> event -> t
(** Invalid plans and stale/duplicate attempts preserve the current obligation.
    Checkpoint [Started] before executing the returned script, and checkpoint
    [Completed] before granting checkout readiness. *)

val resume : t -> t
(** Explicit operator authorization retries an uncertain hook using a fresh
    attempt identity. Old completions cannot acknowledge the new attempt. *)

val is_pending : t -> bool

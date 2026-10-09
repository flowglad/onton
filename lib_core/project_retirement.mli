(* @archlint.module interface
   @archlint.domain project-lifecycle *)

type t [@@deriving eq, compare, sexp_of]
type error = Busy of string | Io_error of string

val error_message : error -> string
val make : slug:string -> t option
val slug : t -> string
val encode : t -> string

val decode : string -> t option
(** Strict, versioned retirement manifest. Only canonical nonempty project slugs
    are accepted; arbitrary file contents never authorize cleanup. *)

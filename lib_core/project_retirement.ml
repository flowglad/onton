(* @archlint.module core
   @archlint.domain project-lifecycle *)

open Base

type error = Busy of string | Io_error of string

let error_message = function
  | Busy path -> "project lifecycle is in use: " ^ path
  | Io_error reason -> reason

type t = { slug : string } [@@deriving eq, compare, sexp_of]

let make ~slug =
  if
    String.is_empty slug
    || not
         (String.for_all slug ~f:(function
           | 'a' .. 'z' | '0' .. '9' | '-' -> true
           | _ -> false))
  then None
  else Some { slug }

let slug t = t.slug
let encode t = "onton-retired-project-v1\n" ^ t.slug ^ "\n"

let decode text =
  match String.split text ~on:'\n' with
  | [ "onton-retired-project-v1"; slug; "" ] -> make ~slug
  | _ -> None

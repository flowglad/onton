(* @archlint.module core
   @archlint.domain git-oid *)

open Base

let valid value =
  (String.length value = 40 || String.length value = 64)
  && String.for_all value ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)

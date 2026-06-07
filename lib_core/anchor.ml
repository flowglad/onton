(* @archlint.module core
   @archlint.domain anchor *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type raw = { base : Types.Branch.t; sha : string; observed_at_remote : bool }
[@@deriving yojson]

type t = raw = {
  base : Types.Branch.t;
  sha : string;
  observed_at_remote : bool;
}
[@@deriving show, eq, ord, sexp_of, compare, hash]

let is_hex_lower_40 s =
  String.length s = 40
  && String.for_all s ~f:(fun c ->
      Char.between c ~low:'0' ~high:'9' || Char.between c ~low:'a' ~high:'f')

let make ~base ~sha ~observed_at_remote =
  let sha = String.strip sha in
  if is_hex_lower_40 sha then Some { base; sha; observed_at_remote } else None

let base t = t.base
let sha t = t.sha
let is_remote t = t.observed_at_remote
let yojson_of_t = yojson_of_raw

let of_yojson_opt json =
  match raw_of_yojson json with
  | exception _ -> None
  | r -> make ~base:r.base ~sha:r.sha ~observed_at_remote:r.observed_at_remote

let t_of_yojson json =
  match of_yojson_opt json with
  | Some t -> t
  | None ->
      raise
        (Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error
           (Failure "Anchor.t_of_yojson: invalid SHA or structure", json))

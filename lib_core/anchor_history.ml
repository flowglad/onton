(* @archlint.module core
   @archlint.domain anchor-history *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type raw = Anchor.t list [@@deriving yojson]
type t = Anchor.t list [@@deriving show, eq, ord, sexp_of, compare, hash]

let cap = 8
let empty : t = []

let same_key (a : Anchor.t) (b : Anchor.t) =
  Types.Branch.equal (Anchor.base a) (Anchor.base b)
  && String.equal (Anchor.sha a) (Anchor.sha b)

let push (t : t) (a : Anchor.t) : t =
  let rest = List.filter t ~f:(fun e -> not (same_key e a)) in
  let combined = a :: rest in
  List.take combined cap

let newest = List.hd
let to_list t = t
let length = List.length
let yojson_of_t = yojson_of_raw

let of_yojson_opt json =
  match raw_of_yojson json with
  | exception _ -> None
  | xs -> Some (List.fold_right xs ~init:empty ~f:(fun a acc -> push acc a))

let t_of_yojson json =
  match of_yojson_opt json with
  | Some t -> t
  | None ->
      raise
        (Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error
           ( Failure
               "Anchor_history.t_of_yojson: invalid structure or anchor SHA",
             json ))

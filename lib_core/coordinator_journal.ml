(* @archlint.module core
   @archlint.domain coordinator-state *)
open Base

[@@@warning "-4"]

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type phase = Pending | Applied of string | Attention [@@deriving yojson]

type entry = { id : string; scope : string; attempts : int; phase : phase }
[@@deriving yojson]

type t = entry list [@@deriving yojson]

let encode t = Yojson.Safe.to_string (yojson_of_t t)
let empty = []
let entries t = t
let find t id = List.find t ~f:(fun e -> String.equal e.id id)

let valid t =
  List.length t
  = List.length
      (List.dedup_and_sort
         (List.map t ~f:(fun e -> e.id))
         ~compare:String.compare)
  && List.for_all t ~f:(fun e ->
      (not (String.is_empty e.id || String.is_empty e.scope))
      && String.length e.id <= 256
      && String.length e.scope <= 262144
      && e.attempts > 0 && e.attempts <= 3
      &&
      match e.phase with
      | Pending -> true
      | Attention -> e.attempts = 3
      | Applied bytes -> String.length bytes <= 1048576)

let decode bytes =
  if String.length bytes > 8388608 then Error "journal too large"
  else
    try
      let t = t_of_yojson (Yojson.Safe.from_string bytes) in
      if valid t then Ok t else Error "invalid journal"
    with _ -> Error "malformed journal"

let replace t e =
  e :: List.filter t ~f:(fun old -> not (String.equal old.id e.id))

let begin_attempt t ~id ~scope =
  let next =
    match find t id with
    | None -> Ok { id; scope; attempts = 1; phase = Pending }
    | Some e when not (String.equal scope e.scope) ->
        Error "changed journal identity"
    | Some { phase = Applied _ | Attention; _ } -> Error "journal entry settled"
    | Some e when e.attempts >= 3 -> Error "journal budget exhausted"
    | Some e -> Ok { e with attempts = e.attempts + 1 }
  in
  Result.bind next ~f:(fun e ->
      let next = replace t e in
      if valid next then Ok next else Error "invalid journal entry")

let settle t ~id ~result =
  match find t id with
  | None -> Error "journal intent missing"
  | Some e -> (
      let phase =
        match result with
        | Some bytes -> Applied bytes
        | None when e.attempts >= 3 -> Attention
        | None -> Pending
      in
      match e.phase with
      | (Applied _ | Attention) when not (Poly.equal e.phase phase) ->
          Error "changed journal result"
      | _ ->
          let next = replace t { e with phase } in
          if valid next then Ok next else Error "invalid journal result")

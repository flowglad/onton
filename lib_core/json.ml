(* @archlint.module core
   @archlint.domain json *)

open Base

type t = Yojson.Safe.t

(* Total accessors. None of these raise: a missing key, an explicit [`Null], or
   a type mismatch all collapse to [None]. This is the sanctioned alternative to
   [Yojson.Safe.Util], whose [member]/[to_string]/[to_*] are partial (they raise
   at runtime, invisibly to the type checker — the failure mode behind PR #333's
   poll crash). Field access here is by direct variant match, so the only thing
   in this module that names [Yojson.Safe.Util] is the exception caught by
   [try_of_yojson]. *)

let field key (j : t) : t option =
  match j with
  | `Assoc kvs -> (
      match List.Assoc.find kvs ~equal:String.equal key with
      | None | Some `Null -> None
      | Some v -> Some v)
  | _ -> None

let string : t -> string option = function `String s -> Some s | _ -> None
let int : t -> int option = function `Int i -> Some i | _ -> None
let bool : t -> bool option = function `Bool b -> Some b | _ -> None
let list : t -> t list option = function `List xs -> Some xs | _ -> None

let assoc : t -> (string * t) list option = function
  | `Assoc kvs -> Some kvs
  | _ -> None

let string_field key j = Option.bind (field key j) ~f:string
let int_field key j = Option.bind (field key j) ~f:int
let bool_field key j = Option.bind (field key j) ~f:bool

(* Wrap a raising [ppx_yojson_conv]-generated deserializer into a Result.t. The
   generated [t_of_yojson] raises [Of_yojson_error] on a shape mismatch (and a
   hand-written decoder may still let a [Type_error] through); this is the one
   place those are caught and turned into an [Error string]. *)
let try_of_yojson f json =
  try Ok (f json) with
  | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
      Error (Stdlib.Printexc.to_string exn)
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error (Printf.sprintf "malformed json: %s" msg)

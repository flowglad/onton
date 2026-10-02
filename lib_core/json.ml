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

(* Validate the full tree before exposing decoded data: key uniqueness is
   local to each object and every numeric value must be finite. *)
let of_string input =
  let rec validate : t -> (unit, string) Result.t = function
    | `Assoc fields ->
        let rec loop keys = function
          | [] -> Ok ()
          | (key, value) :: rest ->
              if Set.mem keys key then Error ("Duplicate JSON key: " ^ key)
              else
                Result.bind (validate value) ~f:(fun () ->
                    loop (Set.add keys key) rest)
        in
        loop (Set.empty (module String)) fields
    | `List values ->
        List.fold_result values ~init:() ~f:(fun () value -> validate value)
    | `Float value when not (Float.is_finite value) ->
        Error "JSON numbers must be finite"
    | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ -> Ok ()
  in
  try
    let json = Yojson.Safe.from_string input in
    Result.map (validate json) ~f:(fun () -> json)
  with exn -> Error (Exn.to_string exn)

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

(* @archlint.module test
   @archlint.domain json *)

open Base

(* Tests for the total JSON accessors in Onton_core.Json. The contract under
   test: a missing key, an explicit null, and a type mismatch all collapse to
   None — never a raise. *)

module J = Onton_core.Json

let parse = Yojson.Safe.from_string

let check name cond =
  if cond then Stdlib.Printf.printf "  %s: OK\n" name
  else (
    Stdlib.Printf.eprintf "  %s: FAIL\n" name;
    Stdlib.exit 1)

let opt_eq eq a b =
  match (a, b) with None, None -> true | Some x, Some y -> eq x y | _ -> false

let () =
  let obj =
    parse
      {|{ "s": "hi", "n": 7, "b": true, "xs": [1,2],
          "nested": { "k": "v" }, "nul": null }|}
  in

  (* field: present non-null -> Some; absent / null / non-object -> None *)
  check "field present" (Option.is_some (J.field "s" obj));
  check "field absent" (Option.is_none (J.field "missing" obj));
  check "field explicit null" (Option.is_none (J.field "nul" obj));
  check "field on non-object" (Option.is_none (J.field "s" (parse "42")));
  check "field on null" (Option.is_none (J.field "s" (parse "null")));

  (* leaf accessors: right type -> Some; wrong type -> None *)
  check "string ok" (opt_eq String.equal (J.string (parse {|"x"|})) (Some "x"));
  check "string wrong type" (Option.is_none (J.string (parse "1")));
  check "int ok" (opt_eq Int.equal (J.int (parse "5")) (Some 5));
  check "int wrong type" (Option.is_none (J.int (parse {|"5"|})));
  check "bool ok" (opt_eq Bool.equal (J.bool (parse "true")) (Some true));
  check "bool wrong type" (Option.is_none (J.bool (parse "1")));
  check "list ok"
    (match J.list (parse "[1,2,3]") with
    | Some l -> List.length l = 3
    | None -> false);
  check "list wrong type" (Option.is_none (J.list (parse "{}")));
  check "assoc ok"
    (match J.assoc obj with Some kvs -> List.length kvs = 6 | None -> false);
  check "assoc wrong type" (Option.is_none (J.assoc (parse "[]")));

  (* *_field composition *)
  check "string_field ok"
    (opt_eq String.equal (J.string_field "s" obj) (Some "hi"));
  check "string_field type mismatch" (Option.is_none (J.string_field "n" obj));
  check "string_field absent" (Option.is_none (J.string_field "missing" obj));
  check "int_field ok" (opt_eq Int.equal (J.int_field "n" obj) (Some 7));
  check "int_field null" (Option.is_none (J.int_field "nul" obj));
  check "bool_field ok" (opt_eq Bool.equal (J.bool_field "b" obj) (Some true));

  (* nested chaining via field + leaf *)
  check "nested chain"
    (opt_eq String.equal
       (Option.bind (J.field "nested" obj) ~f:(J.string_field "k"))
       (Some "v"));

  (* try_of_yojson: success path and caught failure path *)
  (match J.try_of_yojson (fun j -> J.string_field "s" j) obj with
  | Ok s -> check "try_of_yojson ok" (opt_eq String.equal s (Some "hi"))
  | Error _ -> check "try_of_yojson ok" false);
  (match
     J.try_of_yojson
       (fun _ ->
         raise
           (Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error
              (Failure "boom", `Null)))
       obj
   with
  | Error _ -> check "try_of_yojson catches Of_yojson_error" true
  | Ok _ -> check "try_of_yojson catches Of_yojson_error" false);
  (match
     J.try_of_yojson
       (fun _ -> raise (Yojson.Safe.Util.Type_error ("expected string", `Null)))
       obj
   with
  | Error _ -> check "try_of_yojson catches Type_error" true
  | Ok _ -> check "try_of_yojson catches Type_error" false);

  Stdlib.print_endline "test_json: OK"

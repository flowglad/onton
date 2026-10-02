(* @archlint.module test
   @archlint.domain gameplan-document *)

open Onton_core

let totality =
  QCheck2.Test.make ~count:1000
    ~name:"scalar decoding is total for arbitrary bytes"
    QCheck2.Gen.(
      pair (oneof_list [ Gameplan_scalar.Plain; Gameplan_scalar.Quoted ]) string)
    (fun (style, value) ->
      try
        ignore (Gameplan_scalar.decode style value);
        true
      with _ -> false)

let quoted =
  QCheck2.Test.make ~count:500 ~name:"quoted scalars preserve arbitrary bytes"
    QCheck2.Gen.string (fun value ->
      Gameplan_scalar.decode Gameplan_scalar.Quoted value = Ok (`String value))

let integers =
  QCheck2.Test.make ~count:500 ~name:"plain integers remain exact integers"
    QCheck2.Gen.int (fun value ->
      Gameplan_scalar.decode Gameplan_scalar.Plain (string_of_int value)
      = Ok (`Int value))

let finite_decimals =
  QCheck2.Test.make ~count:500 ~name:"plain decimals preserve exact halves"
    QCheck2.Gen.(int_range (-10000) 10000)
    (fun value ->
      let input = Printf.sprintf "%d.5" value in
      let expected = float_of_int value +. if value < 0 then -0.5 else 0.5 in
      Gameplan_scalar.decode Gameplan_scalar.Plain input = Ok (`Float expected))

let boundaries =
  QCheck2.Test.make ~name:"JSON scalar boundaries and YAML string spellings"
    QCheck2.Gen.(
      oneof_list
        [
          ("", `Null);
          ("~", `Null);
          ("null", `Null);
          ("true", `Bool true);
          ("false", `Bool false);
          ("0", `Int 0);
          ("-0", `Int 0);
          ("-1", `Int (-1));
          ("1e2", `Float 100.);
          ("1E-2", `Float 0.01);
          ("9007199254740993", `Int 9007199254740993);
          ("01", `String "01");
          ("+1", `String "+1");
          ("1.", `String "1.");
          (".5", `String ".5");
          ("1e", `String "1e");
          ("on", `String "on");
          ("off", `String "off");
          ("NULL", `String "NULL");
          ("true ", `String "true ");
          ("2026-10-02", `String "2026-10-02");
        ])
    (fun (value, expected) ->
      Gameplan_scalar.decode Gameplan_scalar.Plain value = Ok expected
      && Gameplan_scalar.decode Gameplan_scalar.Quoted value
         = Ok (`String value))

let non_finite =
  QCheck2.Test.make ~name:"numeric overflow is rejected only for plain scalars"
    QCheck2.Gen.(pair bool (int_range 309 10000))
    (fun (negative, exponent) ->
      let value =
        Printf.sprintf "%s1e%d" (if negative then "-" else "") exponent
      in
      (match Gameplan_scalar.decode Gameplan_scalar.Plain value with
        | Error _ -> true
        | Ok _ -> false)
      && Gameplan_scalar.decode Gameplan_scalar.Quoted value
         = Ok (`String value))

let () =
  QCheck_base_runner.run_tests_main
    [ totality; quoted; integers; finite_decimals; boundaries; non_finite ]

(* @archlint.module test
   @archlint.domain json *)

open Onton_core

let totality =
  QCheck2.Test.make ~count:1000
    ~name:"JSON decoding is total for arbitrary bytes" QCheck2.Gen.string
    (fun input ->
      try
        ignore (Gameplan_parser.parse_json_string input);
        true
      with _ -> false)

let roundtrip =
  QCheck2.Test.make ~count:500
    ~name:"JSON normalization preserves nested values"
    QCheck2.Gen.(pair int string)
    (fun (value, text) ->
      let json = `Assoc [ ("nested", `List [ `Int value; `String text ]) ] in
      let input = Yojson.Safe.to_string json in
      match Json.of_string input with
      | Error _ -> false
      | Ok parsed ->
          parsed = json
          && Json.of_string (Yojson.Safe.to_string parsed) = Ok json)

let duplicates =
  QCheck2.Test.make ~count:500 ~name:"duplicate keys are rejected at any depth"
    QCheck2.Gen.(pair string nat_small)
    (fun (key, depth) ->
      let quote = Yojson.Safe.to_string (`String key) in
      let input = "{" ^ quote ^ ":1," ^ quote ^ ":2}" in
      let nested = String.make depth '[' ^ input ^ String.make depth ']' in
      match Json.of_string nested with Error _ -> true | Ok _ -> false)

let boundaries =
  QCheck2.Test.make ~name:"non-finite JSON is rejected including unknown fields"
    QCheck2.Gen.(oneof_list [ "NaN"; "Infinity"; "-Infinity"; "1e999" ])
    (fun value ->
      let input =
        "{\"projectName\":\"p\",\"patches\":[],\"unknown\":[" ^ value ^ "]}"
      in
      (match Json.of_string input with Error _ -> true | Ok _ -> false)
      &&
      match Gameplan_parser.parse_json_string input with
      | Error _ -> true
      | Ok _ -> false)

let distinct_scopes =
  QCheck2.Test.make ~count:500 ~name:"same keys in separate objects are valid"
    QCheck2.Gen.string (fun key ->
      let json = `List [ `Assoc [ (key, `Int 1) ]; `Assoc [ (key, `Int 2) ] ] in
      Json.of_string (Yojson.Safe.to_string json) = Ok json)

let () =
  QCheck_base_runner.run_tests_main
    [ totality; roundtrip; duplicates; boundaries; distinct_scopes ]

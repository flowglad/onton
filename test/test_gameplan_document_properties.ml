(* @archlint.module test
   @archlint.domain gameplan-document *)

open Onton_core

let property ?(count = 1) ?print name gen f =
  QCheck2.Test.make ~count ?print ~name gen f

let once name f = property name QCheck2.Gen.unit (fun () -> f ())
let decode = Gameplan_document.of_string
let quote value = Yojson.Safe.to_string (`String value)
let fails value = match decode value with Error _ -> true | Ok _ -> false

let totality =
  property ~count:1000 "document decoding is total for arbitrary bytes"
    QCheck2.Gen.string (fun input ->
      try
        ignore (decode input);
        ignore (Gameplan_parser.parse_string input);
        true
      with _ -> false)

let ascii = QCheck2.Gen.(string_of (char_range '\000' '\126'))

let strings =
  property ~count:500 ~print:QCheck2.Print.string
    "YAML quoted strings preserve ASCII JSON strings" ascii (fun value ->
      try
        decode ("value: " ^ quote value)
        = Ok (`Assoc [ ("value", `String value) ])
      with _ -> false)

let integers =
  property ~count:500 "YAML integer values remain exact integers"
    QCheck2.Gen.int (fun value ->
      try
        decode ("value: " ^ string_of_int value)
        = Ok (`Assoc [ ("value", `Int value) ])
      with _ -> false)

let roundtrip =
  property ~count:500
    ~print:QCheck2.Print.(pair int string)
    "normalizing a YAML document to JSON preserves its value"
    QCheck2.Gen.(pair int ascii)
    (fun (n, value) ->
      try
        match decode ("n: " ^ string_of_int n ^ "\nvalue: " ^ quote value) with
        | Error _ -> false
        | Ok json -> decode (Yojson.Safe.to_string json) = Ok json
      with _ -> false)

let equivalent_plans =
  property ~count:300 "YAML and JSON plans have identical semantic results"
    QCheck2.Gen.(
      pair (int_range 1 10000)
        (oneof_list [ "on"; "off"; "001"; "null"; "a:b" ]))
    (fun (n, title) ->
      let yaml =
        Printf.sprintf
          "projectName: yaml-test\n\
           owner: flowglad\n\
           repo: onton\n\
           solutionSummary: |\n\
          \  first line\n\
          \  second line\n\
           patches:\n\
          \  - number: %d\n\
          \    title: %s\n\
          \    complexity: 2\n\
          \    spec: |-\n\
          \      spec line\n\
          \    changes: []\n\
           dependencyGraph:\n\
          \  - patch: %d\n\
          \    dependsOn: []\n\
           openQuestions: []\n"
          n (quote title) n
      in
      let json =
        `Assoc
          [
            ("projectName", `String "yaml-test");
            ("owner", `String "flowglad");
            ("repo", `String "onton");
            ("solutionSummary", `String "first line\nsecond line\n");
            ( "patches",
              `List
                [
                  `Assoc
                    [
                      ("number", `Int n);
                      ("title", `String title);
                      ("complexity", `Int 2);
                      ("spec", `String "spec line");
                      ("changes", `List []);
                    ];
                ] );
            ( "dependencyGraph",
              `List [ `Assoc [ ("patch", `Int n); ("dependsOn", `List []) ] ] );
            ("openQuestions", `List []);
          ]
      in
      match
        ( Gameplan_parser.parse_string yaml,
          Gameplan_parser.parse_json_string (Yojson.Safe.to_string json) )
      with
      | Ok a, Ok b ->
          Types.Gameplan.equal a.Gameplan_parser.gameplan
            b.Gameplan_parser.gameplan
          && Base.Map.equal
               (Base.List.equal Types.Patch_id.equal)
               a.Gameplan_parser.dependency_graph
               b.Gameplan_parser.dependency_graph
      | Error _, _ | Ok _, Error _ -> false)

let boundaries =
  once "YAML scalar and block boundaries" (fun () ->
      decode
        "a: on\n\
         b: off\n\
         c: 2026-10-02\n\
         d: \"001\"\n\
         e: true\n\
         f: false\n\
         g: null\n\
         h: ~\n\
         i:\n\
         j: []\n\
         k: {}\n\
         spec: |-\n\
        \  one\n\
        \  two\n\
         large: 9007199254740993\n"
      = Ok
          (`Assoc
             [
               ("a", `String "on");
               ("b", `String "off");
               ("c", `String "2026-10-02");
               ("d", `String "001");
               ("e", `Bool true);
               ("f", `Bool false);
               ("g", `Null);
               ("h", `Null);
               ("i", `Null);
               ("j", `List []);
               ("k", `Assoc []);
               ("spec", `String "one\ntwo");
               ("large", `Int 9007199254740993);
             ]))

let unicode_and_nul =
  once "YAML scalar lengths preserve Unicode and escaped NULs" (fun () ->
      decode "value: \"before\\u0000after café 🌿\""
      = Ok (`Assoc [ ("value", `String "before\000after café 🌿") ]))

let rejected =
  once "YAML rejects ambiguous or unsupported documents" (fun () ->
      List.for_all fails
        [
          "a: 1\na: 2";
          "{\"a\": 1, \"a\": 2}";
          "a: {x: 1, x: 2}";
          "a: &id [1]\nb: *id";
          "a: !custom value";
          "a: !!str value";
          "a: !!map {b: value}";
          "a: [";
          "1: value";
          "[a]: value";
          "<<: {a: 1}";
          "a: 1e999";
          "a: 1\n---\nb: 2";
          "a: 1\n...\ngarbage";
          "";
        ])

let questions =
  once "YAML retains the unresolved-question execution gate" (fun () ->
      match
        Gameplan_parser.parse_string
          "projectName: p\npatches: []\nopenQuestions: [Decide first]\n"
      with
      | Error msg -> Base.String.is_substring msg ~substring:"open question"
      | Ok _ -> false)

let () =
  QCheck_base_runner.run_tests_main
    [
      totality;
      strings;
      integers;
      roundtrip;
      equivalent_plans;
      boundaries;
      unicode_and_nul;
      rejected;
      questions;
    ]

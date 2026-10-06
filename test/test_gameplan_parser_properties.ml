(* @archlint.module test
   @archlint.domain gameplan-parser *)

open Base
open Onton_core
open Types

let pid n = Patch_id.of_string (Int.to_string n)

let proof =
  `Assoc
    [
      ("command", `String "dune build @check");
      ("expectation", `String "Public interface type-checks");
    ]

let requirement id owner consumers =
  `Assoc
    [
      ("id", `String ("FC-" ^ Int.to_string id));
      ("description", `String "Public capability");
      ("ownedBy", `Int owner);
      ("requiredBy", `List (List.map consumers ~f:(fun n -> `Int n)));
      ("verifiedBy", `List [ proof ]);
    ]

let order before after =
  `Assoc
    [
      ("before", `Int before);
      ("after", `Int after);
      ("reason", `String "Serialize migration writes");
    ]

let plan n requirements orders =
  `Assoc
    [
      ("formatVersion", `Int 2);
      ("operationalConsiderations", `Assoc []);
      ("requiredChanges", `List []);
      ("projectName", `String "contracts");
      ("functionalChanges", `List requirements);
      ("orderingConstraints", `List orders);
      ("acceptanceCriteria", `List []);
      ("testMap", `List []);
      ( "patches",
        `List
          (List.init n ~f:(fun i ->
               `Assoc
                 [
                   ("number", `Int (i + 1));
                   ("title", `String "Patch");
                   ("changes", `List []);
                   ("files", `List []);
                 ])) );
    ]

let parse json = Gameplan_parser.parse_json_string (Yojson.Safe.to_string json)

let field json key value =
  match json with
  | `Assoc fields ->
      `Assoc ((key, value) :: List.Assoc.remove fields ~equal:String.equal key)
  | _ -> assert false

let rejected json = Result.is_error (parse json)
let safely f x = try f x with _ -> false

let architecture_decision kind =
  `Assoc
    [
      ("id", `String "AD-1");
      ("topic", `String "ownership");
      ("question", `String "Who owns admission?");
      ("choice", `String "Application service owns admission");
      ( "alternatives",
        `List
          [
            `Assoc
              [
                ("choice", `String "Worker owns admission");
                ("tradeoffs", `String "Requires a second authority boundary");
              ];
          ] );
      ( "resolution",
        `Assoc
          [
            ("kind", `String kind);
            ("evidence", `String "Engineer selected the application boundary");
            ("rationale", `String "One write owner controls admission");
          ] );
    ]

let architecture decisions =
  `Assoc
    [
      ("summary", `String "Application owns admission; tasks execute work");
      ("decisions", `List decisions);
    ]

let () =
  let open QCheck2 in
  let dag =
    Gen.(
      int_range 1 8 >>= fun n ->
      map
        (fun edges -> (n, List.filter edges ~f:(fun (a, b, _) -> a < b)))
        (list_size (int_range 0 40)
           (triple (int_range 1 n) (int_range 1 n) bool)))
  in
  let document n edges =
    let requirements =
      List.filter_mapi edges ~f:(fun i (a, b, capability) ->
          if capability then Some (requirement (i + 1) a [ b ]) else None)
    in
    let orders =
      List.filter_map edges ~f:(fun (a, b, capability) ->
          if capability then None else Some (order a b))
    in
    plan n requirements orders
  in
  let rec json_value depth =
    let scalar =
      Gen.(
        oneof
          [
            return `Null;
            map (fun s -> `String s) string;
            map (fun n -> `Int n) int;
            map (fun b -> `Bool b) bool;
          ])
    in
    if depth = 0 then scalar
    else
      Gen.(
        oneof
          [
            scalar;
            map
              (fun xs -> `List xs)
              (list_size (int_range 0 4) (json_value (depth - 1)));
            map
              (fun xs -> `Assoc xs)
              (list_size (int_range 0 4)
                 (pair
                    (oneof_list
                       [
                         "summary";
                         "decisions";
                         "kind";
                         "evidence";
                         "rationale";
                         "choice";
                         "tradeoffs";
                       ])
                    (json_value (depth - 1))));
          ])
  in
  let tests =
    [
      Test.make
        ~name:
          "v3 admission requires architectural design while v2 remains \
           compatible"
        ~count:1 Gen.unit
        (safely (fun () ->
             let old = plan 1 [] [] in
             let current = field old "formatVersion" (`Int 3) in
             Result.is_ok (parse old)
             && rejected current
             && rejected (field current "architectureDesign" `Null)
             && Result.is_ok
                  (parse (field current "architectureDesign" (architecture [])))
             && Result.is_ok
                  (parse
                     (field current "architectureDesign"
                        (architecture
                           [ architecture_decision "engineer_approved" ])))));
      Test.make
        ~name:
          "v3 retains prerequisite derivation and rejects legacy dependency \
           fields"
        ~count:1 Gen.unit
        (safely (fun () ->
             let current =
               field
                 (field
                    (plan 2 [ requirement 1 1 [ 2 ] ] [])
                    "formatVersion" (`Int 3))
                 "architectureDesign" (architecture [])
             in
             match parse current with
             | Error _ -> false
             | Ok parsed ->
                 List.equal Patch_id.equal
                   (Map.find parsed.dependency_graph (pid 2)
                   |> Option.value ~default:[])
                   [ pid 1 ]
                 && rejected (field current "dependencyGraph" (`List []))));
      Test.make
        ~name:"architecture admission is total over generated structured values"
        ~count:500
        Gen.(pair (int_range 0 3) (json_value 2))
        (safely (fun (layer, value) ->
             let decision = architecture_decision "constrained" in
             let design =
               match layer with
               | 0 -> value
               | 1 -> architecture [ value ]
               | 2 -> architecture [ field decision "resolution" value ]
               | _ ->
                   architecture
                     [ field decision "alternatives" (`List [ value ]) ]
             in
             ignore
               (parse (field (plan 1 [] []) "architectureDesign" design)
                 : (Gameplan_parser.t, string) Result.t);
             true));
      Test.make ~name:"architecture non-object resolution reports its shape"
        ~count:100
        Gen.(
          oneof
            [
              map (fun n -> `Int n) int;
              map (fun s -> `String s) string;
              return `Null;
              return (`List []);
            ])
        (safely (fun resolution ->
             let decision =
               field
                 (architecture_decision "constrained")
                 "resolution" resolution
             in
             match
               parse
                 (field (plan 1 [] []) "architectureDesign"
                    (architecture [ decision ]))
             with
             | Ok _ -> false
             | Error message ->
                 String.is_substring message
                   ~substring:"resolution must be an object"));
      Test.make
        ~name:"architecture resolutions retain context through persistence"
        ~count:1 Gen.unit
        (safely (fun () ->
             List.for_all [ "engineer_approved"; "constrained"; "delegated" ]
               ~f:(fun kind ->
                 match
                   parse
                     (field (plan 1 [] []) "architectureDesign"
                        (architecture [ architecture_decision kind ]))
                 with
                 | Error _ -> false
                 | Ok parsed ->
                     let structural_match =
                       match parsed.gameplan.Gameplan.architecture_design with
                       | None -> false
                       | Some design -> (
                           String.equal design.summary
                             "Application owns admission; tasks execute work"
                           &&
                           match design.decisions with
                           | [ decision ] ->
                               String.equal decision.id "AD-1"
                               && String.equal decision.topic "ownership"
                               && String.equal decision.question
                                    "Who owns admission?"
                               && String.equal decision.choice
                                    "Application service owns admission"
                               && (match decision.alternatives with
                                 | [ alternative ] ->
                                     String.equal alternative.alternative_choice
                                       "Worker owns admission"
                                     && String.equal alternative.tradeoffs
                                          "Requires a second authority boundary"
                                 | _ -> false)
                               &&
                               let actual_kind, basis =
                                 match decision.resolution with
                                 | Architecture_design.Engineer_approved basis
                                   ->
                                     ("engineer_approved", basis)
                                 | Architecture_design.Constrained basis ->
                                     ("constrained", basis)
                                 | Architecture_design.Delegated basis ->
                                     ("delegated", basis)
                               in
                               String.equal actual_kind kind
                               && String.equal basis.evidence
                                    "Engineer selected the application boundary"
                               && String.equal basis.rationale
                                    "One write owner controls admission"
                           | _ -> false)
                     in
                     structural_match
                     && Gameplan.equal
                          (Gameplan.t_of_yojson
                             (Gameplan.yojson_of_t parsed.gameplan))
                          parsed.gameplan)));
      Test.make
        ~name:
          "architecture unresolved rejects independent of format and \
           openQuestions"
        ~count:1 Gen.unit
        (safely (fun () ->
             List.for_all [ Some 2; Some 3; None ] ~f:(fun version ->
                 let json = plan 1 [] [] in
                 let json =
                   match version with
                   | Some n -> field json "formatVersion" (`Int n)
                   | None -> (
                       match json with
                       | `Assoc fields ->
                           `Assoc
                             (List.Assoc.remove fields ~equal:String.equal
                                "formatVersion")
                       | _ -> json)
                 in
                 rejected
                   (field
                      (field json "openQuestions" (`List []))
                      "architectureDesign"
                      (architecture [ architecture_decision "unresolved" ])))));
      Test.make ~name:"architecture malformed and duplicate decisions reject"
        ~count:1 Gen.unit
        (safely (fun () ->
             List.for_all
               [
                 `Null;
                 `String "design";
                 `List [];
                 architecture [ architecture_decision "unknown" ];
                 architecture
                   [
                     architecture_decision "constrained";
                     architecture_decision "constrained";
                   ];
                 architecture
                   [
                     field
                       (architecture_decision "constrained")
                       "resolution"
                       (`Assoc
                          [
                            ("kind", `String "constrained");
                            ("evidence", `String " ");
                            ("rationale", `String "Reason");
                          ]);
                   ];
               ]
               ~f:(fun design ->
                 rejected (field (plan 1 [] []) "architectureDesign" design))));
      Test.make
        ~name:"architecture absence supports previous plans and snapshots"
        ~count:1 Gen.unit
        (safely (fun () ->
             match parse (plan 1 [] []) with
             | Error _ -> false
             | Ok parsed -> (
                 Option.is_none parsed.gameplan.architecture_design
                 &&
                 match Gameplan.yojson_of_t parsed.gameplan with
                 | `Assoc fields ->
                     Option.is_none
                       (Gameplan.t_of_yojson
                          (`Assoc
                             (List.Assoc.remove fields ~equal:String.equal
                                "architecture_design")))
                         .architecture_design
                 | _ -> false)));
      Test.make
        ~name:"architecture routine plan admits an empty decision inventory"
        ~count:1 Gen.unit
        (safely (fun () ->
             Result.is_ok
               (parse
                  (field (plan 1 [] []) "architectureDesign" (architecture [])))));
      Test.make ~name:"v2 requires operational and required-change fields"
        ~count:1 Gen.unit
        (safely (fun () ->
             List.for_all [ "operationalConsiderations"; "requiredChanges" ]
               ~f:(fun key ->
                 List.for_all [ true; false ] ~f:(fun omit ->
                     let json = plan 1 [] [] in
                     let invalid =
                       if omit then
                         match json with
                         | `Assoc fields ->
                             `Assoc
                               (List.Assoc.remove fields ~equal:String.equal key)
                         | _ -> assert false
                       else field json key `Null
                     in
                     rejected invalid))));
      Test.make ~name:"legacy metadata omission keeps empty defaults" ~count:1
        Gen.unit
        (safely (fun () ->
             match
               parse
                 (`Assoc
                    [ ("projectName", `String "legacy"); ("patches", `List []) ])
             with
             | Error _ -> false
             | Ok parsed ->
                 String.is_empty parsed.gameplan.operational_considerations
                 && String.is_empty parsed.gameplan.required_changes));
      Test.make
        ~name:
          "contracts: derived graph equals declared requirement and ordering \
           edges"
        ~count:400 dag
        (safely (fun (n, edges) ->
             match parse (document n edges) with
             | Error _ -> false
             | Ok parsed ->
                 let graph = Graph.of_gameplan parsed.gameplan in
                 List.for_all
                   (List.init n ~f:(fun i -> i + 1))
                   ~f:(fun consumer ->
                     let expected =
                       List.filter_map edges ~f:(fun (a, b, _) ->
                           if b = consumer then Some (pid a) else None)
                       |> List.dedup_and_sort ~compare:Patch_id.compare
                     in
                     List.equal Patch_id.equal expected
                       (Graph.deps graph (pid consumer))
                     && List.equal Patch_id.equal expected
                          (Map.find parsed.dependency_graph (pid consumer)
                          |> Option.value ~default:[]))));
      Test.make
        ~name:"contracts: order and repeated edges do not change scheduling"
        ~count:300 dag
        (safely (fun (n, edges) ->
             match
               ( parse (document n edges),
                 parse (document n (List.rev edges @ edges)) )
             with
             | Ok a, Ok b ->
                 Map.equal
                   (List.equal Patch_id.equal)
                   a.dependency_graph b.dependency_graph
             | Error _, _ | _, Error _ -> false));
      Test.make ~name:"contracts: requirement cycles rejected" ~count:100
        Gen.(int_range 2 12)
        (safely (fun n ->
             rejected
               (plan n
                  (List.init n ~f:(fun i ->
                       requirement (i + 1) (i + 1)
                         [ (if i + 1 = n then 1 else i + 2) ]))
                  [])));
      Test.make
        ~name:"contracts: mixed requirement and ordering cycles rejected"
        ~count:100
        Gen.(int_range 2 12)
        (safely (fun n ->
             rejected
               (plan n
                  (List.init (n - 1) ~f:(fun i ->
                       requirement (i + 1) (i + 1) [ i + 2 ]))
                  [ order n 1 ])));
      Test.make ~name:"contracts: ordering cycles rejected" ~count:100
        Gen.(int_range 2 12)
        (safely (fun n ->
             rejected
               (plan n []
                  (List.init n ~f:(fun i ->
                       order (i + 1) (if i + 1 = n then 1 else i + 2))))));
      Test.make
        ~name:"contracts: dangling owner consumer and self-dependency rejected"
        ~count:100
        Gen.(int_range 1 12)
        (safely (fun n ->
             rejected (plan n [ requirement 1 (n + 1) [] ] [])
             && rejected (plan n [ requirement 1 1 [ n + 1 ] ] [])
             && rejected (plan n [ requirement 1 n [ n ] ] [])
             && rejected (plan n [] [ order (n + 1) 1 ])));
      Test.make
        ~name:"contracts: empty graph and disconnected patches are valid"
        ~count:30
        Gen.(int_range 0 12)
        (safely (fun n ->
             match parse (plan n [] []) with
             | Error _ -> false
             | Ok p ->
                 Map.length p.dependency_graph = n
                 && List.for_all p.gameplan.patches ~f:(fun p ->
                     List.is_empty p.dependencies)));
      Test.make
        ~name:
          "contracts: producer proof references resolve to owner tests and \
           files"
        ~count:100
        Gen.(int_range 2 12)
        (safely (fun n ->
             let guarantee =
               requirement 1 1 [ n ] |> fun fc ->
               field fc "verifiedBy" (`List [ `String "producer proof" ])
             in
             let test owner file =
               `Assoc
                 [
                   ("testName", `String "producer proof");
                   ("implPatch", `Int owner);
                   ("file", `String file);
                 ]
             in
             let document = plan n [ guarantee ] [] in
             let patches =
               match document with
               | `Assoc fields -> (
                   match
                     List.Assoc.find fields ~equal:String.equal "patches"
                   with
                   | Some (`List patches) ->
                       List.mapi patches ~f:(fun i patch ->
                           if i = 0 then
                             field patch "files"
                               (`List
                                  [
                                    `Assoc
                                      [
                                        ("path", `String "proof.ml");
                                        ("action", `String "create");
                                      ];
                                  ])
                           else patch)
                   | _ -> assert false)
               | _ -> assert false
             in
             let document = field document "patches" (`List patches) in
             Result.is_ok
               (parse (field document "testMap" (`List [ test 1 "proof.ml" ])))
             && rejected (field document "testMap" (`List []))
             && rejected
                  (field document "testMap" (`List [ test n "proof.ml" ]))
             && rejected
                  (field document "testMap" (`List [ test 1 "other.ml" ]))));
      Test.make
        ~name:
          "contracts: acceptance follows both producer and consumer, excluding \
           unrelated patches"
        ~count:100
        Gen.(int_range 3 12)
        (safely (fun n ->
             let criterion reference =
               `Assoc
                 [
                   ("id", `String "AC-1");
                   ("description", `String "Ready before consumption");
                   ("tracesTo", `List [ `String reference ]);
                 ]
             in
             let document = plan n [ requirement 1 1 [ n ] ] [] in
             let valid =
               field document "acceptanceCriteria" (`List [ criterion "FC-1" ])
             in
             rejected
               (field document "acceptanceCriteria"
                  (`List [ criterion "missing" ]))
             &&
             match parse valid with
             | Error _ -> false
             | Ok parsed ->
                 List.for_all parsed.gameplan.patches ~f:(fun patch ->
                     let routed =
                       Patch_id.equal patch.id (pid 1)
                       || Patch_id.equal patch.id (pid n)
                     in
                     Bool.equal routed
                       (not (List.is_empty patch.acceptance_criteria)))));
      Test.make ~name:"gameplan parser: arbitrary text is total" ~count:500
        Gen.string
        (safely (fun text ->
             ignore
               (Gameplan_parser.parse_string text
                 : (Gameplan_parser.t, string) Result.t);
             true));
      Test.make ~name:"gameplan parser: malformed contract fields are total"
        ~count:300
        Gen.(
          pair
            (oneof_list
               [
                 "functionalChanges";
                 "orderingConstraints";
                 "patches";
                 "formatVersion";
                 "acceptanceCriteria";
               ])
            string)
        (safely (fun (key, value) ->
             let json =
               match plan 1 [] [] with
               | `Assoc fields ->
                   `Assoc
                     ((key, `String value)
                     :: List.Assoc.remove fields ~equal:String.equal key)
               | other -> other
             in
             ignore (parse json : (Gameplan_parser.t, string) Result.t);
             true));
    ]
  in
  QCheck_base_runner.run_tests_main tests

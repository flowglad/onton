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
  let tests =
    [
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

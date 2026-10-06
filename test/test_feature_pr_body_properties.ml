(* @archlint.module test
   @archlint.domain execution-mode *)
open Base
open Onton_core
open Types

let id n = Patch_id.of_string (Int.to_string n)
let branch n = Branch.of_string ("patch-" ^ Int.to_string n)

let patch n deps =
  Patch.
    {
      id = id n;
      branch = branch n;
      dependencies = List.map deps ~f:id;
      title = "patch";
      description = "";
      spec = "";
      acceptance_criteria = [];
      files = [];
      classification = "";
      changes = [];
      test_stubs_introduced = [];
      test_stubs_implemented = [];
      complexity = None;
      precedents = [];
      required_context = [];
    }

let gameplan =
  Gameplan.
    {
      project_name = "feature-test";
      repo_owner = "test";
      repo_name = "test";
      problem_statement = "";
      architecture_design = None;
      solution_summary = "";
      final_state_spec = "";
      patches = [];
      operational_considerations = "";
      required_changes = "";
      ordering_constraints = [];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
      functional_changes = [];
      context_resources = [];
      publication = None;
      reachability_traces = [];
    }

module G = QCheck2.Gen

let property name gen f =
  QCheck2.Test.make ~name ~count:300 gen (fun input ->
      try f input with _ -> false)

let p n =
  Patch.
    {
      (patch n []) with
      title = "title" ^ Int.to_string n;
      description = "description" ^ Int.to_string n;
      changes = [ "change" ^ Int.to_string n ];
      spec = "spec" ^ Int.to_string n;
    }

let render patches notes = Feature_pr_body.render ~gameplan ~patches ~notes

let tests =
  [
    property "feature PR renders typed decisions across all resolution kinds"
      (G.triple G.string G.string G.string)
      (fun (choice, evidence, tradeoffs) ->
        let choice = "Chosen: " ^ choice in
        let evidence = "Evidence: " ^ evidence in
        let tradeoffs = "Tradeoff: " ^ tradeoffs in
        List.for_all
          [
            ( "engineer_approved",
              fun basis -> Architecture_design.Engineer_approved basis );
            ("constrained", fun basis -> Architecture_design.Constrained basis);
            ("delegated", fun basis -> Architecture_design.Delegated basis);
          ]
          ~f:(fun (kind, resolve) ->
            let basis =
              Architecture_design.{ evidence; rationale = "One write owner" }
            in
            let design =
              Architecture_design.
                {
                  summary = "Application owns admission";
                  decisions =
                    [
                      {
                        id = "AD-1";
                        topic = "ownership";
                        question = "Who owns admission?";
                        choice;
                        alternatives =
                          [
                            {
                              alternative_choice = "Worker admission";
                              tradeoffs;
                            };
                          ];
                        resolution = resolve basis;
                      };
                    ];
                }
            in
            let gameplan =
              Gameplan.{ gameplan with architecture_design = Some design }
            in
            let body = Feature_pr_body.render ~gameplan ~patches:[] ~notes:[] in
            List.for_all
              [
                "Application owns admission";
                "AD-1";
                "Who owns admission?";
                choice;
                evidence;
                "Resolution: " ^ kind ^ " — " ^ evidence;
                "One write owner";
                "Worker admission";
                tradeoffs;
              ]
              ~f:(fun expected -> String.is_substring body ~substring:expected)));
    property "feature PR refresh preserves architectural design once" G.string
      (fun design ->
        let design = "Resolved architecture: " ^ design in
        let gameplan =
          Gameplan.
            {
              gameplan with
              architecture_design =
                Some Architecture_design.{ summary = design; decisions = [] };
            }
        in
        let expected = "## Architectural Design\n\n" ^ design ^ "\n\n" in
        let body = Feature_pr_body.render ~gameplan ~patches:[] ~notes:[] in
        let populated =
          Feature_pr_body.render ~gameplan ~patches:[ p 1; p 2 ] ~notes:[]
        in
        String.equal body expected
        && String.equal body (Feature_pr_body.render_contributions ~gameplan [])
        && String.is_prefix populated ~prefix:expected
        && not
             (String.is_substring
                (String.drop_prefix populated (String.length expected))
                ~substring:expected));
    property "render total over arbitrary patch content and notes"
      (G.list (G.triple G.string G.string G.string))
      (fun entries ->
        let patches =
          List.mapi entries ~f:(fun i (description, spec, note) ->
              Patch.{ (p i) with description; spec; changes = [ note ] })
        in
        let notes = List.mapi entries ~f:(fun i (_, _, note) -> (id i, note)) in
        ignore (render patches notes : string);
        true);
    property "empty optional content emits empty body" G.unit (fun () ->
        String.equal (render [] []) "");
    property "single patch renders reviewer content" G.unit (fun () ->
        String.equal
          (render [ p 1 ] [ (id 1, "decision") ])
          "## Changes\n\n\
           ### Patch 1: title1\n\n\
           - change1\n\n\
           ## Patch Specifications\n\n\
           ### Patch 1: title1\n\n\
           ```\n\
           spec1\n\
           ```\n\n\
           ## Implementation Notes\n\n\
           ### Patch 1: title1\n\n\
           decision\n\n");
    property "canonical changes render once despite parsed description" G.string
      (fun change ->
        let change = String.strip change in
        let input =
          `Assoc
            [
              ("projectName", `String "render-test");
              ( "patches",
                `List
                  [
                    `Assoc
                      [
                        ("number", `Int 1);
                        ("title", `String "patch");
                        ("changes", `List [ `String change ]);
                      ];
                  ] );
            ]
          |> Yojson.Safe.to_string
        in
        match Gameplan_parser.parse_json_string input with
        | Error _ -> false
        | Ok parsed ->
            let body =
              render parsed.Gameplan_parser.gameplan.Gameplan.patches []
            in
            if String.is_empty change then String.is_empty body
            else
              String.equal body
                ("## Changes\n\n### Patch 1: patch\n\n- " ^ change ^ "\n\n"));
    property "blank changes never create bullets"
      (G.list (G.oneof_list [ ""; " "; "\n\t" ]))
      (fun changes ->
        String.is_empty (render [ Patch.{ (patch 1 []) with changes } ] []));
    property "cached contributions preserve supplied reviewer-note order"
      (G.shuffle_list [ 1; 2; 3 ])
      (fun ids ->
        let contributions =
          List.map ids ~f:(fun n ->
              Feature_pr_body.render_contribution ~patch:(patch n [])
                ~notes:(Some ("note" ^ Int.to_string n)))
        in
        let body =
          Feature_pr_body.render_contributions ~gameplan contributions
        in
        let _, valid =
          List.fold ids ~init:(-1, true) ~f:(fun (previous, valid) n ->
              match
                String.substr_index body ~pattern:("note" ^ Int.to_string n)
              with
              | None -> (previous, false)
              | Some position -> (position, valid && position > previous))
        in
        valid);
    property "refresh deterministic for same included patches"
      (G.list (G.int_range 1 8))
      (fun ids ->
        let patches = List.map ids ~f:p in
        String.equal (render patches []) (render patches []));
    property "notes only included for represented patches" G.string
      (fun notes ->
        String.equal (render [ p 1 ] [ (id 2, notes) ]) (render [ p 1 ] []));
    property "whitespace notes omit section"
      (G.oneof_list [ ""; " "; "\n\t" ])
      (fun notes ->
        String.equal (render [ p 1 ] [ (id 1, notes) ]) (render [ p 1 ] []));
    property "incremental integration preserves all previous contributions"
      (G.list (G.int_range 1 8))
      (fun ops ->
        let _, valid =
          List.fold ops ~init:([], true) ~f:(fun (included, valid) n ->
              let after =
                if List.mem included n ~equal:Int.equal then included
                else included @ [ n ]
              in
              let body =
                render (List.map after ~f:p)
                  (List.map after ~f:(fun i -> (id i, "note" ^ Int.to_string i)))
              in
              ( after,
                valid
                && List.for_all after ~f:(fun i ->
                    List.for_all [ "title"; "change"; "spec"; "note" ]
                      ~f:(fun prefix ->
                        String.is_substring body
                          ~substring:(prefix ^ Int.to_string i))) ))
        in
        valid);
    property "note order follows patches independently of artifact read order"
      (G.shuffle_list [ (id 1, "note1"); (id 2, "note2") ])
      (fun notes ->
        String.equal
          (render [ p 1; p 2 ] notes)
          (render [ p 1; p 2 ] [ (id 1, "note1"); (id 2, "note2") ]));
  ]

let () = QCheck_base_runner.run_tests_main tests

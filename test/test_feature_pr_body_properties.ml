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
      solution_summary = "";
      final_state_spec = "";
      patches = [];
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
           description1\n\n\
           - change1\n\n\
           ## Patch Specifications\n\n\
           ### Patch 1: title1\n\n\
           ```\n\
           spec1\n\
           ```\n\n\
           ## Implementation Notes\n\n\
           ### Patch 1: title1\n\n\
           decision\n\n");
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
                    List.for_all [ "description"; "change"; "spec"; "note" ]
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

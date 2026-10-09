(* @archlint.module test
   @archlint.domain project-lifecycle *)

open Onton_core
module P = Project_identity
module G = QCheck2.Gen

let names =
  G.string_size
    ~gen:(G.oneof_list [ 'a'; 'b'; 'c'; '0'; '9'; '-' ])
    (G.int_range 1 60)

let alias name = function
  | 0 -> name
  | 1 -> String.uppercase_ascii name
  | _ -> name ^ "!"

let tests =
  [
    QCheck2.Test.make ~name:"project identity resolution is total" ~count:1000
      (G.pair G.string (G.option G.string))
      (fun (requested, stored) ->
        try
          ignore (P.resolve ~requested ~stored);
          true
        with _ -> false);
    QCheck2.Test.make ~name:"storage normalization is idempotent and path-safe"
      ~count:1000 G.string (fun name ->
        let slug = P.slug name in
        P.slug slug = slug
        && String.for_all
             (function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
             slug);
    QCheck2.Test.make ~name:"fresh identity preserves exact spelling"
      ~count:1000 G.string (fun requested ->
        match P.resolve ~requested ~stored:None with
        | Ok name -> name = requested && P.slug name <> ""
        | Error _ -> P.slug requested = "");
    QCheck2.Test.make
      ~name:"aliases cannot change an established recovery namespace" ~count:500
      (G.pair names (G.list_size (G.int_range 0 100) (G.int_range 0 2)))
      (fun (name, history) ->
        let original = String.uppercase_ascii name ^ "!" in
        let prefix =
          Branch_reconcile.recovery_project_prefix ~project:original
        in
        let final =
          List.fold_left
            (fun state index ->
              match state with
              | Error _ -> state
              | Ok stored ->
                  P.resolve ~requested:(alias name index) ~stored:(Some stored))
            (Ok original) history
        in
        match final with
        | Error _ -> false
        | Ok stored ->
            stored = original
            && Branch_reconcile.recovery_project_prefix ~project:stored = prefix);
    QCheck2.Test.make ~name:"mismatched or empty storage identities fail closed"
      ~count:1000 (G.pair G.string G.string) (fun (requested, stored) ->
        match P.resolve ~requested ~stored:(Some stored) with
        | Ok name ->
            name = stored
            && P.slug requested <> ""
            && P.slug requested = P.slug stored
        | Error _ -> P.slug requested = "" || P.slug requested <> P.slug stored);
    QCheck2.Test.make ~name:"legacy spaces underscores and case remain aliases"
      ~count:1 G.unit (fun () ->
        List.for_all
          (fun requested ->
            P.resolve ~requested ~stored:(Some "Project One") = Ok "Project One")
          [ "Project One"; "PROJECT_ONE"; "project-one"; "project-one!" ]
        && List.for_all
             (fun requested ->
               Result.is_error (P.resolve ~requested ~stored:None))
             [ ""; "..."; "/"; "\000" ]);
  ]

let () = QCheck_base_runner.run_tests_main tests

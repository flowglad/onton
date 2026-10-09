(* @archlint.module test
   @archlint.domain project-lifecycle *)

open Onton_core
module P = Project_retirement
module G = QCheck2.Gen

let valid_slug =
  G.string_size
    ~gen:(G.oneof_list [ 'a'; 'z'; '0'; '9'; '-' ])
    (G.int_range 1 100)

let tests =
  [
    QCheck2.Test.make
      ~name:"lifecycle diagnostics retain contention and IO context" ~count:500
      G.string (fun text ->
        P.error_message (P.Busy text) = "project lifecycle is in use: " ^ text
        && P.error_message (P.Io_error text) = text);
    QCheck2.Test.make ~name:"retirement decoders are total" ~count:1000 G.string
      (fun text ->
        try
          ignore (P.make ~slug:text);
          ignore (P.decode text);
          true
        with _ -> false);
    QCheck2.Test.make
      ~name:"retirement manifest roundtrip and normalization are idempotent"
      ~count:1000 valid_slug (fun slug ->
        try
          match P.make ~slug with
          | None -> false
          | Some manifest -> (
              P.slug manifest = slug
              &&
              match P.decode (P.encode manifest) with
              | Some restored ->
                  P.equal manifest restored
                  && P.encode restored = P.encode manifest
              | None -> false)
        with _ -> false);
    QCheck2.Test.make ~name:"retirement arbitrary manifests cannot select paths"
      ~count:1000 G.string (fun text ->
        match P.decode text with
        | None -> true
        | Some manifest ->
            P.encode manifest = text
            && (not (String.contains (P.slug manifest) '/'))
            && not (String.contains (P.slug manifest) '\000'));
    QCheck2.Test.make ~name:"retirement schema and path boundaries fail closed"
      ~count:200 valid_slug (fun slug ->
        List.for_all
          (fun text -> P.decode text = None)
          [
            "";
            slug;
            "onton-retired-project-v2\n" ^ slug ^ "\n";
            "onton-retired-project-v1\n../" ^ slug ^ "\n";
            "onton-retired-project-v1\n\n";
            "onton-retired-project-v1\n" ^ slug ^ "\nextra\n";
          ]
        && List.for_all
             (fun slug -> P.make ~slug = None)
             [ ""; ".."; "/tmp"; "UPPER"; "a\nb" ]);
  ]

let () = QCheck_base_runner.run_tests_main tests

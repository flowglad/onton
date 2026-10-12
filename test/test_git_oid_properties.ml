(* @archlint.module test
   @archlint.domain git-oid *)

open Onton_core
module G = QCheck2.Gen

let () =
  QCheck_base_runner.run_tests_main
    [
      QCheck2.Test.make
        ~name:"Git OID validation is total over arbitrary strings" ~count:1000
        G.string (fun value ->
          try
            ignore (Git_oid.valid value);
            true
          with _ -> false);
      QCheck2.Test.make
        ~name:"only complete lowercase SHA1 and SHA256 identifiers are accepted"
        ~count:500
        G.(
          string_size
            ~gen:(oneof_list [ 'a'; 'f'; '0'; '9'; 'A'; 'g'; '/'; ' ' ])
            (oneof [ int_range 0 100; return 40; return 64 ]))
        (fun value ->
          Git_oid.valid value
          = ((String.length value = 40 || String.length value = 64)
            && String.for_all
                 (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
                 value));
      QCheck2.Test.make
        ~name:"valid OIDs remain valid under character permutation" ~count:300
        G.(
          oneof
            [
              list_size (return 40) (oneof_list [ 'a'; '0'; 'f'; '9' ]);
              list_size (return 64) (oneof_list [ 'a'; '0'; 'f'; '9' ]);
            ])
        (fun chars ->
          let value = String.of_seq (List.to_seq chars) in
          Git_oid.valid value
          && Git_oid.valid (String.of_seq (List.to_seq (List.rev chars))));
    ]

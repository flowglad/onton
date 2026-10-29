(* @archlint.module test
   @archlint.domain coordinator-state *)
open Onton_core
module A = Coordinator_archive

let test name gen f =
  QCheck2.Test.make ~count:500 ~name gen (fun x -> try f x with _ -> false)

let () =
  assert (A.decode ~max_bytes:0 (A.encode "") = Ok "");
  List.iter
    (fun n ->
      let bytes = String.make n '\255' ^ String.make n '\000' ^ "tail" in
      assert (
        A.decode ~max_bytes:(String.length bytes) (A.encode bytes) = Ok bytes))
    [ 1; 4; 65534; 65535; 65536; 131072 ];
  QCheck_base_runner.run_tests_main
    [
      test "archive decoding is total and bounded" QCheck2.Gen.string
        (fun bytes ->
          match A.decode ~max_bytes:4096 bytes with
          | Ok s -> String.length s <= 4096
          | Error _ -> true);
      test "lossless deterministic byte roundtrip" QCheck2.Gen.string
        (fun bytes ->
          let packed = A.encode bytes in
          packed = A.encode bytes
          && A.decode ~max_bytes:(String.length bytes) packed = Ok bytes);
      test "oversized decoded output is rejected" QCheck2.Gen.string
        (fun bytes ->
          match
            A.decode ~max_bytes:(String.length bytes - 1) (A.encode bytes)
          with
          | Error _ -> true
          | Ok _ -> false);
      test "truncated histories cannot be accepted"
        QCheck2.Gen.(pair string int)
        (fun (bytes, seed) ->
          let packed = A.encode bytes in
          let cut = seed land max_int mod String.length packed in
          match A.decode ~max_bytes:65536 (String.sub packed 0 cut) with
          | Error _ -> true
          | Ok _ -> false);
      test "trailing bytes cannot change a committed history" QCheck2.Gen.string
        (fun bytes ->
          match
            A.decode ~max_bytes:(String.length bytes)
              (A.encode bytes ^ "\000\000\000")
          with
          | Error _ -> true
          | Ok _ -> false);
      test "repetition across the fixed window remains lossless"
        QCheck2.Gen.(pair (int_range 0 200000) string)
        (fun (n, s) ->
          let bytes = s ^ String.make n 'x' ^ s ^ String.make n 'x' in
          A.decode ~max_bytes:(String.length bytes) (A.encode bytes) = Ok bytes);
      QCheck2.Test.make ~count:500
        ~name:"archive public API generated roundtrip" QCheck2.Gen.string
        (fun bytes ->
          let packed = A.encode bytes in
          A.decode ~max_bytes:(String.length bytes) packed = Ok bytes);
    ]

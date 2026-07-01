(* @archlint.module test
   @archlint.domain ci-log-digest *)

open Base
open Onton_core

let read_fixture name =
  let path = "fixtures/ci_logs/" ^ name in
  let ic = Stdlib.open_in_bin path in
  Exn.protect
    ~finally:(fun () -> Stdlib.close_in_noerr ic)
    ~f:(fun () ->
      let len = Stdlib.in_channel_length ic in
      Stdlib.really_input_string ic len)

let contains haystack needle = String.is_substring haystack ~substring:needle

let printable_counterexample s =
  let render chars =
    chars
    |> List.map ~f:(fun c ->
        let code = Char.to_int c in
        if code >= 32 && code <= 126 then String.of_char c
        else Printf.sprintf "\\x%02x" code)
    |> String.concat ~sep:""
  in
  let chars = String.to_list s in
  Printf.sprintf "len=%d head=%S tail=%S" (String.length s)
    (chars |> Fn.flip List.take 160 |> render)
    (chars |> List.rev |> Fn.flip List.take 160 |> List.rev |> render)

let gen_arbitrary_bytes =
  QCheck2.Gen.(
    string_size
      ~gen:(char_range (Char.of_int_exn 0) (Char.of_int_exn 255))
      (int_range 0 20_000))

let gen_plain_line =
  let open QCheck2.Gen in
  let safe_char =
    oneof_list
      [ 'a'; 'b'; 'c'; 'x'; 'y'; 'z'; '0'; '1'; ' '; '-'; '_'; '/'; ':' ]
  in
  string_size ~gen:safe_char (int_range 0 80)

let gen_marker_free_log =
  QCheck2.Gen.(
    list_size (int_range 0 80) gen_plain_line >|= fun lines ->
    String.concat lines ~sep:"\n")

let gen_error_lines_log =
  let open QCheck2.Gen in
  let* prefix = list_size (int_range 0 12) gen_plain_line in
  let* suffix = list_size (int_range 0 12) gen_plain_line in
  let* error_count = int_range 1 8 in
  let* errors =
    list_size (return error_count)
      (map
         (fun msg ->
           "@pkg test:unit: ::error \
            file=src/example.ts,line=4,col=2,title=fail::" ^ msg)
         gen_plain_line)
  in
  return (String.concat (prefix @ errors @ suffix) ~sep:"\n", errors)

let prop_digest_total_and_capped =
  QCheck2.Test.make ~count:500
    ~name:"digest is total and summary_md stays within the byte cap"
    gen_arbitrary_bytes (fun log ->
      try
        let e = Ci_log_digest.digest { annotations = []; log = Some log } in
        String.length e.summary_md <= Ci_log_digest.summary_total_cap_bytes
      with _ -> false)

let prop_literal_error_lines_preserved =
  QCheck2.Test.make
    ~name:"literal ::error lines are preserved in the summary when under cap"
    gen_error_lines_log (fun (log, errors) ->
      try
        let e = Ci_log_digest.digest { annotations = []; log = Some log } in
        e.signal > 0
        && List.for_all errors ~f:(fun line -> contains e.summary_md line)
        &&
        match e.teaser with
        | Some teaser ->
            contains teaser "::error" || not (String.is_empty teaser)
        | None -> false
      with _ -> false)

let prop_marker_free_has_no_signal_or_teaser =
  QCheck2.Test.make ~name:"marker-free logs produce zero signal and no teaser"
    gen_marker_free_log (fun log ->
      try
        let e = Ci_log_digest.digest { annotations = []; log = Some log } in
        Int.equal e.signal 0 && Option.is_none e.teaser
      with _ -> false)

let prop_strip_log_idempotent =
  QCheck2.Test.make ~count:500 ~name:"strip_log is idempotent"
    ~print:printable_counterexample gen_arbitrary_bytes (fun log ->
      let stripped = Ci_log_digest.strip_log log in
      String.equal (Ci_log_digest.strip_log stripped) stripped)

let assert_contains label haystack needle =
  if not (contains haystack needle) then
    failwith (Printf.sprintf "%s: expected substring %S" label needle)

let assert_not_contains label haystack needle =
  if contains haystack needle then
    failwith (Printf.sprintf "%s: unexpected substring %S" label needle)

let test_strip_log_removes_timestamps_and_ansi () =
  let input = "2026-07-01T19:33:24.1348805Z \027[31mred\027[0m\nplain" in
  let stripped = Ci_log_digest.strip_log input in
  assert (String.equal stripped "red\nplain");
  assert (String.equal (Ci_log_digest.strip_log stripped) stripped)

let test_strip_log_normalizes_crlf () =
  let input =
    "2026-07-01T19:33:24.1348805Z first\r\nsecond\rthird\r\r\nfourth"
  in
  let stripped = Ci_log_digest.strip_log input in
  assert (String.equal stripped "first\nsecond\nthird\n\nfourth");
  assert (String.equal (Ci_log_digest.strip_log stripped) stripped)

let test_lowerability_fixture () =
  let log = read_fixture "lowerability_fail.log" in
  let e = Ci_log_digest.digest { annotations = []; log = Some log } in
  assert_contains "lowerability summary" e.summary_md
    "src/lowerability.ts:42:13";
  match e.teaser with
  | Some teaser ->
      assert_contains "lowerability teaser" teaser "src/lowerability.ts:42:13"
  | None -> failwith "lowerability teaser: expected Some _"

let test_bun_fixture () =
  let log = read_fixture "bun_test_fail.log" in
  let e = Ci_log_digest.digest { annotations = []; log = Some log } in
  assert_contains "bun summary" e.summary_md
    "::error file=packages/api/src/routes.test.ts,line=18,col=7,title=TypeError";
  assert_contains "bun summary" e.summary_md
    "TypeError: expected user.id to be a string";
  assert_contains "bun summary" e.summary_md "18 | expect(user.id).toBeString()"

let test_mid_job_fixture () =
  let log = read_fixture "mid_job_fail.log" in
  let e = Ci_log_digest.digest { annotations = []; log = Some log } in
  assert_contains "mid-job summary" e.summary_md
    "lint/src/server.ts:9:3 lint/suspicious/noExplicitAny";
  assert_contains "mid-job summary" e.summary_md
    "Unexpected any. Specify a different type.";
  assert_not_contains "mid-job summary" e.summary_md
    "POST_FAILURE_PASSING_STEP_SENTINEL"

let test_no_marker_fixture () =
  let log = read_fixture "no_marker_fail.log" in
  let e = Ci_log_digest.digest { annotations = []; log = Some log } in
  assert (not (String.is_empty e.summary_md));
  assert_contains "no-marker summary" e.summary_md
    "Deploy failed while waiting for health check"

let test_truncation_is_noted () =
  let message = String.make (Ci_log_digest.summary_total_cap_bytes * 2) 'a' in
  let annotation =
    Ci_log_digest.
      { path = Some "src/large.ts"; line = Some 1; level = "failure"; message }
  in
  let e =
    Ci_log_digest.digest
      { annotations = [ annotation ]; log = Some "::error ::later section" }
  in
  assert (String.length e.summary_md <= Ci_log_digest.summary_total_cap_bytes);
  assert_contains "truncation summary" e.summary_md
    "diagnostic summary truncated"

let test_exact_cap_preserves_content () =
  let prefix = "## Failure annotations\n\n- p:1: " in
  let suffix = "\n" in
  let message_len =
    Ci_log_digest.summary_total_cap_bytes - String.length prefix
    - String.length suffix
  in
  let message = String.make message_len 'z' in
  let annotation =
    Ci_log_digest.{ path = Some "p"; line = Some 1; level = "failure"; message }
  in
  let e =
    Ci_log_digest.digest
      { annotations = [ annotation ]; log = Some "::error ::later section" }
  in
  assert (String.length e.summary_md <= Ci_log_digest.summary_total_cap_bytes);
  assert_not_contains "exact-cap summary" e.summary_md
    "diagnostic summary truncated";
  assert (String.is_suffix e.summary_md ~suffix:(String.make 32 'z' ^ "\n"))

let () =
  test_strip_log_removes_timestamps_and_ansi ();
  test_strip_log_normalizes_crlf ();
  test_lowerability_fixture ();
  test_bun_fixture ();
  test_mid_job_fixture ();
  test_no_marker_fixture ();
  test_truncation_is_noted ();
  test_exact_cap_preserves_content ();
  let tests =
    [
      prop_digest_total_and_capped;
      prop_literal_error_lines_preserved;
      prop_marker_free_has_no_signal_or_teaser;
      prop_strip_log_idempotent;
    ]
  in
  Stdlib.exit (QCheck_base_runner.run_tests ~verbose:false tests)

(* @archlint.module test
   @archlint.domain github *)

open Base
open Onton_core

let assert_ok_annotations body ~f =
  match Onton.Github.parse_check_annotations_response body with
  | Ok annotations -> f annotations
  | Error err ->
      failwith
        (Printf.sprintf "unexpected parse error: %s"
           (Onton.Github.show_error err))

let test_field_mapping_and_nulls () =
  let body =
    {|
[
  {
    "path": "lib/foo.ml",
    "start_line": 42,
    "annotation_level": "failure",
    "message": "expected int",
    "end_line": 42,
    "raw_details": "extra detail"
  },
  {
    "path": null,
    "start_line": null,
    "annotation_level": "warning",
    "message": "ignored shape is still mapped",
    "unexpected": {"nested": true}
  }
]
|}
  in
  assert_ok_annotations body ~f:(function
    | [ first; second ] ->
        assert (
          Option.equal String.equal first.Ci_log_digest.path (Some "lib/foo.ml"));
        assert (Option.equal Int.equal first.Ci_log_digest.line (Some 42));
        assert (String.equal first.Ci_log_digest.level "failure");
        assert (String.equal first.Ci_log_digest.message "expected int");
        assert (Option.is_none second.Ci_log_digest.path);
        assert (Option.is_none second.Ci_log_digest.line);
        assert (String.equal second.Ci_log_digest.level "warning");
        assert (
          String.equal second.Ci_log_digest.message
            "ignored shape is still mapped")
    | annotations ->
        failwith
          (Printf.sprintf "unexpected annotation count: %d"
             (List.length annotations)))

let test_empty_array () =
  assert_ok_annotations "[]" ~f:(function
    | [] -> ()
    | _ -> failwith "expected empty annotation list")

let test_malformed_json () =
  match Onton.Github.parse_check_annotations_response "{not json" with
  | Error (Onton.Github.Json_parse_error _) -> ()
  | Ok _ -> failwith "expected Json_parse_error"
  | Error
      (( Onton.Github.Http_error _ | Onton.Github.Graphql_error _
       | Onton.Github.Timeout _ | Onton.Github.Transport_error _ ) as err) ->
      failwith
        (Printf.sprintf "expected Json_parse_error, got %s"
           (Onton.Github.show_error err))

let () =
  test_field_mapping_and_nulls ();
  test_empty_array ();
  test_malformed_json ();
  Stdlib.print_endline "test_github_check_details: OK"

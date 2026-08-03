(* @archlint.module test
   @archlint.domain repo-config *)

open Onton_core

let known_backends = [ "claude"; "codex" ]

let parse_timeout timeout =
  let raw = Printf.sprintf {|{"automerge_timeout":%.17g}|} timeout in
  match Repo_config.parse_string ~known_backends raw with
  | Ok config -> Ok config.Repo_config.automerge_timeout
  | Error error -> Error error

let equal_parse_result left right =
  match (left, right) with
  | Ok left, Ok right -> Option.equal Float.equal left right
  | Error left, Error right -> String.equal left right
  | Ok _, Error _ | Error _, Ok _ -> false

let parse_is_total =
  QCheck2.Test.make ~name:"repo config parsing is total" ~count:1000
    QCheck2.Gen.string (fun raw ->
      try
        ignore (Repo_config.parse_string ~known_backends raw);
        true
      with _ -> false)

let positive_automerge_timeout_round_trips =
  QCheck2.Test.make
    ~name:"positive finite automerge timeouts parse without changing value"
    ~count:500
    QCheck2.Gen.(float_range 0.001 10000.)
    (fun timeout ->
      match parse_timeout timeout with
      | Ok parsed -> Option.equal Float.equal parsed (Some timeout)
      | Error _ -> false)

let automerge_timeout_boundaries =
  QCheck2.Test.make
    ~name:
      "automerge timeout enforces zero, negative, near-zero, and finite \
       boundaries" ~count:1 QCheck2.Gen.unit (fun () ->
      let rejected_numbers = [ 0.; -0.; -1.; -1e-300 ] in
      let rejected_raw = [ "NaN"; "Infinity"; "-Infinity"; "1e999" ] in
      let rejects_number timeout = Result.is_error (parse_timeout timeout) in
      let rejects_raw timeout =
        Result.is_error
          (Repo_config.parse_string ~known_backends
             (Printf.sprintf {|{"automerge_timeout":%s}|} timeout))
      in
      let near_zero = Float.next_after 0. Float.infinity in
      List.for_all rejects_number rejected_numbers
      && List.for_all rejects_raw rejected_raw
      &&
      match parse_timeout near_zero with
      | Ok parsed -> Option.equal Float.equal parsed (Some near_zero)
      | Error _ -> false)

let repeated_parse_order_independent =
  QCheck2.Test.make ~name:"repeated repo config parses are order independent"
    ~count:300
    QCheck2.Gen.(list_size (int_range 0 40) (float_range 0.001 10000.))
    (fun timeouts ->
      let forward = List.map parse_timeout timeouts in
      let reverse_then_restore =
        List.rev (List.map parse_timeout (List.rev timeouts))
      in
      List.equal equal_parse_result forward reverse_then_restore)

let () =
  QCheck2.Test.check_exn parse_is_total;
  QCheck2.Test.check_exn positive_automerge_timeout_round_trips;
  QCheck2.Test.check_exn automerge_timeout_boundaries;
  QCheck2.Test.check_exn repeated_parse_order_independent

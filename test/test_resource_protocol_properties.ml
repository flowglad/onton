(* @archlint.module test
   @archlint.domain resource-protocol *)

open Onton_core

let request ?(request_id = "id") op extra =
  Printf.sprintf
    {|{"version":"resource-v1","op":"%s","run_id":"r","request_id":%s,"coordinator_id":"c","owner_epoch":"0","revision":"0"%s}|}
    op
    (Yojson.Safe.to_string (`String request_id))
    extra

let check condition message = if not condition then failwith message

let () =
  let open Resource_protocol in
  let valid =
    request "state-write" {|,"write_id":"w","state_version":"0","payload":""|}
  in
  check (Result.is_ok (make_request valid)) "empty state is valid";
  List.iter
    (fun input -> check (Result.is_error (make_request input)) input)
    [
      request "observe" "";
      request "state-write" {|,"write_id":"w","state_version":"01"|};
      request "state-write"
        {|,"write_id":"w","state_version":"0","payload":"AAB="|};
      valid ^ "null";
      {|{"version":"resource-v1","version":"resource-v1"}|};
    ];
  let decode x =
    match decode_response x with
    | Result.Ok x -> x
    | Result.Error e -> failwith e
  in
  let huge =
    decode {|{"status":"ok","reason":"none","revision":9223372036854775807}|}
  in
  check (huge.revision = Some Int64.max_int) "exact int64";
  check
    (Result.is_error
       (decode_response
          {|{"status":"ok","reason":"none","revision":9223372036854775808}|}))
    "int64 overflow";
  check
    (Result.is_error
       (decode_response {|{"status":"ok","reason":"none","revision":0.0}|}))
    "float counter";
  let read = decode {|{"status":"ok","reason":"none","payload":""}|} in
  check (read.payload = Some "") "empty present state";
  let absent = decode {|{"status":"rejected","reason":"not_found"}|} in
  check (absent.payload = None) "missing state";
  check
    (not (retirement_evidence Observe ~process_exit:0 ~matching:true read))
    "observe is not retirement";
  check
    (retirement_evidence Stop ~process_exit:0 ~matching:true read)
    "matching stop is retirement";
  let task = decode {|{"status":"ok","reason":"none","resource_id":"p"}|} in
  check
    (retirement_evidence Replace ~process_exit:0 ~matching:true task)
    "guarded replacement confirms predecessor retirement";
  check
    (not (retirement_evidence Replace ~process_exit:0 ~matching:false task))
    "replacement needs the matching guard";
  check
    (not (retirement_evidence Stop ~process_exit:1 ~matching:true read))
    "nonzero exit";
  let pending = decode {|{"status":"pending","reason":"cleanup_pending"}|} in
  check
    (not (retirement_evidence Stop ~process_exit:0 ~matching:true pending))
    "pending stop does not confirm retirement";
  let reasons =
    [
      "none";
      "invalid_request";
      "not_found";
      "not_owner";
      "stale_guard";
      "changed_request";
      "closed";
      "closing";
      "chain_busy";
      "predecessor_live";
      "launch_uncertain";
      "observation_exhausted";
      "state_version";
      "storage_uncertain";
      "corrupt_object";
      "cleanup_pending";
      "invariant_violation";
    ]
  in
  List.iter
    (fun status ->
      List.iter
        (fun reason ->
          check
            (Result.is_ok
               (decode_response
                  (Printf.sprintf {|{"status":"%s","reason":"%s"}|} status
                     reason)))
            (status ^ "/" ^ reason))
        reasons)
    [ "ok"; "rejected"; "pending"; "attention" ];
  check
    (Result.is_error (make_request (String.make (max_envelope + 1) 'x')))
    "oversized envelope";
  check
    (Result.is_error
       (make_request
          (request "state-write"
             (Printf.sprintf
                {|,"write_id":"w","state_version":"0","payload":"%s"|}
                (String.make (4 * ((((8 * 1024 * 1024) + 3) / 3) + 2)) 'A')))))
    "oversized state";
  check
    (Result.is_error
       (make_request
          (request "observe" {|,"resource_id":"p","resource_id":"p"|})))
    "duplicate request key";
  check
    (Result.is_error
       (decode_response
          {|{"status":"ok","reason":"none","revision":1,"revision":2}|}))
    "duplicate response key";
  List.iter
    (fun (bytes, expected) ->
      check
        (Bool.equal (utf8_valid bytes) expected)
        ("UTF-8 validation: " ^ String.escaped bytes))
    [
      ("", true);
      ("plain ASCII", true);
      ("\xC3\xA9", true);
      ("\xF0\x9F\x8C\xB1", true);
      ("\x00", true);
      ("\x80", false);
      ("\xC0\xAF", false);
      ("\xE2\x82", false);
      ("\xED\xA0\x80", false);
      ("\xF4\x90\x80\x80", false);
    ];
  let gen = QCheck2.Gen.string in
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~count:500 gen (fun s ->
         try
           ignore (make_request s);
           ignore (decode_response s);
           true
         with _ -> false));
  print_endline "resource protocol properties passed";
  QCheck_base_runner.run_tests_main
    [
      QCheck2.Test.make ~name:"utf8_valid is total" ~count:500
        QCheck2.Gen.string (fun s ->
          try
            ignore (utf8_valid s);
            true
          with _ -> false);
      QCheck2.Test.make ~name:"public resource codec helpers" ~count:100
        (QCheck2.Gen.int_bound 1_000_000) (fun seed ->
          let open Resource_protocol in
          let bytes =
            request
              ~request_id:("request-" ^ string_of_int seed)
              "state-write" {|,"write_id":"w","state_version":"0","payload":""|}
          in
          match make_request bytes with
          | Result.Error _ -> false
          | Result.Ok request -> (
              match encode_request request with
              | Result.Error _ -> false
              | Result.Ok encoded -> (
                  let observed =
                    decode_response
                      {|{"status":"ok","reason":"none","resource_id":"p"}|}
                  in
                  String.equal encoded bytes
                  && String.equal (op_name State_write) "state-write"
                  &&
                  match observed with
                  | Result.Error _ -> false
                  | Result.Ok response ->
                      (not
                         (retirement_evidence Observe ~process_exit:0
                            ~matching:true response))
                      && retirement_evidence Stop ~process_exit:0 ~matching:true
                           response)));
    ]

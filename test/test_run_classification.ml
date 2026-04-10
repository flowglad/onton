open Base
open Onton

(** QCheck2 property-based tests for [Run_classification.classify]. *)

let () =
  let open QCheck2 in
  let open Run_classification in
  let open Onton_test_support.Test_generators in
  (* Error -> Process_error *)
  let prop_error_is_process_error =
    Test.make ~name:"classify: Error msg -> Process_error msg" ~count:500
      Gen.(string_size ~gen:printable (int_range 1 80))
      (fun msg ->
        match classify ~is_resume:false (Error msg) with
        | Process_error m -> String.equal m msg
        | No_session_to_resume | Timed_out | Success _ | Session_failed _ ->
            false)
  in

  (* Ok with timed_out -> Timed_out *)
  let prop_timed_out =
    Test.make ~name:"classify: timed_out -> Timed_out" ~count:500
      gen_run_outcome (fun r ->
        let r = { r with timed_out = true } in
        match classify ~is_resume:false (Ok r) with
        | Timed_out -> true
        | Process_error _ | No_session_to_resume | Success _ | Session_failed _
          ->
            false)
  in

  (* Ok with no events + continue -> No_session_to_resume *)
  let prop_no_events_continue =
    Test.make ~name:"classify: no events + continue -> No_session_to_resume"
      ~count:500 gen_run_outcome (fun r ->
        let r = { r with got_events = false; timed_out = false } in
        match classify ~is_resume:true (Ok r) with
        | No_session_to_resume -> true
        | Process_error _ | Timed_out | Success _ | Session_failed _ -> false)
  in

  (* Ok with exit_code=0 -> Success *)
  let prop_exit_zero_success =
    Test.make ~name:"classify: exit_code=0 -> Success" ~count:500
      gen_run_outcome (fun r ->
        let r =
          { r with exit_code = 0; got_events = true; timed_out = false }
        in
        match classify ~is_resume:false (Ok r) with
        | Success _ -> true
        | Process_error _ | No_session_to_resume | Timed_out | Session_failed _
          ->
            false)
  in

  (* Non-zero exit code -> Session_failed *)
  let prop_nonzero_session_failed =
    Test.make ~name:"classify: non-zero exit -> Session_failed" ~count:500
      gen_run_outcome (fun r ->
        let r =
          { r with exit_code = 1; got_events = true; timed_out = false }
        in
        match classify ~is_resume:false (Ok r) with
        | Session_failed _ -> true
        | Process_error _ | No_session_to_resume | Timed_out | Success _ ->
            false)
  in

  (* Detail string always <= 503 chars (500 + "...") *)
  let prop_detail_bounded =
    Test.make ~name:"classify: detail string bounded" ~count:500 gen_run_outcome
      (fun r ->
        let r =
          { r with exit_code = 1; got_events = true; timed_out = false }
        in
        match classify ~is_resume:false (Ok r) with
        | Session_failed { detail; _ } -> String.length detail <= 503
        | Process_error _ | No_session_to_resume | Timed_out | Success _ ->
            false)
  in

  (* continue=false with no events still classifies by exit code *)
  let prop_no_continue_uses_exit_code =
    Test.make ~name:"classify: continue=false, no events -> uses exit code"
      ~count:500 gen_run_outcome (fun r ->
        let r = { r with got_events = false; timed_out = false } in
        match classify ~is_resume:false (Ok r) with
        | No_session_to_resume -> false (* should not happen without continue *)
        | Process_error _ | Timed_out | Success _ | Session_failed _ -> true)
  in

  let suite =
    [
      prop_error_is_process_error;
      prop_timed_out;
      prop_no_events_continue;
      prop_exit_zero_success;
      prop_nonzero_session_failed;
      prop_detail_bounded;
      prop_no_continue_uses_exit_code;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

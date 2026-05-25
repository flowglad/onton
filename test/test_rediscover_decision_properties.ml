open Base
open Onton_core
open Onton_core.Types
module Gen = QCheck2.Gen
module Test = QCheck2.Test
module RD = Rediscover_decision

(** Property tests for [Rediscover_decision.classify]: total, deterministic,
    exhaustive on the 2x3 decision table (2 in_gameplan x 3 result shapes). The
    bug-prevention case is row "Ok None + ~in_gameplan -> Mark_pr_missing" —
    assert it explicitly. *)

let gen_patch_id = Gen.(map Patch_id.of_string (string_size (int_range 3 12)))
let gen_pr_number = Gen.(map Pr_number.of_int (int_range 1 9999))
let gen_branch = Gen.(map Branch.of_string (string_size (int_range 3 20)))

let gen_replacement : RD.replacement Gen.t =
  Gen.map3
    (fun new_pr base_branch merged -> RD.{ new_pr; base_branch; merged })
    gen_pr_number gen_branch Gen.bool

let gen_result_ok_some = Gen.map (fun r -> Ok (Some r)) gen_replacement

let gen_result_ok_none : (RD.replacement option, string) Result.t Gen.t =
  Gen.return (Ok None)

let gen_result_error : (RD.replacement option, string) Result.t Gen.t =
  Gen.map (fun s -> Error s) Gen.(string_size (int_range 1 30))

let gen_input ~gen_result ~in_gameplan : RD.input Gen.t =
  Gen.map3
    (fun patch_id pr_number result ->
      RD.{ patch_id; pr_number; in_gameplan; result })
    gen_patch_id gen_pr_number gen_result

let gen_any_input : RD.input Gen.t =
  let any_result =
    Gen.oneof [ gen_result_ok_some; gen_result_ok_none; gen_result_error ]
  in
  Gen.(
    bool >>= fun in_gameplan -> gen_input ~gen_result:any_result ~in_gameplan)

(* ── Helpers to test variants without wildcard patterns ── *)

let is_switch_to : RD.classification -> bool = function
  | Switch_to_pr _ -> true
  | Clear_pr_for_recreate | Mark_pr_missing | Log_error _ -> false

let is_clear : RD.classification -> bool = function
  | Clear_pr_for_recreate -> true
  | Switch_to_pr _ | Mark_pr_missing | Log_error _ -> false

let is_mark_missing : RD.classification -> bool = function
  | Mark_pr_missing -> true
  | Switch_to_pr _ | Clear_pr_for_recreate | Log_error _ -> false

let is_log_error : RD.classification -> bool = function
  | Log_error _ -> true
  | Switch_to_pr _ | Clear_pr_for_recreate | Mark_pr_missing -> false

(* ── Properties ── *)

let prop_classify_total =
  Test.make ~name:"classify is total (never raises)" ~count:1000 gen_any_input
    (fun input ->
      try
        let _ = RD.classify input in
        true
      with _ -> false)

let prop_classify_deterministic =
  Test.make ~name:"classify is deterministic" ~count:500 gen_any_input
    (fun input ->
      RD.equal_classification (RD.classify input) (RD.classify input))

let prop_ok_some_is_switch =
  Test.make ~name:"Ok (Some _) -> Switch_to_pr regardless of in_gameplan"
    ~count:500
    Gen.(
      bool >>= fun ig ->
      gen_input ~gen_result:gen_result_ok_some ~in_gameplan:ig)
    (fun input -> is_switch_to (RD.classify input))

let prop_ok_none_gameplan_is_clear =
  Test.make ~name:"Ok None + in_gameplan=true -> Clear_pr_for_recreate"
    ~count:300 (gen_input ~gen_result:gen_result_ok_none ~in_gameplan:true)
    (fun input -> is_clear (RD.classify input))

let prop_ok_none_adhoc_is_mark_missing =
  Test.make
    ~name:"Ok None + in_gameplan=false -> Mark_pr_missing (bug-prevention case)"
    ~count:300 (gen_input ~gen_result:gen_result_ok_none ~in_gameplan:false)
    (fun input -> is_mark_missing (RD.classify input))

let prop_error_is_log =
  Test.make ~name:"Error _ -> Log_error regardless of in_gameplan" ~count:500
    Gen.(
      bool >>= fun ig -> gen_input ~gen_result:gen_result_error ~in_gameplan:ig)
    (fun input -> is_log_error (RD.classify input))

let prop_independent_of_patch_id_and_pr_number =
  (* Vary patch_id and pr_number while fixing (in_gameplan, result); the
     classification *tag* must remain the same. Compare via predicate match,
     not equality, because Switch_to_pr's payload is data-bearing. *)
  let same_tag a b =
    Bool.equal (is_switch_to a) (is_switch_to b)
    && Bool.equal (is_clear a) (is_clear b)
    && Bool.equal (is_mark_missing a) (is_mark_missing b)
    && Bool.equal (is_log_error a) (is_log_error b)
  in
  Test.make ~name:"classify depends only on (in_gameplan, result)" ~count:500
    Gen.(
      let base =
        bool >>= fun in_gameplan ->
        oneof [ gen_result_ok_some; gen_result_ok_none; gen_result_error ]
        >>= fun result -> return (in_gameplan, result)
      in
      base >>= fun (in_gameplan, result) ->
      pair gen_patch_id gen_pr_number >>= fun (p1, n1) ->
      pair gen_patch_id gen_pr_number >>= fun (p2, n2) ->
      return
        ( RD.{ patch_id = p1; pr_number = n1; in_gameplan; result },
          RD.{ patch_id = p2; pr_number = n2; in_gameplan; result } ))
    (fun (a, b) -> same_tag (RD.classify a) (RD.classify b))

let suite =
  [
    prop_classify_total;
    prop_classify_deterministic;
    prop_ok_some_is_switch;
    prop_ok_none_gameplan_is_clear;
    prop_ok_none_adhoc_is_mark_missing;
    prop_error_is_log;
    prop_independent_of_patch_id_and_pr_number;
  ]

let () =
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

(* @archlint.module test
   @archlint.domain types *)

open Base
open Onton_core
module Check = Types.Ci_check

let check ?(app_id = Some "actions") ?(check_suite_id = Some 1) ?(id = Some 1)
    ?(name = "build") conclusion : Check.t =
  {
    Check.name;
    conclusion;
    details_url = None;
    description = None;
    started_at = None;
    app_id;
    check_suite_id;
    id;
  }

let gen_check =
  let open QCheck2.Gen in
  map5
    (fun app_id check_suite_id id name conclusion ->
      check ~app_id ~check_suite_id ~id ~name conclusion)
    (option (oneof_list [ "actions"; "review"; "" ]))
    (option (int_range (-1) 3))
    (option (int_range (-1) 100))
    (oneof_list [ "build"; "test"; "" ])
    (oneof_list [ "success"; "failure"; "cancelled"; "pending"; "unknown" ])

let gen_checks = QCheck2.Gen.(list_size (int_range 0 30) gen_check)
let same = List.equal Check.equal

let unordered_same a b =
  same (List.sort a ~compare:Check.compare) (List.sort b ~compare:Check.compare)

let tests =
  let open QCheck2 in
  [
    Test.make ~name:"current runs are total and idempotent" ~count:500
      gen_checks (fun checks ->
        let current = Check.current_runs checks in
        same current (Check.current_runs current));
    Test.make ~name:"current runs are independent of API order" ~count:500
      gen_checks (fun checks ->
        unordered_same
          (Check.current_runs checks)
          (Check.current_runs (List.rev checks)));
    Test.make ~name:"check identity survives snapshot round trips" ~count:300
      gen_check (fun check ->
        try Check.equal check (Check.t_of_yojson (Check.yojson_of_t check))
        with _ -> false);
    Test.make ~name:"older snapshots without an App identity remain readable"
      ~count:300 gen_check (fun check ->
        try
          let json = Check.yojson_of_t check in
          let json =
            match json with
            | `Assoc fields ->
                `Assoc
                  (List.filter fields ~f:(fun (key, _) ->
                       not (String.equal key "app_id")))
            | other -> other
          in
          Check.equal
            { check with Check.app_id = None }
            (Check.t_of_yojson json)
        with _ -> false);
    Test.make ~name:"older snapshots without a suite identity remain readable"
      ~count:300 gen_check (fun check ->
        try
          let json =
            match Check.yojson_of_t check with
            | `Assoc fields ->
                `Assoc
                  (List.filter fields ~f:(fun (key, _) ->
                       not (String.equal key "check_suite_id")))
            | other -> other
          in
          Check.equal
            { check with Check.check_suite_id = None }
            (Check.t_of_yojson json)
        with _ -> false);
    Test.make ~name:"resolving pages separately then together is associative"
      ~count:500
      Gen.(pair gen_checks gen_checks)
      (fun (first, second) ->
        same
          (Check.current_runs (first @ second))
          (Check.current_runs
             (Check.current_runs first @ Check.current_runs second)));
    Test.make ~name:"the newer run governs CI regardless of the old conclusion"
      ~count:300
      Gen.(
        pair (int_range 1 1000)
          (oneof_list [ "failure"; "cancelled"; "pending"; "success" ]))
      (fun (id, old_conclusion) ->
        let old = check ~id:(Some id) old_conclusion in
        let newer = check ~id:(Some (id + 1)) "success" in
        let st = Check.current_runs [ newer; old ] in
        same st [ newer ]
        && Pr_state.equal_check_status
             (Pr_state.derive_check_status st)
             Pr_state.Passing);
    Test.make ~name:"a newer failure or pending run blocks an older pass"
      ~count:300
      Gen.(pair (int_range 1 1000) bool)
      (fun (id, failing) ->
        let conclusion, expected =
          if failing then ("failure", Pr_state.Failing)
          else ("pending", Pr_state.Pending)
        in
        let old = check ~id:(Some id) "success" in
        let newer = check ~id:(Some (id + 1)) conclusion in
        let current = Check.current_runs [ old; newer ] in
        same current [ newer ]
        && Pr_state.equal_check_status
             (Pr_state.derive_check_status current)
             expected);
    Test.make ~name:"suite replacements work independently of App visibility"
      ~count:300
      Gen.(
        triple (int_range 1 1000)
          (option (oneof_list [ "actions"; "review"; "" ]))
          (option (oneof_list [ "actions"; "review"; "" ])))
      (fun (id, old_app, new_app) ->
        let failed = check ~app_id:old_app ~id:(Some id) "failure" in
        let passed = check ~app_id:new_app ~id:(Some (id + 1)) "success" in
        let current = Check.current_runs [ failed; passed ] in
        same current [ passed ]
        && Pr_state.equal_check_status
             (Pr_state.derive_check_status current)
             Pr_state.Passing);
    Test.make
      ~name:"same-named checks in different suites remain independently live"
      ~count:300
      Gen.(triple (int_range 1 1000) (int_range 1 1000) bool)
      (fun (id, suite, failing) ->
        let conclusion, expected =
          if failing then ("failure", Pr_state.Failing)
          else ("cancelled", Pr_state.Pending)
        in
        let old = check ~check_suite_id:(Some suite) ~id:(Some id) conclusion in
        let newer =
          check ~check_suite_id:(Some (suite + 1)) ~id:(Some (id + 1)) "success"
        in
        let current = Check.current_runs [ old; newer ] in
        same current [ old; newer ]
        && Pr_state.equal_check_status
             (Pr_state.derive_check_status current)
             expected);
    Test.make ~name:"missing or invalid suite identities never discard failures"
      ~count:300
      Gen.(pair (oneof_list [ None; Some 0; Some (-1) ]) (int_range 1 1000))
      (fun (check_suite_id, id) ->
        let old = check ~check_suite_id ~id:(Some id) "failure" in
        let newer = check ~check_suite_id ~id:(Some (id + 1)) "success" in
        let current = Check.current_runs [ old; newer ] in
        same current [ old; newer ]
        && Pr_state.equal_check_status
             (Pr_state.derive_check_status current)
             Pr_state.Failing);
    Test.make ~name:"unidentified and equal-ID checks remain conservative"
      ~count:300
      Gen.(oneof_list [ None; Some (-1); Some 0; Some 1 ])
      (fun id ->
        let cancelled = check ~id "cancelled" in
        let passing = check ~id "success" in
        let current = Check.current_runs [ cancelled; passing ] in
        same current [ cancelled; passing ]
        && Pr_state.equal_check_status
             (Pr_state.derive_check_status current)
             Pr_state.Pending);
    Test.make
      ~name:"a cancellation without a matching replacement stays pending"
      ~count:300
      Gen.(int_range 1 1000)
      (fun id ->
        let cancelled = check ~id:(Some id) "cancelled" in
        let passing = check ~name:"test" ~id:(Some (id + 1)) "success" in
        let current = Check.current_runs [ cancelled; passing ] in
        same current [ cancelled; passing ]
        && Pr_state.equal_check_status
             (Pr_state.derive_check_status current)
             Pr_state.Pending);
  ]

let () = List.iter tests ~f:(fun test -> QCheck2.Test.check_exn test)

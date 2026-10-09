(* @archlint.module test
   @archlint.domain forge-observation *)

open Onton_core
module F = Forge_observation
module Gen = QCheck2.Gen

let base : Pr_state.t =
  {
    status = Pr_state.Open;
    is_draft = false;
    merge_state = Pr_state.Unknown;
    merge_ready = false;
    merge_ready_divergence = None;
    review_decision = None;
    check_status = Pr_state.Pending;
    ci_checks = [];
    ci_checks_truncated = false;
    comments = [];
    unresolved_comment_count = 0;
    findings = [];
    pr_number = Some (Types.Pr_number.of_int 7);
    node_id = None;
    merge_queue_required = false;
    merge_queue_entry = None;
    native_stack = false;
    head_branch = None;
    base_oid = Some (String.make 40 'b');
    head_oid = Some (String.make 40 'a');
    merge_commit_sha = None;
    base_branch = Some (Types.Branch.of_string "main");
    is_fork = false;
  }

let request = F.{ id = "request"; pr_number = Types.Pr_number.of_int 7 }

let observe request state =
  F.{ request; outcome = Poll_outcome.Ok_pr_state state }

let accepted response =
  match F.validate ~expected:request response with
  | Ok _ -> true
  | Error _ -> false

let () =
  QCheck_base_runner.run_tests_main
    [
      QCheck2.Test.make
        ~name:
          "forge observations: arbitrary identity and revision data is total"
        ~count:500
        Gen.(triple string int (option string))
        (fun (id, pr_number, oid) ->
          try
            let expected =
              F.{ id; pr_number = Types.Pr_number.of_int pr_number }
            in
            ignore
              (F.validate ~expected
                 (observe expected { base with head_oid = oid; base_oid = oid }));
            true
          with _ -> false);
      QCheck2.Test.make
        ~name:"forge observations: matching response validation is idempotent"
        ~count:200 Gen.bool (fun conflicting ->
          let state =
            {
              base with
              merge_state =
                (if conflicting then Pr_state.Conflicting else Mergeable);
            }
          in
          let response = observe request state in
          F.validate ~expected:request response = Ok response.outcome
          && F.validate ~expected:request response
             = F.validate ~expected:request response);
      QCheck2.Test.make
        ~name:"forge observations: request replacement isolates delayed results"
        ~count:300
        Gen.(list_size (int_range 1 50) bool)
        (fun deliveries ->
          let rec run latest index = function
            | [] -> true
            | replace :: rest ->
                let next =
                  if replace then
                    F.{ request with id = "request-" ^ string_of_int index }
                  else latest
                in
                let old = observe latest base in
                let result = F.validate ~expected:next old in
                (if replace then result = Error "stale_forge_request"
                 else result = Ok old.outcome)
                && run next (index + 1) rest
          in
          run request 1 deliveries);
      QCheck2.Test.make
        ~name:
          "forge observations: response PR identity cannot substitute for \
           request identity" ~count:200 Gen.bool (fun missing ->
          not
            (accepted
               (observe request
                  {
                    base with
                    pr_number =
                      (if missing then None else Some (Types.Pr_number.of_int 8));
                  })));
      QCheck2.Test.make
        ~name:"forge observations: missing open revision context waits"
        ~count:200
        Gen.(int_range 0 4)
        (fun missing ->
          let state =
            match missing with
            | 0 -> { base with head_oid = None }
            | 1 -> { base with base_oid = None }
            | 2 -> { base with base_branch = None }
            | 3 -> { base with head_oid = Some "bad-sha" }
            | _ -> { base with base_oid = Some "" }
          in
          not (accepted (observe request state)));
      QCheck2.Test.make
        ~name:"forge observations: identified terminal PRs outlive deleted refs"
        ~count:200 Gen.bool (fun merged ->
          accepted
            (observe request
               {
                 base with
                 status = (if merged then Pr_state.Merged else Closed);
                 head_oid = None;
                 base_oid = None;
                 base_branch = None;
               }));
      QCheck2.Test.make
        ~name:"forge observations: transport failure remains infrastructure"
        ~count:200 Gen.string (fun msg ->
          let response =
            F.{ request; outcome = Poll_outcome.Transport_failed { msg } }
          in
          F.validate ~expected:request response = Ok response.outcome);
      QCheck2.Test.make
        ~name:
          "forge observations: missing request ID cannot authorize any outcome"
        ~count:200 Gen.bool (fun terminal ->
          let expected = F.{ request with id = "" } in
          let state =
            { base with status = (if terminal then Pr_state.Merged else Open) }
          in
          F.validate ~expected (observe expected state)
          = Error "invalid_forge_request_identity");
    ]

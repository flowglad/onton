(* @archlint.module test
   @archlint.domain patch-agent *)
open Base
open Onton_core
open Types

let agent id =
  Patch_agent.create ~branch:(Branch.of_string "branch") (Patch_id.of_string id)

let tests =
  [
    QCheck2.Test.make
      ~name:
        "branch dispatch preserves backend completion across busy lifecycles"
      ~count:200
      QCheck2.Gen.(pair bool (list bool))
      (fun (implemented, activity) ->
        let initial = agent "patch" in
        let initial =
          if implemented then
            Patch_agent.start initial ~base_branch:(Branch.of_string "main")
          else initial
        in
        let completion =
          Session_result.
            {
              session_uuid = "completed-session";
              delivery_mode = Start;
              kind = None;
              message_id = None;
              result = Session_ok;
              head = None;
              guidance = [];
              turn_accepted = true;
            }
        in
        let initial =
          Patch_agent.record_session_completion initial completion
        in
        let initial, _ =
          Patch_agent.reconcile_branch initial
            Branch_reconcile.(
              Request
                { base = "main"; policy = Rewrite; purpose = Reconcile_base })
        in
        let final =
          List.fold activity ~init:initial ~f:(fun a running ->
              if running then Patch_agent.begin_branch_reconciliation a
              else Patch_agent.complete a)
          |> Patch_agent.complete
        in
        (not final.Patch_agent.busy)
        && Bool.equal initial.Patch_agent.has_session
             final.Patch_agent.has_session
        && Branch_reconcile.equal initial.Patch_agent.branch_reconcile
             final.Patch_agent.branch_reconcile
        && Option.equal Session_result.equal_completion
             final.Patch_agent.session_completion (Some completion));
    QCheck2.Test.make
      ~name:"WONTDO normalizes arbitrary reasons and is idempotent" ~count:500
      QCheck2.Gen.string (fun reason ->
        let a = agent "patch" in
        let refused = Patch_agent.set_wontdo a reason in
        let trimmed = String.strip reason in
        let expected = if String.is_empty trimmed then None else Some trimmed in
        Option.equal String.equal refused.Patch_agent.wontdo_reason expected
        && Bool.equal
             (Patch_agent.needs_intervention refused)
             (Option.is_some expected)
        && Patch_agent.equal refused (Patch_agent.set_wontdo refused reason));
    QCheck2.Test.make
      ~name:"WONTDO pause survives automatic work until explicit reset"
      ~count:500
      QCheck2.Gen.(list (int_range 0 6))
      (fun operations ->
        let _, _, valid =
          List.fold operations
            ~init:(agent "patch", false, true)
            ~f:(fun (a, paused, valid) op ->
              let a, paused =
                match op with
                | 0 -> (Patch_agent.set_wontdo a "Needs prerequisite", true)
                | 1 -> (Patch_agent.reset_intervention_state a, false)
                | 2 -> (Patch_agent.enqueue a Operation_kind.Human, paused)
                | 3 ->
                    (Patch_agent.add_human_message a "Queued guidance", paused)
                | 4 -> (Patch_agent.clear_session_fallback a, paused)
                | 5 -> (Patch_agent.set_ci_checks a [], paused)
                | _ ->
                    let active =
                      Patch_agent.start a ~base_branch:(Branch.of_string "main")
                    in
                    (Patch_agent.complete active, paused)
              in
              ( a,
                paused,
                valid
                && Bool.equal
                     (Option.is_some a.Patch_agent.wontdo_reason)
                     paused
                && ((not paused)
                   || Patch_agent.needs_intervention a
                      && Patch_decision.equal_disposition
                           (Patch_decision.disposition a)
                           Patch_decision.Blocked) ))
        in
        valid);
    QCheck2.Test.make ~name:"blank WONTDO does not clear an existing refusal"
      ~count:50
      QCheck2.Gen.(oneof_list [ ""; " "; "\n\t " ])
      (fun reason ->
        let a = Patch_agent.set_wontdo (agent "patch") "Prerequisite missing" in
        Patch_agent.equal a (Patch_agent.set_wontdo a reason));
    QCheck2.Test.make
      ~name:"publication settles only its captured refresh across interleavings"
      ~count:500
      QCheck2.Gen.(list (int_range 0 3))
      (fun operations ->
        let initial = agent "root" in
        let _, _, _, _, _, valid =
          List.fold operations ~init:(initial, initial, 0, 0, false, true)
            ~f:(fun (a, publication, version, captured, pending, valid) op ->
              let a, publication, version, captured, pending =
                match op with
                | 0 ->
                    ( Patch_agent.request_pr_body_refresh a,
                      publication,
                      version + 1,
                      captured,
                      true )
                | 1 -> (a, a, version, version, pending)
                | 2 ->
                    ( Patch_agent.acknowledge_pr_body_refresh a ~publication,
                      publication,
                      version,
                      captured,
                      pending && not (Int.equal version captured) )
                | _ ->
                    ( Patch_agent.set_pr_body_delivered a true,
                      publication,
                      version,
                      captured,
                      pending )
              in
              ( a,
                publication,
                version,
                captured,
                pending,
                valid
                && Bool.equal a.Patch_agent.pr_body_refresh.Patch_agent.pending
                     pending
                && Int.equal a.Patch_agent.pr_body_refresh.Patch_agent.version
                     version ))
        in
        valid);
    QCheck2.Test.make ~name:"another patch cannot settle refresh" ~count:100
      QCheck2.Gen.bool (fun delivered ->
        let a = Patch_agent.request_pr_body_refresh (agent "root") in
        let publication =
          agent "child" |> Patch_agent.request_pr_body_refresh |> fun a ->
          Patch_agent.set_pr_body_delivered a delivered
        in
        Patch_agent.equal a
          (Patch_agent.acknowledge_pr_body_refresh a ~publication));
  ]

let () = QCheck_base_runner.run_tests_main tests

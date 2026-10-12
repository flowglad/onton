(* @archlint.module test
   @archlint.domain session-result *)
open Base
open Onton_core
module S = Session_result

let completion_gen =
  let open QCheck2.Gen in
  let* session_uuid = string in
  let* head = option string in
  let* detail = option string in
  let* is_fresh = bool in
  let* delivery_mode = oneof_list [ S.Start; Respond ] in
  let* kind = option (oneof_list [ Types.Operation_kind.Human; Ci ]) in
  let* message_id =
    map (Option.map ~f:Types.Message_id.of_string) (option string)
  in
  let+ result =
    oneof_list
      [
        S.Session_ok;
        Session_process_error { is_fresh; detail };
        Session_no_resume;
        Session_timed_out { session_id = head; detail };
        Session_failed { is_fresh; detail };
        Session_wontdo session_uuid;
        Session_give_up;
        Session_no_commits;
        Session_context_exhausted;
      ]
  in
  S.
    {
      session_uuid;
      delivery_mode;
      kind;
      message_id;
      result;
      head;
      guidance = [];
      turn_accepted = true;
    }

let () =
  QCheck_base_runner.run_tests_main
    [
      QCheck2.Test.make ~count:200
        ~name:
          "verified legacy Start resumes PR creation without synthesizing a \
           backend completion"
        QCheck2.Gen.(pair completion_gen (triple bool bool bool))
        (fun (saved, (confirmed, guidance, has_completion)) ->
          try
            let module B = Branch_reconcile in
            let source =
              match B.Commit.make (String.make 40 'a') with
              | Some source -> source
              | None -> assert false
            in
            let reply state result =
              match B.pending state with
              | None -> assert false
              | Some command ->
                  fst
                    (B.step state
                       (B.Result { token = command.token; at = 1.; result }))
            in
            let owner = B.import_legacy_publication B.empty ~base:"main" in
            let owner =
              reply owner
                B.(
                  Observed
                    {
                      source;
                      head = source;
                      target = source;
                      remote = Some source;
                      boundary = Plain;
                      topology = Equal;
                      clean = true;
                      sequencer = None;
                      conflicts = 0;
                      target_included = true;
                      base_contains_source = true;
                      completed_integration = false;
                      destination = Remote_id.of_destination "origin";
                    })
            in
            let owner = reply owner B.Pinned in
            let owner =
              if confirmed then
                reply owner B.(Remote { sha = Some source; topology = Equal })
                |> Onton_core_test_support.Publication_fixture.verify_scope
                     ~step:(fun state event -> fst (B.step state event))
                     ~state:Fn.id
              else owner
            in
            let owner =
              match B.decode (B.yojson_of_t owner) with
              | Ok owner -> owner
              | Error e -> failwith e
            in
            let guidance = if guidance then [ "new work" ] else [] in
            let completion = if has_completion then Some saved else None in
            Bool.equal
              (S.resume_verified_legacy_start ~delivery_mode:Start ~guidance
                 ~publication:owner ~completion)
              (confirmed && List.is_empty guidance && not has_completion)
            && not
                 (S.resume_verified_legacy_start ~delivery_mode:Respond
                    ~guidance ~publication:owner ~completion)
          with _ -> false);
      QCheck2.Test.make
        ~name:
          "local-work success distinguishes Start, Respond and empty branches"
        ~count:100
        QCheck2.Gen.(triple (oneof_list [ S.Start; S.Respond ]) bool bool)
        (fun (delivery_mode, branch_changed, no_work) ->
          let expected =
            if
              no_work
              || S.equal_delivery_mode delivery_mode S.Respond
                 && not branch_changed
            then S.Session_no_commits
            else S.Session_ok
          in
          S.equal expected
            (S.after_local_work ~delivery_mode ~branch_changed ~no_work
               S.Session_ok));
      QCheck2.Test.make
        ~name:"local-work classification preserves failed backend outcomes"
        ~count:300 completion_gen (fun completion ->
          let classified =
            S.after_local_work ~delivery_mode:completion.delivery_mode
              ~branch_changed:false ~no_work:true completion.result
          in
          S.equal classified
            (if S.equal completion.result Session_ok then Session_no_commits
             else completion.result));
      QCheck2.Test.make
        ~name:
          "successful Start resumes publication only for unchanged accepted \
           guidance"
        ~count:300
        QCheck2.Gen.(pair string bool)
        (fun (message, accepted) ->
          let completion =
            S.
              {
                session_uuid = "session";
                delivery_mode = Start;
                kind = Some Types.Operation_kind.Human;
                message_id = None;
                result = Session_ok;
                head = None;
                guidance = [ message ];
                turn_accepted = accepted;
              }
          in
          let publication = Branch_reconcile.empty in
          Bool.equal
            (S.resume_start ~delivery_mode:Start ~guidance:[ message ]
               ~publication completion)
            accepted
          && (not
                (S.resume_start ~delivery_mode:Start
                   ~guidance:[ message; "new guidance" ]
                   ~publication completion))
          && (not
                (S.resume_start ~delivery_mode:Respond ~guidance:[ message ]
                   ~publication completion))
          && Branch_reconcile.equal_purpose
               (S.publication_intent completion ~base:"main" ~policy:Rewrite)
                 .purpose (Publish_session "session"));
      QCheck2.Test.make
        ~name:"backend completion preserves outcome and unknown revision"
        ~count:500 completion_gen (fun completion ->
          try
            match S.decode_completion (S.yojson_of_completion completion) with
            | Ok restored -> S.equal_completion restored completion
            | Error _ -> false
          with _ -> false);
      QCheck2.Test.make ~name:"malformed completion decoding is total"
        ~count:500 QCheck2.Gen.string (fun value ->
          match S.decode_completion (`String value) with
          | Error _ -> true
          | Ok _ -> false);
    ]

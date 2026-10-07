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
        Session_worktree_missing;
        Session_push_failed None;
        Session_push_failed (Some (Hook_failure session_uuid));
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

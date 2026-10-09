(* @archlint.module test
   @archlint.domain forge-observation *)

open Base
open Onton_core
module F = Forge_observation
module X = Onton_core_test_support.Forge_fixture
module G = QCheck2.Gen

let tests =
  [
    QCheck2.Test.make
      ~name:
        "old forge checkpoints retain conflicts without manufacturing base \
         confirmation" ~count:100 G.bool (fun conflicting ->
        try
          let request = X.request "legacy-proof" in
          let history, ticket =
            F.begin_request F.empty_history ~request ~scope:X.scope
          in
          match ticket with
          | None -> false
          | Some ticket -> (
              let observation =
                X.observe request
                  (if conflicting then Pr_state.Conflicting
                   else Pr_state.Mergeable)
              in
              let history, _ =
                F.accept ~confirmed_base:(String.make 40 'b') history ~ticket
                  ~current:X.scope ~confirmed_head:X.base.Pr_state.head_oid
                  observation
              in
              let rec remove_base_proof = function
                | `Assoc fields ->
                    `Assoc
                      (List.filter_map fields ~f:(fun (key, value) ->
                           if String.equal key "confirmed_base" then None
                           else Some (key, remove_base_proof value)))
                | `List values -> `List (List.map values ~f:remove_base_proof)
                | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _)
                  as value ->
                    value
              in
              match
                F.decode_history
                  (remove_base_proof (F.yojson_of_history history))
              with
              | Error _ -> false
              | Ok restored ->
                  (not (F.can_apply restored observation.F.outcome))
                  && Bool.equal
                       (Option.is_some (F.conflict_fact restored))
                       conflicting
                  && Option.exists (F.latest_fact restored) ~f:(fun fact ->
                      Option.is_none fact.F.confirmed_base
                      && not (F.revisions_confirmed fact)))
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "forge revision proof requires both exact direct observations and \
         survives restart"
      ~count:300
      G.(pair (int_range 0 4) bool)
      (fun (variant, restart) ->
        try
          let request = X.request "direct-pair" in
          let history, ticket =
            F.begin_request F.empty_history ~request ~scope:X.scope
          in
          match ticket with
          | None -> false
          | Some ticket ->
              let head =
                if variant = 1 then None
                else if variant = 2 then Some (String.make 40 'e')
                else X.base.Pr_state.head_oid
              in
              let base =
                if variant = 3 then None
                else if variant = 4 then Some (String.make 40 'e')
                else X.base.Pr_state.base_oid
              in
              let history, accepted =
                F.accept ?confirmed_base:base history ~ticket ~current:X.scope
                  ~confirmed_head:head
                  (X.observe request Pr_state.Conflicting)
              in
              let history =
                if restart then
                  match F.decode_history (F.yojson_of_history history) with
                  | Ok history -> history
                  | Error reason -> failwith reason
                else history
              in
              Result.is_ok accepted && F.history_valid history
              && (match accepted with
                | Ok outcome ->
                    Bool.equal (F.can_apply history outcome) (variant = 0)
                | Error _ -> false)
              && Option.exists (F.latest_fact history) ~f:(fun fact ->
                  Bool.equal (F.revisions_confirmed fact) (variant = 0))
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "conflict projection retains contradictory reports only for the same \
         revision pair"
      ~count:300
      G.(pair (int_range 0 4) bool)
      (fun (change, restart) ->
        try
          let accept history id state =
            let request = X.request id in
            let history, ticket =
              F.begin_request history ~request ~scope:X.scope
            in
            match ticket with
            | None -> failwith "valid request rejected"
            | Some ticket ->
                let history, result =
                  F.accept history ~ticket ~current:X.scope ~confirmed_head:None
                    F.{ request; outcome = Poll_outcome.Ok_pr_state state }
                in
                assert (Result.is_ok result);
                history
          in
          let history =
            accept F.empty_history "conflict"
              { X.base with Pr_state.merge_state = Conflicting }
          in
          let state =
            match change with
            | 0 -> { X.base with Pr_state.merge_state = Mergeable }
            | 1 -> X.base
            | 2 -> { X.base with Pr_state.base_oid = Some (String.make 40 'c') }
            | 3 -> { X.base with Pr_state.head_oid = Some (String.make 40 'd') }
            | _ ->
                {
                  X.base with
                  Pr_state.base_branch = Some (Types.Branch.of_string "other");
                }
          in
          let history = accept history "next" state in
          let history =
            if restart then
              match F.decode_history (F.yojson_of_history history) with
              | Ok history -> history
              | Error error -> failwith error
            else history
          in
          Option.is_some (F.conflict_fact history)
          && Bool.equal
               (Option.is_some (F.conflict_for_scope history X.scope))
               (change < 2)
          && Option.is_none
               (F.conflict_for_scope history
                  { X.scope with F.pr_number = Some (Types.Pr_number.of_int 8) })
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "forge terminal responses retain ticket authority through delayed \
         rediscovery" ~count:200 G.bool (fun changed ->
        try
          let request = X.request "closed" in
          let history, ticket =
            F.begin_request F.empty_history ~request ~scope:X.scope
          in
          match ticket with
          | None -> false
          | Some ticket ->
              let current =
                if changed then { X.scope with F.generation = 1 } else X.scope
              in
              let response =
                F.
                  {
                    request;
                    outcome =
                      Poll_outcome.Ok_pr_state
                        {
                          X.base with
                          Pr_state.status = Closed;
                          head_oid = None;
                          base_oid = None;
                          base_branch = None;
                        };
                  }
              in
              let history, result =
                F.accept history ~ticket ~current ~confirmed_head:None response
              in
              Bool.equal (Result.is_ok result) (not changed)
              && Option.is_none (F.latest_fact history)
              && F.history_valid history
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "forge request and response processing is total over arbitrary scope \
         data"
      ~count:500
      G.(
        pair (triple string int int)
          (triple string (option string) (option string)))
      (fun ((id, pr_number, generation), (branch, head, base_branch)) ->
        try
          let request =
            F.{ id; pr_number = Types.Pr_number.of_int pr_number }
          in
          let scope =
            F.
              {
                pr_number = Some request.F.pr_number;
                generation;
                branch;
                head;
                base_branch;
              }
          in
          let history, ticket =
            F.begin_request F.empty_history ~request ~scope
          in
          F.history_valid history
          &&
          match ticket with
          | None -> F.equal_history history F.empty_history
          | Some ticket ->
              let history, _ =
                F.accept history ~ticket ~current:scope ~confirmed_head:head
                  (X.observe request Pr_state.Unknown)
              in
              F.history_valid history
              && Result.is_ok (F.decode_history (F.yojson_of_history history))
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "forge tickets isolate delayed, duplicate and replaced-context results \
         across restart"
      ~count:400
      G.(list_size (int_range 0 70) (pair (int_range 0 4) bool))
      (fun actions ->
        try
          let rec run history issued pending scope next_sequence expected_latest
              expected_conflict = function
            | [] -> true
            | (action, choice) :: rest ->
                let ( history,
                      issued,
                      pending,
                      scope,
                      next_sequence,
                      expected_latest,
                      expected_conflict,
                      valid ) =
                  match action with
                  | 0 -> (
                      let request =
                        X.request ("request-" ^ Int.to_string next_sequence)
                      in
                      let next, ticket =
                        F.begin_request history ~request ~scope
                      in
                      match ticket with
                      | None ->
                          ( history,
                            issued,
                            pending,
                            scope,
                            next_sequence,
                            expected_latest,
                            expected_conflict,
                            false )
                      | Some ticket ->
                          ( next,
                            (ticket, scope) :: issued,
                            Some (next_sequence, scope),
                            scope,
                            next_sequence + 1,
                            expected_latest,
                            expected_conflict,
                            ticket.F.sequence = next_sequence ))
                  | 1 | 2 -> (
                      match
                        if choice then List.last issued else List.hd issued
                      with
                      | None ->
                          ( history,
                            issued,
                            pending,
                            scope,
                            next_sequence,
                            expected_latest,
                            expected_conflict,
                            true )
                      | Some (ticket, captured) ->
                          let matches =
                            Option.exists pending
                              ~f:(fun (seq, expected_scope) ->
                                Int.equal seq ticket.F.sequence
                                && F.equal_scope expected_scope scope)
                          in
                          let merge_state =
                            if action = 1 then Pr_state.Conflicting
                            else Pr_state.Mergeable
                          in
                          let history, result =
                            F.accept history ~ticket ~current:scope
                              ~confirmed_head:None
                              (X.observe ticket.F.request merge_state)
                          in
                          let valid =
                            Bool.equal (Result.is_ok result) matches
                          in
                          let fact =
                            if matches then Some ticket.sequence
                            else expected_latest
                          in
                          let conflict =
                            if matches && action = 1 then Some ticket.sequence
                            else expected_conflict
                          in
                          ignore captured;
                          ( history,
                            issued,
                            (if matches then None else pending),
                            scope,
                            next_sequence,
                            fact,
                            conflict,
                            valid ))
                  | 3 -> (
                      let restored =
                        F.decode_history (F.yojson_of_history history)
                      in
                      match restored with
                      | Ok restored ->
                          ( restored,
                            issued,
                            pending,
                            scope,
                            next_sequence,
                            expected_latest,
                            expected_conflict,
                            F.equal_history history restored )
                      | Error _ ->
                          ( history,
                            issued,
                            pending,
                            scope,
                            next_sequence,
                            expected_latest,
                            expected_conflict,
                            false ))
                  | _ ->
                      let scope =
                        { scope with F.generation = scope.F.generation + 1 }
                      in
                      ( history,
                        issued,
                        pending,
                        scope,
                        next_sequence,
                        expected_latest,
                        expected_conflict,
                        true )
                in
                let observed_sequence fact =
                  Option.map fact ~f:(fun fact -> fact.F.ticket.F.sequence)
                in
                valid && F.history_valid history
                && Option.equal Int.equal
                     (observed_sequence (F.latest_fact history))
                     expected_latest
                && Option.equal Int.equal
                     (observed_sequence (F.conflict_fact history))
                     expected_conflict
                && Option.equal Int.equal
                     (Option.map (F.pending_request history) ~f:(fun ticket ->
                          ticket.F.sequence))
                     (Option.map pending ~f:fst)
                && run history issued pending scope next_sequence
                     expected_latest expected_conflict rest
          in
          run F.empty_history [] None X.scope 1 None None actions
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "forge request scope and response identities cannot substitute for one \
         another"
      ~count:300
      G.(int_range 0 7)
      (fun change ->
        try
          let request = X.request "request" in
          let history, ticket =
            F.begin_request F.empty_history ~request ~scope:X.scope
          in
          match ticket with
          | None -> false
          | Some ticket ->
              let current, observation =
                match change with
                | 0 ->
                    ( {
                        X.scope with
                        F.pr_number = Some (Types.Pr_number.of_int 8);
                      },
                      X.observe request Mergeable )
                | 1 ->
                    ( { X.scope with F.branch = "different" },
                      X.observe request Mergeable )
                | 2 ->
                    ( { X.scope with F.head = Some (String.make 40 'c') },
                      X.observe request Mergeable )
                | 3 ->
                    ( { X.scope with F.base_branch = Some "different" },
                      X.observe request Mergeable )
                | 4 -> (X.scope, X.observe (X.request "wrong") Mergeable)
                | 5 ->
                    ( X.scope,
                      F.
                        {
                          request;
                          outcome =
                            Poll_outcome.Ok_pr_state
                              {
                                X.base with
                                Pr_state.head_branch =
                                  Some (Types.Branch.of_string "wrong");
                              };
                        } )
                | 6 ->
                    ( X.scope,
                      F.
                        {
                          request;
                          outcome =
                            Poll_outcome.Ok_pr_state
                              { X.base with Pr_state.head_oid = None };
                        } )
                | _ ->
                    ( X.scope,
                      F.
                        {
                          request;
                          outcome =
                            Poll_outcome.Ok_pr_state
                              { X.base with Pr_state.is_fork = true };
                        } )
              in
              let history, result =
                F.accept history ~ticket ~current ~confirmed_head:None
                  observation
              in
              Result.is_error result
              && Option.is_none (F.latest_fact history)
              && Option.is_none (F.conflict_fact history)
              && F.history_valid history
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "forge infrastructure and recomputation cannot erase unresolved \
         conflict evidence"
      ~count:200
      G.(int_range 0 3)
      (fun outcome ->
        try
          let request = X.request "conflict" in
          let history, ticket =
            F.begin_request F.empty_history ~request ~scope:X.scope
          in
          match ticket with
          | None -> false
          | Some ticket -> (
              let history, _ =
                F.accept history ~ticket ~current:X.scope
                  ~confirmed_head:X.scope.F.head
                  (X.observe request Conflicting)
              in
              let expected = F.conflict_fact history in
              let request = X.request "later" in
              let history, ticket =
                F.begin_request history ~request ~scope:X.scope
              in
              match ticket with
              | None -> false
              | Some ticket ->
                  let observation =
                    match outcome with
                    | 0 -> X.observe request Unknown
                    | 1 -> X.observe request Mergeable
                    | 2 ->
                        F.
                          {
                            request;
                            outcome = Poll_outcome.Timed_out { seconds = 30. };
                          }
                    | _ ->
                        F.
                          {
                            request;
                            outcome =
                              Poll_outcome.Transport_failed { msg = "offline" };
                          }
                  in
                  let history, _ =
                    F.accept history ~ticket ~current:X.scope
                      ~confirmed_head:None observation
                  in
                  Option.equal F.equal_fact expected (F.conflict_fact history)
                  && Option.exists (F.conflict_fact history) ~f:(fun fact ->
                      F.fact_matches_scope fact X.scope)
                  && F.history_valid history)
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "forge malformed checkpoints and sequence overflow never grant a ticket"
      ~count:200
      G.(pair int string)
      (fun (number, value) ->
        try
          let json =
            `Assoc
              [ ("next_sequence", `Int number); ("pending", `String value) ]
          in
          ignore (F.decode_history json);
          let raw = F.yojson_of_history F.empty_history in
          let overflow =
            match raw with
            | `Assoc fields ->
                `Assoc
                  (("next_sequence", `Int Int.max_value)
                  :: List.Assoc.remove fields ~equal:String.equal
                       "next_sequence")
            | _ -> assert false
          in
          match F.decode_history overflow with
          | Error _ -> false
          | Ok history ->
              let next, ticket =
                F.begin_request history ~request:(X.request "overflow")
                  ~scope:X.scope
              in
              F.equal_history history next && Option.is_none ticket
        with _ -> false);
  ]

let () = QCheck_base_runner.run_tests_main tests

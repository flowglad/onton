(* @archlint.module test
   @archlint.domain coordinator-state *)
open Onton_core

[@@@warning "-4-40-42"]

module J = Coordinator_journal

let test name gen f =
  QCheck2.Test.make ~count:400 ~name gen (fun x -> try f x with _ -> false)

let () =
  QCheck_base_runner.run_tests_main
    [
      test "journal decoder is total" QCheck2.Gen.string (fun bytes ->
          ignore (J.decode bytes);
          true);
      test
        "journal history survives restart, never resets attempts or settled \
         evidence"
        QCheck2.Gen.(list (pair bool bool))
        (fun actions ->
          let rec loop journal (previous : J.entry option) = function
            | [] ->
                J.valid journal && Result.is_ok (J.decode (J.encode journal))
            | (attempt, applied) :: rest ->
                let next =
                  if attempt then
                    J.begin_attempt journal ~id:"effect" ~scope:"immutable"
                  else
                    J.settle journal ~id:"effect"
                      ~result:(if applied then Some "receipt" else None)
                in
                let next = match next with Ok j -> j | Error _ -> journal in
                let current = J.find next "effect" in
                let monotone =
                  match (previous, current) with
                  | Some p, Some c -> (
                      c.attempts >= p.attempts && c.attempts <= 3
                      &&
                      match p.phase with
                      | Applied _ | Attention -> p.phase = c.phase
                      | Pending -> true)
                  | Some _, None -> false
                  | _ -> true
                in
                monotone && loop next current rest
          in
          loop J.empty None actions);
      test
        "changed IDs cannot change scope; three unresolved attempts reach \
         attention"
        QCheck2.Gen.string (fun changed ->
          let step j =
            match J.begin_attempt j ~id:"x" ~scope:"fixed" with
            | Ok j -> (
                match J.settle j ~id:"x" ~result:None with
                | Ok j -> j
                | Error _ -> j)
            | Error _ -> j
          in
          let j = step (step (step J.empty)) in
          (match J.find j "x" with
            | Some e -> e.attempts = 3 && e.phase = J.Attention
            | None -> false)
          && Result.is_error (J.begin_attempt j ~id:"x" ~scope:changed));
      QCheck2.Test.make ~count:400 ~name:"journal generated public API sequence"
        QCheck2.Gen.(list_size (int_range 0 20) bool)
        (fun actions ->
          try
            let journal =
              List.fold_left
                (fun journal applied ->
                  match
                    J.begin_attempt journal ~id:"generated" ~scope:"fixed"
                  with
                  | Error _ -> journal
                  | Ok pending -> (
                      match
                        J.settle pending ~id:"generated"
                          ~result:(if applied then Some "receipt" else None)
                      with
                      | Ok settled -> settled
                      | Error _ -> pending))
                J.empty actions
            in
            let entries = J.entries journal in
            let found = J.find journal "generated" in
            match J.decode (J.encode journal) with
            | Error _ -> false
            | Ok restored ->
                J.valid journal && J.valid restored
                && List.length entries = List.length (J.entries restored)
                && Option.is_some found = not (List.is_empty actions)
          with _ -> false);
    ]

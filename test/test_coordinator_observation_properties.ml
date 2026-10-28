(* @archlint.module test
   @archlint.domain coordinator-forge *)
open Onton_core

[@@@warning "-4-40-42"]

module O = Coordinator_observation

let branch = Types.Branch.of_string
let number = Types.Pr_number.of_int 1
let head = String.make 40 'a'
let base = String.make 40 'b'

let scope : O.scope =
  {
    ticket = "ticket";
    pr = number;
    head_branch = branch "feature";
    base_branch = branch "main";
    head_revision = head;
    base_revision = base;
    push_destination = "push";
    generation = 0;
  }

let pr : Pr_state.t =
  {
    pr_number = Some number;
    status = Open;
    is_draft = false;
    merge_state = Mergeable;
    merge_ready = true;
    merge_ready_divergence = None;
    review_decision = Some "APPROVED";
    check_status = Passing;
    ci_checks = [];
    ci_checks_truncated = false;
    comments = [];
    unresolved_comment_count = 0;
    findings = [];
    node_id = Some "node";
    merge_queue_required = false;
    merge_queue_entry = None;
    native_stack = false;
    head_branch = Some (branch "feature");
    head_oid = Some head;
    base_oid = Some base;
    merge_commit_sha = None;
    base_branch = Some (branch "main");
    is_fork = false;
  }

let agent () =
  Patch_agent.create_adhoc ~complexity:None
    ~patch_id:(Types.Patch_id.of_string "patch")
    ~branch:(branch "feature") ~pr_number:number ~max_ci_failures:3
  |> fun a ->
  Patch_agent.set_base_branch a (branch "main") |> fun a ->
  Patch_agent.set_head_oid a (Some head)

let evidence scope pr =
  match O.make scope pr ~diagnostic:"" with Ok e -> e | Error e -> failwith e

let test name gen f =
  QCheck2.Test.make ~count:400 ~name gen (fun x -> try f x with _ -> false)

let () =
  QCheck_base_runner.run_tests_main
    [
      test "observation decoder totality" QCheck2.Gen.string (fun bytes ->
          ignore (O.decode bytes);
          true);
      test
        "identified snapshots roundtrip; changed generation/revision cannot \
         grant proof"
        QCheck2.Gen.(pair bool bool)
        (fun (generation, revision) ->
          let e = evidence scope (Some pr) in
          let a = agent () in
          let a = if generation then Patch_agent.bump_generation a else a in
          let a =
            if revision then
              Patch_agent.set_head_oid a (Some (String.make 40 'c'))
            else a
          in
          Result.is_ok (O.decode (O.encode e))
          && Patch_agent.forge_revision_pair_confirmed (O.apply a e)
             = not (generation || revision));
      test
        "unknown/unavailable interleavings retain conflict; a confirmed new \
         head supersedes it"
        QCheck2.Gen.(list bool)
        (fun reports ->
          let conflict =
            evidence scope
              (Some { pr with merge_state = Conflicting; merge_ready = false })
          in
          let a = O.apply (agent ()) conflict in
          let final =
            List.fold_left
              (fun a unavailable ->
                let e =
                  if unavailable then
                    evidence { scope with ticket = "unavailable" } None
                  else
                    evidence
                      { scope with ticket = "unknown" }
                      (Some
                         { pr with merge_state = Unknown; merge_ready = false })
                in
                O.apply a e)
              a reports
          in
          Patch_agent.has_conflict final
          && Patch_agent.has_conflict
               (O.apply final
                  (evidence { scope with ticket = "same-pair" } (Some pr)))
          &&
          let head = String.make 40 'c' in
          let final = Patch_agent.set_head_oid final (Some head) in
          not
            (Patch_agent.has_conflict
               (O.apply final
                  (evidence
                     { scope with ticket = "new-head"; head_revision = head }
                     (Some { pr with head_oid = Some head })))));
      test
        "identified discovery adopts an absent PR but cannot replace another PR"
        QCheck2.Gen.bool (fun other_pr ->
          let a =
            if other_pr then
              Patch_agent.set_pr_number (agent ()) (Types.Pr_number.of_int 2)
            else
              Patch_agent.clear_pr (agent ()) |> fun a ->
              Patch_agent.set_base_branch a (branch "main") |> fun a ->
              Patch_agent.set_head_oid a (Some head)
          in
          let next = O.apply a (evidence scope (Some pr)) in
          if other_pr then
            Patch_agent.pr_number next = Some (Types.Pr_number.of_int 2)
            && not (Patch_agent.forge_revision_pair_confirmed next)
          else
            Patch_agent.pr_number next = Some number
            && Patch_agent.forge_revision_pair_confirmed next);
      test
        "vanished queue entries retain repair evidence until an identified \
         removal lookup"
        QCheck2.Gen.bool (fun resolved ->
          let entry : Pr_state.merge_queue_entry =
            { id = "queue"; state = Mq_queued; position = 1 }
          in
          let a = Patch_agent.set_merge_queue_entry (agent ()) (Some entry) in
          let missing = O.apply a (evidence scope (Some pr)) in
          let retry =
            match
              O.make ~observed_at:1000000.
                { scope with ticket = "retry" }
                (Some pr) ~diagnostic:""
            with
            | Ok e -> e
            | Error why -> failwith why
          in
          let waiting = O.apply missing retry in
          let final =
            if resolved then
              let e =
                match
                  O.make ~removal_checks:[] ~observed_at:1000001.
                    { scope with ticket = "removal" }
                    (Some pr) ~diagnostic:""
                with
                | Ok e -> e
                | Error why -> failwith why
              in
              O.apply waiting e
            else waiting
          in
          missing.merge_queue_ejection <> None
          && Scheduler_recovery.pending waiting.scheduler_recovery <> None
          && final.merge_queue_ejection = None = resolved);
      test "failure never invents readiness or erases owner history"
        QCheck2.Gen.string (fun diagnostic ->
          let diagnostic =
            if String.length diagnostic > 4096 then "" else diagnostic
          in
          match O.make scope None ~diagnostic with
          | Error _ -> false
          | Ok e ->
              not
                (Patch_agent.forge_revision_pair_confirmed
                   (O.apply (agent ()) e)));
    ]

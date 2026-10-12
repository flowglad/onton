(* @archlint.module exempt
   @archlint.exempt-reason test-support *)

open Base
open Onton
open Onton_core

(** Ingest an explicitly identified fixture through the same owner ticket
    protocol as the poller. Missing revision identities are deliberately not
    filled in: such fixtures must exercise rejection separately. *)
let apply ?merge_queue_ejection_confirmed ?confirmed_remote_head t pid
    (observation : Patch_controller.poll_observation) =
  let agent = Orchestrator.agent t pid in
  let previous_conflict = Patch_agent.has_conflict agent in
  let pr_number =
    match Patch_agent.pr_number agent with
    | Some number -> number
    | None -> failwith "identified forge fixture requires a PR"
  in
  let request : Forge_observation.request =
    { Forge_observation.id = "fixture-poll"; Forge_observation.pr_number }
    [@warning "-42"]
  in
  let t, ticket = Orchestrator.begin_forge_observation t pid ~request in
  let ticket =
    match ticket with
    | Some ticket -> ticket
    | None -> failwith "identified forge fixture could not begin request"
  in
  let poll = observation.Patch_controller.poll_result in
  let state =
    {
      Onton_core_test_support.Forge_fixture.base with
      Pr_state.pr_number = Some pr_number;
      status =
        (if poll.Poller.merged then Pr_state.Merged
         else if poll.Poller.closed then Pr_state.Closed
         else Pr_state.Open);
      head_branch = Some agent.Patch_agent.branch;
      head_oid = poll.Poller.head_oid;
      base_oid = poll.Poller.base_oid;
      base_branch = poll.Poller.base_branch;
      merge_state = poll.Poller.merge_state;
      merge_ready = poll.Poller.merge_ready;
    }
  in
  let t, accepted =
    (Orchestrator.accept_forge_observation ?confirmed_base:poll.Poller.base_oid
       t pid ~ticket ~confirmed_head:confirmed_remote_head
       ({
          Forge_observation.request;
          Forge_observation.outcome = Poll_outcome.Ok_pr_state state;
        }
         : Forge_observation.t) [@warning "-42"])
  in
  (match accepted with
  | Ok _ -> ()
  | Error error -> failwith ("invalid identified forge fixture: " ^ error));
  Patch_controller.apply_poll_result ~previous_conflict
    ?merge_queue_ejection_confirmed ?confirmed_remote_head t pid observation

let conflicting ~head ~base ~base_branch t pid =
  let state =
    {
      Onton_core_test_support.Forge_fixture.base with
      Pr_state.head_oid = Some head;
      base_oid = Some base;
      base_branch = Some base_branch;
      merge_state = Pr_state.Conflicting;
    }
  in
  let t, _, _ =
    apply ~confirmed_remote_head:head t pid
      Patch_controller.
        {
          poll_result = Poller.poll ~was_merged:false state;
          base_branch = Some base_branch;
          native_stack = false;
          branch_in_root = false;
          worktree_path = None;
        }
  in
  t

let[@warning "-42"] readiness t pid =
  let current = Orchestrator.agent t pid in
  if not (Patch_agent.is_pr_present current) then t
  else
    let agent = Onton_core_test_support.Forge_fixture.readiness_agent current in
    match
      Forge_observation.latest_fact
        (Branch_reconcile.forge_observations agent.Patch_agent.branch_reconcile)
    with
    | None -> t
    | Some fact -> (
        let t =
          Orchestrator.set_head_oid t pid (Some fact.Forge_observation.head)
        in
        let request : Forge_observation.request =
          {
            Forge_observation.id = "readiness-fixture";
            Forge_observation.pr_number =
              fact.Forge_observation.ticket.Forge_observation.request
                .Forge_observation.pr_number;
          }
        in
        let t, ticket = Orchestrator.begin_forge_observation t pid ~request in
        match ticket with
        | None -> failwith "readiness fixture could not begin request"
        | Some ticket -> (
            let state =
              {
                Onton_core_test_support.Forge_fixture.base with
                Pr_state.pr_number = Some request.Forge_observation.pr_number;
                head_branch = Some agent.Patch_agent.branch;
                head_oid = Some fact.Forge_observation.head;
                base_oid = Some fact.Forge_observation.base;
                base_branch =
                  Some
                    (Types.Branch.of_string fact.Forge_observation.base_branch);
                merge_state = fact.Forge_observation.merge_state;
              }
            in
            let t, result =
              Orchestrator.accept_forge_observation
                ~confirmed_base:fact.Forge_observation.base t pid ~ticket
                ~confirmed_head:(Some fact.Forge_observation.head)
                {
                  Forge_observation.request;
                  Forge_observation.outcome = Poll_outcome.Ok_pr_state state;
                }
            in
            match result with Ok _ -> t | Error reason -> failwith reason))

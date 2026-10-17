(* @archlint.module test
   @archlint.domain scheduler-recovery *)
open Base
open Onton_core
open Onton
open Types
module R = Scheduler_recovery
module F = Onton_core_test_support.Publication_fixture
module FF = Onton_core_test_support.Forge_fixture

let pid = Patch_id.of_string
let branch = Branch.of_string
let main = branch "main"

let gp =
  match
    Gameplan_parser.parse_json_string
      {|{"projectName":"recovery","patches":[{"number":1,"title":"one"}]}|}
  with
  | Ok parsed -> parsed.gameplan
  | Error reason -> failwith reason

let id = (List.hd_exn gp.patches).Patch.id

let ready ?(patch = List.hd_exn gp.patches) ?(pr_number = 7) () =
  Patch_agent.create ~branch:patch.Patch.branch patch.id
  |> F.reconciled_agent ~base:main
  |> fun a ->
  Patch_agent.set_pr_number a (Pr_number.of_int pr_number) |> fun a ->
  Patch_agent.set_is_draft a false |> fun a ->
  Patch_agent.set_base_branch a main |> fun a ->
  Patch_agent.set_merge_ready a true |> fun a ->
  Patch_agent.set_pr_body_delivered a true |> fun a ->
  Patch_agent.set_checks_passing a true |> fun a ->
  Patch_agent.set_automerge_enabled a true |> FF.readiness_agent

let orch agent =
  Orchestrator.restore
    ~graph:(Graph.of_patches gp.patches)
    ~agents:(Map.singleton (module Patch_id) id agent)
    ~outbox:(Map.empty (module Message_id))
    ~main_branch:main ()

let tick t now =
  Patch_controller.plan_tick_messages ~now t ~project_name:gp.project_name
    ~gameplan:gp

let checkpoint agent =
  match
    Persistence.patch_agent_of_yojson ~snapshot_version:4 ~main_branch:main
      ~gameplan:gp
      (Persistence.patch_agent_to_yojson agent)
  with
  | Ok agent -> agent
  | Error e -> failwith e

let claim t now =
  let t, _, messages = tick t now in
  let message =
    List.find_exn messages ~f:(fun m ->
        Orchestrator.equal_action m.Orchestrator.action
          (Respond (id, Operation_kind.Human)))
  in
  let before = Orchestrator.agent t id in
  let t, action = Orchestrator.accept_message t message.message_id in
  assert (Option.is_some action);
  let agent = Orchestrator.agent t id in
  assert (Option.is_some agent.scheduler_repair);
  (match
     Patch_decision.respond_delivery ~agent ~kind:Operation_kind.Human
       ~pre_fire_agent:(Some before) ~prefetched_comments:[]
       ~prefetched_findings:[] ~main_branch:"main"
   with
  | Deliver { payload = Human_payload { messages }; _ } ->
      assert (not (List.is_empty messages))
  | Deliver
      {
        payload =
          ( Uncommitted_changes_payload | Ci_payload _ | Review_payload _
          | Findings_payload _ | Pr_body_payload | Merge_conflict_payload );
        _;
      }
  | Respond_stale | Skip_empty ->
      failwith "recovery prompt not delivered");
  (t, agent)

let check_repair label agent =
  let t, _, _ = tick (orch agent) 0. in
  let t, agent = claim t 1000. in
  let restored = checkpoint agent in
  assert (
    Option.equal R.equal_ticket restored.scheduler_repair agent.scheduler_repair);
  assert (R.equal restored.scheduler_recovery agent.scheduler_recovery);
  let idle_checkpoint =
    match Persistence.patch_agent_to_yojson agent with
    | `Assoc fields ->
        `Assoc
          (("busy", `Bool false)
          :: List.Assoc.remove fields "busy" ~equal:String.equal)
    | _ -> failwith "expected checkpoint object"
  in
  let idle =
    match
      Persistence.patch_agent_of_yojson ~snapshot_version:4 ~main_branch:main
        ~gameplan:gp idle_checkpoint
    with
    | Ok agent -> agent
    | Error e -> failwith e
  in
  assert (Option.is_none idle.scheduler_repair);
  assert (Option.is_some (R.pending idle.scheduler_recovery));
  let orphan = orch restored in
  assert (not (Orchestrator.agent orphan id).Patch_agent.busy);
  assert (
    Option.is_some (R.pending (Orchestrator.agent orphan id).scheduler_recovery));
  ignore (claim orphan 1000.);
  let forced =
    Orchestrator.apply_force_complete
      ~message_id:(Option.value_exn agent.current_message_id)
      t id Cancelled
  in
  assert (
    Option.is_some
      (R.attention (Orchestrator.agent forced id).scheduler_recovery));
  let t =
    Orchestrator.apply_respond_outcome ~now:1001. t id Operation_kind.Human
      Respond_ok
  in
  let after = Orchestrator.agent t id in
  assert (Option.is_none after.scheduler_repair);
  let t, _, _ = tick t 1002. in
  let t, _, messages = tick t 100000. in
  assert (List.is_empty messages);
  assert (Patch_agent.needs_intervention (Orchestrator.agent t id));
  Stdlib.Printf.printf
    "%s: bounded repair, accepted completion, explicit escalation and \
     checkpoint: OK\n\
     %!"
    label

let repaired_agent ~base ~target ~candidate =
  let module B = Branch_reconcile in
  let commit value =
    match B.Commit.make value with
    | Some value -> value
    | None -> failwith "invalid fixture revision"
  in
  let source = commit (String.make 40 'a') in
  let target = commit target in
  let candidate = commit candidate in
  let step agent event = fst (Patch_agent.reconcile_branch agent event) in
  let reply agent result =
    match B.pending agent.Patch_agent.branch_reconcile with
    | None -> failwith "missing owned command"
    | Some command ->
        step agent (B.Result { token = command.token; at = 100.; result })
  in
  let observation =
    B.
      {
        source;
        target;
        remote = Some source;
        boundary = Recorded source;
        topology = Equal;
        clean = true;
        sequencer = None;
        conflicts = 0;
        target_included = false;
        base_contains_source = false;
        head = source;
        completed_integration = false;
        destination = Remote_id.of_destination "fixture-origin";
      }
  in
  let agent = Patch_agent.create ~branch:(branch "1") (pid "1") in
  let agent =
    step agent (B.Request { base; policy = Rewrite; purpose = Reconcile_base })
  in
  let agent = reply agent (B.Observed observation) in
  let agent = reply agent B.Pinned in
  let agent = step agent B.Recover in
  let observation = { observation with source = candidate; head = candidate } in
  let agent = reply agent (B.Inspected observation) in
  let command =
    match B.pending agent.Patch_agent.branch_reconcile with
    | Some command -> command
    | None -> failwith "missing recovery command"
  in
  let agent, effects =
    Patch_agent.reconcile_branch agent
      (B.Result
         {
           token = command.token;
           at = 100.;
           result = B.Recovery_observed observation;
         })
  in
  let token =
    match effects with
    | [ B.Repair token ] -> token
    | []
    | (B.Execute _ | B.Start_repair _ | B.Completed _) :: _
    | B.Repair _ :: _ :: _ ->
        failwith "missing repair prompt"
  in
  let agent = step agent (B.Repair_started token) in
  let agent = step agent (B.Repair_completed { token; at = 100. }) in
  agent

let () =
  let a = ready () in
  let unknown =
    Patch_agent.set_mergeability_unknown
      (Patch_agent.set_merge_ready a false)
      true
  in
  assert (Patch_controller.is_automerge_candidate unknown ~main_branch:main);
  let t, _ = Patch_controller.reconcile_automerge (orch unknown) ~now:0. in
  let _, decisions = Patch_controller.reconcile_automerge t ~now:121. in
  assert (not (List.is_empty decisions));
  assert (
    not
      (Patch_controller.is_automerge_candidate
         (Patch_agent.set_review_decision unknown (Some "CHANGES_REQUESTED"))
         ~main_branch:main));
  check_repair "native stack" (Patch_agent.set_native_stack a true);
  check_repair "missing CI"
    (Patch_agent.set_checks_passing (Patch_agent.set_merge_ready a false) false);
  let request =
    Forge_observation.{ id = "unconfirmed"; pr_number = Pr_number.of_int 7 }
  in
  let unconfirmed, ticket = Patch_agent.begin_forge_observation a ~request in
  let ticket = Option.value_exn ticket in
  let state =
    {
      FF.base with
      Pr_state.head_branch = Some a.branch;
      head_oid = a.head_oid;
      base_branch = a.base_branch;
      merge_state = Mergeable;
    }
  in
  let unconfirmed, _ =
    Patch_agent.accept_forge_observation unconfirmed ~ticket
      ~confirmed_head:a.head_oid
      { Forge_observation.request; outcome = Poll_outcome.Ok_pr_state state }
  in
  check_repair "unconfirmed forge pair"
    (Patch_agent.defer_forge_revision_pair unconfirmed);
  let entry =
    Pr_state.{ id = "queue-entry"; state = Mq_awaiting_checks; position = 1 }
  in
  let poll =
    Poller.poll ~was_merged:false
      { FF.base with Pr_state.head_oid = a.head_oid; merge_state = Mergeable }
  in
  let removal =
    Merge_queue_decision.apply ~previous_entry:(Some entry)
      ~previous_head_oid:a.head_oid poll
  in
  let queued = Patch_agent.set_merge_queue_ejection a removal.pending in
  let t, _, _ = tick (orch queued) 0. in
  let t, _ = claim t 1000. in
  let t =
    Orchestrator.apply_respond_outcome ~now:1001. t id Operation_kind.Human
      Respond_ok
  in
  assert (Option.is_none (Orchestrator.agent t id).merge_queue_ejection);
  let t0, _, _ = tick (orch queued) 0. in
  let claimed, _ = claim t0 1000. in
  let new_entry = { entry with Pr_state.id = "new-queue-entry" } in
  let new_removal =
    Merge_queue_decision.apply ~previous_entry:(Some new_entry)
      ~previous_head_oid:a.head_oid poll
  in
  let claimed =
    Orchestrator.set_merge_queue_ejection claimed id new_removal.pending
  in
  let finished =
    Orchestrator.apply_respond_outcome ~now:1001. claimed id
      Operation_kind.Human Respond_ok
  in
  assert (Option.is_some (Orchestrator.agent finished id).merge_queue_ejection);
  Stdlib.Printf.printf
    "queue removal: accepted repair discharges unknown removal: OK\n%!";
  List.iter [ R.Pr_base; R.Pr_draft ] ~f:(fun kind ->
      let a =
        if R.equal_kind kind R.Pr_draft then Patch_agent.set_is_draft a true
        else Patch_agent.set_base_branch a (branch "old")
      in
      let t, effects, _ = tick (orch a) 0. in
      assert (not (List.is_empty effects));
      let t =
        Orchestrator.scheduler_write_failed t id ~pr_number:(Pr_number.of_int 7)
          ~now:1. ~kind
      in
      let t =
        Orchestrator.scheduler_write_failed t id ~pr_number:(Pr_number.of_int 7)
          ~now:2. ~kind
      in
      let t, effects, _ = tick t 2. in
      assert (List.is_empty effects);
      let t, _ = claim t 2. in
      let t =
        Orchestrator.apply_respond_outcome ~now:3. t id Operation_kind.Human
          Respond_failed
      in
      let t, effects, messages = tick t 100000. in
      assert (List.is_empty effects && List.is_empty messages);
      assert (Patch_agent.needs_intervention (Orchestrator.agent t id)));
  let original = orch (Patch_agent.set_is_draft a true) in
  let original, _, _ = tick original 0. in
  let original =
    Orchestrator.scheduler_write_failed original id
      ~pr_number:(Pr_number.of_int 7) ~now:1. ~kind:R.Pr_draft
  in
  let replacement =
    Orchestrator.set_pr_number original id (Pr_number.of_int 8)
  in
  let before = Orchestrator.agent replacement id in
  let stale =
    Patch_controller.apply_github_effect_success replacement
      (Set_pr_draft
         { patch_id = id; pr_number = Pr_number.of_int 7; draft = false })
  in
  assert (Patch_agent.equal before (Orchestrator.agent stale id));
  let stale =
    Orchestrator.scheduler_write_failed replacement id
      ~pr_number:(Pr_number.of_int 7) ~now:2. ~kind:R.Pr_draft
  in
  assert (Patch_agent.equal before (Orchestrator.agent stale id));
  let accepted =
    Patch_controller.apply_github_effect_success original
      (Set_pr_draft
         { patch_id = id; pr_number = Pr_number.of_int 7; draft = false })
  in
  assert (
    not
      (R.contains (Orchestrator.agent accepted id).scheduler_recovery R.Pr_draft));
  Stdlib.Printf.printf
    "base/draft write failures: two failures request repair; failure does not \
     loop: OK\n\
     %!";
  let p = List.hd_exn gp.patches in
  let mk n deps =
    {
      p with
      Patch.id = pid (Int.to_string n);
      branch = branch ("repair-" ^ Int.to_string n);
      dependencies = List.map deps ~f:(fun n -> pid (Int.to_string n));
    }
  in
  let p1 = mk 1 [] and p2 = mk 2 [] and p3 = mk 3 [ 1; 2 ] in
  let agents =
    List.map [ p1; p2; p3 ] ~f:(fun patch ->
        let a =
          ready ~patch
            ~pr_number:(7 + Int.of_string (Patch_id.to_string patch.id))
            ()
        in
        let a =
          if Patch_id.equal patch.id p3.id then
            Patch_agent.enqueue
              (Patch_agent.enqueue
                 (Patch_agent.add_human_message a "repair")
                 Operation_kind.Human)
              Operation_kind.Rebase
          else a
        in
        (patch.id, a))
    |> Map.of_alist_exn (module Patch_id)
  in
  let graph = Graph.of_patches [ p1; p2; p3 ] in
  let t =
    Orchestrator.restore ~graph ~agents
      ~outbox:(Map.empty (module Message_id))
      ~main_branch:main ()
  in
  let actions = Patch_controller.plan_actions t ~patches:[ p1; p2; p3 ] in
  assert (
    List.exists actions ~f:(fun action ->
        Orchestrator.equal_action action (Respond (p3.id, Operation_kind.Human))));
  Stdlib.Printf.printf "Human repair bypasses blocked rebase: OK\n%!";
  let agent =
    repaired_agent ~base:"main" ~target:(F.sha "target")
      ~candidate:(F.sha "repair")
  in
  let state = ref agent.Patch_agent.branch_reconcile in
  for n = 1 to 2 do
    let command = Option.value_exn (Branch_reconcile.pending !state) in
    let at = Float.of_int n *. 1000. in
    let next, _ =
      Branch_reconcile.step !state
        (Result
           {
             token = command.token;
             at;
             result =
               Retryable { reason = "capture unavailable"; retry_after = None };
           })
    in
    state := next;
    if n = 1 then
      let event =
        Option.value_exn (Branch_reconcile.wake_event ~at:(at +. 500.) next)
      in
      state := fst (Branch_reconcile.step next event)
  done;
  let op = Option.value_exn (Branch_reconcile.operation !state) in
  let repair = Option.value_exn op.repair in
  assert (Branch_reconcile.equal_phase op.phase (Repairing repair));
  (match repair.mode with
  | Diagnosis _ -> ()
  | Content_repair | History_recovery _ ->
      failwith "capture did not request diagnosis");
  Stdlib.Printf.printf
    "completed repair capture: two read failures request diagnosis: OK\n%!"

let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:"scheduler waits survive checkpoint and become executable repairs"
       ~count:100
       QCheck2.Gen.(int_range 1 10000)
       (fun pr_number ->
         try
           let a =
             ready ~pr_number () |> fun a -> Patch_agent.set_native_stack a true
           in
           let observed =
             Patch_agent.observe_scheduler a ~now:0. ~pr_base_update:false
               ~pr_draft_update:false
             |> checkpoint
           in
           let due =
             Patch_agent.observe_scheduler observed ~now:1000.
               ~pr_base_update:false ~pr_draft_update:false
           in
           match R.pending due.scheduler_recovery with
           | None -> false
           | Some ticket ->
               R.equal_kind ticket.kind R.Native_stack
               && Option.equal Int.equal ticket.scope.pr (Some pr_number)
               && List.mem due.queue Operation_kind.Human
                    ~equal:Operation_kind.equal
         with _ -> false))

let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:
         "checkpointed scheduler tickets retain lifecycle and write authority"
       ~count:100
       QCheck2.Gen.(int_range 1 10000)
       (fun pr ->
         try
           let scope = Patch_agent.scheduler_scope (ready ~pr_number:pr ()) in
           let kind = R.Pr_draft in
           let observed = R.observe R.empty ~now:0. ~scope ~blocked:[ kind ] in
           let failed = R.failed observed ~now:1. ~scope ~kind in
           let due =
             R.observe failed ~now:(R.grace kind +. 1.) ~scope ~blocked:[ kind ]
           in
           match R.pending due with
           | None -> false
           | Some ticket ->
               let decoded = R.decode_ticket (R.yojson_of_ticket ticket) in
               let running = R.claim (R.decode (R.yojson_of_t due)) ticket in
               let recovered = R.recover running ~running:None in
               let completed =
                 R.complete ~now:1000. running ticket ~success:true
               in
               let attention =
                 R.observe completed ~now:10000. ~scope ~blocked:[ kind ]
               in
               let resumed = R.resume attention in
               let resolved = R.resolve resumed kind in
               Option.equal R.equal_ticket decoded (Some ticket)
               && R.is_running running ticket
               && Option.equal R.equal_ticket (R.pending recovered)
                    (Some ticket)
               && (not (R.write_allowed due kind))
               && Option.is_some (R.attention attention)
               && Option.is_none (R.attention resumed)
               && R.contains completed kind
               && (not (R.contains resolved kind))
               && (not (String.is_empty (R.label kind)))
               && not (String.is_empty (R.prompt kind))
         with _ -> false))

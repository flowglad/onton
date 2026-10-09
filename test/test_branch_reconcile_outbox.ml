(* @archlint.module test
   @archlint.domain orchestrator *)

open Base
open Onton
open Onton_core
open Types
module B = Branch_reconcile

let gameplan =
  match
    Gameplan_parser.parse_string
      "projectName: reconcile-test\n\
       owner: owner\n\
       repo: repo\n\
       problemStatement: test\n\
       solutionSummary: test\n\
       patches:\n\
      \  - number: 1\n\
      \    title: patch\n\
      \    description: patch\n\
      \    dependsOn: []\n\
       dependencyGraph:\n\
      \  - patch: 1\n\
      \    dependsOn: []\n"
  with
  | Ok parsed -> parsed.Gameplan_parser.gameplan
  | Error message -> failwith message

let pid = Patch_id.of_string "1"
let main = Branch.of_string "main"
let create () = Orchestrator.create ~patches:gameplan.patches ~main_branch:main
let state t = (Orchestrator.agent t pid).Patch_agent.branch_reconcile

let request t =
  Orchestrator.reconcile_branch t pid
    (B.Request { base = "main"; policy = Rewrite; purpose = Reconcile_base })
  |> fst

let pending t =
  Orchestrator.all_messages t
  |> List.filter ~f:(fun msg ->
      Orchestrator.equal_message_status msg.Orchestrator.status Pending)

let one_pending t =
  match pending t with
  | [ msg ] -> msg
  | [] | _ :: _ -> failwith "expected one pending message"

let fail_transport t =
  match B.pending (state t) with
  | None -> failwith "expected pending Git command"
  | Some command ->
      Orchestrator.reconcile_branch t pid
        (B.Result
           {
             token = command.token;
             at = 100.;
             result = Retryable { reason = "transport"; retry_after = None };
           })
      |> fst

let poll t n =
  List.init n ~f:Fn.id
  |> List.fold ~init:t ~f:(fun t i ->
      let t =
        Orchestrator.set_base_branch t pid
          (Branch.of_string (if i % 2 = 0 then "base-a" else "base-b"))
      in
      let t, _, _ =
        Patch_controller.plan_tick_messages t
          ~project_name:gameplan.project_name ~gameplan
      in
      t)

let tests =
  [
    QCheck2.Test.make ~count:500
      ~name:"scheduler restart resumes captured reconciliation once per runtime"
      ~print:(fun actions ->
        String.concat ~sep:"," (List.map actions ~f:Int.to_string))
      QCheck2.Gen.(list_size (int_range 0 60) (int_range 0 3))
      (fun actions ->
        try
          Eio_main.run (fun _ ->
              let initial = request (create ()) in
              let original = one_pending initial in
              let owner = state initial in
              let accepted = ref false in
              let tick t =
                let next, _, dispatched =
                  Patch_controller.tick t ~project_name:gameplan.project_name
                    ~gameplan
                in
                assert (List.length dispatched = if !accepted then 0 else 1);
                assert (
                  List.for_all dispatched ~f:(fun action ->
                      Orchestrator.equal_action action
                        (Orchestrator.message_action original)));
                accepted := true;
                assert (
                  Option.equal Message_id.equal
                    (Orchestrator.agent next pid).Patch_agent.current_message_id
                    (Some original.message_id));
                assert (B.equal owner (state next));
                assert (Orchestrator.agent next pid).Patch_agent.busy;
                next
              in
              let restart t =
                let snapshot =
                  Runtime.
                    {
                      orchestrator = t;
                      gameplan;
                      activity_log = Activity_log.empty;
                      transcripts = Hashtbl.create (module Patch_id);
                      applied_control_ids = [];
                    }
                in
                let snapshot =
                  Result.ok_or_failwith
                    (Persistence.snapshot_of_yojson
                       (Persistence.snapshot_to_yojson snapshot))
                in
                let runtime =
                  Runtime.create ~gameplan ~main_branch:main ~snapshot ()
                in
                Runtime.update_orchestrator runtime (fun t ->
                    Orchestrator.reset_busy t pid);
                let restored =
                  Runtime.read runtime (fun snapshot ->
                      snapshot.Runtime.orchestrator)
                in
                assert (B.equal owner (state restored));
                assert (
                  List.exists (Orchestrator.all_messages restored)
                    ~f:(fun message ->
                      Message_id.equal message.Orchestrator.message_id
                        original.message_id));
                accepted := false;
                restored
              in
              let final =
                List.fold actions ~init:(tick initial) ~f:(fun t action ->
                    match action with
                    | 0 -> tick t
                    | 1 -> poll t 1
                    | 2 -> request t
                    | _ -> restart t)
              in
              let final = tick final in
              ignore (tick final);
              true)
        with exn -> QCheck2.Test.fail_report (Exn.to_string exn));
    QCheck2.Test.make ~count:1000
      ~name:
        "scheduler accepts one reconciliation across mixed trigger histories"
      ~print:(fun (root, actions) ->
        Bool.to_string root ^ ":"
        ^ String.concat ~sep:"," (List.map actions ~f:Int.to_string))
      QCheck2.Gen.(pair bool (list_size (int_range 0 100) (int_range 0 4)))
      (fun (root, actions) ->
        try
          let initial = create () in
          let initial =
            if root then
              Orchestrator.set_execution_mode initial
                (Result.ok_or_failwith (Execution_mode.infer_gameplan gameplan))
            else initial
          in
          let initial = request initial in
          let original = one_pending initial in
          let tick t =
            Patch_controller.tick t ~project_name:gameplan.project_name
              ~gameplan
          in
          let running, _, dispatched = tick initial in
          assert (List.length dispatched = 1);
          assert (
            match List.hd_exn dispatched with
            | Orchestrator.Reconcile_branch (id, _) -> Patch_id.equal id pid
            | Orchestrator.Start _ | Orchestrator.Respond _
            | Orchestrator.Rebase _ ->
                false);
          let owner = state running in
          let completion =
            Session_result.
              {
                session_uuid = "completed-during-reconciliation";
                delivery_mode = Start;
                kind = None;
                message_id = None;
                result = Session_ok;
                head = None;
                guidance = [];
                turn_accepted = true;
              }
          in
          let receipt_seen = ref false in
          let final =
            List.fold actions ~init:running ~f:(fun t action ->
                let t =
                  match action with
                  | 0 -> poll t 1
                  | 1 -> request t
                  | 2 ->
                      let next, _, dispatched = tick t in
                      assert (List.is_empty dispatched);
                      next
                  | 3 ->
                      let snapshot =
                        Runtime.
                          {
                            orchestrator = t;
                            gameplan;
                            activity_log = Activity_log.empty;
                            transcripts = Hashtbl.create (module Patch_id);
                            applied_control_ids = [];
                          }
                      in
                      (Result.ok_or_failwith
                         (Persistence.snapshot_of_yojson
                            (Persistence.snapshot_to_yojson snapshot)))
                        .orchestrator
                  | _ ->
                      receipt_seen := true;
                      Orchestrator.record_session_completion t pid completion
                in
                assert (B.equal owner (state t));
                assert (Orchestrator.agent t pid).Patch_agent.busy;
                assert (
                  Option.is_none
                    (snd (Orchestrator.accept_message t original.message_id)));
                if !receipt_seen then
                  assert (
                    Option.equal Session_result.equal_completion
                      (Orchestrator.agent t pid).Patch_agent.session_completion
                      (Some completion));
                t)
          in
          let _, _, dispatched = tick final in
          List.is_empty dispatched
        with exn -> QCheck2.Test.fail_report (Exn.to_string exn));
    QCheck2.Test.make
      ~name:
        "unfinished branch work blocks stale readiness after releasing capacity"
      ~count:100 QCheck2.Gen.bool (fun intervention ->
        try
          let ready =
            create () |> fun t ->
            Orchestrator.set_pr_number t pid (Pr_number.of_int 1) |> fun t ->
            Orchestrator.set_is_draft t pid false |> fun t ->
            Orchestrator.set_base_branch t pid main |> fun t ->
            Orchestrator.set_merge_ready t pid true |> fun t ->
            Orchestrator.set_checks_passing t pid true |> fun t ->
            Orchestrator.set_pr_body_delivered t pid true |> fun t ->
            Onton_test_support.Reconciliation_fixture.rebase ~noop:true t pid
              main
          in
          let branch = (Orchestrator.agent ready pid).Patch_agent.branch in
          let can_cut t =
            match
              Orchestrator.start_eligibility t
                ~base_contains_merged_siblings:true branch
            with
            | Start_eligibility.Allow -> true
            | Start_eligibility.Defer _ -> false
          in
          let baseline =
            Patch_controller.ready_for_review ready pid
            && Patch_agent.is_approved
                 (Orchestrator.agent ready pid)
                 ~main_branch:main
            && can_cut ready
          in
          let t = request ready in
          let t =
            if intervention then
              match B.pending (state t) with
              | None -> assert false
              | Some command ->
                  fst
                    (Orchestrator.reconcile_branch t pid
                       (B.Result
                          {
                            token = command.token;
                            at = 100.;
                            result = Needs_diagnosis "policy";
                          }))
            else fail_transport t
          in
          let agent = Orchestrator.agent t pid in
          baseline && (not agent.busy) && agent.checks_passing
          && agent.merge_ready && agent.pr_body_delivered
          && (not (Patch_controller.ready_for_review t pid))
          && (not (Patch_agent.is_approved agent ~main_branch:main))
          && not (can_cut t)
        with _ -> false);
    QCheck2.Test.make
      ~name:"branch work preserves its independent implementation receipt"
      ~count:100
      QCheck2.Gen.(pair (int_range 0 30) string)
      (fun (count, session_uuid) ->
        try
          let completion =
            Session_result.
              {
                session_uuid;
                delivery_mode = Start;
                kind = None;
                message_id = None;
                result = Session_ok;
                head = None;
                guidance = [];
                turn_accepted = true;
              }
          in
          let t =
            Orchestrator.record_session_completion (create ()) pid completion
          in
          let t = request t |> fail_transport |> fun t -> poll t count in
          Option.equal Session_result.equal_completion
            (Orchestrator.agent t pid).Patch_agent.session_completion
            (Some completion)
          && List.length (pending t) = 1
        with _ -> false);
    QCheck2.Test.make ~name:"branch outbox identity survives poll generations"
      QCheck2.Gen.(int_range 1 30)
      (fun count ->
        try
          let t = request (create ()) in
          let original = one_pending t in
          let t = poll t count in
          let current = one_pending t in
          let t, accepted = Orchestrator.accept_message t original.message_id in
          Message_id.equal original.message_id current.message_id
          && Option.is_some accepted
          && (Orchestrator.agent t pid).Patch_agent.busy
          && (not (Orchestrator.agent t pid).Patch_agent.has_session)
          && Option.is_none
               (snd (Orchestrator.accept_message t original.message_id))
        with _ -> false);
    QCheck2.Test.make ~name:"branch backoff releases capacity and stays pending"
      QCheck2.Gen.(int_range 0 30)
      (fun count ->
        try
          let t = request (create ()) in
          let message = one_pending t in
          let t, _ = Orchestrator.accept_message t message.message_id in
          let t =
            fail_transport t |> fun t ->
            Orchestrator.complete t pid |> fun t -> poll t count
          in
          let current = one_pending t in
          Message_id.equal current.message_id message.message_id
          && (not (Orchestrator.agent t pid).Patch_agent.busy)
          && (not (B.can_execute_git ~at:104. (state t)))
          && B.can_execute_git ~at:105. (state t)
          && Option.value_map
               (B.operation (state t))
               ~default:false
               ~f:(fun op -> op.failures = 1 && Option.is_none op.repair)
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "publication backoff permits independent Start and exports runnable \
         work"
      ~count:100
      QCheck2.Gen.(int_range 0 20)
      (fun polls ->
        try
          let other = Patch_id.of_string "2" in
          let template = List.hd_exn gameplan.patches in
          let patches =
            [
              template;
              {
                template with
                Patch.id = other;
                branch = Branch.of_string "reconcile-test/patch-2";
              };
            ]
          in
          let gameplan = { gameplan with Gameplan.patches } in
          let t =
            Orchestrator.reconcile_branch
              (Orchestrator.create ~patches ~main_branch:main)
              pid
              (B.Request
                 {
                   base = "main";
                   policy = Rewrite;
                   purpose =
                     Publish_revision
                       (Option.value_exn (B.Commit.make (String.make 40 'a')));
                 })
            |> fst |> fail_transport
          in
          let retry = one_pending t in
          let owner = state t in
          let t =
            List.fold
              (List.init (polls + 1) ~f:Fn.id)
              ~init:t
              ~f:(fun t _ ->
                let t, _, _ =
                  Patch_controller.plan_tick_messages t
                    ~project_name:gameplan.project_name ~gameplan
                in
                t)
          in
          let start =
            List.find_exn (pending t) ~f:(fun message ->
                match message.Orchestrator.action with
                | Orchestrator.Start (patch, _) -> Patch_id.equal patch other
                | Orchestrator.Respond _ | Orchestrator.Rebase _
                | Orchestrator.Reconcile_branch _ ->
                    false)
          in
          let t, accepted = Orchestrator.accept_message t start.message_id in
          let snapshot =
            Runtime.
              {
                orchestrator = t;
                gameplan;
                activity_log = Activity_log.empty;
                transcripts = Hashtbl.create (module Patch_id);
                applied_control_ids = [];
              }
          in
          let json = Persistence.snapshot_to_yojson snapshot in
          let restored =
            Result.ok_or_failwith (Persistence.snapshot_of_yojson json)
          in
          Option.is_some accepted
          && (Orchestrator.agent t other).Patch_agent.busy
          && (not (Orchestrator.agent t pid).Patch_agent.busy)
          && B.equal owner (state t)
          && (not (B.can_execute_git ~at:104. (state t)))
          && B.can_execute_git ~at:105. (state t)
          && Option.value (Json.bool_field "runnable" json) ~default:false
          && List.exists (pending restored.orchestrator) ~f:(fun message ->
              Message_id.equal message.Orchestrator.message_id retry.message_id)
          && B.equal owner (state restored.orchestrator)
        with _ -> false);
    QCheck2.Test.make
      ~name:"terminal patch completes held reconciliation delivery" ~count:50
      QCheck2.Gen.bool (fun merged ->
        try
          let t = request (create ()) in
          let message = one_pending t in
          let t, accepted = Orchestrator.accept_message t message.message_id in
          let t =
            if merged then Orchestrator.mark_merged t pid
            else
              Orchestrator.apply_session_result t pid
                (Orchestrator.Session_wontdo "superseded")
          in
          let t = Orchestrator.complete t pid in
          Option.is_some accepted
          && List.for_all (pending t) ~f:(fun msg ->
              not (Message_id.equal msg.message_id message.message_id))
        with _ -> false);
    QCheck2.Test.make
      ~name:"session completion cannot erase pending publication"
      QCheck2.Gen.(int_range 1 30)
      (fun count ->
        try
          let t = create () in
          let message =
            Orchestrator.message_of_action (Orchestrator.agent t pid)
              (Orchestrator.Start (pid, main))
          in
          let t = Orchestrator.reconcile_message t message in
          let t, _ = Orchestrator.accept_message t message.message_id in
          let t = request t in
          let branch_message = one_pending t in
          let t = poll t count in
          let t = Orchestrator.complete t pid in
          Message_id.equal (one_pending t).message_id branch_message.message_id
          && List.length (Orchestrator.runnable_messages t) = 1
        with _ -> false);
    QCheck2.Test.make ~name:"pending branch work survives snapshot restart"
      QCheck2.Gen.(int_range 0 30)
      (fun count ->
        try
          let t =
            request (create ()) |> fail_transport |> fun t -> poll t count
          in
          let snap =
            Runtime.
              {
                orchestrator = t;
                gameplan;
                activity_log = Activity_log.empty;
                transcripts = Hashtbl.create (module Patch_id);
                applied_control_ids = [];
              }
          in
          match
            Persistence.snapshot_of_yojson (Persistence.snapshot_to_yojson snap)
          with
          | Error _ -> false
          | Ok restored ->
              Message_id.equal (one_pending t).message_id
                (one_pending restored.orchestrator).message_id
              && B.equal (state t) (state restored.orchestrator)
        with _ -> false);
  ]

let () =
  let original =
    create () |> fun o -> Orchestrator.set_pr_number o pid (Pr_number.of_int 12)
  in
  let snapshot =
    Runtime.
      {
        orchestrator = original;
        gameplan;
        activity_log = Activity_log.empty;
        transcripts = Hashtbl.create (module Patch_id);
        applied_control_ids = [];
      }
  in
  let rec legacy = function
    | `Assoc fields ->
        `Assoc
          (List.filter_map fields ~f:(fun (key, value) ->
               if String.equal key "branch_reconcile" then None
               else
                 Some
                   ( key,
                     if String.equal key "version" then `Int 1 else legacy value
                   )))
    | `List values -> `List (List.map values ~f:legacy)
    | value -> value
  in
  match
    Persistence.snapshot_of_yojson
      (legacy (Persistence.snapshot_to_yojson snapshot))
  with
  | Error reason -> failwith reason
  | Ok snapshot ->
      let restored = snapshot.Runtime.orchestrator in
      let message = one_pending restored in
      assert (B.is_pending (state restored));
      assert (
        match Orchestrator.message_action message with
        | Orchestrator.Reconcile_branch (id, _) -> Patch_id.equal id pid
        | Orchestrator.Start _ | Orchestrator.Respond _ | Orchestrator.Rebase _
          ->
            false)

let () =
  let initial = request (create ()) in
  let before = one_pending initial in
  let command = Option.value_exn (B.pending (state initial)) in
  let stopped, _ =
    Orchestrator.reconcile_branch initial pid
      (B.Result
         {
           token = command.token;
           at = 100.;
           result = B.Needs_diagnosis "permissions";
         })
  in
  let stopped =
    Onton_core_test_support.Publication_fixture.exhaust_diagnosis ~state
      ~step:(fun t e -> fst (Orchestrator.reconcile_branch t pid e))
      stopped
  in
  assert (List.is_empty (pending stopped));
  List.iter [ false; true ] ~f:(fun human ->
      let resumed =
        if human then
          Orchestrator.send_human_message stopped pid "retry after repair"
        else Orchestrator.reset_intervention_state stopped pid
      in
      let message = one_pending resumed in
      assert (Message_id.equal before.message_id message.message_id);
      assert (
        match B.pending (state resumed) with
        | Some c -> B.equal_command_kind c.kind B.Inspect
        | None -> false))

let () =
  let source = String.make 40 'a'
  and target = String.make 40 'b'
  and candidate = String.make 40 'c' in
  let sha s = Option.value_exn (B.Commit.make s) in
  let initial =
    Orchestrator.set_pr_number (create ()) pid (Pr_number.of_int 1)
  in
  let reply t result =
    let command = Option.value_exn (B.pending (state t)) in
    fst
      (Orchestrator.reconcile_branch t pid
         (B.Result { token = command.token; at = 100.; result }))
  in
  let t =
    request initial |> fun t ->
    reply t
      (B.Observed
         {
           head = sha source;
           source = sha source;
           target = sha target;
           remote = Some (sha source);
           boundary = B.Recorded (sha source);
           topology = B.Equal;
           clean = true;
           sequencer = None;
           conflicts = 0;
           target_included = false;
           base_contains_source = false;
           completed_integration = false;
           destination =
             Branch_reconcile.Remote_id.of_destination "fixture-origin";
         })
    |> fun t ->
    reply t B.Pinned |> fun t ->
    reply t (B.Integrated (sha candidate)) |> fun t ->
    reply t B.Published |> fun t ->
    reply t (B.Remote { sha = Some (sha candidate); topology = B.Equal })
  in
  let observation head =
    Patch_controller.
      {
        poll_result =
          {
            Poller.queue = [ Operation_kind.Merge_conflict ];
            merged = false;
            closed = false;
            is_draft = false;
            merge_state = Pr_state.Conflicting;
            merge_ready = false;
            base_branch = Some main;
            base_oid = Some target;
            head_oid = Some head;
            review_decision = None;
            unresolved_comment_count = 0;
            merge_queue_required = false;
            merge_queue_entry = None;
            checks_passing = true;
            ci_checks = [];
            merge_commit_sha = None;
          };
        base_branch = Some main;
        native_stack = false;
        branch_in_root = false;
        worktree_path = None;
      }
  in
  let initial_publication = t in
  let receipt = Option.value_exn (B.unobserved_publication (state t)) in
  let generation = (Orchestrator.agent t pid).Patch_agent.generation in
  let direct = Orchestrator.observe_publication_head t pid (Some candidate) in
  assert ((Orchestrator.agent direct pid).Patch_agent.generation = generation);
  assert (
    Option.is_none
      (Patch_agent.expected_remote_head_oid (Orchestrator.agent direct pid)));
  let unidentified = Orchestrator.observe_publication_head t pid None in
  assert (B.equal (state t) (state unidentified));
  let verified =
    fst
      (B.step (state t)
         (B.Publication_observed
            {
              publication = B.publication_identity receipt;
              head = sha candidate;
              remote = Some (sha candidate);
            }))
  in
  let repeated =
    List.fold (List.init 5 ~f:Fn.id) ~init:t ~f:(fun t _ ->
        let t, logs, _ =
          Onton_test_support.Forge_poll_fixture.apply
            ~confirmed_remote_head:candidate t pid (observation candidate)
        in
        assert (not (List.is_empty logs));
        let agent = Orchestrator.agent t pid in
        assert (agent.mergeability_unknown && not agent.merge_ready);
        assert (
          not
            (List.mem agent.queue Operation_kind.Merge_conflict
               ~equal:Operation_kind.equal));
        assert (
          Option.equal B.equal_operation (B.operation verified)
            (B.operation agent.branch_reconcile));
        assert (
          List.equal B.equal_integration_receipt (B.integrations verified)
            (B.integrations agent.branch_reconcile));
        assert (
          List.equal B.equal_publication_receipt (B.publications verified)
            (B.publications agent.branch_reconcile));
        assert (
          Option.is_some
            (Forge_observation.latest_fact
               (B.forge_observations agent.branch_reconcile)));
        t)
  in
  assert (
    Option.is_none
      (Patch_agent.expected_remote_head_oid (Orchestrator.agent repeated pid)));
  List.iter [ None; Some candidate; Some source ]
    ~f:(fun confirmed_remote_head ->
      let observed, _, _ =
        Onton_test_support.Forge_poll_fixture.apply ?confirmed_remote_head
          repeated pid (observation source)
      in
      let agent = Orchestrator.agent observed pid in
      if Option.equal String.equal confirmed_remote_head (Some source) then (
        assert (Patch_agent.has_conflict agent);
        assert (Option.equal String.equal agent.head_oid (Some source)))
      else (
        assert (not (Patch_agent.has_conflict agent));
        assert ((not agent.checks_passing) && not agent.merge_ready);
        assert (Option.equal String.equal agent.head_oid (Some candidate))));
  let t = Orchestrator.set_head_oid initial_publication pid (Some source) in
  let restored =
    Persistence.patch_agent_to_yojson (Orchestrator.agent t pid)
    |> Persistence.patch_agent_of_yojson ~main_branch:main ~gameplan
    |> Result.ok_or_failwith
  in
  assert (
    Option.equal String.equal
      (Patch_agent.expected_remote_head_oid restored)
      (Some candidate));
  (* Restart preserves both the pending identity and an accepted acknowledgement.
     No process-local publication marker may erase or re-arm either state. *)
  List.iter [ false; true ] ~f:(fun acknowledged ->
      let before =
        if acknowledged then
          Orchestrator.observe_publication_head t pid (Some candidate)
        else t
      in
      let runtime =
        Runtime.create ~gameplan ~main_branch:main
          ~snapshot:
            {
              Runtime.orchestrator = before;
              activity_log = Activity_log.empty;
              gameplan;
              transcripts = Hashtbl.create (module Patch_id);
              applied_control_ids = [];
            }
          ()
      in
      let after =
        Runtime.read runtime (fun snapshot -> snapshot.Runtime.orchestrator)
      in
      assert (B.equal (state before) (state after));
      assert (
        Option.equal String.equal
          (Patch_agent.expected_remote_head_oid (Orchestrator.agent after pid))
          (if acknowledged then None else Some candidate)));
  List.iter [ None; Some candidate; Some source ]
    ~f:(fun confirmed_remote_head ->
      let observed, _, _ =
        Onton_test_support.Forge_poll_fixture.apply ?confirmed_remote_head t pid
          (observation source)
      in
      let agent = Orchestrator.agent observed pid in
      if Option.equal String.equal confirmed_remote_head (Some source) then (
        assert (Patch_agent.has_conflict agent);
        assert (Option.is_none (Patch_agent.expected_remote_head_oid agent));
        assert (Option.equal String.equal agent.head_oid (Some source)))
      else (
        assert (not (Patch_agent.has_conflict agent));
        assert (
          Option.equal String.equal
            (Patch_agent.expected_remote_head_oid agent)
            (Some candidate))));
  let unidentified = observation candidate in
  let unidentified =
    {
      unidentified with
      poll_result = { unidentified.poll_result with head_oid = None };
    }
  in
  let observed, _, _ =
    Patch_controller.apply_poll_result ~confirmed_remote_head:candidate t pid
      unidentified
  in
  assert (
    Option.equal String.equal
      (Patch_agent.expected_remote_head_oid (Orchestrator.agent observed pid))
      (Some candidate))

let () =
  List.iter [ false; true ] ~f:(fun merged ->
      let initial = request (create ()) in
      let message = one_pending initial in
      let held =
        if merged then Orchestrator.mark_merged initial pid
        else
          Orchestrator.apply_session_result initial pid
            (Orchestrator.Session_wontdo "hold")
      in
      assert (B.equal (state initial) (state held));
      assert (List.is_empty (Orchestrator.runnable_messages held));
      let held, accepted =
        Orchestrator.accept_message held message.message_id
      in
      assert (Option.is_none accepted);
      assert (not (Orchestrator.agent held pid).Patch_agent.busy);
      assert (Message_id.equal (one_pending held).message_id message.message_id);
      if not merged then
        let resumed = Orchestrator.reset_intervention_state held pid in
        assert (List.length (Orchestrator.runnable_messages resumed) = 1))

let () = QCheck_base_runner.run_tests_main tests

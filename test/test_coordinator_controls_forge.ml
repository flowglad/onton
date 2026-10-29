(* @archlint.module test
   @archlint.domain coordinator-forge *)
open Onton_core
open Onton

[@@@warning "-4-40-42"]

let check ok why = if not ok then failwith why

let control ~id ~kind ~patch_id ~payload =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("version", `Int 3);
         ("id", `String id);
         ("run_id", `String "r");
         ("config_id", `String "cfg");
         ("kind", `String kind);
         ("patch_id", patch_id);
         ("payload", `String payload);
       ])

let applied host id kind patch payload =
  Coordinator_control.handle host (control ~id ~kind ~patch_id:patch ~payload)

let test_control_store () =
  let module F = Onton_test_support.Coordinator_store_fixture in
  let f = F.fixture () in
  let store, state, next = F.setup f in
  ignore
    (F.committed
       (Coordinator_store.commit store ~expected_version:0L ~write_id:"initial"
          (Coordinator_store.initial state)));
  let restore () =
    match Coordinator_host.restore ~store ~next_write_id:next with
    | Ok host -> host
    | Error _ -> failwith "restore"
  in
  let host = restore () in
  let none = `Null in
  check
    (String.equal (applied host "sleep" "sleep" none "") "accepted")
    "sleep not committed";
  check
    (String.equal (applied host "sleep" "sleep" none "") "already_applied")
    "duplicate sleep not deduplicated";
  check
    (String.equal (applied host "sleep" "cancel" none "") "rejected")
    "changed ID reused";
  check
    (String.equal (applied host "wake" "wake" none "") "accepted")
    "wake not committed";
  check
    (not (Coordinator_state.sleep_ready (Coordinator_host.state host)))
    "wake must invalidate old sleep generation";
  let restarted = restore () in
  check
    (String.equal (applied restarted "sleep" "sleep" none "") "already_applied")
    "control history lost across restore";
  check
    (String.equal (applied restarted "cancel" "cancel" none "") "accepted")
    "cancel not committed";
  check
    (String.equal (applied restarted "late-wake" "wake" none "") "rejected")
    "late wake must not revive cancellation";
  check
    (String.equal (applied restarted "late-sleep" "sleep" none "") "rejected")
    "cancel must dominate sleep";
  check
    (match Coordinator_host.submit restarted Cancel_cleanup_confirmed with
    | Error (Coordinator_host.Invalid _) -> true
    | _ -> false)
    "unbound close attestation admitted";
  let close =
    match
      Resource_protocol.make_request
        {|{"version":"resource-v1","op":"close","run_id":"r","request_id":"close-1","coordinator_id":"c","owner_epoch":"1","revision":"1"}|}
    with
    | Ok request -> request
    | Error e -> failwith e
  in
  let response =
    match
      Resource_protocol.decode_response
        {|{"status":"ok","reason":"none","run_id":"r","owner_epoch":1,"revision":1}|}
    with
    | Ok reply -> reply
    | Error e -> failwith e
  in
  check
    (match
       Coordinator_host.cancel_closed restarted ~close_request:close ~response
     with
    | Error (Coordinator_host.Invalid _) -> true
    | _ -> false)
    "unpersisted close acknowledged";
  check
    (match
       Coordinator_host.submit restarted
         (Resource_request (close.request_id, close.bytes))
     with
    | Ok (Coordinator_host.Applied [ Accepted ]) -> true
    | _ -> false)
    "close request not committed";
  check
    (match
       Coordinator_host.cancel_closed restarted ~close_request:close ~response
     with
    | Ok (Coordinator_host.Applied [ Cancel_complete ]) -> true
    | _ -> false)
    "matching close not confirmed";
  check
    (Coordinator_state.cancellation_complete (Coordinator_host.state restarted))
    "cancel not complete after confirmed cleanup";
  check
    (String.equal
       (applied restarted "guidance" "guidance" (`String "unknown") "fix")
       "rejected")
    "unknown patch admitted";
  check
    (Coordinator_state.inputs (Coordinator_host.state restarted) = [])
    "rejected guidance created an input";
  let reject = F.fixture () in
  let store, state, next = F.setup reject in
  ignore
    (F.committed
       (Coordinator_store.commit store ~expected_version:0L ~write_id:"initial"
          (Coordinator_store.initial state)));
  let host =
    match Coordinator_host.restore ~store ~next_write_id:next with
    | Ok h -> h
    | Error _ -> failwith "restore reject fixture"
  in
  reject.reject_write <- true;
  check
    (String.equal (applied host "uncommitted" "sleep" none "") "not_accepted")
    "rejected storage write acknowledged";
  check
    (Coordinator_state.controls (Coordinator_host.state host) = [])
    "rejected write published memory"

let test_guidance_atomic () =
  let module F = Onton_test_support.Coordinator_store_fixture in
  let f = F.fixture () in
  let store, state, next = F.setup f in
  let op : Coordinator_state.operation =
    {
      id = "op";
      patch = Types.Patch_id.of_string "patch";
      used = 0;
      budget = 3;
      retry_after = 0L;
      attention = false;
      unfinished = true;
      maintenance = false;
    }
  in
  let state, effects = Coordinator_state.step state (Admit op) in
  check (effects = [ Accepted ]) "operation missing";
  ignore
    (F.committed
       (Coordinator_store.commit store ~expected_version:0L ~write_id:"initial"
          (Coordinator_store.initial state)));
  let host =
    match Coordinator_host.restore ~store ~next_write_id:next with
    | Ok h -> h
    | Error _ -> failwith "restore guidance"
  in
  let patch = `String "patch" in
  check
    (String.equal
       (applied host "guide-1" "guidance" patch "fix this")
       "accepted")
    "guidance not committed";
  check
    (Coordinator_state.inputs (Coordinator_host.state host)
    = [ ("guide-1", "patch", 1L, "fix this", false) ])
    "guidance not retained atomically as unconsumed input";
  check
    (String.equal
       (applied host "guide-1" "guidance" patch "changed")
       "rejected")
    "changed guidance reused ID";
  check
    (String.equal
       (applied host "guide-2" "guidance" patch "new guidance")
       "accepted")
    "later guidance not committed";
  check
    (Coordinator_state.inputs (Coordinator_host.state host)
    = [
        ("guide-2", "patch", 2L, "new guidance", false);
        ("guide-1", "patch", 1L, "fix this", false);
      ])
    "new guidance erased old input"

let test_stop_request_is_not_container () =
  let state =
    Coordinator_state.create ~run_id:"r" ~config_id:"cfg" ~plan_id:"plan"
      ~plan_bytes:"plan" ~config_bytes:"config" ~capacity:2
      ~budget_bytes:8388608
  in
  let stop =
    {|{"version":"resource-v1","op":"stop","run_id":"r","request_id":"stop-1","coordinator_id":"c","owner_epoch":"1","revision":"1","resource_id":"resource-1"}|}
  in
  let state, effects =
    Coordinator_state.step state (Resource_request ("stop-1", stop))
  in
  check (effects = [ Accepted ]) "stop request not stored";
  let command =
    match
      Coordinator_state.decode_control
        (control ~id:"sleep" ~kind:"sleep" ~patch_id:`Null ~payload:"")
    with
    | Ok c -> c
    | Error e -> failwith e
  in
  let state, effects = Coordinator_state.step state (Control command) in
  check (effects = [ Accepted ]) "sleep intent not recorded";
  let state, effects = Coordinator_state.step state (Sleep_ready "sleep") in
  check
    (effects = [ Ready_to_sleep "sleep" ])
    "retained stop request must not count as live container";
  check (Coordinator_state.sleep_ready state) "sleep not ready"

let test_scope () =
  let open Types in
  let scope : Coordinator_forge.scope =
    {
      ticket = "ticket-1";
      pr = Pr_number.of_int 7;
      head_branch = Branch.of_string "feature";
      base_branch = Branch.of_string "main";
      head_revision = "aaa";
      base_revision = "bbb";
      push_destination = "push-url";
      generation = 0;
    }
  in
  let stale = { scope with ticket = "ticket-0" } in
  check
    (not
       (Coordinator_forge.contribution_credit ~expected:scope ~current:stale
          ~matching_receipt:true ~already_credited:false))
    "stale ticket credited";
  check
    (not
       (Coordinator_forge.contribution_credit ~expected:scope ~current:scope
          ~matching_receipt:false ~already_credited:false))
    "stdout credited without receipt";
  check
    (Coordinator_forge.contribution_credit ~expected:scope ~current:scope
       ~matching_receipt:true ~already_credited:false)
    "matching receipt not credited";
  check
    (not
       (Coordinator_forge.ready ~expected:scope ~current:scope
          ~policy_ready:true (Coordinator_forge.Unconfirmed "stale checks")))
    "unconfirmed pair ready";
  let seen = ref [] in
  let io : Branch_reconcile_executor.io =
    {
      git =
        (fun args ->
          seen := args :: !seen;
          (2, "", "missing"));
    }
  in
  check
    (Branch_reconcile_executor.remote_head_at io ~destination:"push-url"
       ~branch:"feature"
    = Ok None)
    "missing ref is not an error";
  check
    (List.exists
       (fun args ->
         args
         = [
             "ls-remote";
             "--exit-code";
             "--refs";
             "--";
             "push-url";
             "refs/heads/feature";
           ])
       !seen)
    "explicit push destination not probed";
  let io = { Branch_reconcile_executor.git = (fun _ -> (1, "", "denied")) } in
  check
    (Result.is_error
       (Branch_reconcile_executor.remote_head_at io ~destination:"origin"
          ~branch:"main"))
    "failed probe misread as missing ref"

let creates = ref 0
let provider_reads = ref 0
let provider_state : Pr_state.t option ref = ref None
let mutations = ref []
let lose_effect = ref false
let after_read = ref (fun () -> ())
let after_effect = ref (fun () -> ())

module Fake_forge : Forge.S with type error = string = struct
  type error = string

  let name = "fixture"
  let owner = "fixture"
  let show_error e = e
  let poll_error e = Poll_outcome.Transport_failed { msg = e }
  let is_duplicate_change_error _ = false
  let is_permanent_error _ = false
  let is_merge_queue_required_error _ = false
  let supports_reviews = false
  let supports_pull_request_changes = true
  let supports_branch_changes = false

  type merge_result =
    | Merge_succeeded
    | Merge_queued of string
    | Merge_unconfirmed

  type enqueue_result =
    | Enqueued of Pr_state.merge_queue_entry
    | Already_enqueued of Pr_state.merge_queue_entry

  let change_url _ = None
  let branch_state ?known_state:_ _ = Error "unused"

  let pr_state _ =
    incr provider_reads;
    let result =
      match !provider_state with
      | Some pr -> Ok pr
      | None -> Error "unavailable"
    in
    !after_read ();
    result

  let merge_queue_removal_checks ~pr_number:_ = Ok []
  let check_failure_details ~check:_ = Error "unused"
  let rerun_failed_jobs_for_check ~check:_ = Error "unused"
  let list_prs ~branch:_ ?base:_ ~state:_ () = Ok []

  let update_pr_body ~pr_number:_ ~body:_ =
    mutations := "body" :: !mutations;
    !after_effect ();
    if !lose_effect then Error "lost body response" else Ok ()

  let reply_to_review_comment ~pr_number:_ ~comment_id:_ ~body:_ =
    Error "unused"

  let resolve_review_thread ~thread_id:_ = Error "unused"
  let viewer_login () = None

  let create_pull_request ~title:_ ~head:_ ~base:_ ~body:_ ~draft:_ =
    incr creates;
    Error "lost create response"

  let update_pr_base ~pr_number:_ ~base:_ = Ok ()
  let request_review ~pr_number:_ ~team_slug:_ = Ok ()
  let set_draft ~pr_number:_ ~draft:_ = Ok ()

  let merge_pr ~pr_number:_ =
    mutations := "merge" :: !mutations;
    if !lose_effect then Error "lost merge response"
    else (
      provider_state :=
        Option.map
          (fun pr -> { pr with Pr_state.status = Merged })
          !provider_state;
      Ok Merge_succeeded)

  let enqueue_pr ~pr_number:_ = Error "unused"
  let dequeue_pr ~pr_number:_ = Error "unused"
  let check_repo_access () = Ok ()
end

let test_ambiguous_create () =
  let module F = Onton_test_support.Coordinator_store_fixture in
  let fixture = F.fixture () in
  let store, state, next = F.setup fixture in
  let patch = Types.Patch_id.of_string "patch" in
  let op : Coordinator_state.operation =
    {
      id = "op";
      patch;
      used = 0;
      budget = 3;
      retry_after = 0L;
      attention = false;
      unfinished = true;
      maintenance = false;
    }
  in
  let state, effects = Coordinator_state.step state (Admit op) in
  check (effects = [ Accepted ]) "operation not admitted";
  ignore
    (F.committed
       (Coordinator_store.commit store ~expected_version:0L ~write_id:"initial"
          (Coordinator_store.initial state)));
  let restore () =
    match Coordinator_host.restore ~store ~next_write_id:next with
    | Ok h -> h
    | Error _ -> failwith "restore"
  in
  let scope : Coordinator_forge.scope =
    {
      ticket = "ticket";
      pr = Types.Pr_number.of_int 0;
      head_branch = Types.Branch.of_string "feature";
      base_branch = Types.Branch.of_string "main";
      head_revision = String.make 40 'a';
      base_revision = String.make 40 'b';
      push_destination = "push";
      generation = 0;
    }
  in
  let io : Branch_reconcile_executor.io =
    {
      git =
        (fun args ->
          let revision =
            if List.mem "refs/heads/feature" args then String.make 40 'a'
            else String.make 40 'b'
          in
          (0, revision ^ "\t" ^ List.hd (List.rev args) ^ "\n", ""));
    }
  in
  let call host scope =
    Coordinator_forge.create_or_discover
      (module Fake_forge)
      ~host ~io ~scope ~patch ~effect_id:"create-1" ~renewal_id:None
      ~title:"title" ~body:"body" ~draft:true
  in
  creates := 0;
  let host = restore () in
  provider_reads := 0;
  check
    (match
       Coordinator_forge.observe_committed
         (module Fake_forge)
         ~host ~io ~patch ~scope
     with
    | Coordinator_forge.Unavailable _ -> true
    | _ -> false)
    "provider error invented proof";
  check (!provider_reads = 1) "committed ticket not read";
  check
    (match
       Coordinator_forge.observe_committed
         (module Fake_forge)
         ~host ~io ~patch
         ~scope:{ scope with head_revision = "changed" }
     with
    | Coordinator_forge.Unconfirmed _ -> true
    | _ -> false)
    "changed ticket identity accepted";
  check (!provider_reads = 1) "changed ticket read provider";
  check
    (match
       Coordinator_forge.change
         (module Fake_forge)
         ~host ~io ~scope ~patch ~effect_id:"draft-1"
         ~view:
           {
             Scheduler_decision.graph = Graph.of_patches [];
             mode = Execution_mode.mainline;
             agents = [];
             main = Types.Branch.of_string "main";
           }
         ~now:0. (Coordinator_forge.Set_draft true)
     with
    | Coordinator_forge.Change_pending _ -> true
    | _ -> false)
    "unavailable provider effect not pending";
  check
    (not
       (List.exists
          (fun (c : Coordinator_state.control) -> String.equal c.id "draft-1")
          (Coordinator_state.controls (Coordinator_host.state host))))
    "unconfirmed provider read consumed mutation identity";
  check
    (match call host scope with
    | Coordinator_forge.Pending _ -> true
    | _ -> false)
    "ambiguous create not pending";
  check (!creates = 1) "first create not attempted";
  check
    (match call host scope with
    | Coordinator_forge.Pending _ -> true
    | _ -> false)
    "absent discovery not pending";
  check (!creates = 1) "absent eventual read duplicated create";
  let restarted = restore () in
  check
    (match call restarted scope with
    | Coordinator_forge.Pending _ -> true
    | _ -> false)
    "restart not pending";
  check (!creates = 1) "restart duplicated create";
  let changed : Coordinator_forge.scope =
    { scope with head_revision = "ccc" }
  in
  check
    (match call restarted changed with
    | Coordinator_forge.Blocked _ -> true
    | _ -> false)
    "changed effect ID accepted";
  check (!creates = 1) "changed reuse wrote to provider";
  check
    (match
       Coordinator_forge.create_or_discover
         (module Fake_forge)
         ~host:restarted ~io ~scope:changed ~patch ~effect_id:"create-2"
         ~renewal_id:None ~title:"title" ~body:"body" ~draft:true
     with
    | Coordinator_forge.Blocked _ -> true
    | _ -> false)
    "disappeared publication recreated without explicit renewal";
  check (!creates = 1) "unrenewed create wrote to provider"

let test_committed_evidence_and_effects () =
  let module F = Onton_test_support.Coordinator_store_fixture in
  let f = F.fixture () in
  let store, state, next = F.setup f in
  let patch = Types.Patch_id.of_string "patch" in
  let branch = Types.Branch.of_string in
  let number = Types.Pr_number.of_int 1 in
  let head = String.make 40 'a' and base = String.make 40 'b' in
  let scope : Coordinator_forge.scope =
    {
      ticket = "poll";
      pr = number;
      head_branch = branch "feature";
      base_branch = branch "main";
      head_revision = head;
      base_revision = base;
      push_destination = "push";
      generation = 0;
    }
  in
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
  in
  let agent =
    Patch_agent.create_adhoc ~complexity:None ~patch_id:patch
      ~branch:(branch "feature") ~pr_number:number ~max_ci_failures:3
    |> fun a ->
    Patch_agent.set_base_branch a (branch "main") |> fun a ->
    Patch_agent.set_head_oid a (Some head) |> fun a ->
    Patch_agent.set_automerge_enabled a true
  in
  let view : Scheduler_decision.input =
    {
      graph =
        Graph.of_patches
          [
            {
              Types.Patch.id = patch;
              title = "patch";
              description = "patch";
              branch = branch "feature";
              dependencies = [];
              spec = "";
              acceptance_criteria = [];
              files = [];
              classification = "";
              changes = [];
              test_stubs_introduced = [];
              test_stubs_implemented = [];
              complexity = None;
              precedents = [];
              required_context = [];
            };
          ];
      mode = Execution_mode.mainline;
      agents = [ agent ];
      main = branch "main";
    }
  in
  let op : Coordinator_state.operation =
    {
      id = "work";
      patch;
      used = 0;
      budget = 3;
      retry_after = 0L;
      attention = false;
      unfinished = true;
      maintenance = false;
    }
  in
  let state, _ = Coordinator_state.step state (Admit op) in
  ignore
    (F.committed
       (Coordinator_store.commit store ~expected_version:0L ~write_id:"initial"
          (Coordinator_store.initial state)));
  let restore () =
    let client =
      Resource_client.create ~helper:"fixture" ~transport:(F.transport f)
    in
    let store =
      Coordinator_store.create ~client ~run_id:"r" ~coordinator_id:"c"
        ~owner_epoch:1L ~revision:1L ~next_id:next
    in
    match Coordinator_host.restore ~store ~next_write_id:next with
    | Ok h -> h
    | Error _ -> failwith "restore"
  in
  let io : Branch_reconcile_executor.io =
    {
      git =
        (fun args ->
          let revision =
            if List.mem "refs/heads/feature" args then head else base
          in
          (0, revision ^ "\t" ^ List.hd (List.rev args) ^ "\n", ""));
    }
  in
  let observe host ticket =
    Coordinator_forge.observe_committed
      (module Fake_forge)
      ~host ~io ~patch ~scope:{ scope with ticket }
  in
  let host = restore () in
  provider_state := Some pr;
  (after_read := fun () -> f.reject_write <- true);
  check
    (match observe host "rejected-result" with
    | Unavailable _ -> true
    | _ -> false)
    "uncommitted observation returned as proof";
  check
    (Coordinator_state.inputs (Coordinator_host.state host) = [])
    "uncommitted evidence queued";
  (after_read := fun () -> ());
  f.reject_write <- false;
  let host = restore () in
  (match observe host "committed-result" with
  | Confirmed _ -> ()
  | Unavailable why | Unconfirmed why ->
      failwith ("observation result not committed: " ^ why));
  let projected =
    Coordinator_state.scheduling_view (Coordinator_host.state host) view
  in
  check
    (Patch_agent.forge_revision_pair_confirmed (List.hd projected.agents))
    "durable evidence not used for policy";
  let pending =
    match
      Coordinator_inputs.pending
        (Coordinator_host.state host)
        patch ~selected_base:"main"
    with
    | Ok (Some i) -> i
    | Ok None -> failwith "versioned worker evidence absent"
    | Error why -> failwith why
  in
  check
    (pending.evidence <> [] && pending.version = 1L)
    "worker evidence not versioned";
  check
    (Patch_agent.forge_revision_pair_confirmed
       (Worker_protocol_v3.apply_input agent pending))
    "worker owner did not adopt evidence";
  let reads = !provider_reads in
  ignore (observe (restore ()) "committed-result");
  check (!provider_reads = reads)
    "same immutable observation ticket reread provider";
  mutations := [];
  let change host ~id ~now action =
    Coordinator_forge.change
      (module Fake_forge)
      ~host ~io ~scope ~patch ~effect_id:id ~view ~now action
  in
  check
    (change host ~id:"merge" ~now:0. Merge
    = Change_pending "current shared scheduler policy/idle window not ready")
    "merge skipped durable idle window";
  check (!mutations = []) "merge before idle window";
  check
    (match change host ~id:"merge" ~now:119. Merge with
    | Change_pending _ -> true
    | _ -> false)
    "open idle boundary";
  let host = restore () in
  (match change host ~id:"merge" ~now:120. Merge with
  | Applied -> ()
  | Change_pending why | Change_blocked why ->
      let current =
        List.hd
          (Coordinator_state.scheduling_view (Coordinator_host.state host) view)
            .agents
      in
      failwith
        (Printf.sprintf
           "closed idle boundary/restart: %s candidate=%b deadline=%s queue=%d \
            proof=%b"
           why
           (Scheduler_decision.automerge_candidate current ~main:view.main)
           (match current.automerge_deadline with
           | None -> "none"
           | Some f -> string_of_float f)
           (List.length current.queue)
           (Patch_agent.forge_revision_pair_confirmed current)));
  check (!mutations = [ "merge" ]) "merge count";
  check
    (change (restore ()) ~id:"merge" ~now:121. Merge = Applied)
    "committed effect result not retained";
  check (!mutations = [ "merge" ]) "successful effect replayed";
  provider_state := Some pr;
  check
    (change host ~id:"body-ok" ~now:122. (Set_body "notes") = Applied)
    "body result not committed";
  check
    (change (restore ()) ~id:"body-ok" ~now:123. (Set_body "notes") = Applied)
    "body success lost at restart";
  check (List.length !mutations = 2) "body success replayed";
  check
    (match change host ~id:"body-ok" ~now:124. (Set_body "changed") with
    | Change_blocked _ -> true
    | _ -> false)
    "changed immutable effect accepted";
  lose_effect := true;
  check
    (match change host ~id:"body-lost" ~now:125. (Set_body "unknown") with
    | Change_pending _ -> true
    | _ -> false)
    "ambiguous body effect not pending";
  let count = List.length !mutations in
  ignore (change (restore ()) ~id:"body-lost" ~now:126. (Set_body "unknown"));
  ignore (change (restore ()) ~id:"body-lost" ~now:127. (Set_body "unknown"));
  check
    (match
       Coordinator_journal.find
         (Coordinator_state.journal (Coordinator_host.state (restore ())))
         "effect:body-lost"
     with
    | Some { phase = Attention; _ } -> true
    | _ -> false)
    "ambiguous effect never reaches durable attention";
  check
    (match
       change (restore ()) ~id:"new-body" ~now:128. (Set_body "unknown")
     with
    | Change_blocked _ -> true
    | _ -> false)
    "new ID bypassed unresolved effect";
  check (List.length !mutations = count) "ambiguous body resubmitted";
  lose_effect := false;
  provider_state := Some { pr with check_status = Failing; merge_ready = false };
  let host = restore () in
  ignore (observe host "failing");
  let projected =
    Coordinator_state.scheduling_view (Coordinator_host.state host) view
  in
  check
    (not (List.hd projected.agents).checks_passing)
    "current failing checks ignored";
  provider_state := Some pr;
  provider_state := None;
  for n = 1 to 3 do
    ignore (observe (restore ()) ("unavailable-" ^ string_of_int n))
  done;
  let host = restore () in
  check
    (List.exists
       (fun (op : Coordinator_state.operation) -> op.attention)
       (Coordinator_state.operations (Coordinator_host.state host)))
    "unavailable forge observations wait forever";
  check
    (String.equal
       (applied host "bump-after-wait" "bump" (`String "patch") "")
       "accepted")
    "bump not accepted";
  let projected =
    Coordinator_state.scheduling_view (Coordinator_host.state host) view
  in
  check
    (Patch_agent.needs_intervention (List.hd projected.agents))
    "bump renewed the coordinator observation allowance";
  check
    (String.equal
       (applied host "renew-wait" "renewal" (`String "patch") "work")
       "accepted")
    "matching explicit observation renewal rejected";
  check
    (not
       (List.exists
          (fun (op : Coordinator_state.operation) -> op.attention)
          (Coordinator_state.operations (Coordinator_host.state host))))
    "renewal did not release attention";
  print_endline
    "forge: result commit barrier, durable scheduling/evidence, idle-window \
     restart and applied/ambiguous effect settlement"

let () =
  test_committed_evidence_and_effects ();
  test_control_store ();
  test_guidance_atomic ();
  test_stop_request_is_not_container ();
  test_ambiguous_create ();
  test_scope ();
  print_endline
    "coordinator controls, bounded scope and direct remote probes: OK"

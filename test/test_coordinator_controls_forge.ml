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
  let pr_state _ = Error "unavailable"
  let merge_queue_removal_checks ~pr_number:_ = Error "unused"
  let check_failure_details ~check:_ = Error "unused"
  let rerun_failed_jobs_for_check ~check:_ = Error "unused"
  let list_prs ~branch:_ ?base:_ ~state:_ () = Ok []
  let update_pr_body ~pr_number:_ ~body:_ = Ok ()

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
  let merge_pr ~pr_number:_ = Error "unused"
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
      head_revision = "aaa";
      base_revision = "bbb";
      push_destination = "push";
    }
  in
  let io : Branch_reconcile_executor.io =
    { git = (fun _ -> (2, "", "missing")) }
  in
  let call host scope =
    Coordinator_forge.create_or_discover
      (module Fake_forge)
      ~host ~io ~scope ~patch ~effect_id:"create-1" ~renewal_id:None
      ~title:"title" ~body:"body" ~draft:true
  in
  creates := 0;
  let host = restore () in
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

let () =
  test_control_store ();
  test_guidance_atomic ();
  test_stop_request_is_not_container ();
  test_ambiguous_create ();
  test_scope ();
  print_endline
    "coordinator controls, bounded scope and direct remote probes: OK"

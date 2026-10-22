(* @archlint.module test
   @archlint.domain worker-checkpoint *)

open Onton
open Onton_core
open Onton_test_support.Branch_reconcile_git_fixture

let check label condition = if not condition then failwith label

let () =
  Eio_main.run @@ fun _ ->
  let runtime = make_runtime () in
  let before = Runtime.read runtime (fun snap -> snap.Runtime.orchestrator) in
  let entered, signal_entered = Eio.Promise.create () in
  let ack, signal_ack = Eio.Promise.create () in
  let finished = ref false in
  Eio.Fiber.both
    (fun () ->
      let result =
        Runtime.update_persisting runtime
          ~persist:(fun _ ->
            Eio.Promise.resolve signal_entered ();
            Eio.Promise.await ack;
            Ok ())
          (fun snap ->
            let orchestrator =
              Orchestrator.set_worktree_path snap.Runtime.orchestrator
                (Types.Patch_id.of_string "1")
                "/acknowledged"
            in
            ({ snap with Runtime.orchestrator }, ()))
      in
      check "accepted checkpoint" (Result.is_ok result);
      finished := true)
    (fun () ->
      Eio.Promise.await entered;
      check "candidate not published before ACK" (not !finished);
      (* The ACK path uses its own mailbox. It must not call Runtime.read here:
         update_persisting intentionally holds the runtime mutex. *)
      Eio.Promise.resolve signal_ack ());
  let after = Runtime.read runtime (fun snap -> snap.Runtime.orchestrator) in
  check "ACK publishes candidate" (not (before == after));
  let rejected =
    Runtime.update_persisting runtime
      ~persist:(fun _ -> Error "rejected")
      (fun snap -> ({ snap with Runtime.orchestrator = before }, ()))
  in
  check "rejected checkpoint" (Result.is_error rejected);
  check "rejection retains prior memory"
    (Runtime.read runtime (fun snap -> snap.Runtime.orchestrator == after));
  print_endline "execution checkpoint barrier: acknowledged, rejected, delayed";
  Eio_main.run @@ fun env ->
  let runtime = make_runtime () in
  let calls = ref 0 in
  let module W = struct
    let reject () =
      incr calls;
      failwith "worktree effect before checkpoint"

    let resolve_main_root () = reject ()
    let is_checked_out_in_repo_root _ = reject ()
    let remote_branch_exists _ = reject ()
    let create ~project_name:_ ~patch_id:_ ~branch:_ ~base_ref:_ = reject ()
    let remove ~discard:_ _ = reject ()
    let detect_branch ~path:_ = reject ()
    let list_with_branches () = reject ()
    let find_for_branch _ = reject ()
    let prune_stale_for_branch _ = reject ()
    let ensure_ready ~path:_ ~branch:_ = reject ()
    let inspect_existing ~path:_ ~branch:_ = reject ()
    let run_hook ~clock:_ ~script:_ ~cwd:_ ~env:_ () = reject ()
    let fetch_origin ~fetch_lock:_ ~path:_ = reject ()
    let fetch_origin_branch ~fetch_lock:_ ~branch:_ = reject ()
    let git_status ~path:_ = reject ()
    let has_uncommitted_changes ~path:_ = reject ()
    let conflict_diff ~path:_ = reject ()
    let read_branch_sha ~path:_ ~ref_name:_ = reject ()
    let is_ancestor ~path:_ ~ancestor:_ ~descendant:_ = reject ()
    let commit_gameplan ~path:_ ~publication:_ ~message:_ = reject ()
    let materialization ~path:_ ~project_name:_ ~branch:_ = reject ()
    let reconcile ~path:_ ~project_name:_ ~branch:_ ~operation:_ _ = reject ()
  end in
  let module Env = struct
    let runtime = runtime
    let clock = Eio.Stdenv.clock env
    let fs = Eio.Stdenv.fs env
    let worktree_mutex = Eio.Mutex.create ()
    let hook_mutex = Eio.Mutex.create ()
    let fetch_mutex = Eio.Mutex.create ()
    let project_name = "checkpoint-test"
    let user_config = { User_config.on_worktree_create = None }
  end in
  let module C = struct
    let persist _ = Error "host rejected"
    let record_session_id ~patch_id:_ ~session_id:_ = Error "host rejected"
  end in
  let module WS = Worktree_setup.Make_with_checkpoint (C) (W) (Env) in
  let refused =
    try
      Runtime.with_patch_ownership runtime ~patch_id:id (fun owner ->
          ignore (WS.ensure_owned ~owner ());
          false)
    with _ -> true
  in
  check "strict setup refuses failed checkpoint" refused;
  check "strict setup stops before worktree effects" (!calls = 0);
  print_endline "strict setup: rejected before worktree effect"

(* @archlint.module test
   @archlint.domain orchestrator *)

open Base
open Onton
open Onton_core

(** Missing checkout retains the publication obligation and releases the worker
    without charging implementation-session budgets. *)

let assert_eq label want got =
  if not (String.equal want got) then
    failwith (Printf.sprintf "%s: expected %S got %S" label want got)

let assert_true label cond =
  if not cond then failwith (Printf.sprintf "%s: assertion failed" label)

let nonexistent_path () =
  Stdlib.Filename.concat
    (Stdlib.Filename.get_temp_dir_name ())
    (Printf.sprintf "onton-worktree-missing-%d-%d" (Unix.getpid ())
       (Random.bits ()))

let () =
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in

  let path = nonexistent_path () in
  assert_true "precondition: path does not exist"
    (not (Stdlib.Sys.file_exists path));
  let module B = Branch_reconcile in
  let state, _ =
    B.step B.empty
      (B.Request
         {
           base = "main";
           policy = Rewrite;
           purpose = Publish_session "missing";
         })
  in
  let command = Option.value_exn (B.pending state) in
  let operation = Option.value_exn (B.operation state) in
  let io = Branch_reconcile_executor.make_io ~clock ~process_mgr ~path in
  let result =
    Branch_reconcile_executor.execute ~io ~prefix:"refs/onton/reconcile/missing"
      ~branch:"feat" ~operation command
  in
  let waiting, _ =
    B.step state (B.Result { token = command.token; at = 100.; result })
  in
  assert_true "missing checkout keeps publication pending"
    (B.is_pending waiting);
  assert_true "missing checkout cannot acknowledge publication"
    (List.is_empty (B.publications waiting));
  assert_true "missing checkout checkpoint survives restart"
    (match B.decode (B.yojson_of_t waiting) with
    | Ok restored -> B.equal restored waiting
    | Error _ -> false);
  Stdlib.print_endline
    "test1: missing checkout leaves durable publication pending";

  let main = Types.Branch.of_string "main" in
  let pid = Types.Patch_id.of_string "wm-pid" in
  let patches =
    [
      Types.Patch.
        {
          id = pid;
          title = "P";
          description = "";
          branch = Types.Branch.of_string "wm";
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
    ]
  in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  (* Drive the agent to busy without a PR (start path). *)
  let orch = Orchestrator.fire orch (Orchestrator.Start (pid, main)) in
  let orch = Orchestrator.set_worktree_path orch pid "/tmp/deleted-worktree" in
  let agent_before = Orchestrator.agent orch pid in
  assert_true "agent_before busy" agent_before.Patch_agent.busy;
  assert_true "agent_before materialized"
    (Patch_agent.equal_worktree_state
       (Patch_agent.worktree_state agent_before)
       (Patch_agent.Materialized "/tmp/deleted-worktree"));
  let attempts_before = agent_before.Patch_agent.start_attempts_without_pr in
  let orch, _ =
    Orchestrator.reconcile_branch orch pid
      (B.Request
         {
           base = "main";
           policy = Rewrite;
           purpose = Publish_session "missing";
         })
  in
  let pending = (Orchestrator.agent orch pid).Patch_agent.branch_reconcile in
  let token = (Option.value_exn (B.pending pending)).B.token in
  let orch, _ =
    Orchestrator.reconcile_branch orch pid
      (B.Result { token; at = 100.; result })
  in
  let orch =
    Orchestrator.apply_start_outcome orch pid Orchestrator.Start_failed
  in
  let agent_after = Orchestrator.agent orch pid in
  assert_true "missing checkout releases worker"
    (not agent_after.Patch_agent.busy);
  assert_true "missing checkout retains the path for inspection"
    (Option.equal String.equal agent_after.worktree_path
       agent_before.worktree_path);
  assert_true "missing checkout does not consume implementation attempts"
    (Int.equal agent_after.start_attempts_without_pr attempts_before);
  assert_true "missing checkout preserves session fallback"
    (Patch_agent.equal_session_fallback agent_before.session_fallback
       agent_after.session_fallback);
  assert_true "missing checkout has durable pending work"
    (B.is_pending agent_after.branch_reconcile);
  assert_true "missing checkout does not acknowledge publication"
    (List.is_empty (B.publications agent_after.branch_reconcile));
  Stdlib.print_endline
    "test2: missing checkout retains owner work without spending session budget";

  assert_eq "all tests passed marker" "ok" "ok";
  Stdlib.print_endline "All worktree-missing recovery tests passed."

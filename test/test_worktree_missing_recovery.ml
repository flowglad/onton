open Base
open Onton
open Onton_core

(** End-to-end recovery pipeline for "worktree deleted between session start and
    post-session push". Drives:

    1. {!Worktree.force_push_with_lease} against a path that does not exist —
    must return [Push_worktree_missing] without spawning git. 2.
    {!Orchestrator.combine_session_and_push} with [Session_ok] folded against
    [Push_worktree_missing] — must collapse to [Session_worktree_missing] (not
    the misleading [Session_push_failed]). 3.
    {!Orchestrator.apply_session_result} given the combined outcome — must take
    the pre-session-failure cleanup branch so the next scheduling tick can
    rebuild the worktree from scratch via [Worktree_setup.ensure_worktree]. *)

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

  (* (1) Precheck path: missing directory -> Push_worktree_missing without
     spawning git. We use a path that has never existed, so any subsequent
     git invocation would fail with exit 128. *)
  let path = nonexistent_path () in
  assert_true "precondition: path does not exist"
    (not (Stdlib.Sys.file_exists path));
  let push_outcome =
    Worktree.force_push_with_lease ~process_mgr ~path
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
  in
  if
    not (Worktree.equal_push_result push_outcome Worktree.Push_worktree_missing)
  then
    failwith
      (Printf.sprintf
         "force_push_with_lease on nonexistent path returned %s, expected \
          Push_worktree_missing"
         (Worktree.show_push_result push_outcome));
  Stdlib.print_endline
    "test1: force_push_with_lease on missing path -> Push_worktree_missing";

  (* (2) Combine: Session_ok + Push_worktree_missing -> Session_worktree_missing.
     Even though the LLM session itself ran cleanly, the local commits are
     gone with the directory, so the combined outcome must route through
     the pre-session-failure path. *)
  let combined =
    Orchestrator.combine_session_and_push ~session:Orchestrator.Session_ok
      ~push:Worktree.Push_worktree_missing
  in
  if
    not
      (Orchestrator.equal_session_result combined
         Orchestrator.Session_worktree_missing)
  then
    failwith
      (Printf.sprintf "combine returned %s, expected Session_worktree_missing"
         (Orchestrator.show_session_result combined));
  Stdlib.print_endline
    "test2: combine Session_ok + Push_worktree_missing -> \
     Session_worktree_missing";

  (* (3) apply_session_result Session_worktree_missing -> agent.busy=false +
     start_attempts_without_pr incremented (the [on_pre_session_failure]
     bookkeeping that backs needs_intervention). The next scheduling tick
     will see the agent idle and re-enqueue work; the next session will
     reconstruct the worktree via [ensure_worktree]. *)
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
        };
    ]
  in
  let orch = Orchestrator.create ~patches ~main_branch:main in
  (* Drive the agent to busy without a PR (start path). *)
  let orch = Orchestrator.fire orch (Orchestrator.Start (pid, main)) in
  let agent_before = Orchestrator.agent orch pid in
  assert_true "agent_before busy" agent_before.Patch_agent.busy;
  let attempts_before = agent_before.Patch_agent.start_attempts_without_pr in
  let orch =
    Orchestrator.apply_session_result orch pid
      Orchestrator.Session_worktree_missing
  in
  let agent_after = Orchestrator.agent orch pid in
  assert_true "agent_after not busy" (not agent_after.Patch_agent.busy);
  let attempts_after = agent_after.Patch_agent.start_attempts_without_pr in
  if not (Int.equal attempts_after (attempts_before + 1)) then
    failwith
      (Printf.sprintf
         "start_attempts_without_pr should increment by 1: before=%d after=%d"
         attempts_before attempts_after);
  Stdlib.print_endline
    "test3: apply_session_result Session_worktree_missing -> agent idle, \
     start_attempts incremented (next tick will rebuild)";

  assert_eq "all tests passed marker" "ok" "ok";
  Stdlib.print_endline "All worktree-missing recovery tests passed."

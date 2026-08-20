(* @archlint.module test
   @archlint.domain adhoc-branch *)

open Onton
open Onton_core
open Onton_core.Types

let empty_gameplan : Gameplan.t =
  Gameplan.
    {
      project_name = "adhoc-branch-test";
      repo_owner = "alice";
      repo_name = "demo";
      problem_statement = "";
      solution_summary = "";
      final_state_spec = "";
      patches = [];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
      functional_changes = [];
      context_resources = [];
      reachability_traces = [];
    }

module Fake_forge = struct
  type error = string

  let name = "Fake reviewless forge"
  let show_error error = error
  let supports_reviews = false
  let requested = ref []

  let list_prs ~branch ?base ~state:_ () =
    requested := (branch, base) :: !requested;
    let base = Option.value base ~default:(Branch.of_string "main") in
    Ok [ (Pr_number.of_int 77, base, false) ]
end

module Fake_worktree = struct
  let result = ref Worktree.Fetch_branch_ok
  let fetched = ref []

  let fetch_origin_branch ~fetch_lock:_ ~branch =
    fetched := branch :: !fetched;
    !result
end

module Add_branch = Adhoc_branch.Make (Fake_forge) (Fake_worktree)

let fresh_runtime () =
  Runtime.create ~gameplan:empty_gameplan ~main_branch:(Branch.of_string "main")
    ()

let test_add_and_idempotence () =
  Fake_forge.requested := [];
  Fake_worktree.fetched := [];
  Fake_worktree.result := Worktree.Fetch_branch_ok;
  let runtime = fresh_runtime () in
  let registered = ref [] in
  let register_change ~patch_id ~pr_number =
    registered := (patch_id, pr_number) :: !registered
  in
  let branch = Branch.of_string "feature/login" in
  let first =
    Add_branch.add ~runtime ~fetch_mutex:(Eio.Mutex.create ()) ~register_change
      ~branch
  in
  let patch_id =
    Adhoc_target.branch_patch_id ~change_id:(Pr_number.of_int 77)
  in
  assert (
    Adhoc_branch.(first = Added { patch_id; change_id = Pr_number.of_int 77 }));
  assert (
    Stdlib.List.equal String.equal !Fake_worktree.fetched [ "feature/login" ]);
  assert (Stdlib.List.length !Fake_forge.requested = 1);
  assert (Stdlib.List.length !registered = 1);
  let agent =
    Runtime.read runtime (fun snapshot ->
        Orchestrator.find_agent snapshot.Runtime.orchestrator patch_id)
  in
  assert (
    Option.exists
      (fun (agent : Patch_agent.t) ->
        Branch.equal agent.Patch_agent.branch branch)
      agent);
  let second =
    Add_branch.add ~runtime ~fetch_mutex:(Eio.Mutex.create ()) ~register_change
      ~branch
  in
  assert (Adhoc_branch.(second = Already_registered patch_id));
  assert (Stdlib.List.length !Fake_worktree.fetched = 1)

let test_missing_remote_is_not_registered () =
  Fake_worktree.fetched := [];
  Fake_worktree.result := Worktree.Fetch_branch_no_remote_ref;
  let runtime = fresh_runtime () in
  let branch = Branch.of_string "missing" in
  let outcome =
    Add_branch.add ~runtime ~fetch_mutex:(Eio.Mutex.create ())
      ~register_change:(fun ~patch_id:_ ~pr_number:_ -> assert false)
      ~branch
  in
  assert (Adhoc_branch.(outcome = Remote_not_found));
  let agents =
    Runtime.read runtime (fun snapshot ->
        Orchestrator.all_agents snapshot.Runtime.orchestrator)
  in
  assert (Stdlib.List.is_empty agents)

let () =
  Eio_main.run @@ fun _env ->
  test_add_and_idempotence ();
  test_missing_remote_is_not_registered ();
  print_endline "test_adhoc_branch: OK"

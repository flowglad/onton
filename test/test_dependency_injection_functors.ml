open Onton
open Onton_core
open Onton_core.Types

(** Patch 1 compile-time signature check.

    Verifies that [Worktree_setup.Make(W)(Env)] exposes [ensure_worktree] with
    only patch-specific inputs after the stable provisioning environment is
    captured as a functor argument. The type annotation on [_check_narrowed]
    below is the authoritative assertion: if the functor adds or retains stable
    parameters on [ensure_worktree], this file will not compile. *)

module Fake_worktree : Worktree.S = struct
  let resolve_main_root () = assert false
  let is_checked_out_in_repo_root _ = assert false
  let remote_branch_exists _ = assert false
  let create ~project_name:_ ~patch_id:_ ~branch:_ ~base_ref:_ = assert false
  let remove _ = assert false
  let detect_branch ~path:_ = assert false
  let list_with_branches () = assert false
  let find_for_branch _ = assert false
  let prune_admin () = assert false
  let run_hook ~clock:_ ~script:_ ~cwd:_ ~env:_ () = assert false
  let fetch_origin ~fetch_lock:_ ~path:_ = assert false
  let git_status ~path:_ = assert false
  let conflict_diff ~path:_ = assert false

  let rebase_onto ~path:_ ~target:_ ~project_name:_ ~ancestor_ids:_ =
    assert false

  let read_in_progress_conflict_info ~path:_ ~target:_ ~project_name:_
      ~ancestor_ids:_ =
    assert false

  let force_push_with_lease ~path:_ ~branch:_ ~base:_ = assert false
  let rebase_in_progress ~path:_ = assert false
end

module Fake_env : Worktree_setup.ENV = struct
  let runtime =
    Runtime.create
      ~gameplan:
        {
          Gameplan.project_name = "";
          repo_owner = "";
          repo_name = "";
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
        }
      ~main_branch:(Branch.of_string "main") ()

  (* Eio resources cannot be created outside Eio_main.run; they are never
     dereferenced because Worktree_setup.Make defines functions only. *)
  let clock : float Eio.Time.clock_ty Eio.Time.clock = Obj.magic ()
  let fs : Eio.Fs.dir_ty Eio.Path.t = Obj.magic ()
  let worktree_mutex : Eio.Mutex.t = Obj.magic ()
  let hook_mutex : Eio.Mutex.t = Obj.magic ()
  let project_name = ""
  let user_config = { User_config.on_worktree_create = None }
end

module WS = Worktree_setup.Make (Fake_worktree) (Fake_env)

(* Compile-time assertion: ensure_worktree accepts only patch-specific inputs.
   Runtime, clock, fs, project_name, user_config, and mutexes are gone from
   the call surface — they live in Env now. *)
let _check_narrowed :
    patch_id:Patch_id.t ->
    agent:Patch_agent.t ->
    ?branch:Branch.t ->
    ?base_ref:string ->
    unit ->
    string option =
  WS.ensure_worktree

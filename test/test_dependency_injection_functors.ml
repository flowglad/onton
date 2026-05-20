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
  (* Runtime.create is Eio-free: test_runtime_create.ml verifies it can be
     called outside Eio_main.run without raising an Unhandled effect. *)
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

  (* Eio resources require the scheduler; they are never dereferenced here
     because Worktree_setup.Make defines functions only. *)
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

(** Patch 2 compile-time signature check.

    Verifies that [Session_driver.Make(W)(Env)] exposes [run] and
    [run_long_lived] with only per-session inputs after the stable session
    environment is captured as a functor argument. *)

module Fake_sd_env : Session_driver.ENV = struct
  let runtime = Fake_env.runtime

  (* Eio resources cannot be created outside Eio_main.run. Obj.magic () is
     safe here because both Worktree_setup.Make and Session_driver.Make consist
     entirely of [let f = ...] definitions with no top-level side-effects, so
     these values are never dereferenced at functor-application time. If either
     Make body gains a top-level expression that dereferences Env.clock or
     Env.fs, this test will segfault — that is the intended alarm. *)
  let clock : float Eio.Time.clock_ty Eio.Time.clock = Obj.magic ()
  let fs : Eio.Fs.dir_ty Eio.Path.t = Obj.magic ()
  let worktree_mutex : Eio.Mutex.t = Obj.magic ()
  let hook_mutex : Eio.Mutex.t = Obj.magic ()
  let project_name = ""
  let owner = ""
  let repo = ""
  let transcripts = Stdlib.Hashtbl.create 0
  let user_config = { User_config.on_worktree_create = None }
end

module SD = Session_driver.Make (Fake_worktree) (Fake_sd_env)

(* Compile-time assertion: run accepts only per-session inputs.
   Runtime, clock, fs, project_name, owner, repo, transcripts, user_config,
   and mutexes are gone from the call surface — they live in Fake_sd_env now. *)
let _check_narrowed_run :
    kind:Operation_kind.t option ->
    patch_id:Patch_id.t ->
    prompt:string ->
    agent:Patch_agent.t ->
    on_pr_detected:(Pr_number.t -> unit) ->
    backend:Llm_backend.t ->
    complexity:int option ->
    [ `Ok | `Failed | `Retry_push ] * (string * string) list =
  SD.run

(* Compile-time assertion: run_long_lived accepts only per-session inputs. *)
let _check_narrowed_run_long_lived :
    sw:Eio.Switch.t ->
    kind:Operation_kind.t option ->
    patch_id:Patch_id.t ->
    prompt:string ->
    agent:Patch_agent.t ->
    on_pr_detected:(Pr_number.t -> unit) ->
    session:SD.long_lived_session ->
    complexity:int option ->
    [ `Ok | `Failed | `Retry_push ] * (string * string) list =
  SD.run_long_lived

let () =
  print_endline
    "Patch 1: Worktree_setup.Make(W)(Env).ensure_worktree narrowed signature: \
     OK";
  print_endline
    "Patch 2: Session_driver.Make(W)(Env).run narrowed signature: OK";
  print_endline
    "Patch 2: Session_driver.Make(W)(Env).run_long_lived narrowed signature: OK"

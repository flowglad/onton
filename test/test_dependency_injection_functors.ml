(* @archlint.module test
   @archlint.domain session-meta *)

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

  let fetch_origin_branch ~fetch_lock:_ ~branch:_ : Worktree.fetch_branch_result
      =
    assert false

  let git_status ~path:_ = assert false
  let conflict_diff ~path:_ = assert false

  let rebase_onto ~path:_ ~target:_ ~upstream:_ ~project_name:_ ~ancestor_ids:_
      () =
    assert false

  let read_branch_sha ~path:_ ~ref_name:_ = None
  let is_ancestor ~path:_ ~ancestor:_ ~descendant:_ = false

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
          reachability_traces = [];
        }
      ~main_branch:(Branch.of_string "main") ()

  (* Eio resources require the scheduler; they are never dereferenced here
     because Worktree_setup.Make defines functions only. *)
  let clock : float Eio.Time.clock_ty Eio.Time.clock = Obj.magic ()
  let fs : Eio.Fs.dir_ty Eio.Path.t = Obj.magic ()
  let worktree_mutex : Eio.Mutex.t = Obj.magic ()
  let hook_mutex : Eio.Mutex.t = Obj.magic ()
  let fetch_mutex : Eio.Mutex.t = Obj.magic ()
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
    Worktree_setup.ensure_result =
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
  let fetch_mutex : Eio.Mutex.t = Obj.magic ()
  let project_name = ""
  let owner = ""
  let repo = ""
  let transcripts = Stdlib.Hashtbl.create 0
  let user_config = { User_config.on_worktree_create = None }
  let event_log = Event_log.create ~path:"/dev/null"
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
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let fs = Eio.Stdenv.fs env in
  let gameplan =
    {
      Gameplan.project_name = "test";
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
      reachability_traces = [];
    }
  in
  let runtime =
    Runtime.create ~gameplan ~main_branch:(Branch.of_string "main") ()
  in
  let module Env : Worktree_setup.ENV = struct
    let runtime = runtime
    let (clock : float Eio.Time.clock_ty Eio.Time.clock) = clock
    let fs = fs
    let project_name = "test"
    let user_config = { User_config.on_worktree_create = None }
    let worktree_mutex = Eio.Mutex.create ()
    let hook_mutex = Eio.Mutex.create ()
    let fetch_mutex = Eio.Mutex.create ()
  end in
  let module WS = Worktree_setup.Make (Fake_worktree) (Env) in
  (* Compile-time assertion: ensure_worktree accepts only patch-specific inputs.
     Runtime, clock, fs, project_name, user_config, and mutexes are gone from
     the call surface — they live in Env now. *)
  let check_narrowed :
      patch_id:Patch_id.t ->
      agent:Patch_agent.t ->
      ?branch:Branch.t ->
      ?base_ref:string ->
      unit ->
      Worktree_setup.ensure_result =
    WS.ensure_worktree
  in
  ignore check_narrowed;
  print_endline
    "Patch 1: Worktree_setup.Make(W)(Env).ensure_worktree narrowed signature: \
     OK";
  (* Patch 2: Session_driver.Make(W)(Env) signature checks. *)
  let transcripts : (Patch_id.t, string) Stdlib.Hashtbl.t =
    Stdlib.Hashtbl.create 0
  in
  let module SD_Env : Session_driver.ENV = struct
    let runtime = runtime
    let (clock : float Eio.Time.clock_ty Eio.Time.clock) = clock
    let fs = fs
    let project_name = "test"
    let owner = "test-owner"
    let repo = "test-repo"
    let transcripts = transcripts
    let user_config = { User_config.on_worktree_create = None }
    let worktree_mutex = Eio.Mutex.create ()
    let hook_mutex = Eio.Mutex.create ()
    let fetch_mutex = Eio.Mutex.create ()
    let event_log = Event_log.create ~path:"/dev/null"
  end in
  let module SD = Session_driver.Make (Fake_worktree) (SD_Env) in
  (* Compile-time assertion: run accepts only per-session inputs.
     Runtime, clock, fs, project_name, owner, repo, transcripts, user_config,
     and mutexes are gone from the call surface — they live in SD_Env now. *)
  let check_narrowed_run :
      kind:Operation_kind.t option ->
      patch_id:Patch_id.t ->
      prompt:string ->
      agent:Patch_agent.t ->
      on_pr_detected:(Pr_number.t -> unit) ->
      backend:Llm_backend.t ->
      complexity:int option ->
      [ `Ok | `Failed | `Retry_push ] * (string * string) list =
    SD.run
  in
  ignore check_narrowed_run;
  print_endline
    "Patch 2: Session_driver.Make(W)(Env).run narrowed signature: OK";
  print_endline
    "Patch 2: Session_driver.Make(W)(Env).run_long_lived narrowed signature: OK";
  (* Patch 3: Make_fibers environment derivation.
     Both Worktree_setup.ENV and Session_driver.ENV are derived from a single
     fiber-level environment, mirroring what Make_fibers(Forge)(W)(Fiber_env)
     does in bin/main.ml.  The compile-time constraint is: given one struct of
     run-level values, both sub-envs type-check without any extra parameters. *)
  let module Fake_fiber_env = struct
    let runtime = runtime
    let (clock : float Eio.Time.clock_ty Eio.Time.clock) = clock
    let fs = fs
    let project_name = "test"
    let owner = "test-owner"
    let repo = "test-repo"
    let user_config = { User_config.on_worktree_create = None }
    let worktree_mutex = Eio.Mutex.create ()
    let hook_mutex = Eio.Mutex.create ()
    let fetch_mutex = Eio.Mutex.create ()
    let transcripts = transcripts
    let event_log = Event_log.create ~path:"/dev/null"
  end in
  let module WS3_Env : Worktree_setup.ENV = struct
    let runtime = Fake_fiber_env.runtime
    let clock = Fake_fiber_env.clock
    let fs = Fake_fiber_env.fs
    let project_name = Fake_fiber_env.project_name
    let user_config = Fake_fiber_env.user_config
    let worktree_mutex = Fake_fiber_env.worktree_mutex
    let hook_mutex = Fake_fiber_env.hook_mutex
    let fetch_mutex = Fake_fiber_env.fetch_mutex
  end in
  let module SD3_Env : Session_driver.ENV = struct
    let runtime = Fake_fiber_env.runtime
    let clock = Fake_fiber_env.clock
    let fs = Fake_fiber_env.fs
    let project_name = Fake_fiber_env.project_name
    let owner = Fake_fiber_env.owner
    let repo = Fake_fiber_env.repo
    let transcripts = Fake_fiber_env.transcripts
    let user_config = Fake_fiber_env.user_config
    let worktree_mutex = Fake_fiber_env.worktree_mutex
    let hook_mutex = Fake_fiber_env.hook_mutex
    let fetch_mutex = Fake_fiber_env.fetch_mutex
    let event_log = Fake_fiber_env.event_log
  end in
  let module WS3 = Worktree_setup.Make (Fake_worktree) (WS3_Env) in
  let module SD3 = Session_driver.Make (Fake_worktree) (SD3_Env) in
  ignore
    (WS3.ensure_worktree
      : patch_id:_ -> agent:_ -> ?branch:_ -> ?base_ref:_ -> unit -> _);
  ignore
    (SD3.run
      : kind:_ ->
        patch_id:_ ->
        prompt:_ ->
        agent:_ ->
        on_pr_detected:_ ->
        backend:_ ->
        complexity:_ ->
        _);
  ignore
    (SD3.run_long_lived
      : sw:_ ->
        kind:_ ->
        patch_id:_ ->
        prompt:_ ->
        agent:_ ->
        on_pr_detected:_ ->
        session:_ ->
        complexity:_ ->
        _);
  print_endline
    "Patch 3: Make_fibers env derivation (WS + SD from shared fiber env): OK"

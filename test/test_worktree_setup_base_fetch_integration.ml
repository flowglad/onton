(* @archlint.module test
   @archlint.domain orchestrator *)

open Base
open Onton
open Onton_core
module Git_env = Onton_test_support.Git_env

(** Integration test: drive the real [Worktree_setup.ensure_worktree] against
    real git fixtures to verify which ref a brand-new branch is cut from
    ({!Start_point_plan.base_start_point} wiring).

    The production bug this locks out (connector-adapter-shape-unification patch
    4 / PR #3809): the orchestrator never advances the managed clone's local
    main ref, so a Start that fired right after a dependency's squash-merge cut
    the new branch from yesterday's main — missing the dependency's commits and
    forcing an immediate freshen rebase that then conflicted. [ensure_worktree]
    must fetch [origin/<main>] and cut from its resolved SHA.

    Scenarios:

    - "stale local main" — origin's main advances after the clone; local [main]
      and the remote-tracking ref both lag. Cutting with [base_ref = "main"]
      must land on origin's {e current} tip, not the clone-time tip. (Red before
      the base-fetch fix.)
    - "dep branch base is local-canonical" — the base is a dependency patch's
      branch whose local ref is ahead of [origin/<dep>] (the dep's worktree
      writes locally first; origin lags until push). The cut must use the local
      tip — never the remote.
    - "fetch failure falls back to local main" — origin is unreachable. The cut
      proceeds fail-open from the local main ref (pre-fix behavior; the
      freshen-rebase detectors remain the backstop).

    Every git command runs against the real binary; no mocks. *)

let sh ?(dir = ".") cmd = Git_env.sh ~dir cmd
let git_capture ?(dir = ".") args = Git_env.git_capture ~cwd:dir args

let with_temp_dir f =
  let dir =
    Stdlib.Filename.concat
      (Stdlib.Filename.get_temp_dir_name ())
      (Printf.sprintf "onton-base-fetch-%d-%d" (Unix.getpid ()) (Random.bits ()))
  in
  Unix.mkdir dir 0o755;
  (* Worktree paths derive from $HOME (see [Worktree.worktree_dir]); redirect
     HOME into the temp dir so the worktree lives inside our sandbox. Restored
     (and the sandbox wiped) before the next scenario runs. *)
  let prior_home = Stdlib.Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" dir;
  Stdlib.Fun.protect
    ~finally:(fun () ->
      (match prior_home with
      | Some h -> Unix.putenv "HOME" h
      | None -> Unix.putenv "HOME" (Stdlib.Filename.get_temp_dir_name ()));
      try
        Git_env.sh ~dir:"/"
          (Printf.sprintf "rm -rf %s" (Stdlib.Filename.quote dir))
      with _ -> ())
    (fun () -> f dir)

let setup_origin_with_main ~origin_dir =
  let seed_dir = origin_dir ^ ".seed" in
  Unix.mkdir seed_dir 0o755;
  sh ~dir:seed_dir "git init -q --initial-branch=main";
  sh ~dir:seed_dir "git config user.email 'test@example.com'";
  sh ~dir:seed_dir "git config user.name 'Test'";
  sh ~dir:seed_dir "echo base > README.md";
  sh ~dir:seed_dir "git add README.md";
  sh ~dir:seed_dir "git commit -q -m 'base'";
  sh
    (Printf.sprintf "git clone --bare -q %s %s"
       (Stdlib.Filename.quote seed_dir)
       (Stdlib.Filename.quote origin_dir))

let clone_into ~origin_dir ~managed_dir =
  sh
    (Printf.sprintf "git clone -q %s %s"
       (Stdlib.Filename.quote origin_dir)
       (Stdlib.Filename.quote managed_dir));
  sh ~dir:managed_dir "git config user.email 'test@example.com'";
  sh ~dir:managed_dir "git config user.name 'Test'"

let assert_string label want got =
  if not (String.equal want got) then
    failwith (Printf.sprintf "%s: expected %S got %S" label want got)

let mk_patch ~pid ~branch =
  Types.Patch.
    {
      id = pid;
      title = "P";
      description = "";
      branch = Types.Branch.of_string branch;
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
    }

let empty_gameplan =
  {
    Types.Gameplan.project_name = "";
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

(** Instantiate the real [Worktree_setup.Make] over a real git-backed [W] for
    [managed_dir], then run [ensure_worktree] for a brand-new [branch] cut from
    [base_ref]. Returns the worktree path. *)
let run_ensure env ~managed_dir ~project_name ~pid ~branch ~base_ref =
  let process_mgr = Eio.Stdenv.process_mgr env in
  let module W = (val Worktree.make ~process_mgr ~repo_root:managed_dir) in
  let patch = mk_patch ~pid ~branch in
  let gameplan = { empty_gameplan with Types.Gameplan.patches = [ patch ] } in
  let module Env : Worktree_setup.ENV = struct
    let runtime =
      Runtime.create ~gameplan ~main_branch:(Types.Branch.of_string "main") ()

    let clock = Eio.Stdenv.clock env
    let fs = Eio.Stdenv.fs env
    let worktree_mutex = Eio.Mutex.create ()
    let hook_mutex = Eio.Mutex.create ()
    let fetch_mutex = Eio.Mutex.create ()
    let project_name = project_name
    let user_config = { User_config.on_worktree_create = None }
  end in
  let module WS = Worktree_setup.Make (W) (Env) in
  let agent =
    Runtime.read Env.runtime (fun snap ->
        Orchestrator.agent snap.Runtime.orchestrator pid)
  in
  match
    WS.ensure_worktree ~patch_id:pid ~agent
      ~branch:(Types.Branch.of_string branch)
      ~base_ref ()
  with
  | Worktree_setup.Path p -> p
  | Worktree_setup.Missing -> failwith "ensure_worktree: Missing"
  | Worktree_setup.Refused -> failwith "ensure_worktree: Refused"

(** Origin's main advances after the clone; both the local [main] ref and the
    clone-time remote-tracking ref lag. The cut must land on origin's current
    tip. *)
let scenario_stale_local_main env =
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  let writer_dir = Stdlib.Filename.concat root "writer" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  clone_into ~origin_dir ~managed_dir:writer_dir;
  let clone_time_main = git_capture ~dir:managed_dir [ "rev-parse"; "main" ] in
  (* The dependency's squash-merge lands on origin main AFTER the clone. *)
  sh ~dir:writer_dir "echo dep-squash > dep.txt";
  sh ~dir:writer_dir "git add dep.txt";
  sh ~dir:writer_dir "git commit -q -m 'dep squash'";
  sh ~dir:writer_dir "git push -q origin main";
  let origin_tip = git_capture ~dir:writer_dir [ "rev-parse"; "main" ] in
  assert_string "precondition: local main is stale" clone_time_main
    (git_capture ~dir:managed_dir [ "rev-parse"; "main" ]);
  let wt =
    run_ensure env ~managed_dir ~project_name:"stale-main"
      ~pid:(Types.Patch_id.of_string "1")
      ~branch:"stale-main/patch-1" ~base_ref:"main"
  in
  let head = git_capture ~dir:wt [ "rev-parse"; "HEAD" ] in
  assert_string "stale_local_main: worktree HEAD == origin's current main tip"
    origin_tip head;
  Stdlib.print_endline "  stale_local_main: OK"

(** The base is a dependency patch's branch whose local ref is ahead of
    [origin/<dep>] — the cut must use the local tip (local-canonical), never the
    remote one. *)
let scenario_dep_base_local_canonical env =
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  let writer_dir = Stdlib.Filename.concat root "writer" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir:writer_dir;
  (* Origin has the dep branch at the base commit. *)
  sh ~dir:writer_dir "git checkout -q -b dep";
  sh ~dir:writer_dir "git push -q -u origin dep";
  clone_into ~origin_dir ~managed_dir;
  (* The dep's worktree advanced the local branch; origin lags (unpushed). *)
  sh ~dir:managed_dir "git checkout -q -b dep origin/dep";
  sh ~dir:managed_dir "echo dep-work > work.txt";
  sh ~dir:managed_dir "git add work.txt";
  sh ~dir:managed_dir "git commit -q -m 'dep work'";
  let local_dep_tip = git_capture ~dir:managed_dir [ "rev-parse"; "dep" ] in
  sh ~dir:managed_dir "git checkout -q main";
  let wt =
    run_ensure env ~managed_dir ~project_name:"dep-base"
      ~pid:(Types.Patch_id.of_string "2")
      ~branch:"dep-base/patch-2" ~base_ref:"dep"
  in
  let head = git_capture ~dir:wt [ "rev-parse"; "HEAD" ] in
  assert_string "dep_base: worktree HEAD == local dep tip (not origin/dep)"
    local_dep_tip head;
  Stdlib.print_endline "  dep_base_local_canonical: OK"

(** Origin is unreachable: the main-base fetch fails and the cut proceeds
    fail-open from the local main ref (the pre-fetch behavior, with the
    freshen-rebase detectors as backstop). *)
let scenario_fetch_failure_falls_back env =
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  let local_main = git_capture ~dir:managed_dir [ "rev-parse"; "main" ] in
  sh ~dir:managed_dir
    (Printf.sprintf "git remote set-url origin %s"
       (Stdlib.Filename.quote (Stdlib.Filename.concat root "gone")));
  let wt =
    run_ensure env ~managed_dir ~project_name:"fetch-fail"
      ~pid:(Types.Patch_id.of_string "3")
      ~branch:"fetch-fail/patch-3" ~base_ref:"main"
  in
  let head = git_capture ~dir:wt [ "rev-parse"; "HEAD" ] in
  assert_string "fetch_failure: worktree HEAD == local main (fail-open)"
    local_main head;
  Stdlib.print_endline "  fetch_failure_falls_back: OK"

let () =
  Eio_main.run @@ fun env ->
  Stdlib.print_endline "worktree_setup base-fetch integration:";
  scenario_stale_local_main env;
  scenario_dep_base_local_canonical env;
  scenario_fetch_failure_falls_back env;
  Stdlib.print_endline "all base-fetch integration scenarios passed"

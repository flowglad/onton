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
  let prior_data = Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" in
  Unix.putenv "HOME" dir;
  Unix.putenv "ONTON_DATA_DIR" (Stdlib.Filename.concat dir "state");
  Stdlib.Fun.protect
    ~finally:(fun () ->
      Unix.putenv "ONTON_DATA_DIR" (Option.value prior_data ~default:"");
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

let remote_head_sha ~dir ~ref_name =
  match
    git_capture ~dir [ "ls-remote"; "origin"; "refs/heads/" ^ ref_name ]
    |> String.lsplit2 ~on:'\t'
  with
  | Some (sha, _) when not (String.is_empty sha) -> Some sha
  | _ -> None

let wait_for_remote_head ~dir ~ref_name ~sha =
  let rec loop attempts_left =
    match remote_head_sha ~dir ~ref_name with
    | Some visible when String.equal visible sha -> visible
    | _ when attempts_left > 0 ->
        Unix.sleepf 0.05;
        loop (attempts_left - 1)
    | Some visible ->
        failwith
          (Printf.sprintf
             "remote %s did not advertise pushed tip: expected %S got %S"
             ref_name sha visible)
    | None ->
        failwith (Printf.sprintf "remote %s did not advertise any tip" ref_name)
  in
  loop 40

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
    architecture_design = None;
    solution_summary = "";
    final_state_spec = "";
    patches = [];
    operational_considerations = "";
    required_changes = "";
    ordering_constraints = [];
    current_state_analysis = "";
    explicit_opinions = "";
    acceptance_criteria = [];
    open_questions = [];
    functional_changes = [];
    context_resources = [];
    publication = None;
    reachability_traces = [];
  }

(** Instantiate the real [Worktree_setup.Make] over a real git-backed [W] for
    [managed_dir], then run [ensure_worktree] for a brand-new [branch] cut from
    [base_ref]. Returns the worktree path. *)
let run_ensure ?cancel_stage ?materialization_error ?(fail_checkpoint = false)
    env ~managed_dir ~project_name ~pid ~branch ~base_ref =
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let module Real =
    (val Worktree.make ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
           ~clock ~process_mgr ~repo_root:managed_dir)
  in
  let attempted = ref false and created = ref false in
  let check_hook = ref (fun () -> ()) in
  let hook_calls = ref 0 in
  let cancellation =
    let bt = Stdlib.Printexc.get_callstack 0 in
    Eio.Exn.Multiple
      [
        (Failure "cleanup failed", bt);
        (Eio.Exn.Multiple [ (Eio.Cancel.Cancelled (Failure "stop"), bt) ], bt);
      ]
  in
  let module W = struct
    include Real

    let materialization ~path ~project_name ~branch =
      match materialization_error with
      | Some reason -> Error reason
      | None -> Real.materialization ~path ~project_name ~branch

    let ensure_ready ~path ~branch =
      if
        Poly.equal cancel_stage (Some `Ready)
        || (Poly.equal cancel_stage (Some `Recovery) && !attempted)
      then raise cancellation;
      if !created then Error "transient re-inspection failure"
      else Real.ensure_ready ~path ~branch

    let find_for_branch branch =
      if Poly.equal cancel_stage (Some `Recovery) && !attempted then
        Some (Stdlib.Filename.concat managed_dir "recovery")
      else Real.find_for_branch branch

    let prune_stale_for_branch = Real.prune_stale_for_branch

    let run_hook ~clock ~script ~cwd ~env () =
      Int.incr hook_calls;
      !check_hook ();
      Real.run_hook ~clock ~script ~cwd ~env ()

    let create ~project_name ~patch_id ~branch ~base_ref =
      attempted := true;
      (match cancel_stage with
      | Some `Create -> raise cancellation
      | Some `Recovery -> failwith "creation interrupted"
      | Some `Ready | None -> ());
      let result = Real.create ~project_name ~patch_id ~branch ~base_ref in
      created := Result.is_ok result;
      result
  end in
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
    let user_config = { User_config.on_worktree_create = Some "true" }
  end in
  let check_materialization () =
    let checkpoint = Project_store.snapshot_path project_name in
    let persisted =
      match Persistence.load ~path:checkpoint with
      | Ok snapshot -> snapshot
      | Error message -> failwith message
    in
    let state =
      (Orchestrator.agent persisted.Runtime.orchestrator pid)
        .Patch_agent.branch_reconcile
    in
    match Branch_reconcile.materialization state with
    | Some receipt ->
        let actual =
          git_capture ~dir:managed_dir [ "rev-parse"; "refs/heads/" ^ branch ]
        in
        assert_string "materialization persisted before hook" actual
          (Branch_reconcile.Commit.to_string
             (Branch_reconcile.materialization_head receipt))
    | None -> failwith "materialization provenance missing"
  in
  check_hook := if fail_checkpoint then fun () -> () else check_materialization;
  let module WS = Worktree_setup.Make (W) (Env) in
  let agent =
    Runtime.read Env.runtime (fun snap ->
        Orchestrator.agent snap.Runtime.orchestrator pid)
  in
  let ensure () =
    WS.ensure_worktree ~patch_id:pid ~agent
      ~branch:(Types.Branch.of_string branch)
      ~base_ref ()
  in
  match materialization_error with
  | Some _ -> (
      match ensure () with
      | Worktree_setup.Unavailable (Worktree_provision.Unsafe _)
        when !hook_calls = 0 ->
          created := false;
          let result =
            Runtime.with_patch_ownership Env.runtime ~patch_id:pid (fun owner ->
                WS.ensure_owned ~owner ())
          in
          (match result with
          | Worktree_setup.Path _ ->
              failwith "refused provisioning reported success"
          | Worktree_setup.Unavailable _ -> ());
          if !hook_calls <> 0 then failwith "refused provisioning ran a hook";
          let after =
            Runtime.read Env.runtime (fun snap ->
                Orchestrator.agent snap.Runtime.orchestrator pid)
          in
          if
            not
              (Onton_core_test_support.Publication_fixture.is_diagnosis
                 after.Patch_agent.branch_reconcile)
          then failwith "unsafe provisioning did not request agent diagnosis";
          if
            (not (Poly.equal agent.session_fallback after.session_fallback))
            || agent.context_exhaustion_count <> after.context_exhaustion_count
            || agent.start_attempts_without_pr
               <> after.start_attempts_without_pr
          then failwith "unsafe provisioning consumed implementation budgets";
          "refused"
      | Worktree_setup.Unavailable
          (Worktree_provision.Temporary _ | Worktree_provision.Unsafe _)
      | Worktree_setup.Path _ ->
          failwith "unsafe materialization was not refused before hook")
  | None -> (
      if fail_checkpoint then (
        let checkpoint = Project_store.snapshot_path project_name in
        Project_store.ensure_dir (Stdlib.Filename.dirname checkpoint);
        Unix.mkdir checkpoint 0o700;
        (match ensure () with
        | Worktree_setup.Unavailable (Worktree_provision.Temporary _) -> ()
        | Worktree_setup.Path _
        | Worktree_setup.Unavailable (Worktree_provision.Unsafe _) ->
            failwith "failed checkpoint did not defer provisioning");
        if !hook_calls <> 0 then
          failwith "hook ran before the materialization checkpoint";
        if !attempted then
          failwith "failed hook-plan checkpoint allowed checkout creation";
        let state =
          Runtime.read Env.runtime (fun snap ->
              (Orchestrator.agent snap.Runtime.orchestrator pid)
                .Patch_agent.branch_reconcile)
        in
        if Option.is_some (Branch_reconcile.materialization state) then
          failwith "failed checkpoint was published in memory";
        Unix.rmdir checkpoint;
        check_hook := check_materialization;
        created := false);
      match ensure () with
      | Worktree_setup.Path p ->
          if fail_checkpoint then !check_hook ();
          let original = git_capture ~dir:p [ "rev-parse"; "HEAD" ] in
          let moved =
            git_capture ~dir:managed_dir
              [
                "commit-tree";
                original ^ "^{tree}";
                "-p";
                original;
                "-m";
                "base advanced after materialization";
              ]
          in
          let origin =
            git_capture ~dir:managed_dir [ "remote"; "get-url"; "origin" ]
          in
          if Stdlib.Sys.file_exists origin then
            Git_env.run_git ~cwd:managed_dir
              [ "push"; "-q"; "origin"; moved ^ ":refs/heads/" ^ base_ref ];
          Git_env.run_git ~cwd:managed_dir
            [ "update-ref"; "refs/remotes/origin/" ^ base_ref; moved ];
          let module Owned = Worktree_setup.Make (Real) (Env) in
          let result =
            Runtime.with_patch_ownership Env.runtime ~patch_id:pid (fun owner ->
                Owned.ensure_owned ~owner ())
          in
          (match result with
          | Worktree_setup.Path _ -> ()
          | Worktree_setup.Unavailable failure ->
              failwith (Worktree_provision.message failure));
          let receipt =
            Runtime.read Env.runtime (fun snap ->
                Branch_reconcile.materialization
                  (Orchestrator.agent snap.Runtime.orchestrator pid)
                    .Patch_agent.branch_reconcile)
          in
          (match
             Option.bind receipt ~f:Branch_reconcile.materialization_boundary
           with
          | Some boundary ->
              assert_string "owner retains original materialization boundary"
                original
                (Branch_reconcile.Commit.to_string boundary)
          | None -> failwith "missing owner materialization boundary");

          p
      | Worktree_setup.Unavailable failure ->
          failwith (Worktree_provision.message failure)
      | exception exn when Worktree.has_cancellation exn ->
          if Option.is_none cancel_stage then raise exn;
          if not (phys_equal exn cancellation) then
            failwith "cancellation changed";
          let after =
            Runtime.read Env.runtime (fun snap ->
                Orchestrator.agent snap.Runtime.orchestrator pid)
          in
          let without_owner agent =
            match Persistence.patch_agent_to_yojson agent with
            | `Assoc fields ->
                `Assoc
                  (List.Assoc.remove fields ~equal:String.equal
                     "branch_reconcile")
            | _ -> failwith "agent checkpoint"
          in
          if not (Poly.equal (without_owner agent) (without_owner after)) then
            failwith "cancellation changed unrelated agent state";
          let without_hook =
            match Branch_reconcile.yojson_of_t after.branch_reconcile with
            | `Assoc fields ->
                `Assoc
                  (List.Assoc.remove fields ~equal:String.equal "worktree_hook")
            | _ -> failwith "owner checkpoint"
          in
          if
            not
              (Result.equal Branch_reconcile.equal String.equal
                 (Branch_reconcile.decode without_hook)
                 (Ok agent.branch_reconcile))
          then failwith "cancellation changed unrelated owner state";
          if
            not
              (Bool.equal
                 (Worktree_hook.is_pending
                    (Branch_reconcile.worktree_hook after.branch_reconcile))
                 (not (Poly.equal cancel_stage (Some `Ready))))
          then failwith "cancellation lost its pending creation hook";
          "cancelled")

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
  let pushed_tip = git_capture ~dir:writer_dir [ "rev-parse"; "main" ] in
  let origin_tip =
    wait_for_remote_head ~dir:managed_dir ~ref_name:"main" ~sha:pushed_tip
  in
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

let scenario_checkpoint_failure env =
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  ignore
    (run_ensure ~fail_checkpoint:true env ~managed_dir
       ~project_name:"checkpoint-failure"
       ~pid:(Types.Patch_id.of_string "1")
       ~branch:"checkpoint-failure/patch-1" ~base_ref:"main");
  Stdlib.print_endline "  hook_plan_checkpoint_failure_prevents_creation: OK"

let scenario_hook_restart env =
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  let project_name = "hook-restart" in
  let pid = Types.Patch_id.of_string "1" in
  let branch = "hook-restart/patch-1" in
  let main = Types.Branch.of_string "main" in
  let checkpoint = Project_store.snapshot_path project_name in
  let path = Worktree.worktree_dir ~project_name ~patch_id:pid in
  let hook_path = Worktree_backend.canonical path in
  let gameplan = { empty_gameplan with patches = [ mk_patch ~pid ~branch ] } in
  let load () =
    match Persistence.load ~path:checkpoint with
    | Ok snapshot -> snapshot
    | Error reason -> failwith reason
  in
  let persisted_hook () =
    Branch_reconcile.worktree_hook
      (Orchestrator.agent (load ()).Runtime.orchestrator pid)
        .Patch_agent.branch_reconcile
  in
  let captured_script = Stdlib.Filename.concat root "prepare-checkout" in
  Stdlib.Out_channel.with_open_bin captured_script (fun channel ->
      Stdlib.output_string channel "#!/bin/sh\nprintf 'hook\\n' >> hook-runs\n");
  Unix.chmod captured_script 0o700;
  let configured_script = ref captured_script in
  let create_cancel = ref true and hook_cancel = ref true in
  let hook_calls = ref 0 and creates = ref 0 in
  let cancellation =
    Eio.Cancel.Cancelled (Failure "hook lifecycle interruption")
  in
  let module Real =
    (val Worktree.make ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
           ~clock:(Eio.Stdenv.clock env)
           ~process_mgr:(Eio.Stdenv.process_mgr env)
           ~repo_root:managed_dir)
  in
  let module W = struct
    include Real

    let create ~project_name ~patch_id ~branch ~base_ref =
      (match
         Worktree_hook.next (persisted_hook ()) ~path:hook_path
           ~branch:(Types.Branch.to_string branch)
       with
      | Worktree_hook.Run run
        when String.equal run.request.script captured_script ->
          ()
      | Worktree_hook.Run _ | Worktree_hook.Ready | Worktree_hook.Stop _ ->
          failwith "checkout creation preceded the captured hook plan");
      Int.incr creates;
      let result = Real.create ~project_name ~patch_id ~branch ~base_ref in
      if !create_cancel then (
        create_cancel := false;
        raise cancellation);
      result

    let run_hook ~clock ~script ~cwd ~env () =
      if not (String.equal script captured_script) then
        failwith "restart substituted a newly configured hook";
      if
        not
          (Worktree_hook.equal_decision
             (Worktree_hook.next (persisted_hook ()) ~path:hook_path ~branch)
             (Worktree_hook.Stop "worktree_create_hook_outcome_unknown"))
      then failwith "hook ran without a durable attempt claim";
      Int.incr hook_calls;
      let result = Real.run_hook ~clock ~script ~cwd ~env () in
      (match result with Ok () -> () | Error reason -> failwith reason);
      if !hook_cancel then (
        hook_cancel := false;
        raise cancellation);
      result
  end in
  let ensure runtime =
    let module Env = struct
      let runtime = runtime
      let clock = Eio.Stdenv.clock env
      let fs = Eio.Stdenv.fs env
      let worktree_mutex = Eio.Mutex.create ()
      let hook_mutex = Eio.Mutex.create ()
      let fetch_mutex = Eio.Mutex.create ()
      let project_name = project_name

      let user_config =
        { User_config.on_worktree_create = Some !configured_script }
    end in
    let module WS = Worktree_setup.Make (W) (Env) in
    Runtime.with_patch_ownership runtime ~patch_id:pid (fun owner ->
        WS.ensure_owned ~owner ())
  in
  let expect_cancel runtime =
    match ensure runtime with
    | exception exn when Worktree.has_cancellation exn -> ()
    | Worktree_setup.Path _ ->
        failwith "injected hook cancellation was swallowed"
    | Worktree_setup.Unavailable failure ->
        failwith
          ("hook cancellation deferred: " ^ Worktree_provision.message failure)
  in
  let restore () =
    Runtime.create ~gameplan ~main_branch:main ~snapshot:(load ()) ()
  in
  let runtime = Runtime.create ~gameplan ~main_branch:main () in
  expect_cancel runtime;
  if !creates <> 1 || !hook_calls <> 0 then
    failwith "creation interruption dispatched a hook";
  configured_script := "exit 99";
  expect_cancel (restore ());
  if !creates <> 1 || !hook_calls <> 1 then
    failwith "pending hook did not resume exactly once";
  let runtime = restore () in
  (match ensure runtime with
  | Worktree_setup.Unavailable
      (Worktree_provision.Temporary "branch_reconciliation_pending") ->
      ()
  | Worktree_setup.Path _
  | Worktree_setup.Unavailable
      (Worktree_provision.Temporary _ | Worktree_provision.Unsafe _) ->
      failwith "uncertain hook did not require explicit resume");
  if !hook_calls <> 1 then failwith "uncertain hook was repeated";
  let exhaust runtime =
    Runtime.update_orchestrator runtime (fun orch ->
        let state orch =
          (Orchestrator.agent orch pid).Patch_agent.branch_reconcile
        in
        let step orch event =
          fst (Orchestrator.reconcile_branch orch pid event)
        in
        Onton_core_test_support.Publication_fixture.exhaust_diagnosis ~state
          ~step orch)
  in
  exhaust runtime;
  let other_path = Stdlib.Filename.concat root "other-checkout" in
  Runtime.update_orchestrator runtime (fun orch ->
      let orch = Orchestrator.set_worktree_path orch pid other_path in
      fst (Orchestrator.reconcile_branch orch pid Branch_reconcile.Resume));
  (match ensure runtime with
  | Worktree_setup.Unavailable
      (Worktree_provision.Temporary "branch_reconciliation_pending") ->
      ()
  | Worktree_setup.Path _
  | Worktree_setup.Unavailable
      (Worktree_provision.Temporary _ | Worktree_provision.Unsafe _) ->
      failwith "explicit resume authorized a hook in a different checkout");
  if !hook_calls <> 1 || Stdlib.Sys.file_exists other_path then
    failwith "changed hook context caused checkout side effects";
  exhaust runtime;
  Runtime.update_orchestrator runtime (fun orch ->
      Orchestrator.set_worktree_path orch pid path);
  Runtime.update_orchestrator runtime (fun orch ->
      fst (Orchestrator.reconcile_branch orch pid Branch_reconcile.Resume));
  (match ensure runtime with
  | Worktree_setup.Path actual ->
      assert_string "hook resumed checkout" hook_path
        (Worktree_backend.canonical actual)
  | Worktree_setup.Unavailable failure ->
      failwith (Worktree_provision.message failure));
  if !creates <> 1 || !hook_calls <> 2 then
    failwith "explicit resume did not retain hook identity";
  (match ensure (restore ()) with
  | Worktree_setup.Path _ -> ()
  | Worktree_setup.Unavailable failure ->
      failwith (Worktree_provision.message failure));
  if !hook_calls <> 2 then failwith "completed hook repeated after restart";
  let contents =
    Stdlib.In_channel.with_open_bin
      (Stdlib.Filename.concat path "hook-runs")
      Stdlib.In_channel.input_all
  in
  assert_string "only authorized hook attempts wrote checkout data"
    "hook\nhook\n" contents;
  Stdlib.print_endline "  durable_hook_creation_execution_resume_restart: OK"

let scenario_hook_checkpoint_failure ?(materialization = false) env completion =
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  let project_name = "hook-checkpoint" in
  let pid = Types.Patch_id.of_string "1" in
  let branch = "hook-checkpoint/patch-1" in
  let path = Worktree.worktree_dir ~project_name ~patch_id:pid in
  let checkpoint = Project_store.snapshot_path project_name in
  let backup = checkpoint ^ ".saved" in
  let blocked = ref false in
  let block_checkpoint () =
    Unix.rename checkpoint backup;
    Unix.mkdir checkpoint 0o700;
    blocked := true
  in
  let restore_checkpoint () =
    if !blocked then (
      Unix.rmdir checkpoint;
      Unix.rename backup checkpoint;
      blocked := false)
  in
  let load () =
    match Persistence.load ~path:checkpoint with
    | Ok snapshot -> snapshot
    | Error reason -> failwith reason
  in
  let script = Stdlib.Filename.concat root "prepare-checkout" in
  Stdlib.Out_channel.with_open_bin script (fun channel ->
      Stdlib.output_string channel "#!/bin/sh\nprintf 'hook\\n' >> hook-runs\n");
  Unix.chmod script 0o700;
  let hook_calls = ref 0 and creates = ref 0 in
  let inject_materialization_failure = ref materialization in
  let inject_completion_failure = ref completion in
  let module Real =
    (val Worktree.make ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
           ~clock:(Eio.Stdenv.clock env)
           ~process_mgr:(Eio.Stdenv.process_mgr env)
           ~repo_root:managed_dir)
  in
  let module W = struct
    include Real

    let create ~project_name ~patch_id ~branch ~base_ref =
      Int.incr creates;
      Real.create ~project_name ~patch_id ~branch ~base_ref

    let materialization ~path ~project_name ~branch =
      let result = Real.materialization ~path ~project_name ~branch in
      (match result with
      | Ok (Some _) when !inject_materialization_failure ->
          inject_materialization_failure := false;
          block_checkpoint ()
      | Ok _ | Error _ -> ());
      result

    let run_hook ~clock ~script ~cwd ~env () =
      Int.incr hook_calls;
      let result = Real.run_hook ~clock ~script ~cwd ~env () in
      (match result with Ok () -> () | Error reason -> failwith reason);
      if !inject_completion_failure then (
        inject_completion_failure := false;
        block_checkpoint ());
      result
  end in
  let gameplan = { empty_gameplan with patches = [ mk_patch ~pid ~branch ] } in
  let main_branch = Types.Branch.of_string "main" in
  let runtime = Runtime.create ~gameplan ~main_branch () in
  let hook_mutex = Eio.Mutex.create () in
  let ensure runtime =
    let module Env = struct
      let runtime = runtime
      let clock = Eio.Stdenv.clock env
      let fs = Eio.Stdenv.fs env
      let worktree_mutex = Eio.Mutex.create ()
      let hook_mutex = hook_mutex
      let fetch_mutex = Eio.Mutex.create ()
      let project_name = project_name
      let user_config = { User_config.on_worktree_create = Some script }
    end in
    let module WS = Worktree_setup.Make (W) (Env) in
    Runtime.with_patch_ownership runtime ~patch_id:pid (fun owner ->
        WS.ensure_owned ~owner ())
  in
  let expect_checkpoint_failure = function
    | Worktree_setup.Unavailable (Worktree_provision.Temporary _) -> ()
    | Worktree_setup.Path _
    | Worktree_setup.Unavailable (Worktree_provision.Unsafe _) ->
        failwith "hook checkpoint failure was not retryable"
  in
  Stdlib.Fun.protect ~finally:restore_checkpoint (fun () ->
      if completion || materialization then
        expect_checkpoint_failure (ensure runtime)
      else
        Eio.Switch.run (fun sw ->
            Eio.Mutex.lock hook_mutex;
            let held = ref true in
            let release () =
              if !held then (
                held := false;
                Eio.Mutex.unlock hook_mutex)
            in
            Stdlib.Fun.protect ~finally:release (fun () ->
                let result =
                  Eio.Fiber.fork_promise ~sw (fun () -> ensure runtime)
                in
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                    while
                      Runtime.read runtime (fun snap ->
                          Option.is_none
                            (Branch_reconcile.materialization
                               (Orchestrator.agent snap.Runtime.orchestrator pid)
                                 .Patch_agent.branch_reconcile))
                    do
                      Eio.Time.sleep (Eio.Stdenv.clock env) 0.001
                    done;
                    block_checkpoint ();
                    release ();
                    expect_checkpoint_failure (Eio.Promise.await_exn result)))));
  let expected_initial = if completion then 1 else 0 in
  if !hook_calls <> expected_initial then
    failwith "failed hook checkpoint allowed an unauthorized execution";
  let persisted = load () in
  if materialization then (
    let owner snapshot =
      (Orchestrator.agent snapshot.Runtime.orchestrator pid)
        .Patch_agent.branch_reconcile
    in
    if
      Option.is_some (Branch_reconcile.materialization (owner persisted))
      || Runtime.read runtime (fun snapshot ->
          Option.is_some (Branch_reconcile.materialization (owner snapshot)))
    then failwith "refused materialization checkpoint installed a receipt";
    if !creates <> 1 || not (Stdlib.Sys.file_exists path) then
      failwith "materialization refusal did not retain its created checkout");
  let hook =
    Branch_reconcile.worktree_hook
      (Orchestrator.agent persisted.Runtime.orchestrator pid)
        .Patch_agent.branch_reconcile
  in
  (match
     Worktree_hook.next hook ~path:(Worktree_backend.canonical path) ~branch
   with
  | Worktree_hook.Run run when (not completion) && run.attempt = 1 -> ()
  | Worktree_hook.Stop "worktree_create_hook_outcome_unknown" when completion ->
      ()
  | Worktree_hook.Run _ | Worktree_hook.Stop _ | Worktree_hook.Ready ->
      failwith "checkpoint failure changed durable hook authority");
  let restored = Runtime.create ~gameplan ~main_branch ~snapshot:persisted () in
  if completion then (
    (match ensure restored with
    | Worktree_setup.Unavailable
        (Worktree_provision.Temporary "branch_reconciliation_pending") ->
        ()
    | Worktree_setup.Path _
    | Worktree_setup.Unavailable
        (Worktree_provision.Temporary _ | Worktree_provision.Unsafe _) ->
        failwith "lost completion repeated the hook after restart");
    if !hook_calls <> 1 then failwith "unacknowledged hook repeated";
    Runtime.update_orchestrator restored (fun orch ->
        let state orch =
          (Orchestrator.agent orch pid).Patch_agent.branch_reconcile
        in
        let step orch event =
          fst (Orchestrator.reconcile_branch orch pid event)
        in
        let orch =
          Onton_core_test_support.Publication_fixture.exhaust_diagnosis ~state
            ~step orch
        in
        step orch Branch_reconcile.Resume));
  (match ensure restored with
  | Worktree_setup.Path _ -> ()
  | Worktree_setup.Unavailable failure ->
      failwith (Worktree_provision.message failure));
  let expected_final = if completion then 2 else 1 in
  if !hook_calls <> expected_final then failwith "hook retry did not converge";
  let restarted =
    Runtime.create ~gameplan ~main_branch ~snapshot:(load ()) ()
  in
  (match ensure restarted with
  | Worktree_setup.Path _ -> ()
  | Worktree_setup.Unavailable failure ->
      failwith (Worktree_provision.message failure));
  if !hook_calls <> expected_final then failwith "acknowledged hook repeated";
  if !creates <> 1 then
    failwith "checkpoint recovery recreated an existing checkout";
  let saved = load () in
  if
    Option.is_none
      (Branch_reconcile.materialization
         (Orchestrator.agent saved.Runtime.orchestrator pid)
           .Patch_agent.branch_reconcile)
  then failwith "checkpoint recovery lost its materialization receipt";
  let contents =
    Stdlib.In_channel.with_open_bin
      (Stdlib.Filename.concat path "hook-runs")
      Stdlib.In_channel.input_all
  in
  assert_string "only authorized attempts wrote checkout data"
    (if completion then "hook\nhook\n" else "hook\n")
    contents;
  Stdlib.print_endline "  hook_claim_completion_checkpoint_failure_restart: OK"

let scenario_hook_changes_checkout env move_checkout =
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  let project_name = "hook-checkout" in
  let pid = Types.Patch_id.of_string "1" in
  let branch = "hook-checkout/patch-1" in
  let path = Worktree.worktree_dir ~project_name ~patch_id:pid in
  let script = Stdlib.Filename.concat root "change-checkout" in
  Stdlib.Out_channel.with_open_bin script (fun channel ->
      Stdlib.output_string channel
        (if move_checkout then
           "#!/bin/sh\nexec git worktree move . ../moved-checkout\n"
         else "#!/bin/sh\nexec git switch -c hook-other-branch\n"));
  Unix.chmod script 0o700;
  let gameplan = { empty_gameplan with patches = [ mk_patch ~pid ~branch ] } in
  let module W =
    (val Worktree.make ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
           ~clock:(Eio.Stdenv.clock env)
           ~process_mgr:(Eio.Stdenv.process_mgr env)
           ~repo_root:managed_dir)
  in
  let module Env = struct
    let runtime =
      Runtime.create ~gameplan ~main_branch:(Types.Branch.of_string "main") ()

    let clock = Eio.Stdenv.clock env
    let fs = Eio.Stdenv.fs env
    let worktree_mutex = Eio.Mutex.create ()
    let hook_mutex = Eio.Mutex.create ()
    let fetch_mutex = Eio.Mutex.create ()
    let project_name = project_name
    let user_config = { User_config.on_worktree_create = Some script }
  end in
  let module WS = Worktree_setup.Make (W) (Env) in
  let result =
    Runtime.with_patch_ownership Env.runtime ~patch_id:pid (fun owner ->
        WS.ensure_owned ~owner ())
  in
  if move_checkout then (
    if Stdlib.Sys.file_exists path then failwith "hook did not move checkout")
  else
    assert_string "hook changed branch" "hook-other-branch"
      (git_capture ~dir:path [ "branch"; "--show-current" ]);
  (match result with
  | Worktree_setup.Unavailable
      (Worktree_provision.Unsafe _ | Worktree_provision.Temporary _) ->
      ()
  | Worktree_setup.Path _ ->
      failwith "hook changed checkout but provisioning reported readiness");
  Stdlib.print_endline "  hook_checkout_mutation_refuses_readiness: OK"

let scenario_hook_terminal_while_waiting env merged =
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  let project_name = "hook-capacity" in
  let pid = Types.Patch_id.of_string "1" in
  let branch = "hook-capacity/patch-1" in
  let gameplan = { empty_gameplan with patches = [ mk_patch ~pid ~branch ] } in
  let hook_calls = ref 0 in
  let module Real =
    (val Worktree.make ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
           ~clock:(Eio.Stdenv.clock env)
           ~process_mgr:(Eio.Stdenv.process_mgr env)
           ~repo_root:managed_dir)
  in
  let module W = struct
    include Real

    let run_hook ~clock:_ ~script:_ ~cwd:_ ~env:_ () =
      Int.incr hook_calls;
      Ok ()
  end in
  let module Env = struct
    let runtime =
      Runtime.create ~gameplan ~main_branch:(Types.Branch.of_string "main") ()

    let clock = Eio.Stdenv.clock env
    let fs = Eio.Stdenv.fs env
    let worktree_mutex = Eio.Mutex.create ()
    let hook_mutex = Eio.Mutex.create ()
    let fetch_mutex = Eio.Mutex.create ()
    let project_name = project_name
    let user_config = { User_config.on_worktree_create = Some "true" }
  end in
  let module WS = Worktree_setup.Make (W) (Env) in
  let owner_state () =
    Runtime.read Env.runtime (fun snap ->
        (Orchestrator.agent snap.Runtime.orchestrator pid)
          .Patch_agent.branch_reconcile)
  in
  Eio.Switch.run (fun sw ->
      Eio.Mutex.lock Env.hook_mutex;
      let held = ref true in
      let release () =
        if !held then (
          held := false;
          Eio.Mutex.unlock Env.hook_mutex)
      in
      Stdlib.Fun.protect ~finally:release (fun () ->
          let finished =
            Eio.Fiber.fork_promise ~sw (fun () ->
                Runtime.with_patch_ownership Env.runtime ~patch_id:pid
                  (fun owner -> WS.ensure_owned ~owner ()))
          in
          Eio.Time.with_timeout_exn Env.clock 10. (fun () ->
              while
                Option.is_none
                  (Branch_reconcile.materialization (owner_state ()))
              do
                Eio.Time.sleep Env.clock 0.001
              done;
              Runtime.update_orchestrator Env.runtime (fun orch ->
                  if merged then Orchestrator.mark_merged orch pid
                  else
                    Orchestrator.apply_session_result orch pid
                      (Orchestrator.Session_wontdo "stop before initialization"));
              release ();
              match Eio.Promise.await_exn finished with
              | Worktree_setup.Unavailable (Worktree_provision.Unsafe reason)
                when String.equal reason
                       (if merged then "patch_merged" else "patch_wontdo") ->
                  ()
              | Worktree_setup.Path _
              | Worktree_setup.Unavailable
                  (Worktree_provision.Temporary _ | Worktree_provision.Unsafe _)
                ->
                  failwith
                    "terminal patch did not suspend checkout initialization")));
  if !hook_calls <> 0 then
    failwith "terminal patch launched a hook after waiting for capacity";
  let path =
    Worktree_backend.canonical
      (Worktree.worktree_dir ~project_name ~patch_id:pid)
  in
  (match
     Worktree_hook.next
       (Branch_reconcile.worktree_hook (owner_state ()))
       ~path ~branch
   with
  | Worktree_hook.Run run when run.attempt = 1 -> ()
  | Worktree_hook.Run _ | Worktree_hook.Ready | Worktree_hook.Stop _ ->
      failwith "terminal patch consumed or lost its unstarted hook");
  Stdlib.print_endline "  terminal_hook_capacity_interleaving: OK"

let scenario_owned_provisioning env =
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  let project_name = "owned-provision" in
  let checkpoint = Project_store.snapshot_path project_name in
  let pid = Types.Patch_id.of_string "1" in
  let branch = "owned-provision/patch-1" in
  let main = Types.Branch.of_string "main" in
  let gameplan = { empty_gameplan with patches = [ mk_patch ~pid ~branch ] } in
  let load () =
    match Persistence.load ~path:checkpoint with
    | Ok snapshot -> snapshot
    | Error reason -> failwith reason
  in
  let persisted_owner () =
    (Orchestrator.agent (load ()).Runtime.orchestrator pid)
      .Patch_agent.branch_reconcile
  in
  let inspect_authority () =
    match Branch_reconcile.operation (persisted_owner ()) with
    | Some operation
      when Branch_reconcile.is_provisioning operation.intent.purpose
           && Option.is_some operation.pending ->
        ()
    | Some _ | None ->
        failwith "checkout effect preceded durable provisioning command"
  in
  let module Real =
    (val Worktree.make ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
           ~clock:(Eio.Stdenv.clock env)
           ~process_mgr:(Eio.Stdenv.process_mgr env)
           ~repo_root:managed_dir)
  in
  let offline = ref true and probes = ref 0 and creates = ref 0 in
  let cancel_probe = ref false in
  let cancellation =
    Eio.Cancel.Cancelled (Failure "interrupted provisioning")
  in
  let module W = struct
    include Real

    let ensure_ready ~path ~branch =
      inspect_authority ();
      Int.incr probes;
      if !cancel_probe then raise cancellation;
      if !offline then Error "checkout probe offline"
      else Real.ensure_ready ~path ~branch

    let create ~project_name ~patch_id ~branch ~base_ref =
      inspect_authority ();
      Int.incr creates;
      Real.create ~project_name ~patch_id ~branch ~base_ref

    let reconcile ~path:_ ~project_name:_ ~branch:_ ~operation:_ _ =
      failwith "provisioning invoked integration or publication"
  end in
  let module Env = struct
    let runtime = Runtime.create ~gameplan ~main_branch:main ()
    let clock = Eio.Stdenv.clock env
    let fs = Eio.Stdenv.fs env
    let worktree_mutex = Eio.Mutex.create ()
    let hook_mutex = Eio.Mutex.create ()
    let fetch_mutex = Eio.Mutex.create ()
    let project_name = project_name
    let user_config = { User_config.on_worktree_create = None }
  end in
  let module WS = Worktree_setup.Make (W) (Env) in
  let agent runtime =
    Runtime.read runtime (fun snapshot ->
        Orchestrator.agent snapshot.Runtime.orchestrator pid)
  in
  let ensure runtime run =
    Runtime.with_patch_ownership runtime ~patch_id:pid (fun owner ->
        run ~owner ())
  in
  let temporary = function
    | Worktree_setup.Unavailable (Worktree_provision.Temporary _) -> ()
    | Worktree_setup.Unavailable (Worktree_provision.Unsafe _)
    | Worktree_setup.Path _ ->
        failwith "expected deferred checkout provisioning"
  in
  Runtime.update_orchestrator Env.runtime (fun orch ->
      let orch =
        Orchestrator.send_human_message orch pid "preserve this guidance"
      in
      let orch = Orchestrator.set_tried_fresh orch pid in
      let orch =
        Orchestrator.set_llm_session_id orch pid (Some "existing-session")
      in
      Orchestrator.fire orch (Orchestrator.Start (pid, main)));
  let before = agent Env.runtime in
  let check_budgets actual =
    if
      (not
         (Poly.equal before.Patch_agent.session_fallback
            actual.Patch_agent.session_fallback))
      || (not
            (Option.equal String.equal before.llm_session_id
               actual.llm_session_id))
      || before.start_attempts_without_pr <> actual.start_attempts_without_pr
      || before.context_exhaustion_count <> actual.context_exhaustion_count
    then failwith "provisioning changed implementation-session budgets"
  in
  Project_store.ensure_dir (Stdlib.Filename.dirname checkpoint);
  Unix.mkdir checkpoint 0o700;
  temporary (ensure Env.runtime (WS.ensure_owned ?base_ref:None));
  if !probes <> 0 || !creates <> 0 then
    failwith "failed checkpoint allowed checkout effects";
  if
    not
      (Branch_reconcile.equal before.branch_reconcile
         (agent Env.runtime).branch_reconcile)
  then failwith "failed checkpoint installed an owner operation";
  check_budgets (agent Env.runtime);
  Unix.rmdir checkpoint;
  cancel_probe := true;
  (match ensure Env.runtime (WS.ensure_owned ?base_ref:None) with
  | Worktree_setup.Path _ | Worktree_setup.Unavailable _ ->
      failwith "owned provisioning swallowed cancellation"
  | exception exn when Worktree.has_cancellation exn ->
      if not (phys_equal exn cancellation) then failwith "cancellation changed");
  let interrupted = persisted_owner () in
  if Option.is_none (Branch_reconcile.pending interrupted) || !creates <> 0 then
    failwith "cancellation lost the pending command or created a checkout";
  check_budgets (agent Env.runtime);
  cancel_probe := false;
  temporary (ensure Env.runtime (WS.ensure_owned ?base_ref:None));
  let waiting = persisted_owner () in
  if
    not
      (Option.equal Int.equal
         (Option.map (Branch_reconcile.operation interrupted) ~f:(fun op ->
              op.id))
         (Option.map (Branch_reconcile.operation waiting) ~f:(fun op -> op.id)))
  then failwith "retry replaced the interrupted provisioning operation";
  let deadline =
    match Branch_reconcile.operation waiting with
    | Some { phase = Branch_reconcile.Waiting { until; _ }; _ } -> until
    | Some
        {
          phase =
            ( Branch_reconcile.Preparing | Integrating | Repairing _
            | Publishing | Confirming | Recovering | Settled | Intervention _ );
          _;
        }
    | None ->
        failwith "probe failure did not persist backoff"
  in
  let probe_count = !probes in
  temporary (ensure Env.runtime (WS.ensure_owned ?base_ref:None));
  if !probes <> probe_count || !creates <> 0 then
    failwith "retry bypassed owner backoff";
  if not (Branch_reconcile.equal waiting (persisted_owner ())) then
    failwith "new inspection request superseded pending retry";
  check_budgets (agent Env.runtime);
  Runtime.update_orchestrator Env.runtime (fun orch ->
      Orchestrator.apply_start_outcome orch pid Orchestrator.Start_failed);
  let after = agent Env.runtime in
  check_budgets after;
  if
    after.busy
    || (not
          (List.equal String.equal after.human_messages
             [ "preserve this guidance" ]))
    || not (List.is_empty after.inflight_human_messages)
  then failwith "deferred Start lost guidance or retained worker capacity";
  let save snapshot = Persistence.save_snapshot ~path:checkpoint snapshot in
  (match Runtime.read Env.runtime save with
  | Ok () -> ()
  | Error reason -> failwith reason);
  let module Restored = struct
    include Env

    let runtime =
      Runtime.create ~gameplan ~main_branch:main ~snapshot:(load ()) ()
  end in
  let module Resumed = Worktree_setup.Make (W) (Restored) in
  offline := false;
  let outcome =
    Branch_reconcile_runner.run ~runtime:Restored.runtime ~persist:save
      ~patch_id:pid
      ~now:(fun () -> deadline +. 1.)
      ~execute:(Resumed.execute_reconciliation ~patch_id:pid)
      (Branch_reconcile.Tick (deadline +. 1.))
  in
  (match outcome with
  | Branch_reconcile_runner.Idle -> ()
  | Waiting | Repair_needed _ | Intervention _ | Checkpoint_failed _ ->
      failwith "restored provisioning failed to settle");
  check_budgets (agent Restored.runtime);
  if !creates <> 1 then
    failwith "restored provisioning did not create exactly once";
  let settled = persisted_owner () in
  if Option.is_none (Branch_reconcile.materialization settled) then
    failwith "restored provisioning omitted durable materialization";
  let probe_count = !probes in
  (match ensure Restored.runtime (Resumed.ensure_owned ?base_ref:None) with
  | Worktree_setup.Path _ -> ()
  | Worktree_setup.Unavailable failure ->
      failwith (Worktree_provision.message failure));
  if !creates <> 1 || !probes <= probe_count then
    failwith "later checkout request failed to reinspect existing checkout";
  if Branch_reconcile.equal settled (persisted_owner ()) then
    failwith "later checkout inspection reused settled request identity";
  if Option.is_some (remote_head_sha ~dir:managed_dir ~ref_name:branch) then
    failwith "provisioning published the patch branch";
  Stdlib.print_endline "  owned_provisioning_checkpoint_retry_restart: OK"

let scenario_adopted_publication env policy history =
  let module B = Branch_reconcile in
  let module R = Branch_reconcile_runner in
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  let project_name = "adopted-publication" in
  let branch = "adopted-publication/patch-1" in
  let pid = Types.Patch_id.of_string "1" in
  let git args = Git_env.run_git ~cwd:managed_dir args in
  git [ "checkout"; "-q"; "-b"; branch ];
  if not (Poly.equal history `Ahead) then (
    sh ~dir:managed_dir "echo remote > remote.txt";
    git [ "add"; "remote.txt" ];
    git [ "commit"; "-qm"; "remote contribution" ]);
  git [ "push"; "-q"; "origin"; branch ];
  let remote = git_capture ~dir:managed_dir [ "rev-parse"; "HEAD" ] in
  (match history with
  | `Ahead -> ()
  | `Diverged -> git [ "reset"; "--hard"; "main" ]
  | `Unrelated ->
      git [ "checkout"; "--orphan"; "unrelated-local" ];
      git [ "rm"; "-rf"; "." ]);
  sh ~dir:managed_dir "echo local > local.txt";
  git [ "add"; "local.txt" ];
  git [ "commit"; "-qm"; "local contribution" ];
  let local = git_capture ~dir:managed_dir [ "rev-parse"; "HEAD" ] in
  git [ "checkout"; "-q"; "main" ];
  git [ "update-ref"; "refs/heads/" ^ branch; local ];
  let gameplan = { empty_gameplan with patches = [ mk_patch ~pid ~branch ] } in
  let module W =
    (val Worktree.make ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
           ~clock:(Eio.Stdenv.clock env)
           ~process_mgr:(Eio.Stdenv.process_mgr env)
           ~repo_root:managed_dir)
  in
  let module Env = struct
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
  let path =
    match
      Runtime.with_patch_ownership Env.runtime ~patch_id:pid (fun owner ->
          WS.ensure_owned ~owner ())
    with
    | Worktree_setup.Path path -> path
    | Unavailable failure -> failwith (Worktree_provision.message failure)
  in
  let state () =
    Runtime.read Env.runtime (fun snapshot ->
        (Orchestrator.agent snapshot.Runtime.orchestrator pid)
          .Patch_agent.branch_reconcile)
  in
  assert_string "adoption preserves local tip" local
    (git_capture ~dir:path [ "rev-parse"; "HEAD" ]);
  assert_string "adoption preserves remote tip" remote
    (git_capture ~dir:origin_dir [ "rev-parse"; branch ]);
  (match B.materialization (state ()) with
  | Some (B.Adopted_branch head)
    when String.equal (B.Commit.to_string head) local ->
      ()
  | Some (B.Adopted_branch _ | B.New_branch _) | None ->
      failwith "adoption fabricated a replay boundary");
  if not (List.is_empty (B.publications (state ()))) then
    failwith "provisioning granted publication authority";
  let checkpoint = Project_store.snapshot_path project_name in
  let persist snapshot = Persistence.save_snapshot ~path:checkpoint snapshot in
  let now () = 100. in
  let source =
    match B.Commit.make local with
    | Some source -> source
    | None -> failwith "invalid fixture revision"
  in
  let outcome =
    R.run ~runtime:Env.runtime ~persist ~patch_id:pid ~now
      ~execute:(WS.execute_reconciliation ~patch_id:pid)
      (B.Request B.{ base = "main"; policy; purpose = Publish_revision source })
  in
  let calls = ref 0 in
  let settle = function
    | R.Idle -> ()
    | R.Waiting ->
        failwith
          ("adopted publication unexpectedly deferred: "
          ^ Yojson.Safe.to_string (B.yojson_of_t (state ())))
    | R.Repair_needed _ -> failwith "adopted publication still requires repair"
    | R.Intervention reason | R.Checkpoint_failed reason -> failwith reason
  in
  (match (history, outcome) with
  | (`Unrelated | `Diverged), R.Repair_needed token ->
      let active_token = ref token in
      let backend =
        Llm_backend.
          {
            name = "adopted-history-recovery";
            run_streaming =
              (fun ~project_name:_
                ~cwd:_
                ~patch_id:_
                ~prompt:_
                ~resume_session:_
                ~session_uuid:_
                ~complexity:_
                ~on_event:_
              ->
                Int.incr calls;
                let saved =
                  match Persistence.load ~path:checkpoint with
                  | Ok saved -> saved
                  | Error reason -> failwith reason
                in
                let saved_state =
                  (Orchestrator.agent saved.Runtime.orchestrator pid)
                    .Patch_agent.branch_reconcile
                in
                (match B.repair_turn saved_state ~branch !active_token with
                | Some { mode = B.History_recovery _; _ } -> ()
                | Some { mode = B.Content_repair | B.Diagnosis _; _ } | None ->
                    failwith
                      "recovery agent lacks durable history-recovery authority");
                Git_env.run_git ~cwd:path
                  [
                    "merge"; "--allow-unrelated-histories"; "--no-edit"; remote;
                  ];
                {
                  exit_code = 0;
                  stdout = "";
                  stderr = "";
                  got_events = true;
                  saw_final_result = true;
                  timed_out = false;
                });
          }
      in
      let repair () =
        R.run_repair ~runtime:Env.runtime ~persist ~patch_id:pid
          ~with_capacity:(fun run -> run ())
          ~now
          ~execute:(fun ~agent:_ ~operation command ->
            WS.execute_reconciliation ~patch_id:pid ~operation command)
          ~perform:(fun ~agent:_ ~turn ->
            active_token := turn.B.token;
            Branch_repair_session.run
              ~on_event:(fun _ -> ())
              ~context:"" ~guidance:[] ~backend
              ~cwd:Eio.Path.(Env.fs / path)
              ~project_name ~patch_id:pid ~complexity:None ~turn
              ~read_head:(fun () -> W.read_branch_sha ~path ~ref_name:"HEAD")
              ~now)
          token
      in
      (match repair () with
      | R.Repair_needed _ -> ()
      | R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _ ->
          failwith "unproven remote merge did not retain owned repair");
      ignore (repair ());
      if !calls <> 1 then failwith "duplicate recovery claim reran agent"
  | `Ahead, outcome -> settle outcome
  | ( (`Unrelated | `Diverged),
      (R.Idle | R.Waiting | R.Intervention _ | R.Checkpoint_failed _) ) ->
      failwith "unrelated adopted history did not reach agent recovery");
  if Poly.equal history `Ahead then (
    let published = git_capture ~dir:origin_dir [ "rev-parse"; branch ] in
    assert_string "published local work" "local"
      (git_capture ~dir:origin_dir [ "show"; branch ^ ":local.txt" ]);
    Git_env.run_git ~cwd:path
      [ "merge-base"; "--is-ancestor"; local; published ];
    if List.is_empty (B.publications (state ())) then
      failwith "verified publication omitted receipt")
  else (
    assert_string "unproven adoption preserves the remote" remote
      (git_capture ~dir:origin_dir [ "rev-parse"; branch ]);
    if not (List.is_empty (B.publications (state ()))) then
      failwith "adoption fabricated remote contribution authority");
  Stdlib.print_endline
    ("  adopted_publication_"
    ^ (match policy with
      | B.Rewrite -> "rewrite_"
      | B.Preserve_ancestry -> "merge_")
    ^ (match history with
      | `Ahead -> "ahead"
      | `Diverged -> "diverged"
      | `Unrelated -> "agent_recovery")
    ^ ": OK")

let scenario_receipt_recovery env =
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  let project_name = "receipt-recovery" in
  let branch = "receipt-recovery/patch-1" in
  let wt =
    run_ensure env ~managed_dir ~project_name
      ~pid:(Types.Patch_id.of_string "1")
      ~branch ~base_ref:"main"
  in
  let prefix = Branch_reconcile.recovery_prefix ~project:project_name ~branch in
  Git_env.run_git ~cwd:managed_dir
    [ "update-ref"; "-d"; prefix ^ "/materialized-base" ];
  let io =
    Branch_reconcile_executor.make_io
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~clock:(Eio.Stdenv.clock env) ~path:wt
  in
  let head =
    match
      Branch_reconcile.Commit.make (git_capture ~dir:wt [ "rev-parse"; "HEAD" ])
    with
    | Some head -> head
    | None -> failwith "invalid checkout HEAD"
  in
  (match
     Branch_reconcile_executor.pin_materialization_intent ~io ~prefix head
   with
  | Ok pinned when Branch_reconcile.Commit.equal pinned head -> ()
  | Ok _ -> failwith "materialization base changed"
  | Error message -> failwith message);
  (match
     Branch_reconcile_executor.recover_materialization ~io ~prefix ~branch
   with
  | Ok (Some (Branch_reconcile.New_branch head)) ->
      assert_string "recovered new-branch boundary"
        (git_capture ~dir:wt [ "rev-parse"; "HEAD" ])
        (Branch_reconcile.Commit.to_string head)
  | Ok (Some (Branch_reconcile.Adopted_branch _)) | Ok None | Error _ ->
      failwith "creation intent did not recover its receipt");
  Stdlib.print_endline "  receipt_recovery: OK"

let scenario_unsafe_materialization env =
  List.iter
    [ "materialization_revision_changed"; "materialization_branch_changed" ]
    ~f:(fun reason ->
      with_temp_dir @@ fun root ->
      let origin_dir = Stdlib.Filename.concat root "origin" in
      let managed_dir = Stdlib.Filename.concat root "managed" in
      setup_origin_with_main ~origin_dir;
      clone_into ~origin_dir ~managed_dir;
      assert_string "unsafe provenance refuses checkout" "refused"
        (run_ensure ~materialization_error:reason env ~managed_dir
           ~project_name:"unsafe-materialization"
           ~pid:(Types.Patch_id.of_string "1")
           ~branch:"unsafe-materialization/patch-1" ~base_ref:"main"))

let scenario_creation_retry_reuses_base env =
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  let old_head = git_capture ~dir:managed_dir [ "rev-parse"; "main" ] in
  let project_name = "creation-retry" in
  let branch = Types.Branch.of_string "creation-retry/patch-1" in
  let prefix =
    Branch_reconcile.recovery_prefix ~project:project_name
      ~branch:(Types.Branch.to_string branch)
  in
  let io =
    Branch_reconcile_executor.make_io
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~clock:(Eio.Stdenv.clock env) ~path:managed_dir
  in
  let old_head_sha =
    match Branch_reconcile.Commit.make old_head with
    | Some head -> head
    | None -> failwith "invalid fixture head"
  in
  (match
     Branch_reconcile_executor.pin_materialization_intent ~io ~prefix
       old_head_sha
   with
  | Ok _ -> ()
  | Error message -> failwith message);
  sh ~dir:managed_dir
    "git commit --allow-empty -q -m 'base advanced after failed setup'";
  let module W =
    (val Worktree.make ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
           ~clock:(Eio.Stdenv.clock env)
           ~process_mgr:(Eio.Stdenv.process_mgr env)
           ~repo_root:managed_dir)
  in
  (match
     W.create ~project_name
       ~patch_id:(Types.Patch_id.of_string "1")
       ~branch ~base_ref:"main"
   with
  | Error _ -> failwith "retry refused retained creation intent"
  | Ok _ -> ());
  assert_string "retry uses original pinned base" old_head
    (git_capture ~dir:managed_dir
       [ "rev-parse"; Types.Branch.to_string branch ])

let scenario_nested_cancellation env =
  List.iter [ `Ready; `Create; `Recovery ] ~f:(fun cancel_stage ->
      with_temp_dir (fun root ->
          let origin_dir = Stdlib.Filename.concat root "origin" in
          let managed_dir = Stdlib.Filename.concat root "managed" in
          setup_origin_with_main ~origin_dir;
          clone_into ~origin_dir ~managed_dir;
          let result =
            run_ensure ~cancel_stage env ~managed_dir ~project_name:"cancel"
              ~pid:(Types.Patch_id.of_string "4")
              ~branch:"cancel/patch-4" ~base_ref:"main"
          in
          assert_string "nested cancellation propagated without refusal"
            "cancelled" result))

let () =
  Eio_main.run @@ fun env ->
  Stdlib.print_endline "worktree_setup base-fetch integration:";
  scenario_nested_cancellation env;
  scenario_checkpoint_failure env;
  scenario_owned_provisioning env;
  List.iter [ false; true ] ~f:(scenario_hook_changes_checkout env);
  List.iter [ false; true ] ~f:(scenario_hook_checkpoint_failure env);
  scenario_hook_checkpoint_failure ~materialization:true env false;
  scenario_hook_restart env;
  List.iter [ false; true ] ~f:(scenario_hook_terminal_while_waiting env);
  List.iter [ Branch_reconcile.Rewrite; Preserve_ancestry ] ~f:(fun policy ->
      List.iter
        [ `Ahead; `Diverged; `Unrelated ]
        ~f:(scenario_adopted_publication env policy));
  scenario_receipt_recovery env;
  scenario_unsafe_materialization env;
  scenario_creation_retry_reuses_base env;
  scenario_stale_local_main env;
  scenario_dep_base_local_canonical env;
  scenario_fetch_failure_falls_back env;
  Stdlib.print_endline "all base-fetch integration scenarios passed"

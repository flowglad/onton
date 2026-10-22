(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton
open Onton_core
module Git = Onton_test_support.Git_env
module B = Branch_reconcile

let sha = Onton_test_support.Branch_reconcile_git_fixture.sha

[@@@warning "-42"]

let check label condition = if not condition then failwith label

let with_sandbox f =
  let dir = Stdlib.Filename.temp_dir "onton-worker-recovery-" "" in
  let prior_home = Stdlib.Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" dir;
  Stdlib.Fun.protect
    ~finally:(fun () ->
      Unix.putenv "HOME"
        (Option.value prior_home
           ~default:(Stdlib.Filename.get_temp_dir_name ()));
      Git.sh ~dir:"/" ("rm -rf " ^ Stdlib.Filename.quote dir))
    (fun () -> f dir)

let clone ~origin ~dest =
  Git.sh ~dir:"/"
    (Printf.sprintf "git clone --no-local -q %s %s"
       (Stdlib.Filename.quote origin)
       (Stdlib.Filename.quote dest));
  Git.run_git ~cwd:dest [ "config"; "user.email"; "test@example.com" ];
  Git.run_git ~cwd:dest [ "config"; "user.name"; "test" ];
  Git.run_git ~cwd:dest [ "config"; "commit.gpgsign"; "false" ]

let () =
  Eio_main.run @@ fun env ->
  with_sandbox @@ fun sandbox ->
  let seed = Stdlib.Filename.concat sandbox "seed" in
  Unix.mkdir seed 0o755;
  Git.init_repo seed;
  let base =
    Onton_test_support.Branch_reconcile_git_fixture.commit seed "base" "base\n"
      "base"
  in
  let origin = Stdlib.Filename.concat sandbox "origin.git" in
  Git.sh ~dir:"/"
    (Printf.sprintf "git clone --bare -q %s %s"
       (Stdlib.Filename.quote seed)
       (Stdlib.Filename.quote origin));
  let old = Stdlib.Filename.concat sandbox "old" in
  clone ~origin ~dest:old;
  let runtime =
    Onton_test_support.Branch_reconcile_git_fixture.make_runtime ()
  in
  let patch_id = Onton_test_support.Branch_reconcile_git_fixture.id in
  Runtime.update_orchestrator runtime (fun orch ->
      Orchestrator.set_llm_session_id orch patch_id (Some "lost-conversation"));
  let agent =
    Runtime.read runtime (fun snap ->
        Orchestrator.agent snap.Runtime.orchestrator patch_id)
  in
  let branch = Types.Branch.to_string agent.Patch_agent.branch in
  Git.run_git ~cwd:old [ "checkout"; "-q"; "-b"; branch ];
  let patch =
    Onton_test_support.Branch_reconcile_git_fixture.commit old "pushed"
      "remote marker\n" "patch"
  in
  Git.run_git ~cwd:old [ "push"; "-q"; "origin"; branch ];
  let unpublished =
    Onton_test_support.Branch_reconcile_git_fixture.commit old "unpushed"
      "local-only marker\n" "local"
  in
  Git.sh ~dir:"/" ("rm -rf " ^ Stdlib.Filename.quote old);
  check "old object database destroyed" (not (Stdlib.Sys.file_exists old));
  let fresh = Stdlib.Filename.concat sandbox "fresh" in
  clone ~origin ~dest:fresh;
  check "replacement has no alternates"
    (not
       (Stdlib.Sys.file_exists
          (Stdlib.Filename.concat fresh ".git/objects/info/alternates")));
  check "replacement cannot resolve unpushed commit"
    (Git.git_exit_code ~cwd:fresh [ "cat-file"; "-e"; unpublished ] <> 0);
  let module Real =
    (val Worktree.make ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
           ~clock:(Eio.Stdenv.clock env)
           ~process_mgr:(Eio.Stdenv.process_mgr env)
           ~repo_root:fresh)
  in
  let module Env = struct
    let runtime = runtime
    let fs = Eio.Stdenv.fs env
    let clock = Eio.Stdenv.clock env
    let worktree_mutex = Eio.Mutex.create ()
    let hook_mutex = Eio.Mutex.create ()
    let fetch_mutex = Eio.Mutex.create ()
    let project_name = "worker-recovery-git"
    let user_config = { User_config.on_worktree_create = None }
  end in
  let module C = struct
    let persist _ = Ok ()
    let record_session_id ~patch_id:_ ~session_id:_ = Ok ()
  end in
  let module WS = Worktree_setup.Make_with_checkpoint (C) (Real) (Env) in
  let request =
    B.Request B.{ base = "main"; policy = Rewrite; purpose = Reconcile_base }
  in
  Runtime.update_orchestrator runtime (fun orch ->
      fst (Orchestrator.reconcile_branch orch patch_id request));
  let reply result =
    let command =
      Runtime.read runtime (fun snap ->
          B.pending
            (Orchestrator.agent snap.Runtime.orchestrator patch_id)
              .Patch_agent.branch_reconcile)
    in
    match command with
    | Some command ->
        Runtime.update_orchestrator runtime (fun orch ->
            fst
              (Orchestrator.reconcile_branch orch patch_id
                 (B.Result { token = command.B.token; at = 1.; result })))
    | None -> failwith "missing old owner command"
  in
  reply
    (B.Observed
       B.
         {
           destination = Remote_id.of_destination origin;
           head = sha unpublished;
           source = sha unpublished;
           target = sha base;
           remote = Some (sha patch);
           boundary = Plain;
           topology = Includes;
           clean = true;
           sequencer = None;
           conflicts = 0;
           target_included = false;
           base_contains_source = false;
           completed_integration = false;
         });
  reply B.Pinned;
  reply (B.Integrated (sha unpublished));
  let recovered =
    Runtime.with_patch_ownership runtime ~patch_id (fun owner ->
        WS.recover_owned ~owner ~request:"loss-1" ~renewal_id:None ())
  in
  (match recovered with
  | Worktree_setup.Recovery_ready path ->
      check "adopted pushed HEAD"
        (String.equal (Git.git_capture ~cwd:path [ "rev-parse"; "HEAD" ]) patch);
      check "remote marker exists"
        (Stdlib.Sys.file_exists (Stdlib.Filename.concat path "pushed"));
      check "local-only marker absent"
        (not (Stdlib.Sys.file_exists (Stdlib.Filename.concat path "unpushed")))
  | Worktree_setup.Recovery_wait reason ->
      failwith ("pushed branch wait: " ^ reason)
  | Worktree_setup.Recovery_attention _ ->
      failwith "pushed branch requires renewal"
  | Worktree_setup.Recovery_merged _ -> failwith "pushed branch marked merged");
  check "old conversation not restored"
    (Runtime.read runtime (fun snap ->
         let a = Orchestrator.agent snap.Runtime.orchestrator patch_id in
         Option.is_none a.Patch_agent.llm_session_id));
  check "base observed"
    (String.equal
       (Git.git_capture ~cwd:fresh [ "rev-parse"; "origin/main" ])
       base);
  Git.run_git ~cwd:fresh [ "push"; "-q"; "origin"; "--delete"; branch ];
  let fresh2 = Stdlib.Filename.concat sandbox "fresh2" in
  clone ~origin ~dest:fresh2;
  check "second replacement cannot resolve deleted patch"
    (Git.git_exit_code ~cwd:fresh2 [ "cat-file"; "-e"; patch ] <> 0);
  let runtime2 =
    Onton_test_support.Branch_reconcile_git_fixture.make_runtime ()
  in
  let module Real2 =
    (val Worktree.make ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
           ~clock:(Eio.Stdenv.clock env)
           ~process_mgr:(Eio.Stdenv.process_mgr env)
           ~repo_root:fresh2)
  in
  let module Env2 = struct
    include Env

    let runtime = runtime2
    let project_name = "worker-recovery-absent"
  end in
  let module WS2 = Worktree_setup.Make_with_checkpoint (C) (Real2) (Env2) in
  Runtime.update_orchestrator runtime2 (fun orch ->
      fst
        (Orchestrator.reconcile_branch orch patch_id
           (B.Request
              B.
                {
                  base = "main";
                  policy = Rewrite;
                  purpose = Provision_checkout "first-push";
                })));
  (match
     Runtime.with_patch_ownership runtime2 ~patch_id (fun owner ->
         WS2.recover_owned ~owner ~request:"never-published" ~renewal_id:None ())
   with
  | Worktree_setup.Recovery_ready path ->
      check "never-published branch starts at confirmed base"
        (String.equal (Git.git_capture ~cwd:path [ "rev-parse"; "HEAD" ]) base)
  | Worktree_setup.Recovery_wait reason ->
      let owner =
        Runtime.read runtime2 (fun snap ->
            (Orchestrator.agent snap.Runtime.orchestrator patch_id)
              .Patch_agent.branch_reconcile)
      in
      failwith
        ("never-published branch wait: " ^ reason ^ "; request="
        ^ Option.value (B.workspace_request owner) ~default:"none"
        ^ "; pending="
        ^ string_of_bool (Option.is_some (B.pending owner)))
  | Worktree_setup.Recovery_attention _ ->
      failwith "never-published branch requested renewal"
  | Worktree_setup.Recovery_merged _ ->
      failwith "never-published branch reported merge");
  check "first-push branch remains absent before backend work"
    (String.equal
       (Git.git_capture ~cwd:fresh2
          [ "ls-remote"; "origin"; "refs/heads/" ^ branch ])
       "");
  let creates = ref 0 in
  let module Offline = struct
    include Real2

    let create ~project_name ~patch_id ~branch ~base_ref =
      incr creates;
      Real2.create ~project_name ~patch_id ~branch ~base_ref

    let reconcile ~path ~project_name ~branch ~operation command =
      if operation.B.lost_workspace then
        B.Retryable { reason = "remote probe unavailable"; retry_after = None }
      else Real2.reconcile ~path ~project_name ~branch ~operation command
  end in
  let runtime3 =
    Onton_test_support.Branch_reconcile_git_fixture.make_runtime ()
  in
  let module Env3 = struct
    include Env

    let runtime = runtime3
    let project_name = "worker-recovery-offline"
  end in
  let module WS3 = Worktree_setup.Make_with_checkpoint (C) (Offline) (Env3) in
  Runtime.update_orchestrator runtime3 (fun orch ->
      fst (Orchestrator.reconcile_branch orch patch_id request));
  (match
     Runtime.with_patch_ownership runtime3 ~patch_id (fun owner ->
         WS3.recover_owned ~owner ~request:"offline" ~renewal_id:None ())
   with
  | Worktree_setup.Recovery_wait reason ->
      check "failed probe reason retained"
        (String.equal reason "remote probe unavailable")
  | Worktree_setup.Recovery_ready _ | Worktree_setup.Recovery_attention _
  | Worktree_setup.Recovery_merged _ ->
      failwith "failed probe granted recovery");
  check "failed probe did not create checkout" (!creates = 0)

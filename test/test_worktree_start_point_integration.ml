(* @archlint.module test
   @archlint.domain session-meta *)

open Base
open Onton
open Onton_core
module Git_env = Onton_test_support.Git_env

(** Integration test: drive [Worktree.create] against real git fixtures to cover
    every {!Start_point_plan} arm.

    Builds two scratch repositories per scenario — an "origin" bare repo and a
    "managed" clone — populated to produce the precise local/remote-ref state
    the test scenario needs:

    - "brand new" — neither [refs/heads/<branch>] nor
      [refs/remotes/origin/<branch>] exists in the managed clone. Expected
      action: [Create_new_branch_from_base].
    - "remote ahead of stale local" — local ref exists at the base (the PR #315
      scenario: stale local left behind on a working clone); remote ref has
      commits the local one doesn't. Expected action:
      [Reset_and_use_remote_tracking]; worktree HEAD must equal remote.
    - "local only" — local branch exists, no remote ref. Expected action:
      [Use_local_branch_unchanged].
    - "local diverged" — local has commits remote doesn't, and remote also has
      commits local doesn't. Expected refusal: [Local_diverged_from_remote].
    - "local strictly ahead" — local has commits not on remote (no diverge).
      Expected refusal: [Local_has_unpushed_commits].

    Every git command runs against the real binary; no mocks. *)

let with_temp_dir f =
  let dir =
    Stdlib.Filename.concat
      (Stdlib.Filename.get_temp_dir_name ())
      (Printf.sprintf "onton-start-point-%d-%d" (Unix.getpid ())
         (Random.bits ()))
  in
  Unix.mkdir dir 0o755;
  (* Worktree paths derive from $HOME (see [Worktree.worktree_dir]); redirect
     HOME into the temp dir so the worktree lives inside our sandbox. Restore
     it before the next scenario runs; otherwise a failed/aborted scenario can
     strand later ones on the previous temp HOME and reuse stale worktrees. *)
  let prior_home = Stdlib.Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" dir;
  Stdlib.Fun.protect
    ~finally:(fun () ->
      (match prior_home with
      | Some h -> Unix.putenv "HOME" h
      | None ->
          (* HOME was unset on entry; leaving it pointed at the temp dir we are
             about to delete would strand later [worktree_dir] lookups on a
             dangling path. Point it at a directory that exists. *)
          Unix.putenv "HOME" (Stdlib.Filename.get_temp_dir_name ()));
      try
        Git_env.sh ~dir:"/"
          (Printf.sprintf "rm -rf %s" (Stdlib.Filename.quote dir))
      with _ -> ())
    (fun () -> f dir)

(* Delegate to the scrubbed-env helpers in {!Onton_test_support.Git_env}. These
   fixtures run under [dune runtest], which the pre-commit hook invokes with the
   host repo's [GIT_DIR]/[GIT_INDEX_FILE]/[GIT_WORK_TREE] exported into the
   environment; spawning git with the ambient env let those vars redirect a
   sandbox commit onto the host worktree (see lib/git_env.mli). *)
let sh ?(dir = ".") cmd = Git_env.sh ~dir cmd
let git_capture ?(dir = ".") args = Git_env.git_capture ~cwd:dir args

let setup_origin_with_main ~origin_dir =
  Unix.mkdir origin_dir 0o755;
  sh ~dir:origin_dir "git init -q --initial-branch=main";
  sh ~dir:origin_dir "git config user.email 'test@example.com'";
  sh ~dir:origin_dir "git config user.name 'Test'";
  sh ~dir:origin_dir "echo base > README.md";
  sh ~dir:origin_dir "git add README.md";
  sh ~dir:origin_dir "git commit -q -m 'base'"

let clone_into ~origin_dir ~managed_dir =
  sh
    (Printf.sprintf "git clone -q %s %s"
       (Stdlib.Filename.quote origin_dir)
       (Stdlib.Filename.quote managed_dir));
  sh ~dir:managed_dir "git config user.email 'test@example.com'";
  sh ~dir:managed_dir "git config user.name 'Test'"

let read_worktree_head ~managed_dir =
  git_capture ~dir:managed_dir [ "rev-parse"; "HEAD" ]

let assert_string label want got =
  if not (String.equal want got) then
    failwith (Printf.sprintf "%s: expected %S got %S" label want got)

let assert_true label cond =
  if not cond then failwith (Printf.sprintf "%s: assertion failed" label)

let scenario_brand_new env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  (* No "feat" branch anywhere. *)
  let res =
    Worktree.create ~process_mgr ~repo_root:managed_dir
      ~project_name:"brand-new"
      ~patch_id:(Types.Patch_id.of_string "1")
      ~branch:(Types.Branch.of_string "feat")
      ~base_ref:"origin/main"
  in
  match res with
  | Result.Ok wt ->
      let head = read_worktree_head ~managed_dir:wt.path in
      let main = git_capture ~dir:managed_dir [ "rev-parse"; "origin/main" ] in
      assert_string "brand_new: worktree HEAD == origin/main" main head;
      Stdlib.print_endline "  brand_new: OK"
  | Result.Error r ->
      failwith
        (Printf.sprintf "brand_new: expected Ok, got Error %s"
           (Start_point_plan.show_refusal r))

let scenario_remote_ahead_of_stale_local env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  (* Origin has the feat branch with extra commits. *)
  sh ~dir:origin_dir "git checkout -q -b feat";
  sh ~dir:origin_dir "echo work > work.txt";
  sh ~dir:origin_dir "git add work.txt";
  sh ~dir:origin_dir "git commit -q -m 'work'";
  let remote_feat_sha = git_capture ~dir:origin_dir [ "rev-parse"; "HEAD" ] in
  sh ~dir:origin_dir "git checkout -q main";
  clone_into ~origin_dir ~managed_dir;
  (* Some git clone modes only materialize the remote HEAD tracking ref in the
     managed clone. Force origin/feat to exist so this scenario asserts the
     stale-local-vs-remote-ahead branch selection, not clone transport quirks. *)
  sh ~dir:managed_dir "git fetch -q origin feat:refs/remotes/origin/feat";
  (* Set up the stale local branch — points at main (the PR base), missing
     the work commit. This mirrors the #315 state. *)
  let main_sha = git_capture ~dir:managed_dir [ "rev-parse"; "origin/main" ] in
  sh ~dir:managed_dir
    (Printf.sprintf "git branch feat %s" (Stdlib.Filename.quote main_sha));
  (* Verify the precondition: local feat == origin/main, NOT origin/feat. *)
  let local_feat_pre = git_capture ~dir:managed_dir [ "rev-parse"; "feat" ] in
  let remote_feat_pre =
    git_capture ~dir:managed_dir [ "rev-parse"; "origin/feat" ]
  in
  assert_string "precondition: local feat is stale" main_sha local_feat_pre;
  assert_string "precondition: remote feat is ahead" remote_feat_sha
    remote_feat_pre;
  let res =
    Worktree.create ~process_mgr ~repo_root:managed_dir
      ~project_name:"stale-local"
      ~patch_id:(Types.Patch_id.of_string "2")
      ~branch:(Types.Branch.of_string "feat")
      ~base_ref:"origin/main"
  in
  match res with
  | Result.Ok wt ->
      let head = read_worktree_head ~managed_dir:wt.path in
      assert_string "stale_local: worktree HEAD == remote feat" remote_feat_sha
        head;
      let local_feat_post =
        git_capture ~dir:managed_dir [ "rev-parse"; "feat" ]
      in
      assert_string "stale_local: refs/heads/feat reset to remote"
        remote_feat_sha local_feat_post;
      Stdlib.print_endline "  remote_ahead_of_stale_local: OK"
  | Result.Error r ->
      failwith
        (Printf.sprintf "stale_local: expected Ok, got Error %s"
           (Start_point_plan.show_refusal r))

let scenario_local_only env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  clone_into ~origin_dir ~managed_dir;
  (* Local branch only — no origin/feat. *)
  sh ~dir:managed_dir "git checkout -q -b feat origin/main";
  sh ~dir:managed_dir "echo local > local.txt";
  sh ~dir:managed_dir "git add local.txt";
  sh ~dir:managed_dir "git commit -q -m 'local'";
  let local_feat = git_capture ~dir:managed_dir [ "rev-parse"; "feat" ] in
  sh ~dir:managed_dir "git checkout -q main";
  let res =
    Worktree.create ~process_mgr ~repo_root:managed_dir
      ~project_name:"local-only"
      ~patch_id:(Types.Patch_id.of_string "3")
      ~branch:(Types.Branch.of_string "feat")
      ~base_ref:"origin/main"
  in
  match res with
  | Result.Ok wt ->
      let head = read_worktree_head ~managed_dir:wt.path in
      assert_string "local_only: worktree HEAD == local feat" local_feat head;
      Stdlib.print_endline "  local_only: OK"
  | Result.Error r ->
      failwith
        (Printf.sprintf "local_only: expected Ok, got Error %s"
           (Start_point_plan.show_refusal r))

let scenario_diverged env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  sh ~dir:origin_dir "git checkout -q -b feat";
  sh ~dir:origin_dir "echo r > r.txt";
  sh ~dir:origin_dir "git add r.txt";
  sh ~dir:origin_dir "git commit -q -m 'remote commit'";
  sh ~dir:origin_dir "git checkout -q main";
  clone_into ~origin_dir ~managed_dir;
  (* Build a diverged local: branch off main, commit something different. *)
  sh ~dir:managed_dir "git checkout -q -b feat origin/main";
  sh ~dir:managed_dir "echo l > l.txt";
  sh ~dir:managed_dir "git add l.txt";
  sh ~dir:managed_dir "git commit -q -m 'local commit'";
  sh ~dir:managed_dir "git checkout -q main";
  let res =
    Worktree.create ~process_mgr ~repo_root:managed_dir ~project_name:"diverged"
      ~patch_id:(Types.Patch_id.of_string "4")
      ~branch:(Types.Branch.of_string "feat")
      ~base_ref:"origin/main"
  in
  match res with
  | Result.Ok _ -> failwith "diverged: expected Error refusal, got Ok"
  | Result.Error r -> (
      match r with
      | Start_point_plan.Local_diverged_from_remote _ ->
          Stdlib.print_endline "  diverged: OK (refused)"
      | Start_point_plan.Local_has_unpushed_commits _
      | Start_point_plan.Branch_checked_out_in_main_root
      | Start_point_plan.Worktree_already_registered _ ->
          failwith
            (Printf.sprintf
               "diverged: expected Local_diverged_from_remote, got %s"
               (Start_point_plan.show_refusal r)))

let scenario_local_strictly_ahead env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  sh ~dir:origin_dir "git checkout -q -b feat";
  sh ~dir:origin_dir "echo r > r.txt";
  sh ~dir:origin_dir "git add r.txt";
  sh ~dir:origin_dir "git commit -q -m 'shared commit'";
  let shared_sha = git_capture ~dir:origin_dir [ "rev-parse"; "HEAD" ] in
  sh ~dir:origin_dir "git checkout -q main";
  clone_into ~origin_dir ~managed_dir;
  (* Non-bare clone defaults do not reliably materialize non-HEAD remote
     branches as remote-tracking refs. Fetch [feat] explicitly so the scenario
     deterministically exercises the local-ahead-vs-remote planner branch
     instead of the separate local-only path. *)
  sh ~dir:managed_dir "git fetch -q origin feat:refs/remotes/origin/feat";
  (* Build a local that strictly contains origin/feat plus extra commits. *)
  sh ~dir:managed_dir
    (Printf.sprintf "git checkout -q -b feat %s"
       (Stdlib.Filename.quote shared_sha));
  sh ~dir:managed_dir "echo extra > extra.txt";
  sh ~dir:managed_dir "git add extra.txt";
  sh ~dir:managed_dir "git commit -q -m 'local extra'";
  sh ~dir:managed_dir "git checkout -q main";
  (* Sanity-check the precondition: local-ahead, not diverged. *)
  let local = git_capture ~dir:managed_dir [ "rev-parse"; "feat" ] in
  let remote = git_capture ~dir:managed_dir [ "rev-parse"; "origin/feat" ] in
  let is_remote_ancestor_of_local =
    Git_env.git_exit_code ~cwd:managed_dir
      [ "merge-base"; "--is-ancestor"; remote; local ]
    = 0
  in
  assert_true "precondition: remote is ancestor of local"
    is_remote_ancestor_of_local;
  let res =
    Worktree.create ~process_mgr ~repo_root:managed_dir
      ~project_name:"local-ahead"
      ~patch_id:(Types.Patch_id.of_string "5")
      ~branch:(Types.Branch.of_string "feat")
      ~base_ref:"origin/main"
  in
  match res with
  | Result.Ok _ ->
      failwith "local_strictly_ahead: expected Error refusal, got Ok"
  | Result.Error r -> (
      match r with
      | Start_point_plan.Local_has_unpushed_commits _ ->
          Stdlib.print_endline "  local_strictly_ahead: OK (refused)"
      | Start_point_plan.Local_diverged_from_remote _
      | Start_point_plan.Branch_checked_out_in_main_root
      | Start_point_plan.Worktree_already_registered _ ->
          failwith
            (Printf.sprintf
               "local_strictly_ahead: expected Local_has_unpushed_commits, got \
                %s"
               (Start_point_plan.show_refusal r)))

(* Part B regression, end-to-end against real git:

   A→B→C stacking. Patch B's branch is cut from main *before* patch A merges to
   main. Then A merges (origin/main advances to include A). We now cut a
   brand-new branch C from B while origin/main is NOT an ancestor of B's
   branch. This must SUCCEED: base freshness vs main is dependency-scoped and
   enforced by the orchestrator's scheduling gate, not here. A dependent cut
   from B's tip contains everything in B; main advancing for unrelated reasons
   must not block the cut (the previous main-scoped veto livelocked the run —
   main moves faster than rebase-and-restart). The new worktree must contain
   B's commit; it need not contain A's. *)
let scenario_base_stale_vs_main_now_allowed env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  (* Patch B's branch is cut from the original main tip and pushed. *)
  sh ~dir:origin_dir "git checkout -q -b patch-b";
  sh ~dir:origin_dir "echo b > b.txt";
  sh ~dir:origin_dir "git add b.txt";
  sh ~dir:origin_dir "git commit -q -m 'patch b work'";
  sh ~dir:origin_dir "git checkout -q main";
  (* Patch A merges to main AFTER B was cut: main now has a commit that B's
     branch does not contain. *)
  sh ~dir:origin_dir "echo a > a.txt";
  sh ~dir:origin_dir "git add a.txt";
  sh ~dir:origin_dir "git commit -q -m 'patch a merged to main'";
  clone_into ~origin_dir ~managed_dir;
  (* Precondition: origin/main is NOT an ancestor of origin/patch-b. *)
  let stale =
    match
      Git_env.git_exit_code ~cwd:managed_dir
        [ "merge-base"; "--is-ancestor"; "origin/main"; "origin/patch-b" ]
    with
    | 1 -> true
    | 0 -> false
    | n ->
        failwith
          (Printf.sprintf
             "precondition: merge-base --is-ancestor failed with exit %d" n)
  in
  assert_true "precondition: origin/main not ancestor of origin/patch-b" stale;
  let res =
    Worktree.create ~process_mgr ~repo_root:managed_dir
      ~project_name:"base-stale"
      ~patch_id:(Types.Patch_id.of_string "c")
      ~branch:(Types.Branch.of_string "patch-c")
      ~base_ref:"origin/patch-b"
  in
  match res with
  | Result.Ok wt ->
      (* The new worktree must contain patch B's commit (b.txt), proving it was
         cut from B's tip; it need not (and does not) contain A's a.txt. *)
      let has_file name =
        Stdlib.Sys.command
          (Printf.sprintf "test -f %s"
             (Stdlib.Filename.quote (Stdlib.Filename.concat wt.path name)))
        = 0
      in
      assert_true "stale-vs-main: worktree contains base commit"
        (has_file "b.txt");
      Stdlib.print_endline "  base_stale_vs_main_now_allowed: OK (created)"
  | Result.Error r ->
      failwith
        (Printf.sprintf
           "base_stale_vs_main_now_allowed: expected Ok (main-freshness must \
            not block a stacked cut), got refusal %s"
           (Start_point_plan.show_refusal r))

(* The complement: a stacked base that legitimately CONTAINS the latest main
   must NOT be refused. Patch B's branch is cut from main and then main does
   not advance past it, so origin/main is an ancestor of origin/patch-b. C must
   be created successfully. Guards against the freshness gate over-blocking
   normal stacked work. *)
let scenario_base_fresh_stacked env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin_with_main ~origin_dir;
  sh ~dir:origin_dir "git checkout -q -b patch-b";
  sh ~dir:origin_dir "echo b > b.txt";
  sh ~dir:origin_dir "git add b.txt";
  sh ~dir:origin_dir "git commit -q -m 'patch b work'";
  sh ~dir:origin_dir "git checkout -q main";
  clone_into ~origin_dir ~managed_dir;
  let fresh =
    Git_env.git_exit_code ~cwd:managed_dir
      [ "merge-base"; "--is-ancestor"; "origin/main"; "origin/patch-b" ]
    = 0
  in
  assert_true "precondition: origin/main IS ancestor of origin/patch-b" fresh;
  let res =
    Worktree.create ~process_mgr ~repo_root:managed_dir
      ~project_name:"base-fresh"
      ~patch_id:(Types.Patch_id.of_string "c2")
      ~branch:(Types.Branch.of_string "patch-c2")
      ~base_ref:"origin/patch-b"
  in
  match res with
  | Result.Ok wt ->
      (* The new worktree must contain patch B's commit (b.txt). *)
      let head_has_b =
        Stdlib.Sys.command
          (Printf.sprintf "test -f %s"
             (Stdlib.Filename.quote (Stdlib.Filename.concat wt.path "b.txt")))
        = 0
      in
      assert_true "fresh stacked: worktree contains base commit" head_has_b;
      Stdlib.print_endline "  base_fresh_stacked: OK (created)"
  | Result.Error r ->
      failwith
        (Printf.sprintf "base_fresh_stacked: expected Ok, got refusal %s"
           (Start_point_plan.show_refusal r))

let () =
  Eio_main.run @@ fun env ->
  Stdlib.print_endline "Worktree.create + Start_point_plan integration:";
  scenario_brand_new env;
  scenario_remote_ahead_of_stale_local env;
  scenario_local_only env;
  scenario_diverged env;
  scenario_local_strictly_ahead env;
  scenario_base_stale_vs_main_now_allowed env;
  scenario_base_fresh_stacked env;
  Stdlib.print_endline "All start-point integration scenarios passed."

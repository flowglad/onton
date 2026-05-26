open Base
open Onton
open Onton_core

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
     HOME into the temp dir so the worktree lives inside our sandbox and is
     wiped by at_exit. Without this, [~/worktrees/<project>/patch-<id>]
     persists across scenarios and the [Sys.file_exists path] short-circuit
     in [Worktree.create] returns a stale Ok from a previous run. *)
  let prior_home = Stdlib.Sys.getenv_opt "HOME" in
  Unix.putenv "HOME" dir;
  Stdlib.at_exit (fun () ->
      (match prior_home with Some h -> Unix.putenv "HOME" h | None -> ());
      try
        let _ =
          Stdlib.Sys.command
            (Printf.sprintf "rm -rf %s" (Stdlib.Filename.quote dir))
        in
        ()
      with _ -> ());
  f dir

let sh ?(dir = ".") cmd =
  let full = Printf.sprintf "cd %s && %s" (Stdlib.Filename.quote dir) cmd in
  let code = Stdlib.Sys.command full in
  if code <> 0 then
    failwith (Printf.sprintf "command failed (exit %d): %s" code full)

let git_capture ?(dir = ".") args =
  let argstr =
    String.concat ~sep:" " (List.map ~f:Stdlib.Filename.quote args)
  in
  let cmd =
    Printf.sprintf "cd %s && git %s" (Stdlib.Filename.quote dir) argstr
  in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 128 in
  (try
     while true do
       Stdlib.Buffer.add_channel buf ic 4096
     done
   with End_of_file -> ());
  let _ = Unix.close_process_in ic in
  String.strip (Buffer.contents buf)

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
      ~base_ref:"origin/main" ~main_branch:"main"
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
  (* Set up the stale local branch — points at main (the PR base), missing
     the work commit. This mirrors the #315 state. *)
  let main_sha = git_capture ~dir:managed_dir [ "rev-parse"; "origin/main" ] in
  sh ~dir:managed_dir
    (Printf.sprintf "git branch feat %s" (Stdlib.Filename.quote main_sha));
  (* Verify the precondition: local feat == origin/main, NOT origin/feat. *)
  let local_feat_pre = git_capture ~dir:managed_dir [ "rev-parse"; "feat" ] in
  assert_string "precondition: local feat is stale" main_sha local_feat_pre;
  let res =
    Worktree.create ~process_mgr ~repo_root:managed_dir
      ~project_name:"stale-local"
      ~patch_id:(Types.Patch_id.of_string "2")
      ~branch:(Types.Branch.of_string "feat")
      ~base_ref:"origin/main" ~main_branch:"main"
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
      ~base_ref:"origin/main" ~main_branch:"main"
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
      ~base_ref:"origin/main" ~main_branch:"main"
  in
  match res with
  | Result.Ok _ -> failwith "diverged: expected Error refusal, got Ok"
  | Result.Error r -> (
      match r with
      | Start_point_plan.Local_diverged_from_remote _ ->
          Stdlib.print_endline "  diverged: OK (refused)"
      | Start_point_plan.Local_has_unpushed_commits _
      | Start_point_plan.Branch_checked_out_in_main_root
      | Start_point_plan.Worktree_already_registered _
      | Start_point_plan.Base_branch_stale_vs_main _ ->
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
    let cmd =
      Printf.sprintf "cd %s && git merge-base --is-ancestor %s %s"
        (Stdlib.Filename.quote managed_dir)
        (Stdlib.Filename.quote remote)
        (Stdlib.Filename.quote local)
    in
    Stdlib.Sys.command cmd = 0
  in
  assert_true "precondition: remote is ancestor of local"
    is_remote_ancestor_of_local;
  let res =
    Worktree.create ~process_mgr ~repo_root:managed_dir
      ~project_name:"local-ahead"
      ~patch_id:(Types.Patch_id.of_string "5")
      ~branch:(Types.Branch.of_string "feat")
      ~base_ref:"origin/main" ~main_branch:"main"
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
      | Start_point_plan.Worktree_already_registered _
      | Start_point_plan.Base_branch_stale_vs_main _ ->
          failwith
            (Printf.sprintf
               "local_strictly_ahead: expected Local_has_unpushed_commits, got \
                %s"
               (Start_point_plan.show_refusal r)))

(* The event-stream-pages witness, end-to-end against real git:

   A→B→C stacking. Patch B's branch is cut from main *before* patch A merges to
   main. Then A merges (origin/main advances to include A). We now try to cut a
   brand-new branch C from B. Because origin/main is NOT an ancestor of B's
   branch, [Worktree.create] must refuse with [Base_branch_stale_vs_main]
   rather than silently producing a worktree missing A's commit. *)
let scenario_base_stale_vs_main env =
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
    let cmd =
      Printf.sprintf
        "cd %s && git merge-base --is-ancestor origin/main origin/patch-b"
        (Stdlib.Filename.quote managed_dir)
    in
    Stdlib.Sys.command cmd <> 0
  in
  assert_true "precondition: origin/main not ancestor of origin/patch-b" stale;
  let res =
    Worktree.create ~process_mgr ~repo_root:managed_dir
      ~project_name:"base-stale"
      ~patch_id:(Types.Patch_id.of_string "c")
      ~branch:(Types.Branch.of_string "patch-c")
      ~base_ref:"origin/patch-b" ~main_branch:"main"
  in
  match res with
  | Result.Ok _ ->
      failwith
        "base_stale: expected Error (Base_branch_stale_vs_main), got Ok — a \
         worktree cut from a stale dep branch would silently drop main's \
         commits"
  | Result.Error (Start_point_plan.Base_branch_stale_vs_main _) ->
      Stdlib.print_endline "  base_stale_vs_main: OK (refused)"
  | Result.Error
      (( Start_point_plan.Local_diverged_from_remote _
       | Start_point_plan.Local_has_unpushed_commits _
       | Start_point_plan.Branch_checked_out_in_main_root
       | Start_point_plan.Worktree_already_registered _ ) as r) ->
      failwith
        (Printf.sprintf "base_stale: expected Base_branch_stale_vs_main, got %s"
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
    let cmd =
      Printf.sprintf
        "cd %s && git merge-base --is-ancestor origin/main origin/patch-b"
        (Stdlib.Filename.quote managed_dir)
    in
    Stdlib.Sys.command cmd = 0
  in
  assert_true "precondition: origin/main IS ancestor of origin/patch-b" fresh;
  let res =
    Worktree.create ~process_mgr ~repo_root:managed_dir
      ~project_name:"base-fresh"
      ~patch_id:(Types.Patch_id.of_string "c2")
      ~branch:(Types.Branch.of_string "patch-c2")
      ~base_ref:"origin/patch-b" ~main_branch:"main"
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
  scenario_base_stale_vs_main env;
  scenario_base_fresh_stacked env;
  Stdlib.print_endline "All start-point integration scenarios passed."

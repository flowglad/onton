(* @archlint.module test
   @archlint.domain start-point-plan *)

open Base
open Onton
open Onton_core
module Git_env = Onton_test_support.Git_env

let check label condition = if not condition then failwith label

(** Git-contract test for the two effectful primitives that feed
    {!Start_point_plan.plan} from a real repository:
    {!Worktree.read_repo_ref_sha} and {!Worktree.compute_repo_ancestry}.

    These are the inputs that {!Worktree.create} reads before deciding a
    worktree's start point. The {e decision} those inputs drive is covered
    purely in {!test_start_point_plan_properties}, and the {e wiring} from
    inputs to executed action is covered with in-memory fakes in
    {!test_worktree_create_wiring}. What neither of those can verify is that the
    git commands actually return the SHAs and ancestry classification the fakes
    assume — that contract is what this test pins, against a real commit graph.

    Ref observations distinguish verified absence from symbolic refs, non-commit
    objects and failed probes. Ancestry failures stay unknown. The owned
    provisioning-to-publication contract is exercised separately by
    [test_worktree_setup_base_fetch_integration]. *)

let assert_sha_opt label ~want got =
  let show = function Some s -> Printf.sprintf "Some %S" s | None -> "None" in
  if not (Option.equal String.equal want got) then
    failwith
      (Printf.sprintf "%s: expected %s got %s" label (show want) (show got))

let assert_ancestry label ~want got =
  if not (Start_point_plan.equal_ancestry want got) then
    failwith
      (Printf.sprintf "%s: expected %s got %s" label
         (Start_point_plan.show_ancestry want)
         (Start_point_plan.show_ancestry got))

(* Commit [file]'s content in [dir] and return the resulting HEAD SHA. A real
   file change keeps trees (and therefore SHAs) distinct across commits. *)
let commit ~dir ~file ~content ~msg =
  Git_env.sh ~dir (Printf.sprintf "printf %s > %s" content file);
  Git_env.run_git ~cwd:dir [ "add"; file ];
  Git_env.run_git ~cwd:dir [ "commit"; "-q"; "-m"; msg ];
  Git_env.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let () =
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  Git_env.with_temp_repo @@ fun dir ->
  Stdlib.print_endline "Worktree git-read contract:";
  (* main: base -> ahead. [base] is a strict ancestor of [ahead]. *)
  let base_sha = commit ~dir ~file:"f.txt" ~content:"base" ~msg:"base" in
  let ahead_sha = commit ~dir ~file:"f.txt" ~content:"work" ~msg:"work" in
  (* A STALE local [feat] left at [base] — the PR #315 state. Use a real
     [git branch] under [refs/heads], not a fabricated remote-tracking ref:
     resolving a [refs/remotes/origin] ref created by [update-ref] without a
     configured [origin] is rejected by older git, and is irrelevant anyway —
     [read_repo_ref_sha] is just [rev-parse --verify <name>] and is
     namespace-agnostic, so real branch refs exercise its full contract. *)
  Git_env.run_git ~cwd:dir [ "branch"; "feat"; base_sha ];
  (* Two commits diverging off [base] (each adds a different file). *)
  Git_env.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "ldiv"; base_sha ];
  let l_sha = commit ~dir ~file:"l.txt" ~content:"l" ~msg:"L" in
  Git_env.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "rdiv"; base_sha ];
  let r_sha = commit ~dir ~file:"r.txt" ~content:"r" ~msg:"R" in

  let read ref_name =
    match Worktree.read_repo_ref_sha ~process_mgr ~repo_root:dir ~ref_name with
    | Ok revision -> revision
    | Error reason -> failwith reason
  in
  let ancestry ~local ~remote =
    Worktree.compute_repo_ancestry ~process_mgr ~repo_root:dir ~local ~remote
  in

  (* read_repo_ref_sha resolves existing refs to their SHA and reports absence
     as None. [main] is at [ahead] (two commits); [feat] is the stale branch. *)
  assert_sha_opt "read heads/feat (stale)" ~want:(Some base_sha)
    (read "refs/heads/feat");
  assert_sha_opt "read heads/main (ahead)" ~want:(Some ahead_sha)
    (read "refs/heads/main");
  assert_sha_opt "read absent ref is None" ~want:None
    (read "refs/heads/does-not-exist");
  Stdlib.print_endline "  read_repo_ref_sha: OK";

  (* compute_repo_ancestry classifies all four determinate relationships. The
     PR #315 case is [Remote_ahead]: a stale local ([base]) must be detected as
     behind the remote tip ([ahead]) so the planner resets to it. The SHAs are
     passed directly — the function takes commit SHAs, not ref names. *)
  assert_ancestry "stale local vs ahead remote"
    ~want:Start_point_plan.Remote_ahead
    (ancestry ~local:base_sha ~remote:ahead_sha);
  assert_ancestry "ahead local vs stale remote"
    ~want:Start_point_plan.Local_ahead
    (ancestry ~local:ahead_sha ~remote:base_sha);
  assert_ancestry "identical refs" ~want:Start_point_plan.Equal
    (ancestry ~local:ahead_sha ~remote:ahead_sha);
  assert_ancestry "diverged" ~want:Start_point_plan.Diverged
    (ancestry ~local:l_sha ~remote:r_sha);
  Stdlib.print_endline "  compute_repo_ancestry: OK";

  Git_env.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "equiv-local"; base_sha ];
  let equiv_local =
    commit ~dir ~file:"patch.txt" ~content:"patch" ~msg:"equivalent patch"
  in
  Git_env.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "equiv-remote"; base_sha ];
  Git_env.run_git ~cwd:dir [ "commit"; "--allow-empty"; "-qm"; "rewrite base" ];
  Git_env.run_git ~cwd:dir [ "cherry-pick"; equiv_local ];
  let equiv_remote = Git_env.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ] in
  let preserves_local local remote =
    Start_point_plan.equal_decision
      (Start_point_plan.plan ~local_ref:(Some local) ~remote_ref:(Some remote)
         ~ancestry:(ancestry ~local ~remote) ~base_branch:"main"
         ~branch_checked_out_in_main_root:false ~existing_worktree_path:None)
      (Start_point_plan.Plan
         (Start_point_plan.Use_local_branch_unchanged { local_sha = local }))
  in
  check "equivalent divergence is retained for owned reconciliation"
    (preserves_local equiv_local equiv_remote);
  Git_env.run_git ~cwd:dir [ "revert"; "--no-edit"; equiv_remote ];
  let reverted_remote = Git_env.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ] in
  let cherry_after_revert =
    Git_env.git_capture ~cwd:dir [ "cherry"; reverted_remote; equiv_local ]
  in
  check "git cherry still reports equivalence after a remote revert"
    (String.is_prefix cherry_after_revert ~prefix:"-");
  check "remote revert cannot discard the equivalent local patch"
    (preserves_local equiv_local reverted_remote);
  check "distinct local history survives provisioning"
    (preserves_local l_sha r_sha);
  let fails ref_name =
    match Worktree.read_repo_ref_sha ~process_mgr ~repo_root:dir ~ref_name with
    | Error _ -> true
    | Ok _ -> false
  in
  Git_env.run_git ~cwd:dir
    [ "symbolic-ref"; "refs/heads/symbolic"; "refs/heads/missing-target" ];
  check "dangling symbolic ref is not absence" (fails "refs/heads/symbolic");
  Git_env.run_git ~cwd:dir
    [ "tag"; "-a"; "-m"; "tag object"; "tag-object"; equiv_local ];
  check "tag object is not a branch commit" (fails "refs/tags/tag-object");
  check "failed repository probe is not absence"
    (match
       Worktree.read_repo_ref_sha ~process_mgr ~repo_root:(dir ^ "/missing")
         ~ref_name:"refs/heads/main"
     with
    | Error _ -> true
    | Ok _ -> false);
  check "failed ancestry probe stays unknown"
    (Start_point_plan.equal_ancestry
       (ancestry ~local:l_sha ~remote:(String.make 40 'f'))
       Start_point_plan.Unknown);
  Stdlib.print_endline "All git-read contract checks passed."

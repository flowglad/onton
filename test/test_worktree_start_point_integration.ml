(* @archlint.module test
   @archlint.domain start-point-plan *)

open Base
open Onton
open Onton_core
module Git_env = Onton_test_support.Git_env

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

    Deliberately scoped to the ref/ancestry reads only: it builds the graph with
    [git update-ref] in a single repo and never creates a worktree, redirects
    [$HOME], or touches the [Sys.file_exists] short-circuit. The previous
    version drove [Worktree.create] end-to-end across seven HOME-sandboxed
    scenarios; that machinery proved sensitive to the CI git/filesystem
    environment (passing locally, failing in CI) without adding coverage beyond
    the pure planner and the wiring fakes. The irreducible live-git surface —
    "do [rev-parse]/[merge-base --is-ancestor] mean what we think?" — is kept
    minimal and deterministic here. *)

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
  (* Two commits diverging off [base] (each adds a different file). *)
  Git_env.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "ldiv"; base_sha ];
  let l_sha = commit ~dir ~file:"l.txt" ~content:"l" ~msg:"L" in
  Git_env.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "rdiv"; base_sha ];
  let r_sha = commit ~dir ~file:"r.txt" ~content:"r" ~msg:"R" in
  (* Model the PR #315 state directly: a STALE local [feat] left at [base], and
     a remote-tracking [origin/feat] that is AHEAD at [ahead]. Set with
     [update-ref] so there is no clone-materialization step to be flaky. *)
  Git_env.run_git ~cwd:dir [ "update-ref"; "refs/heads/feat"; base_sha ];
  Git_env.run_git ~cwd:dir
    [ "update-ref"; "refs/remotes/origin/feat"; ahead_sha ];

  let read ref_name =
    Worktree.read_repo_ref_sha ~process_mgr ~repo_root:dir ~ref_name
  in
  let ancestry ~local ~remote =
    Worktree.compute_repo_ancestry ~process_mgr ~repo_root:dir ~local ~remote
  in

  (* read_repo_ref_sha resolves both ref namespaces and reports absence. *)
  assert_sha_opt "read local heads/feat (stale)" ~want:(Some base_sha)
    (read "refs/heads/feat");
  assert_sha_opt "read remote origin/feat (ahead)" ~want:(Some ahead_sha)
    (read "refs/remotes/origin/feat");
  assert_sha_opt "read absent ref is None" ~want:None
    (read "refs/heads/does-not-exist");
  Stdlib.print_endline "  read_repo_ref_sha: OK";

  (* compute_repo_ancestry classifies all four determinate relationships. The
     PR #315 case is [Remote_ahead]: a stale local must be detected as behind
     the remote so the planner resets to it. *)
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
  Stdlib.print_endline "All git-read contract checks passed."

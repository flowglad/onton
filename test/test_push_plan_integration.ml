(* @archlint.module test
   @archlint.domain push-plan *)

open Base
open Onton
open Onton_core
module Git_env = Onton_test_support.Git_env

(** Integration test: drive [Worktree.force_push_with_lease] against real git
    fixtures to cover the {!Push_plan} refusal arms that the unit/property tests
    can only assert structurally.

    Scenarios:

    - "branch_switched" — worktree HEAD is on a recovery branch, not the named
      branch the push command would target. Mirrors the codex workaround that
      put PR #315 in danger.
    - "local_missing_remote" — local branch is at an older commit than remote
      (force-push would wipe commits the local clone doesn't have).
    - "happy_path_force_push" — local includes remote, commits ahead of base.
      Push proceeds; the actual remote receives the local branch.

    Every git command runs against the real binary; no mocks. *)

let with_temp_dir f =
  let dir =
    Stdlib.Filename.concat
      (Stdlib.Filename.get_temp_dir_name ())
      (Printf.sprintf "onton-push-plan-%d-%d" (Unix.getpid ()) (Random.bits ()))
  in
  Unix.mkdir dir 0o755;
  Stdlib.at_exit (fun () ->
      try
        let _ =
          Stdlib.Sys.command
            (Printf.sprintf "rm -rf %s" (Stdlib.Filename.quote dir))
        in
        ()
      with _ -> ());
  f dir

(* Delegate to the scrubbed-env helpers in {!Onton_test_support.Git_env} so an
   inherited [GIT_*] var (e.g. from the pre-commit hook that runs [dune
   runtest]) cannot redirect these fixtures' git at the host repo. See
   lib_test/git_env.mli. *)
let sh ?(dir = ".") cmd = Git_env.sh ~dir cmd
let git_capture ?(dir = ".") args = Git_env.git_capture ~cwd:dir args

let setup_origin ~origin_dir =
  Unix.mkdir origin_dir 0o755;
  sh ~dir:origin_dir "git init -q --bare --initial-branch=main"

let setup_seed_clone ~origin_dir ~managed_dir =
  sh
    (Printf.sprintf "git clone -q %s %s"
       (Stdlib.Filename.quote origin_dir)
       (Stdlib.Filename.quote managed_dir));
  sh ~dir:managed_dir "git config user.email 'test@example.com'";
  sh ~dir:managed_dir "git config user.name 'Test'";
  sh ~dir:managed_dir "echo seed > seed.txt";
  sh ~dir:managed_dir "git add seed.txt";
  sh ~dir:managed_dir "git commit -q -m 'seed'";
  sh ~dir:managed_dir "git push -q -u origin main"

let scenario_branch_switched env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin.git" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin ~origin_dir;
  setup_seed_clone ~origin_dir ~managed_dir;
  (* Create the agent's branch with a real commit, then switch HEAD away to
     simulate a codex "git switch pr-X-review" mid-session. *)
  sh ~dir:managed_dir "git checkout -q -b feat";
  sh ~dir:managed_dir "echo work > work.txt";
  sh ~dir:managed_dir "git add work.txt";
  sh ~dir:managed_dir "git commit -q -m 'feat work'";
  sh ~dir:managed_dir "git checkout -q -b recovery";
  let outcome =
    Worktree.force_push_with_lease ~process_mgr ~path:managed_dir
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
  in
  (match outcome with
  | Worktree.Push_rejected (Push_reject_classify.Local_state_unsafe { reason })
    ->
      if String.equal reason "refuse_branch_switched" then
        Stdlib.print_endline "  branch_switched: OK (refused)"
      else
        failwith
          (Printf.sprintf
             "branch_switched: expected reason refuse_branch_switched, got %s"
             reason)
  | Worktree.Push_rejected
      ( Push_reject_classify.Workflow_scope_missing
      | Push_reject_classify.Branch_protection
      | Push_reject_classify.Push_pattern_block
      | Push_reject_classify.Lease_violation
      | Push_reject_classify.Hook_failure _ | Push_reject_classify.Unknown _ )
  | Worktree.Push_ok | Worktree.Push_up_to_date | Worktree.Push_no_commits
  | Worktree.Push_worktree_missing | Worktree.Push_error _ ->
      failwith
        (Printf.sprintf
           "branch_switched: expected Push_rejected Local_state_unsafe, got %s"
           (Worktree.show_push_result outcome)));
  (* Verify nothing was pushed to remote. *)
  let remote_has_feat =
    Git_env.git_exit_code ~cwd:managed_dir
      [ "ls-remote"; "--exit-code"; "origin"; "feat" ]
  in
  if remote_has_feat = 0 then
    failwith "branch_switched: remote received feat — push was not refused"

let scenario_local_missing_remote env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin.git" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin ~origin_dir;
  setup_seed_clone ~origin_dir ~managed_dir;
  sh ~dir:managed_dir "git checkout -q -b feat";
  sh ~dir:managed_dir "echo shared > work.txt";
  sh ~dir:managed_dir "git add work.txt";
  sh ~dir:managed_dir "git commit -q -m 'shared work'";
  let shared_feat_sha = git_capture ~dir:managed_dir [ "rev-parse"; "HEAD" ] in
  sh ~dir:managed_dir "git push -q -u origin feat";
  sh ~dir:managed_dir "echo remote > remote.txt";
  sh ~dir:managed_dir "git add remote.txt";
  sh ~dir:managed_dir "git commit -q -m 'remote work'";
  sh ~dir:managed_dir "git push -q -u origin feat";
  let remote_feat_sha = git_capture ~dir:managed_dir [ "rev-parse"; "HEAD" ] in
  (* Reset local feat to an older commit that is still ahead of base. This
     makes local a strict ancestor of origin/feat while keeping
     commits_ahead_of_base > 0, so the ancestry refusal is not pre-empted by
     No_commits_ahead_of_base. *)
  (* Rewind the checked-out branch in place so HEAD stays on [feat]. The
     previous main -> branch -f -> feat hop was enough for CI to occasionally
     observe the branch-switched guard instead of the stale-local refusal. *)
  sh ~dir:managed_dir
    (Printf.sprintf "git reset -q --hard %s"
       (Stdlib.Filename.quote shared_feat_sha));
  (* Sanity: local feat != remote feat. *)
  let local_feat = git_capture ~dir:managed_dir [ "rev-parse"; "feat" ] in
  if String.equal local_feat remote_feat_sha then
    failwith "precondition: local feat was supposed to be stale";
  let outcome =
    Worktree.force_push_with_lease ~process_mgr ~path:managed_dir
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
  in
  (match outcome with
  | Worktree.Push_rejected (Push_reject_classify.Local_state_unsafe { reason })
    ->
      if String.equal reason "refuse_local_behind" then
        Stdlib.print_endline "  local_missing_remote: OK (refused)"
      else
        failwith
          (Printf.sprintf
             "local_missing_remote: expected reason refuse_local_behind, got %s"
             reason)
  | Worktree.Push_rejected
      ( Push_reject_classify.Workflow_scope_missing
      | Push_reject_classify.Branch_protection
      | Push_reject_classify.Push_pattern_block
      | Push_reject_classify.Lease_violation
      | Push_reject_classify.Hook_failure _ | Push_reject_classify.Unknown _ )
  | Worktree.Push_ok | Worktree.Push_up_to_date | Worktree.Push_no_commits
  | Worktree.Push_worktree_missing | Worktree.Push_error _ ->
      failwith
        (Printf.sprintf
           "local_missing_remote: expected Push_rejected Local_state_unsafe, \
            got %s"
           (Worktree.show_push_result outcome)));
  (* Verify remote still at the real (un-wiped) commit. *)
  let post_remote_sha =
    git_capture ~dir:managed_dir [ "ls-remote"; "origin"; "refs/heads/feat" ]
  in
  if not (String.is_prefix post_remote_sha ~prefix:remote_feat_sha) then
    failwith
      (Printf.sprintf
         "local_missing_remote: remote feat changed from %s to %s — push was \
          not refused"
         remote_feat_sha post_remote_sha)

let scenario_happy_path env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin.git" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  setup_origin ~origin_dir;
  setup_seed_clone ~origin_dir ~managed_dir;
  sh ~dir:managed_dir "git checkout -q -b feat";
  sh ~dir:managed_dir "echo shared > work.txt";
  sh ~dir:managed_dir "git add work.txt";
  sh ~dir:managed_dir "git commit -q -m 'shared feat work'";
  sh ~dir:managed_dir "git push -q -u origin feat";
  sh ~dir:managed_dir "echo local > local.txt";
  sh ~dir:managed_dir "git add local.txt";
  sh ~dir:managed_dir "git commit -q -m 'local feat work'";
  let local_sha = git_capture ~dir:managed_dir [ "rev-parse"; "HEAD" ] in
  let outcome =
    Worktree.force_push_with_lease ~process_mgr ~path:managed_dir
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
  in
  (match outcome with
  | Worktree.Push_ok -> Stdlib.print_endline "  happy_path: OK"
  | Worktree.Push_up_to_date | Worktree.Push_no_commits
  | Worktree.Push_rejected
      ( Push_reject_classify.Workflow_scope_missing
      | Push_reject_classify.Branch_protection
      | Push_reject_classify.Push_pattern_block
      | Push_reject_classify.Lease_violation
      | Push_reject_classify.Hook_failure _ | Push_reject_classify.Unknown _
      | Push_reject_classify.Local_state_unsafe _ )
  | Worktree.Push_worktree_missing | Worktree.Push_error _ ->
      failwith
        (Printf.sprintf "happy_path: expected Push_ok, got %s"
           (Worktree.show_push_result outcome)));
  let remote_sha =
    let raw =
      git_capture ~dir:managed_dir [ "ls-remote"; "origin"; "refs/heads/feat" ]
    in
    String.prefix raw 40
  in
  if not (String.equal remote_sha local_sha) then
    failwith
      (Printf.sprintf "happy_path: remote feat=%s expected %s" remote_sha
         local_sha)

(* ── Property: Push_plan.to_push_reject_classify_rejection mapping ────────────

   The escalation contract (see push_plan.mli): local-state refusals route to
   [Some (Local_state_unsafe { reason })] where [reason] is the planner's
   [short_label] for that decision; the two refusals with dedicated
   non-rejection handlers map to [None]. Generate every refusal shape and assert
   the partition. *)
let gen_refusal : Push_plan.refusal QCheck2.Gen.t =
  let open QCheck2.Gen in
  let gen_sha = string_size ~gen:(char_range 'a' 'f') (int_range 7 40) in
  let gen_branch_name =
    string_size ~gen:(char_range 'a' 'z') (int_range 1 12)
  in
  oneof
    [
      return Push_plan.No_commits_ahead_of_base;
      return Push_plan.Worktree_missing;
      map
        (fun branch -> Push_plan.Branch_ref_missing { branch })
        gen_branch_name;
      map2
        (fun expected got -> Push_plan.Branch_switched { expected; got })
        gen_branch_name (option gen_branch_name);
      map2
        (fun local_sha remote_sha ->
          Push_plan.Local_missing_remote_commits { local_sha; remote_sha })
        gen_sha gen_sha;
    ]

let to_rejection_partition =
  QCheck2.Test.make ~name:"to_push_reject_classify_rejection partition"
    ~count:300 gen_refusal (fun refusal ->
      match Push_plan.to_push_reject_classify_rejection refusal with
      | None -> (
          (* Only the two handler-owned refusals map to None. *)
          match refusal with
          | Push_plan.No_commits_ahead_of_base | Push_plan.Worktree_missing ->
              true
          | _ -> false)
      | Some (Push_reject_classify.Local_state_unsafe { reason }) -> (
          (* Local-state refusals carry the planner's short_label as reason. *)
          match refusal with
          | Push_plan.Branch_switched _
          | Push_plan.Local_missing_remote_commits _
          | Push_plan.Branch_ref_missing _ ->
              String.equal reason
                (Push_plan.short_label (Push_plan.Refuse refusal))
          | _ -> false)
      | Some
          ( Push_reject_classify.Workflow_scope_missing
          | Push_reject_classify.Branch_protection
          | Push_reject_classify.Push_pattern_block
          | Push_reject_classify.Lease_violation
          | Push_reject_classify.Hook_failure _ | Push_reject_classify.Unknown _
            ) ->
          false)

let () =
  Eio_main.run @@ fun env ->
  Stdlib.print_endline "Worktree.force_push_with_lease + Push_plan integration:";
  scenario_branch_switched env;
  scenario_local_missing_remote env;
  scenario_happy_path env;
  QCheck2.Test.check_exn to_rejection_partition;
  Stdlib.print_endline "All push-plan integration scenarios passed."

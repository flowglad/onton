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
  let clock = Eio.Stdenv.clock env in
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
    Worktree.force_push_with_lease ~clock ~process_mgr ~path:managed_dir
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
      ()
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
      | Push_reject_classify.Merge_queue_locked
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
  let clock = Eio.Stdenv.clock env in
  with_temp_dir @@ fun root ->
  let origin_dir = Stdlib.Filename.concat root "origin.git" in
  let managed_dir = Stdlib.Filename.concat root "managed" in
  let remote_writer_dir = Stdlib.Filename.concat root "remote-writer" in
  setup_origin ~origin_dir;
  setup_seed_clone ~origin_dir ~managed_dir;
  sh ~dir:managed_dir "git checkout -q -b feat";
  sh ~dir:managed_dir "echo shared > work.txt";
  sh ~dir:managed_dir "git add work.txt";
  sh ~dir:managed_dir "git commit -q -m 'shared work'";
  let shared_feat_sha = git_capture ~dir:managed_dir [ "rev-parse"; "HEAD" ] in
  sh ~dir:managed_dir "git push -q -u origin feat";

  (* Advance the remote from a separate clone so the managed worktree is truly
     stale instead of relying on same-clone push/reflog/reset interactions. *)
  sh
    (Printf.sprintf "git clone -q %s %s"
       (Stdlib.Filename.quote origin_dir)
       (Stdlib.Filename.quote remote_writer_dir));
  sh ~dir:remote_writer_dir "git config user.email 'test@example.com'";
  sh ~dir:remote_writer_dir "git config user.name 'Test'";
  sh ~dir:remote_writer_dir "git checkout -q -b feat origin/feat";
  sh ~dir:remote_writer_dir "echo remote > remote.txt";
  sh ~dir:remote_writer_dir "git add remote.txt";
  sh ~dir:remote_writer_dir "git commit -q -m 'remote work'";
  sh ~dir:remote_writer_dir "git push -q origin feat";
  let remote_feat_sha =
    git_capture ~dir:remote_writer_dir [ "rev-parse"; "HEAD" ]
  in

  (* The stale-local refusal is based on refs/remotes/origin/<branch>. Some
     git/CI combinations leave that tracking ref at the previous push's SHA
     after the remote moves independently, which would make
     the planner permit the push and defer to git's less-specific rejection.
     Refresh it explicitly so this fixture exercises the planner path. *)
  sh ~dir:managed_dir "git fetch -q origin feat:refs/remotes/origin/feat";
  let tracked_feat =
    git_capture ~dir:managed_dir [ "rev-parse"; "refs/remotes/origin/feat" ]
  in
  if not (String.equal tracked_feat remote_feat_sha) then
    failwith
      (Printf.sprintf "precondition: origin/feat=%s expected remote feat %s"
         tracked_feat remote_feat_sha);
  (* Pin HEAD to [feat] deterministically. We never left [feat] in this
     scenario, so the working tree and index already match it; the only thing
     that must be true for the planner is that HEAD is a symbolic ref to
     [refs/heads/feat]. Earlier revisions used [git checkout -q -B feat] here,
     but a full checkout touches the index/worktree and is sensitive to ambient
     config — on CI it intermittently left the planner observing the
     branch-switched guard instead of the intended stale-local refusal. Writing
     the symref directly avoids the checkout heuristics entirely. *)
  sh ~dir:managed_dir "git symbolic-ref HEAD refs/heads/feat";
  (* Verify the exact value the planner reads ([rev-parse --abbrev-ref HEAD]):
     if HEAD is not on [feat] the planner would (correctly) refuse with
     branch-switched, so surface that as a clear precondition rather than a
     misleading stale-local assertion failure. *)
  let head_branch =
    git_capture ~dir:managed_dir [ "rev-parse"; "--abbrev-ref"; "HEAD" ]
  in
  if not (String.equal head_branch "feat") then
    failwith (Printf.sprintf "precondition: HEAD=%s expected feat" head_branch);
  (* Sanity: local feat != remote feat. *)
  let local_feat = git_capture ~dir:managed_dir [ "rev-parse"; "feat" ] in
  if not (String.equal local_feat shared_feat_sha) then
    failwith
      (Printf.sprintf "precondition: local feat=%s expected shared feat %s"
         local_feat shared_feat_sha);
  if String.equal local_feat remote_feat_sha then
    failwith "precondition: local feat was supposed to be stale";
  let outcome =
    Worktree.force_push_with_lease ~clock ~process_mgr ~path:managed_dir
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
      ()
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
      | Push_reject_classify.Merge_queue_locked
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
  let clock = Eio.Stdenv.clock env in
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
  (* Some CI/git combinations leave HEAD on [feat] but make
     [refs/heads/feat] intermittently unreadable to [rev-parse --verify].
     Refresh the branch through git's normal branch machinery and assert the
     named ref is visible before exercising the push path. *)
  sh ~dir:managed_dir "git checkout -q -B feat";
  let feat_ref_sha =
    git_capture ~dir:managed_dir [ "rev-parse"; "--verify"; "refs/heads/feat" ]
  in
  let local_sha = git_capture ~dir:managed_dir [ "rev-parse"; "HEAD" ] in
  if not (String.equal feat_ref_sha local_sha) then
    failwith
      (Printf.sprintf "precondition: refs/heads/feat=%s expected %s"
         feat_ref_sha local_sha);
  (* A deadline must turn a non-returning push into data that the runner can
     feed through [combine_session_and_push]. A zero-second deadline exercises
     the timeout branch deterministically without depending on network timing. *)
  let timed_out =
    Worktree.force_push_with_lease ~timeout_seconds:0.0 ~clock ~process_mgr
      ~path:managed_dir
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
      ()
  in
  (match timed_out with
  | Worktree.Push_error msg when String.is_substring msg ~substring:"timed out"
    ->
      Stdlib.print_endline "  push_timeout: OK (classified as Push_error)"
  | other ->
      failwith
        (Printf.sprintf "push_timeout: expected Push_error, got %s"
           (Worktree.show_push_result other)));
  let outcome =
    Worktree.force_push_with_lease ~clock ~process_mgr ~path:managed_dir
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
      ()
  in
  (match outcome with
  | Worktree.Push_ok -> Stdlib.print_endline "  happy_path: OK"
  | Worktree.Push_up_to_date | Worktree.Push_no_commits
  | Worktree.Push_rejected
      ( Push_reject_classify.Workflow_scope_missing
      | Push_reject_classify.Branch_protection
      | Push_reject_classify.Push_pattern_block
      | Push_reject_classify.Lease_violation
      | Push_reject_classify.Merge_queue_locked
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

(* Reproduce the fresh-clone/rewrite path with a real remote. Interleave an
   independent writer either before planning (and a background fetch), or in
   Git's pre-push hook after the planner has captured its immutable lease. *)
let scenario_rewrite_interleavings env (commits, race, preserve_history) =
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  with_temp_dir @@ fun root ->
  let origin = Stdlib.Filename.concat root "origin.git" in
  let seed = Stdlib.Filename.concat root "seed" in
  let managed = Stdlib.Filename.concat root "managed" in
  let worktree = Stdlib.Filename.concat root "worktree" in
  setup_origin ~origin_dir:origin;
  setup_seed_clone ~origin_dir:origin ~managed_dir:seed;
  let base = git_capture ~dir:seed [ "rev-parse"; "HEAD" ] in
  sh ~dir:seed "git checkout -q -b feat";
  for n = 1 to commits do
    sh ~dir:seed
      (Printf.sprintf
         "echo %d >> patch.txt; git add patch.txt; git commit -q -m patch" n)
  done;
  sh ~dir:seed "git push -q origin feat";
  let old_remote = git_capture ~dir:seed [ "rev-parse"; "HEAD" ] in
  sh ~dir:seed
    "echo writer > writer.txt; git add writer.txt; git commit -q -m writer";
  let writer = git_capture ~dir:seed [ "rev-parse"; "HEAD" ] in
  sh ~dir:seed "git push -q origin HEAD:refs/heads/writer-object";
  sh ~dir:seed
    "git checkout -q main; echo advanced > advanced.txt; git add advanced.txt; \
     git commit -q -m advanced; git push -q origin main";
  sh
    (Printf.sprintf "git clone -q --branch main %s %s"
       (Stdlib.Filename.quote origin)
       (Stdlib.Filename.quote managed));
  sh ~dir:managed
    "git config user.email test@example.com; git config user.name Test";
  sh ~dir:managed
    (Printf.sprintf
       "git update-ref refs/heads/feat %s; git worktree add -q %s feat"
       old_remote
       (Stdlib.Filename.quote worktree));
  if race = 3 then (
    (* The branch once contained the writer, but a rewind discarded it. Its
       stale tip remains in the branch reflog when the remote later advances. *)
    sh ~dir:worktree ("git reset -q --hard " ^ writer);
    sh ~dir:worktree ("git reset -q --hard " ^ old_remote));
  sh ~dir:worktree (Printf.sprintf "git rebase -q --onto origin/main %s" base);
  let rewritten = git_capture ~dir:worktree [ "rev-parse"; "HEAD" ] in
  let publish_writer =
    Printf.sprintf "git --git-dir=%s update-ref refs/heads/feat %s %s"
      (Stdlib.Filename.quote origin)
      writer old_remote
  in
  if race = 1 || race = 3 then (
    sh publish_writer;
    sh ~dir:managed "git fetch -q origin")
  else if race = 2 then (
    let hooks = Stdlib.Filename.concat managed "hooks" in
    Unix.mkdir hooks 0o755;
    let hook = Stdlib.Filename.concat hooks "pre-push" in
    let oc = Stdlib.open_out hook in
    Stdlib.output_string oc
      ("#!/bin/sh\nset -eu\n" ^ publish_writer ^ "\ngit fetch -q origin\n");
    Stdlib.close_out oc;
    Unix.chmod hook 0o755;
    sh ~dir:managed ("git config core.hooksPath " ^ Stdlib.Filename.quote hooks));
  let outcome =
    Worktree.force_push_with_lease ~preserve_history ~clock ~process_mgr
      ~path:worktree
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
      ()
  in
  let remote =
    git_capture ~dir:seed [ "ls-remote"; "origin"; "refs/heads/feat" ]
    |> fun value -> String.prefix value 40
  in
  if race = 0 then (
    if
      (not (Worktree.equal_push_result outcome Worktree.Push_ok))
      || not (String.equal remote rewritten)
    then
      failwith
        ("unchanged remote did not receive rewrite: "
        ^ Worktree.show_push_result outcome);
    let contents =
      git_capture ~dir:seed [ "--git-dir=" ^ origin; "show"; "feat:patch.txt" ]
    in
    if List.length (String.split_lines contents) <> commits then
      failwith "patch commits lost")
  else (
    if not (String.equal remote writer) then
      failwith "concurrent writer was overwritten";
    match outcome with
    | Worktree.Push_rejected _ | Worktree.Push_error _ -> ()
    | Worktree.Push_ok | Worktree.Push_up_to_date | Worktree.Push_no_commits
    | Worktree.Push_worktree_missing ->
        failwith
          ("concurrent update was accepted: "
          ^ Worktree.show_push_result outcome))

let scenario_initial_publication env ~preserve_history ~race =
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  with_temp_dir @@ fun root ->
  let origin = Stdlib.Filename.concat root "origin.git" in
  let managed = Stdlib.Filename.concat root "managed" in
  setup_origin ~origin_dir:origin;
  setup_seed_clone ~origin_dir:origin ~managed_dir:managed;
  let base = git_capture ~dir:managed [ "rev-parse"; "HEAD" ] in
  sh ~dir:managed
    "git checkout -q -b feat; echo patch > patch.txt; git add patch.txt; git \
     commit -q -m patch";
  let local = git_capture ~dir:managed [ "rev-parse"; "HEAD" ] in
  if race then (
    let hooks = Stdlib.Filename.concat managed "hooks" in
    Unix.mkdir hooks 0o755;
    let hook = Stdlib.Filename.concat hooks "pre-push" in
    let oc = Stdlib.open_out hook in
    Stdlib.output_string oc
      (Printf.sprintf
         "#!/bin/sh\nset -eu\ngit --git-dir=%s update-ref refs/heads/feat %s\n"
         (Stdlib.Filename.quote origin)
         base);
    Stdlib.close_out oc;
    Unix.chmod hook 0o755;
    sh ~dir:managed ("git config core.hooksPath " ^ Stdlib.Filename.quote hooks));
  let outcome =
    Worktree.force_push_with_lease ~preserve_history ~clock ~process_mgr
      ~path:managed
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
      ()
  in
  let remote =
    git_capture ~dir:managed [ "ls-remote"; "origin"; "refs/heads/feat" ]
    |> fun value -> String.prefix value 40
  in
  if race then (
    if not (String.equal remote base) then
      failwith "initial push overwrote writer";
    (match outcome with
    | Worktree.Push_rejected _ -> ()
    | _ -> failwith ("initial race: " ^ Worktree.show_push_result outcome));
    if
      Git_env.git_exit_code ~cwd:managed
        [ "config"; "--get"; "branch.feat.remote" ]
      = 0
    then failwith "failed initial push configured upstream")
  else (
    if
      (not (Worktree.equal_push_result outcome Worktree.Push_ok))
      || not (String.equal remote local)
    then failwith ("initial publication: " ^ Worktree.show_push_result outcome);
    let upstream =
      git_capture ~dir:managed
        [ "rev-parse"; "--abbrev-ref"; "feat@{upstream}" ]
    in
    if not (String.equal upstream "origin/feat") then
      failwith "initial upstream missing";
    sh ~dir:managed "git -c push.default=simple push -q --dry-run";
    sh ~dir:managed "git pull -q --ff-only");
  Stdlib.print_endline "  initial_publication: OK"

let scenario_unmatched_merge env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  with_temp_dir @@ fun root ->
  let origin = Stdlib.Filename.concat root "origin.git" in
  let managed = Stdlib.Filename.concat root "managed" in
  setup_origin ~origin_dir:origin;
  setup_seed_clone ~origin_dir:origin ~managed_dir:managed;
  sh ~dir:managed
    "git checkout -q -b side; echo side > side.txt; git add side.txt; git \
     commit -q -m side";
  let side = git_capture ~dir:managed [ "rev-parse"; "HEAD" ] in
  sh ~dir:managed
    "git checkout -q -b feat main; echo feat > feat.txt; git add feat.txt; git \
     commit -q -m feat";
  let before_merge = git_capture ~dir:managed [ "rev-parse"; "HEAD" ] in
  sh ~dir:managed "git merge -q --no-ff side -m merge";
  (* A merge can contain resolution changes absent from both parents. *)
  sh ~dir:managed
    "echo resolution > resolution.txt; git add resolution.txt; git commit -q \
     --amend --no-edit; git push -q origin feat";
  let remote = git_capture ~dir:managed [ "rev-parse"; "HEAD" ] in
  sh ~dir:managed ("git reset -q --hard " ^ before_merge);
  let _ = git_capture ~dir:managed [ "cherry-pick"; side ] in
  let outcome =
    Worktree.force_push_with_lease ~clock ~process_mgr ~path:managed
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
      ()
  in
  (match outcome with
  | Worktree.Push_rejected Push_reject_classify.Lease_violation -> ()
  | _ -> failwith ("unmatched merge: " ^ Worktree.show_push_result outcome));
  let actual =
    git_capture ~dir:managed [ "ls-remote"; "origin"; "refs/heads/feat" ]
    |> fun value -> String.prefix value 40
  in
  if not (String.equal actual remote) then failwith "merge resolution lost";
  Stdlib.print_endline "  unmatched_merge: OK (refused)"

type missing_tracking_case =
  | Fast_forward
  | Behind
  | Diverged
  | Concurrent_write
  | Remote_unavailable

let scenario_missing_tracking env ~preserve_history case =
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  with_temp_dir @@ fun root ->
  let origin = Stdlib.Filename.concat root "origin.git" in
  let seed = Stdlib.Filename.concat root "seed" in
  let managed = Stdlib.Filename.concat root "managed" in
  setup_origin ~origin_dir:origin;
  setup_seed_clone ~origin_dir:origin ~managed_dir:seed;
  sh ~dir:seed
    "git checkout -q -b feat; echo shared > shared.txt; git add shared.txt; \
     git commit -q -m shared; git push -q origin feat";
  let shared = git_capture ~dir:seed [ "rev-parse"; "HEAD" ] in
  sh
    (Printf.sprintf "git clone -q --branch feat %s %s"
       (Stdlib.Filename.quote origin)
       (Stdlib.Filename.quote managed));
  sh ~dir:managed
    "git config user.name Test; git config user.email test@example.com";
  sh ~dir:managed
    "git update-ref -d refs/remotes/origin/feat; printf sentinel > \
     .git/FETCH_HEAD";
  (match case with
  | Behind -> ()
  | Fast_forward | Diverged | Concurrent_write | Remote_unavailable ->
      sh ~dir:managed
        "echo local > local.txt; git add local.txt; git commit -q -m local");
  let local = git_capture ~dir:managed [ "rev-parse"; "HEAD" ] in
  sh ~dir:seed
    "echo writer > writer.txt; git add writer.txt; git commit -q -m writer";
  let writer = git_capture ~dir:seed [ "rev-parse"; "HEAD" ] in
  (match case with
  | Behind | Diverged ->
      sh ~dir:seed "git push -q origin feat";
      if
        Git_env.git_exit_code ~cwd:managed
          [ "cat-file"; "-e"; writer ^ "^{commit}" ]
        = 0
      then failwith "precondition: writer object already exists locally"
  | Concurrent_write ->
      sh ~dir:seed "git push -q origin HEAD:refs/heads/writer-object";
      let hooks = Stdlib.Filename.concat managed "hooks" in
      Unix.mkdir hooks 0o755;
      let hook = Stdlib.Filename.concat hooks "pre-push" in
      let oc = Stdlib.open_out hook in
      Stdlib.output_string oc
        (Printf.sprintf
           "#!/bin/sh\n\
            set -eu\n\
            if git show-ref --verify --quiet refs/remotes/origin/feat; then \
            exit 88; fi\n\
            git --git-dir=%s update-ref refs/heads/feat %s %s\n"
           (Stdlib.Filename.quote origin)
           writer shared);
      Stdlib.close_out oc;
      Unix.chmod hook 0o755;
      sh ~dir:managed
        ("git config core.hooksPath " ^ Stdlib.Filename.quote hooks)
  | Remote_unavailable ->
      sh ~dir:managed
        ("git remote set-url origin "
        ^ Stdlib.Filename.quote (Stdlib.Filename.concat root "missing.git"))
  | Fast_forward -> ());
  let result =
    Worktree.force_push_with_lease ~preserve_history ~clock ~process_mgr
      ~path:managed
      ~branch:(Types.Branch.of_string "feat")
      ~base:(Types.Branch.of_string "main")
      ()
  in
  (match (case, result) with
  | Fast_forward, Worktree.Push_ok -> ()
  | ( Behind,
      Worktree.Push_rejected
        (Push_reject_classify.Local_state_unsafe { reason }) )
    when String.equal reason "refuse_local_behind" ->
      ()
  | Diverged, Worktree.Push_rejected Push_reject_classify.Lease_violation -> ()
  | Concurrent_write, Worktree.Push_rejected _ -> ()
  | Remote_unavailable, Worktree.Push_error detail
    when String.is_substring detail ~substring:"Cannot observe remote branch" ->
      ()
  | _ -> failwith ("missing tracking: " ^ Worktree.show_push_result result));
  let expected =
    match case with
    | Fast_forward -> local
    | Behind | Diverged | Concurrent_write -> writer
    | Remote_unavailable -> shared
  in
  let actual =
    git_capture ~dir:seed [ "ls-remote"; "origin"; "refs/heads/feat" ]
    |> fun value -> String.prefix value 40
  in
  if not (String.equal actual expected) then
    failwith "missing tracking lost remote work";
  let fetch_head =
    Stdlib.open_in (Stdlib.Filename.concat managed ".git/FETCH_HEAD")
  in
  let contents =
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_in_noerr fetch_head)
      (fun () ->
        Stdlib.really_input_string fetch_head
          (Stdlib.in_channel_length fetch_head))
  in
  if not (String.equal contents "sentinel") then
    failwith "observation modified shared FETCH_HEAD";
  (match case with
  | Fast_forward -> ()
  | Behind | Diverged | Concurrent_write | Remote_unavailable ->
      if
        Git_env.git_exit_code ~cwd:managed
          [ "show-ref"; "--verify"; "--quiet"; "refs/remotes/origin/feat" ]
        = 0
      then failwith "observation modified shared tracking refs");
  Stdlib.print_endline "  missing_tracking: OK"

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
      map
        (fun remote_sha -> Push_plan.Remote_not_integrated { remote_sha })
        gen_sha;
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
      | Some Push_reject_classify.Lease_violation -> (
          match refusal with
          | Push_plan.Remote_not_integrated _ -> true
          | _ -> false)
      | Some
          ( Push_reject_classify.Workflow_scope_missing
          | Push_reject_classify.Branch_protection
          | Push_reject_classify.Push_pattern_block
          | Push_reject_classify.Merge_queue_locked
          | Push_reject_classify.Hook_failure _ | Push_reject_classify.Unknown _
            ) ->
          false)

let () =
  Eio_main.run @@ fun env ->
  Stdlib.print_endline "Worktree.force_push_with_lease + Push_plan integration:";
  scenario_branch_switched env;
  scenario_local_missing_remote env;
  scenario_happy_path env;
  scenario_unmatched_merge env;
  List.iter [ false; true ] ~f:(fun preserve_history ->
      List.iter
        [ Fast_forward; Behind; Diverged; Concurrent_write; Remote_unavailable ]
        ~f:(scenario_missing_tracking env ~preserve_history);
      List.iter [ false; true ] ~f:(fun race ->
          scenario_initial_publication env ~preserve_history ~race);
      List.iter [ 0; 1; 2; 3 ] ~f:(fun race ->
          scenario_rewrite_interleavings env (2, race, preserve_history)));
  QCheck2.Test.check_exn
    (QCheck2.Test.make
       ~name:"rewrite publication remains live and preserves concurrent writers"
       ~count:24
       ~print:(fun (commits, race, preserve_history) ->
         Printf.sprintf "commits=%d race=%d preserve_history=%b" commits race
           preserve_history)
       QCheck2.Gen.(triple (int_range 1 4) (int_range 0 3) bool)
       (fun input ->
         try
           scenario_rewrite_interleavings env input;
           true
         with _ -> false));
  QCheck2.Test.check_exn to_rejection_partition;
  Stdlib.print_endline "All push-plan integration scenarios passed."

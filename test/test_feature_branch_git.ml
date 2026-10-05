(* @archlint.module test
   @archlint.domain worktree *)
open Base
open Onton
open Onton_core
open Types
module Git = Onton_test_support.Git_env

let check label value = if not value then failwith label

(* Inject a failed probe through the process boundary while all other commands
   operate on real Git state. *)
let failing_conflict_probe_mgr (type tag)
    (mgr : tag Eio.Process.mgr_ty Eio.Resource.t) probed =
  let (Eio.Resource.T (state, ops)) = mgr in
  let module Original = (val Eio.Resource.get ops Eio.Process.Pi.Mgr) in
  let module Faults = struct
    include Original

    let spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
      match args with
      | [ "git"; "-C"; _; "diff"; "--name-only"; "--diff-filter=U" ] ->
          probed := true;
          Original.spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env
            ~executable:"/bin/sh"
            [ "/bin/sh"; "-c"; "echo index-probe-stderr >&2; exit 23" ]
      | _ ->
          Original.spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args
  end in
  Eio.Resource.T (state, Eio.Process.Pi.mgr (module Faults))

let write path content =
  let oc = Stdlib.open_out path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () -> Stdlib.output_string oc content)

let commit dir file content =
  write (Stdlib.Filename.concat dir file) content;
  Git.run_git ~cwd:dir [ "add"; file ];
  Git.run_git ~cwd:dir [ "commit"; "-q"; "-m"; file ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let integrated = function
  | Worktree.Integrated sha -> sha
  | Worktree.Integration_conflict files ->
      failwith ("unexpected conflict: " ^ files)
  | Worktree.Integration_error e -> failwith e

let empty_gameplan =
  Gameplan.
    {
      project_name = "git-test";
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
      publication = None;
      reachability_traces = [];
    }

let () =
  Eio_main.run (fun env ->
      Git.with_temp_repo (fun dir ->
          let bare = dir ^ "/remote.git" in
          Git.run_git ~cwd:dir [ "init"; "--bare"; "-q"; bare ];
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; bare ];
          let base = commit dir "shared" "base\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "main" ];
          Git.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "root" ];
          let initial = commit dir "root-file" "root\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "root" ];
          Git.run_git ~cwd:dir [ "checkout"; "-q"; "main" ];
          let root_path = dir ^ "/root-checkout" in
          Git.run_git ~cwd:dir [ "worktree"; "add"; "-q"; root_path; "root" ];
          let child name file content =
            Git.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; name; initial ];
            let sha = commit dir file content in
            Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; name ];
            Git.run_git ~cwd:dir [ "checkout"; "-q"; "main" ];
            sha
          in
          let head2 = child "child2" "two" "two\n" in
          let head3 = child "child3" "three" "three\n" in
          let conflict_head = child "conflict" "shared" "child\n" in
          let next_head = child "next" "next-file" "next\n" in
          let client =
            Worktree.make_with_protection
              ~protected_branch:(Some (Branch.of_string "root"))
              ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
              ~clock:(Eio.Stdenv.clock env)
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~repo_root:dir
          in
          let module W = (val client) in
          let rt =
            Runtime.create ~gameplan:empty_gameplan
              ~main_branch:(Branch.of_string "main") ()
          in
          let integrate name head_sha =
            Runtime.with_root_write rt (fun () ->
                W.integrate ~root_path ~root_branch:(Branch.of_string "root")
                  ~descendant_branch:(Branch.of_string name) ~head_sha)
          in
          let results =
            Eio.Fiber.List.map
              (fun (name, sha) -> integrated (integrate name sha))
              [ ("child2", head2); ("child3", head3) ]
          in
          let final = Git.git_capture ~cwd:bare [ "rev-parse"; "root" ] in
          List.iter [ base; initial; head2; head3 ] ~f:(fun ancestor ->
              check "ancestry preserved"
                (Git.git_exit_code ~cwd:dir
                   [ "merge-base"; "--is-ancestor"; ancestor; final ]
                = 0));
          check "normal two-parent merge"
            (List.length
               (String.split
                  (Git.git_capture ~cwd:dir
                     [ "show"; "-s"; "--format=%P"; final ])
                  ~on:' ')
            = 2);
          check "siblings serialized"
            (List.length (List.dedup_and_sort results ~compare:String.compare)
            = 2);
          Git.run_git ~cwd:root_path [ "reset"; "--hard"; initial ];
          check "crash after publication reconciles same commit"
            (String.equal (integrated (integrate "child2" head2)) final);
          check "recovery fast forwards root"
            (String.equal
               (Git.git_capture ~cwd:root_path [ "rev-parse"; "HEAD" ])
               final);
          write (root_path ^ "/user-change") "preserve me\n";
          check "dirty root refused"
            (match integrate "next" next_head with
            | Worktree.Integration_error _ -> true
            | Worktree.Integrated _ | Worktree.Integration_conflict _ -> false);
          check "user changes retained"
            (Stdlib.Sys.file_exists (root_path ^ "/user-change"));
          Unix.unlink (root_path ^ "/user-change");
          check "moved checked head refused"
            (match integrate "next" head2 with
            | Worktree.Integration_error _ -> true
            | Worktree.Integrated _ | Worktree.Integration_conflict _ -> false);
          let local = commit root_path "unpublished" "local\n" in
          check "unpublished root refused without reset"
            (match integrate "next" next_head with
            | Worktree.Integration_error _ -> true
            | Worktree.Integrated _ | Worktree.Integration_conflict _ -> false);
          check "local commit retained"
            (String.equal
               (Git.git_capture ~cwd:root_path [ "rev-parse"; "HEAD" ])
               local);
          check "root normal publication"
            (match
               W.force_push_with_lease ~path:root_path
                 ~branch:(Branch.of_string "root")
                 ~base:(Branch.of_string "main")
             with
            | Worktree.Push_ok | Worktree.Push_up_to_date -> true
            | Worktree.Push_no_commits | Worktree.Push_rejected _
            | Worktree.Push_worktree_missing | Worktree.Push_error _ ->
                false);
          let _ = commit root_path "shared" "root\n" in
          Git.run_git ~cwd:root_path [ "push"; "-q"; "origin"; "root" ];
          check "conflict reported"
            (match integrate "conflict" conflict_head with
            | Worktree.Integration_conflict files ->
                String.is_substring files ~substring:"shared"
            | Worktree.Integrated _ | Worktree.Integration_error _ -> false);
          check "conflict leaves root clean"
            (String.is_empty
               (Git.git_capture ~cwd:root_path [ "status"; "--porcelain" ]));
          check "temporary worktrees cleaned"
            (not
               (String.is_substring
                  (Git.git_capture ~cwd:dir
                     [ "worktree"; "list"; "--porcelain" ])
                  ~substring:"onton-integration-"));
          (* Race the publication using the remote's pre-receive hook. The race commit
     is already in the remote object store, so updating its ref is legal. *)
          Git.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "race"; "root" ];
          let race = commit dir "raced" "remote update\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "race" ];
          Git.run_git ~cwd:dir [ "checkout"; "-q"; "main" ];
          let hook = bare ^ "/hooks/pre-receive" in
          Git.run_git ~cwd:bare [ "config"; "core.hooksPath"; bare ^ "/hooks" ];
          write hook
            ("#!/bin/sh\n\
              unset GIT_QUARANTINE_PATH GIT_OBJECT_DIRECTORY \
              GIT_ALTERNATE_OBJECT_DIRECTORIES\n\
              git update-ref refs/heads/root " ^ race ^ "\n");
          Unix.chmod hook 0o755;
          check "remote race rejected"
            (match integrate "next" next_head with
            | Worktree.Integration_error _ -> true
            | Worktree.Integrated _ | Worktree.Integration_conflict _ -> false);
          check "race commit preserved"
            (String.equal
               (Git.git_capture ~cwd:bare [ "rev-parse"; "root" ])
               race);
          Unix.unlink hook;
          let combined = integrated (integrate "next" next_head) in
          check "retry includes raced commit"
            (Git.git_exit_code ~cwd:dir
               [ "merge-base"; "--is-ancestor"; race; combined ]
            = 0);
          (* Root upstream updates are merges, preserving every integrated parent. *)
          let main_tip = commit dir "upstream" "main update\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "main" ];
          let fetch_lock = Eio.Mutex.create () in
          check "fetch"
            (Result.is_ok (W.fetch_origin ~fetch_lock ~path:root_path));
          check "root merges main"
            (match
               W.rebase_onto ~path:root_path
                 ~target:(Branch.of_string "origin/main")
                 ~upstream:base ~project_name:"git-test" ~ancestor_ids:[] ()
             with
            | Worktree.Ok -> true
            | Worktree.Noop | Worktree.Conflict _ | Worktree.Merge_conflict _
            | Worktree.Uncommitted_changes _ | Worktree.Error _ ->
                false);
          let root_tip =
            Git.git_capture ~cwd:root_path [ "rev-parse"; "HEAD" ]
          in
          List.iter [ combined; main_tip ] ~f:(fun ancestor ->
              check "root merge preserves parents"
                (Git.git_exit_code ~cwd:dir
                   [ "merge-base"; "--is-ancestor"; ancestor; root_tip ]
                = 0));
          (* The normal runner publishes the protected-root merge before the
             next integration. Keep the fixture in that same state so the
             cancellation test reaches its sleeping push hook on fast hosts. *)
          Git.run_git ~cwd:root_path [ "push"; "-q"; "origin"; "root" ];
          let _ = commit dir "shared" "upstream conflict\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "main" ];
          check "fetch conflicting upstream"
            (Result.is_ok (W.fetch_origin ~fetch_lock ~path:root_path));
          let conflicting_tip =
            Git.git_capture ~cwd:dir [ "rev-parse"; "main" ]
          in
          let merge_root () =
            W.rebase_onto ~path:root_path
              ~target:(Branch.of_string "origin/main")
              ~upstream:base ~project_name:"git-test" ~ancestor_ids:[] ()
          in
          let expect_merge_conflict result =
            check "root conflict classified for repair"
              (match result with
              | Worktree.Merge_conflict sha -> String.equal sha conflicting_tip
              | Worktree.Ok | Worktree.Noop | Worktree.Conflict _
              | Worktree.Uncommitted_changes _ | Worktree.Error _ ->
                  false)
          in
          let probed = ref false in
          let module Failed_probe =
            (val Worktree.make_with_protection
                   ~protected_branch:(Some (Branch.of_string "root"))
                   ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
                   ~clock:(Eio.Stdenv.clock env)
                   ~process_mgr:
                     (failing_conflict_probe_mgr
                        (Eio.Stdenv.process_mgr env)
                        probed)
                   ~repo_root:dir)
          in
          check "failed conflict probe reports error without aborting"
            (match
               Failed_probe.rebase_onto ~path:root_path
                 ~target:(Branch.of_string "origin/main")
                 ~upstream:base ~project_name:"git-test" ~ancestor_ids:[] ()
             with
            | Worktree.Error detail ->
                !probed
                && String.is_substring detail
                     ~substring:"Root merge failed (exit 1)"
                && String.is_substring detail
                     ~substring:
                       "Unmerged-index probe failed (exit 23): \
                        index-probe-stderr"
                && String.is_substring detail
                     ~substring:
                       ("Pending root merge preserved (MERGE_HEAD "
                      ^ conflicting_tip ^ ")")
                && String.is_substring detail
                     ~substring:"next retry will route to merge repair"
            | Worktree.Ok | Worktree.Noop | Worktree.Conflict _
            | Worktree.Merge_conflict _ | Worktree.Uncommitted_changes _ ->
                false);
          check "root conflict retains merge state and unmerged file"
            (String.equal
               (Git.git_capture ~cwd:root_path [ "rev-parse"; "MERGE_HEAD" ])
               conflicting_tip
            && String.equal
                 (Git.git_capture ~cwd:root_path
                    [ "diff"; "--name-only"; "--diff-filter=U" ])
                 "shared");
          expect_merge_conflict (merge_root ());
          check "root conflict preserves HEAD"
            (String.equal
               (Git.git_capture ~cwd:root_path [ "rev-parse"; "HEAD" ])
               root_tip);
          let before_retry = W.conflict_diff ~path:root_path in
          (* A fresh client models a supervisor restart; a moved target must not
             replace the target of the merge already in progress. *)
          let module Restarted =
            (val Worktree.make_with_protection
                   ~protected_branch:(Some (Branch.of_string "root"))
                   ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
                   ~clock:(Eio.Stdenv.clock env)
                   ~process_mgr:(Eio.Stdenv.process_mgr env)
                   ~repo_root:dir)
          in
          expect_merge_conflict
            (Restarted.rebase_onto ~path:root_path
               ~target:(Branch.of_string "missing-target")
               ~upstream:base ~project_name:"git-test" ~ancestor_ids:[] ());
          check "retry preserves conflict contents"
            (String.equal before_retry (W.conflict_diff ~path:root_path));
          write (root_path ^ "/shared") "root and upstream resolved\n";
          Git.run_git ~cwd:root_path [ "add"; "shared" ];
          expect_merge_conflict (merge_root ());
          check "retry preserves staged resolution"
            (String.is_empty (Git.git_capture ~cwd:root_path [ "diff" ])
            && not
                 (String.is_empty
                    (Git.git_capture ~cwd:root_path [ "diff"; "--cached" ])));
          Git.run_git ~cwd:root_path
            [ "-c"; "core.editor=true"; "merge"; "--continue" ];
          let repaired =
            Git.git_capture ~cwd:root_path [ "rev-parse"; "HEAD" ]
          in
          check "repair preserves both parents"
            (String.equal
               (Git.git_capture ~cwd:root_path
                  [ "show"; "-s"; "--format=%P"; "HEAD" ])
               (root_tip ^ " " ^ conflicting_tip));
          check "completed repair is a noop"
            (Worktree.equal_rebase_result (merge_root ()) Worktree.Noop);
          check "repair publishes normally"
            (match
               W.force_push_with_lease ~path:root_path
                 ~branch:(Branch.of_string "root")
                 ~base:(Branch.of_string "main")
             with
            | Worktree.Push_ok -> true
            | Worktree.Push_up_to_date | Worktree.Push_no_commits
            | Worktree.Push_rejected _ | Worktree.Push_worktree_missing
            | Worktree.Push_error _ ->
                false);
          check "remote contains repaired merge"
            (String.equal
               (Git.git_capture ~cwd:bare [ "rev-parse"; "root" ])
               repaired);
          check "invalid target has command diagnostics"
            (match
               W.rebase_onto ~path:root_path
                 ~target:(Branch.of_string "missing-target")
                 ~upstream:base ~project_name:"git-test" ~ancestor_ids:[] ()
             with
            | Worktree.Error detail ->
                String.is_substring detail ~substring:"missing-target"
                && String.is_substring detail ~substring:"exit"
            | Worktree.Ok | Worktree.Noop | Worktree.Conflict _
            | Worktree.Merge_conflict _ | Worktree.Uncommitted_changes _ ->
                false);
          (* A failed commit hook leaves MERGE_HEAD but no unmerged index. It is
             an execution error, so abort that failed attempt and retain output. *)
          let _ = commit dir "hook-upstream" "upstream\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "main" ];
          check "fetch hook upstream"
            (Result.is_ok (W.fetch_origin ~fetch_lock ~path:root_path));
          let merge_hook = dir ^ "/.git/hooks/pre-merge-commit" in
          Git.run_git ~cwd:dir
            [ "config"; "core.hooksPath"; dir ^ "/.git/hooks" ];
          write merge_hook
            "#!/bin/sh\n\
             echo merge-hook-stdout\n\
             echo merge-hook-stderr >&2\n\
             exit 1\n";
          Unix.chmod merge_hook 0o755;
          check "hook failure retains both diagnostic streams"
            (match merge_root () with
            | Worktree.Error detail ->
                String.is_substring detail ~substring:"merge-hook-stdout"
                && String.is_substring detail ~substring:"merge-hook-stderr"
            | Worktree.Ok | Worktree.Noop | Worktree.Conflict _
            | Worktree.Merge_conflict _ | Worktree.Uncommitted_changes _ ->
                false);
          Unix.unlink merge_hook;
          check "hook failure cleans only the failed merge"
            (String.is_empty
               (Git.git_capture ~cwd:root_path [ "status"; "--porcelain" ])
            && Git.git_exit_code ~cwd:root_path [ "rev-parse"; "MERGE_HEAD" ]
               <> 0
            && String.equal
                 (Git.git_capture ~cwd:root_path [ "rev-parse"; "HEAD" ])
                 repaired);
          check "no temporary worktree remains"
            (not
               (String.is_substring
                  (Git.git_capture ~cwd:dir
                     [ "worktree"; "list"; "--porcelain" ])
                  ~substring:"onton-integration-"));
          let late_head = child "late" "late-file" "late\n" in
          let marker = dir ^ "/integration-hook-started" in
          let filter_pid = dir ^ "/smudge-pid" in
          let helper_pid = dir ^ "/smudge-helper-pid" in
          let check_cleanup () =
            List.iter [ filter_pid; helper_pid ] ~f:(fun file ->
                if Stdlib.Sys.file_exists file then
                  let pid =
                    Stdlib.In_channel.with_open_bin file
                      Stdlib.In_channel.input_all
                    |> String.strip |> Int.of_string
                  in
                  let gone =
                    match Unix.kill pid 0 with
                    | () -> false
                    | exception Unix.Unix_error (Unix.ESRCH, _, _) -> true
                  in
                  check "subprocess tree reaped before cleanup returns" gone);
            check "cancelled worktree cleaned"
              (not
                 (String.is_substring
                    (Git.git_capture ~cwd:dir
                       [ "worktree"; "list"; "--porcelain" ])
                    ~substring:"onton-integration-"));
            check "root lock released after cancellation"
              (Runtime.with_root_write rt (fun () -> true))
          in
          let cancel_at_hook () =
            let cancelled = ref false in
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                Eio.Fiber.first
                  (fun () ->
                    try
                      ignore
                        (integrate "late" late_head
                          : Worktree.integration_result);
                      failwith "integration finished before cancellation"
                    with exn when Worktree.has_cancellation exn ->
                      cancelled := true;
                      raise exn)
                  (fun () ->
                    while not (Stdlib.Sys.file_exists marker) do
                      Eio.Time.sleep (Eio.Stdenv.clock env) 0.001
                    done));
            check "integration cancellation propagates" !cancelled;
            Unix.unlink marker;
            check_cleanup ()
          in
          (* Cancel while Git is checking out files, before worktree add releases
             its initialization lock. The marker makes this independent of host speed. *)
          let filter = dir ^ "/slow-smudge" in
          let stalled_script =
            "#!/bin/sh\necho $$ > "
            ^ Stdlib.Filename.quote filter_pid
            ^ "\nsleep 30 &\necho $! > "
            ^ Stdlib.Filename.quote helper_pid
            ^ "\ntouch "
            ^ Stdlib.Filename.quote marker
            ^ "\nwait\n"
          in
          write filter stalled_script;
          Unix.chmod filter 0o755;
          write
            (dir ^ "/.git/info/attributes")
            "shared filter=integration-cancel\n";
          Git.run_git ~cwd:dir
            [ "config"; "filter.integration-cancel.smudge"; filter ];
          (* The implementation must reap both the filter and its helper.
             Fixture teardown only removes files; it never kills subprocesses. *)
          let with_stalled_filter f =
            Stdlib.Fun.protect
              ~finally:(fun () ->
                List.iter [ filter_pid; helper_pid; marker ] ~f:(fun file ->
                    if Stdlib.Sys.file_exists file then Unix.unlink file))
              f
          in
          with_stalled_filter cancel_at_hook;
          with_stalled_filter cancel_at_hook;
          with_stalled_filter (fun () ->
              let clock = Eio.Stdenv.clock env in
              let started = Eio.Time.now clock in
              let result =
                Eio.Time.with_timeout clock 10. (fun () ->
                    Ok (integrate "late" late_head))
              in
              check "checkout reached before deadline"
                (Stdlib.Sys.file_exists marker);
              check "stalled checkout deadline propagates"
                (match result with Error `Timeout -> true | Ok _ -> false);
              check "deadline does not wait for stalled checkout"
                Float.(Eio.Time.now clock -. started < 20.);
              check_cleanup ());
          Unix.unlink (dir ^ "/.git/info/attributes");
          Git.run_git ~cwd:dir
            [ "config"; "--unset"; "filter.integration-cancel.smudge" ];
          (* Also cancel after acquisition, while publication is in progress. *)
          write hook stalled_script;
          Unix.chmod hook 0o755;
          with_stalled_filter cancel_at_hook;
          Unix.unlink hook;
          Stdlib.print_endline
            "PASS local Git feature integration, races, recovery and ancestry"))

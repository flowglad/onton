(* @archlint.module test
   @archlint.domain worktree *)
open Base
open Onton
open Onton_core
open Types
module Git = Onton_test_support.Git_env

let check label value = if not value then failwith label

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
            | Worktree.Noop | Worktree.Conflict _
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
          let _ = commit dir "shared" "upstream conflict\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "main" ];
          check "fetch conflicting upstream"
            (Result.is_ok (W.fetch_origin ~fetch_lock ~path:root_path));
          check "root merge conflict aborts"
            (match
               W.rebase_onto ~path:root_path
                 ~target:(Branch.of_string "origin/main")
                 ~upstream:base ~project_name:"git-test" ~ancestor_ids:[] ()
             with
            | Worktree.Error _ -> true
            | Worktree.Ok | Worktree.Noop | Worktree.Conflict _
            | Worktree.Uncommitted_changes _ ->
                false);
          check "root checkout clean after failed merge"
            (String.is_empty
               (Git.git_capture ~cwd:root_path [ "status"; "--porcelain" ])
            && Git.git_exit_code ~cwd:root_path [ "rev-parse"; "MERGE_HEAD" ]
               <> 0);
          check "no temporary worktree remains"
            (not
               (String.is_substring
                  (Git.git_capture ~cwd:dir
                     [ "worktree"; "list"; "--porcelain" ])
                  ~substring:"onton-integration-"));
          let late_head = child "late" "late-file" "late\n" in
          write hook "#!/bin/sh\nsleep 1\n";
          Unix.chmod hook 0o755;
          let cancelled =
            Eio.Time.with_timeout (Eio.Stdenv.clock env) 0.05 (fun () ->
                Ok (integrate "late" late_head))
          in
          check "integration cancellation propagates"
            (match cancelled with Error `Timeout -> true | Ok _ -> false);
          Unix.unlink hook;
          check "cancelled worktree cleaned"
            (not
               (String.is_substring
                  (Git.git_capture ~cwd:dir
                     [ "worktree"; "list"; "--porcelain" ])
                  ~substring:"onton-integration-"));
          check "root lock released after cancellation"
            (Runtime.with_root_write rt (fun () -> true));
          Stdlib.print_endline
            "PASS local Git feature integration, races, recovery and ancestry"))

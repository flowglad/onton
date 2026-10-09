(* @archlint.module test
   @archlint.domain worktree-parser *)

open Base
open Onton
open Onton_core
module Git_env = Onton_test_support.Git_env
module B = Branch_reconcile
module E = Branch_reconcile_executor

(* ───────────────────────────────────────────────────────────────────────
   Integration tests for owner-driven base reconciliation

   Each test creates a temporary git repo with a realistic branch topology,
   then drives durable owner commands and checks the resulting history.
   ─────────────────────────────────────────────────────────────────────── *)

(** Run a git command in [dir], fail on non-zero exit. Captures stdout (used by
    [commit_file] to read back the new commit's SHA). The env is scrubbed via
    {!Git_env.clean_env} so tests are unaffected when run inside a git hook
    (which would otherwise leak [GIT_DIR] / [GIT_WORK_TREE]). *)
let git ~process_mgr ~dir args =
  let stdout_buf = Buffer.create 64 in
  let stderr_buf = Buffer.create 64 in
  Eio.Switch.run (fun sw ->
      let env = Git_env.clean_env () in
      let child =
        Eio.Process.spawn ~sw process_mgr ~env
          ~stdout:(Eio.Flow.buffer_sink stdout_buf)
          ~stderr:(Eio.Flow.buffer_sink stderr_buf)
          ([ "git"; "-C"; dir ] @ args)
      in
      match Eio.Process.await child with
      | `Exited 0 -> ()
      | `Exited n ->
          failwith
            (Printf.sprintf "git %s failed (exit %d): %s"
               (String.concat ~sep:" " args)
               n
               (Buffer.contents stderr_buf))
      | `Signaled s -> failwith (Printf.sprintf "git signaled %d" s));
  String.strip (Buffer.contents stdout_buf)

(** Create a fresh git repo in a temp dir. No initial commit — callers add their
    own commits via [commit_file] to build a precise graph. *)
let init_repo () =
  let dir = Stdlib.Filename.temp_dir "onton_rebase_test_" "" in
  Git_env.init_repo dir;
  dir

(** Write a file, add, commit. Returns the commit SHA. *)
let commit_file ~process_mgr ~dir ~filename ~content ~msg =
  let path = Stdlib.Filename.concat dir filename in
  let oc = Stdlib.open_out path in
  Stdlib.output_string oc content;
  Stdlib.close_out oc;
  git ~process_mgr ~dir [ "add"; filename ] |> ignore;
  git ~process_mgr ~dir [ "commit"; "-m"; msg ] |> ignore;
  git ~process_mgr ~dir [ "rev-parse"; "HEAD" ]

let assert_eq label expected actual =
  if not (String.equal expected actual) then
    failwith (Printf.sprintf "%s: expected %s, got %s" label expected actual)

type reconciliation = {
  state : B.t;
  before : string;
  after : string;
  dirty : string;
  dirty_after : string;
}

let reconcile ~process_mgr ~clock ~path ~target ~project_name ~ancestor_ids () =
  (* Give every standalone history fixture a real publication destination. Its
     object store lives under .git so it cannot dirty the checkout. *)
  let git args = git ~process_mgr ~dir:path args in
  let remote = Stdlib.Filename.concat path ".git/fixture-origin.git" in
  ignore (git [ "init"; "--bare"; "-q"; remote ]);
  ignore (git [ "remote"; "add"; "origin"; remote ]);
  let branch = git [ "symbolic-ref"; "--short"; "HEAD" ] in
  let target = Types.Branch.to_string target in
  ignore (git [ "push"; "-q"; "origin"; target; branch ]);
  let before = git [ "rev-parse"; "HEAD" ] in
  let dirty = git [ "status"; "--porcelain" ] in
  let io = E.make_io ~process_mgr ~clock ~path in
  let intent =
    B.
      {
        base = target;
        policy = Rewrite;
        purpose =
          Reconcile_scoped
            {
              request = "fixture";
              project = project_name;
              ancestors = ancestor_ids;
            };
      }
  in
  let rec run remaining state =
    if remaining = 0 then failwith "reconciliation fixture did not terminate";
    let state =
      match B.decode (B.yojson_of_t state) with
      | Ok restored -> restored
      | Error reason -> failwith reason
    in
    match (B.operation state, B.pending state) with
    | Some operation, Some command ->
        let result =
          E.execute ~io ~prefix:"refs/onton/reconcile/fixture" ~branch
            ~operation command
        in
        run (remaining - 1)
          (fst
             (B.step state
                (B.Result { token = command.token; at = 100.; result })))
    | None, _ | _, None -> state
  in
  let state = run 100 (fst (B.step B.empty (B.Request intent))) in
  {
    state;
    before;
    after = git [ "rev-parse"; "HEAD" ];
    dirty;
    dirty_after = git [ "status"; "--porcelain" ];
  }

let assert_rebase_ok label result =
  if not (Option.equal B.equal_phase (B.phase result.state) (Some B.Settled))
  then
    failwith
      (label ^ ": "
      ^ String.concat ~sep:"; "
          (List.map (B.diagnostics result.state) ~f:(fun (key, value) ->
               key ^ "=" ^ value)))

let assert_rebase_noop label result =
  assert_rebase_ok label result;
  assert_eq (label ^ ": unchanged HEAD") result.before result.after

let assert_rebase_conflict label result =
  match B.phase result.state with
  | Some (B.Repairing { mode = B.Content_repair; _ }) -> ()
  | None
  | Some
      ( B.Preparing | Integrating
      | Repairing { mode = B.History_recovery _; _ }
      | Publishing | Confirming | Waiting _ | Recovering | Settled
      | Intervention _ ) ->
      failwith
        (label ^ ": "
        ^ String.concat ~sep:"; "
            (List.map (B.diagnostics result.state) ~f:(fun (key, value) ->
                 key ^ "=" ^ value)))

let assert_rebase_uncommitted label result =
  assert_eq (label ^ ": preserves HEAD") result.before result.after;
  if String.is_empty result.dirty then failwith (label ^ ": missing dirty files");
  assert_eq
    (label ^ ": preserves index and worktree status")
    result.dirty result.dirty_after;
  if
    not
      (Option.equal B.equal_phase (B.phase result.state)
         (Some (B.Intervention "dirty_worktree")))
  then failwith (label ^ ": expected recoverable dirty-worktree intervention")

(** Simulate squash-merge of [branch] into main: checkout main, create a single
    new commit with the same tree diff, then delete [branch]. *)
let squash_merge ~process_mgr ~dir ~branch =
  git ~process_mgr ~dir [ "checkout"; "main" ] |> ignore;
  git ~process_mgr ~dir [ "merge"; "--squash"; branch ] |> ignore;
  git ~process_mgr ~dir [ "commit"; "-m"; "squash-merge " ^ branch ] |> ignore;
  git ~process_mgr ~dir [ "branch"; "-D"; branch ] |> ignore

(** Read file contents from the working tree. *)
let read_file ~dir ~filename =
  let path = Stdlib.Filename.concat dir filename in
  let ic = Stdlib.open_in path in
  let content = Stdlib.input_line ic in
  Stdlib.close_in ic;
  content

let () =
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env in

  (* ── Test 1: basic rebase onto main (no dep commits) ─────────────── *)
  (* main: A -- B
     feat:    \-- C
     After rebase onto main: A -- B -- C *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   commit_file ~process_mgr ~dir ~filename:"b.txt" ~content:"b" ~msg:"B"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat"; "HEAD~1" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"c.txt" ~content:"c" ~msg:"C"
   |> ignore;
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "main")
       ~project_name:"" ~ancestor_ids:[] ()
   in
   assert_rebase_ok "test1: simple rebase" result;
   (* C should now be on top of B *)
   let log = git ~process_mgr ~dir [ "log"; "--oneline"; "--format=%s" ] in
   let lines = String.split_lines log in
   assert_eq "test1: head" "C" (List.hd_exn lines);
   assert_eq "test1: parent" "B" (List.nth_exn lines 1);
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 2: dep squash-merged, strip dep commits with --onto ───── *)
  (* Setup:
     main: A
     dep:  A -- D1 -- D2           (dep's commits)
     feat: A -- D1 -- D2 -- F1     (feat branched off dep)

     Then dep is squash-merged into main:
     main: A -- S                   (S = squash of D1+D2)

     Reconciling feat onto main should produce:
     main: A -- S -- F1'            (only F1 replayed, not D1/D2) *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   (* Create dep branch with 2 commits *)
   git ~process_mgr ~dir [ "checkout"; "-b"; "dep" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"d1.txt" ~content:"d1" ~msg:"D1"
   |> ignore;
   commit_file ~process_mgr ~dir ~filename:"d2.txt" ~content:"d2" ~msg:"D2"
   |> ignore;
   (* Create feat branch off dep *)
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"f1.txt" ~content:"f1" ~msg:"F1"
   |> ignore;
   (* Squash-merge dep into main *)
   squash_merge ~process_mgr ~dir ~branch:"dep";
   (* Now rebase feat onto main *)
   git ~process_mgr ~dir [ "checkout"; "feat" ] |> ignore;
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "main")
       ~project_name:"" ~ancestor_ids:[] ()
   in
   assert_rebase_ok "test2: onto after squash" result;
   let log = git ~process_mgr ~dir [ "log"; "--oneline"; "--format=%s" ] in
   let lines = String.split_lines log in
   (* Should be F1, squash-merge dep, A — NOT F1, D2, D1, ... *)
   assert_eq "test2: head is F1" "F1" (List.hd_exn lines);
   assert_eq "test2: parent is squash" "squash-merge dep" (List.nth_exn lines 1);
   assert_eq "test2: grandparent is A" "A" (List.nth_exn lines 2);
   assert_eq "test2: exactly 3 commits" "3" (Int.to_string (List.length lines));
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 3: multiple dep commits, multiple feat commits ─────────── *)
  (* dep:  A -- D1 -- D2 -- D3
     feat: A -- D1 -- D2 -- D3 -- F1 -- F2 -- F3
     After squash-merge of dep and rebase:
     main: A -- S -- F1' -- F2' -- F3' *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "dep" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"d1.txt" ~content:"d1" ~msg:"D1"
   |> ignore;
   commit_file ~process_mgr ~dir ~filename:"d2.txt" ~content:"d2" ~msg:"D2"
   |> ignore;
   commit_file ~process_mgr ~dir ~filename:"d3.txt" ~content:"d3" ~msg:"D3"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"f1.txt" ~content:"f1" ~msg:"F1"
   |> ignore;
   commit_file ~process_mgr ~dir ~filename:"f2.txt" ~content:"f2" ~msg:"F2"
   |> ignore;
   commit_file ~process_mgr ~dir ~filename:"f3.txt" ~content:"f3" ~msg:"F3"
   |> ignore;
   squash_merge ~process_mgr ~dir ~branch:"dep";
   git ~process_mgr ~dir [ "checkout"; "feat" ] |> ignore;
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "main")
       ~project_name:"" ~ancestor_ids:[] ()
   in
   assert_rebase_ok "test3: multi-commit" result;
   let log = git ~process_mgr ~dir [ "log"; "--oneline"; "--format=%s" ] in
   let lines = String.split_lines log in
   assert_eq "test3: commit count" "5" (Int.to_string (List.length lines));
   assert_eq "test3: head" "F3" (List.hd_exn lines);
   assert_eq "test3: F2" "F2" (List.nth_exn lines 1);
   assert_eq "test3: F1" "F1" (List.nth_exn lines 2);
   assert_eq "test3: squash" "squash-merge dep" (List.nth_exn lines 3);
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 4: already up-to-date → Noop ──────────────────────────── *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"f.txt" ~content:"f" ~msg:"F"
   |> ignore;
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "main")
       ~project_name:"" ~ancestor_ids:[] ()
   in
   assert_rebase_noop "test4: already up-to-date" result;
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 4b: dirty worktree remains recoverable ─────────────── *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"f.txt" ~content:"f" ~msg:"F"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "main" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"m.txt" ~content:"m" ~msg:"M"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "feat" ] |> ignore;
   let dirty_path = Stdlib.Filename.concat dir "dirty.txt" in
   let oc = Stdlib.open_out dirty_path in
   Stdlib.output_string oc "dirty";
   Stdlib.close_out oc;
   git ~process_mgr ~dir [ "add"; "dirty.txt" ] |> ignore;
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "main")
       ~project_name:"" ~ancestor_ids:[] ()
   in
   assert_rebase_uncommitted "test4b: dirty worktree" result;
   let rebase_merge = Stdlib.Filename.concat dir ".git/rebase-merge" in
   let rebase_apply = Stdlib.Filename.concat dir ".git/rebase-apply" in
   if Stdlib.Sys.file_exists rebase_merge || Stdlib.Sys.file_exists rebase_apply
   then failwith "test4b: rebase should not be in progress";
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 5: conflict during rebase → Conflict, working dir clean ─ *)
  (* Both main and feat modify the same file differently after dep merge *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"shared.txt" ~content:"base" ~msg:"A"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "dep" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"d.txt" ~content:"d" ~msg:"D"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"shared.txt" ~content:"feat-version"
     ~msg:"F"
   |> ignore;
   squash_merge ~process_mgr ~dir ~branch:"dep";
   (* Also modify shared.txt on main to create conflict *)
   commit_file ~process_mgr ~dir ~filename:"shared.txt" ~content:"main-version"
     ~msg:"M"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "feat" ] |> ignore;
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "main")
       ~project_name:"" ~ancestor_ids:[] ()
   in
   assert_rebase_conflict "test5: conflict" result;
   (* Rebase should be left in progress for the agent to resolve *)
   let rebase_dir = Stdlib.Filename.concat dir ".git/rebase-merge" in
   if not (Stdlib.Sys.file_exists rebase_dir) then
     failwith "test5: rebase should still be in progress";
   (* Clean up: abort so we can delete the dir *)
   git ~process_mgr ~dir [ "rebase"; "--abort" ] |> ignore;
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 6: chained deps — dep1 merged, dep2 still open ────────── *)
  (* main: A
     dep1: A -- D1
     dep2: A -- D1 -- D2            (branched off dep1)
     feat: A -- D1 -- D2 -- F1      (branched off dep2)

     dep1 squash-merged into main. feat rebases onto dep2 (not main).
     dep2 still has D1 in its history so this tests rebasing onto a
     non-main target that shares commits. *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "dep1" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"d1.txt" ~content:"d1" ~msg:"D1"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "dep2" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"d2.txt" ~content:"d2" ~msg:"D2"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"f1.txt" ~content:"f1" ~msg:"F1"
   |> ignore;
   (* Rebase feat onto dep2 — dep2 is already ancestor, should be Noop *)
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "dep2")
       ~project_name:"" ~ancestor_ids:[] ()
   in
   assert_rebase_noop "test6: dep2 already ancestor" result;
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 7: dep merged via real merge commit (not squash) ───────── *)
  (* Verifies --onto still works when dep is merge-committed (the dep
     commits ARE in main's history, so cherry-pick filtering should
     identify only feat's own commits). *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "dep" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"d1.txt" ~content:"d1" ~msg:"D1"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"f1.txt" ~content:"f1" ~msg:"F1"
   |> ignore;
   (* Merge dep into main (non-squash, real merge commit) *)
   git ~process_mgr ~dir [ "checkout"; "main" ] |> ignore;
   git ~process_mgr ~dir [ "merge"; "--no-ff"; "dep"; "-m"; "Merge dep" ]
   |> ignore;
   git ~process_mgr ~dir [ "branch"; "-d"; "dep" ] |> ignore;
   git ~process_mgr ~dir [ "checkout"; "feat" ] |> ignore;
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "main")
       ~project_name:"" ~ancestor_ids:[] ()
   in
   assert_rebase_ok "test7: real merge dependency" result;
   let log = git ~process_mgr ~dir [ "log"; "--oneline"; "--format=%s" ] in
   let lines = String.split_lines log in
   (* F1 should be the HEAD *)
   assert_eq "test7: head is F1" "F1" (List.hd_exn lines);
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 8: feat has no unique commits (all are dep's) ──────────── *)
  (* Edge case: feat branch = dep branch exactly. After dep squash-merge,
     rebasing feat onto main should ideally be a noop or produce an empty
     branch (all commits are duplicates). *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "dep" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"d1.txt" ~content:"d1" ~msg:"D1"
   |> ignore;
   (* feat = exact same commit as dep, no additional commits *)
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   squash_merge ~process_mgr ~dir ~branch:"dep";
   git ~process_mgr ~dir [ "checkout"; "feat" ] |> ignore;
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "main")
       ~project_name:"" ~ancestor_ids:[] ()
   in
   assert_rebase_ok "test8: empty replay settles" result;
   assert_eq "test8: empty branch equals target"
     (git ~process_mgr ~dir [ "rev-parse"; "main" ])
     result.after;
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 9: feat modifies a dep file, squash-merge, no conflict ── *)
  (* dep:  creates file X with "v1"
     feat: modifies X to "v2", adds own file
     After dep squash-merge, rebase should apply feat's changes cleanly *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "dep" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"x.txt" ~content:"v1" ~msg:"D1"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"x.txt" ~content:"v2" ~msg:"F1"
   |> ignore;
   commit_file ~process_mgr ~dir ~filename:"f.txt" ~content:"f" ~msg:"F2"
   |> ignore;
   squash_merge ~process_mgr ~dir ~branch:"dep";
   git ~process_mgr ~dir [ "checkout"; "feat" ] |> ignore;
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "main")
       ~project_name:"" ~ancestor_ids:[] ()
   in
   assert_rebase_ok "test9: patch edits retained after dependency squash" result;
   let lines =
     String.split_lines (git ~process_mgr ~dir [ "log"; "--format=%s" ])
   in
   assert_eq "test9: head" "F2" (List.hd_exn lines);
   assert_eq "test9: F1" "F1" (List.nth_exn lines 1);
   assert_eq "test9: squash" "squash-merge dep" (List.nth_exn lines 2);
   assert_eq "test9: x.txt content" "v2" (read_file ~dir ~filename:"x.txt");
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 10: ancestor-subject filter strips drifted dep commits ─── *)
  (* Regression for the trigger-only-execution / patch-7 case. A dep's
     commit survives on our branch with the conventional
     [<project>] Patch N: prefix but with *modified* content — so
     git log --cherry-pick cannot equate it with the squash on main by
     patch-id. Without the ancestor_ids fallback, the old dep commit
     would be replayed onto main; with ancestor_ids=["1"] the owner
     picks a newer old_base and only our own commit survives. *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "dep" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"dep.txt" ~content:"dep-v1"
     ~msg:"[proj] Patch 1: add dep.txt"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   (* Simulate the drift with a fresh branch-local recommit of Patch 1 rather
     than [commit --amend]. This keeps the scenario identical
     (same subject, different patch-id from main's squash) while avoiding the
     occasional amend-specific instability seen in CI. *)
   git ~process_mgr ~dir [ "reset"; "--hard"; "HEAD~1" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"dep.txt" ~content:"dep-v1-drift"
     ~msg:"[proj] Patch 1: add dep.txt"
   |> ignore;
   commit_file ~process_mgr ~dir ~filename:"mine.txt" ~content:"mine"
     ~msg:"[proj] Patch 7: add mine.txt"
   |> ignore;
   squash_merge ~process_mgr ~dir ~branch:"dep";
   git ~process_mgr ~dir [ "checkout"; "feat" ] |> ignore;
   (* Sanity: the cherry-pick filter alone keeps the drifted Patch 1 commit,
      so the positive end-state assertion below is specifically exercising
      the subject-filter code path. We verify both that the log contains
      both commits (not just Patch 7) *and* that the oldest kept SHA is
      Patch 1, so a future git version that patch-id-equates the drift with
      the squash surfaces as a line-count mismatch rather than a confusing
      SHA mismatch. *)
   let raw_log =
     git ~process_mgr ~dir
       [
         "log";
         "--cherry-pick";
         "--right-only";
         "--no-merges";
         "--no-show-signature";
         "--format=%H %s";
         "main...HEAD";
       ]
   in
   let log_lines =
     List.filter (String.split_lines raw_log) ~f:(fun l ->
         not (String.is_empty (String.strip l)))
   in
   assert_eq "test10: cherry-pick log has 2 commits (sanity)" "2"
     (Int.to_string (List.length log_lines));
   let patch1_sha = git ~process_mgr ~dir [ "rev-parse"; "HEAD~1" ] in
   (* The purpose of this precondition is to show that the raw cherry-pick walk
      still includes the drifted Patch 1 commit before the subject filter runs.
      Do not assert a specific line order here: [git log --cherry-pick] can
      vary its presentation order across environments. The owner receipt below
      establishes which replay boundary was actually selected. *)
   let log_shas =
     List.filter_map log_lines ~f:(fun line ->
         String.lsplit2 line ~on:' ' |> Option.map ~f:fst)
   in
   if not (List.mem log_shas patch1_sha ~equal:String.equal) then
     failwith
       (Printf.sprintf
          "test10: cherry-pick alone should still list drifted Patch 1 (%s)"
          patch1_sha);
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "main")
       ~project_name:"proj"
       ~ancestor_ids:[ Types.Patch_id.of_string "1" ]
       ()
   in
   let expected_boundary =
     match B.Commit.make patch1_sha with
     | Some revision -> B.Subject_inferred revision
     | None -> failwith "invalid fixture dependency revision"
   in
   if
     not
       (List.exists (B.integrations result.state) ~f:(fun receipt ->
            B.equal_boundary receipt.B.capture.replay_boundary expected_boundary))
   then failwith "test10: owner did not record the drifted dependency boundary";
   (* Subject inference cannot authorize dropping the drifted dependency from
      an already-published branch. Deterministic remote replay must try to
      restore that work before agent recovery; this fixture has a real content
      conflict between the drifted dependency and the captured base. *)
   (match B.phase result.state with
   | Some (B.Repairing { mode = B.Content_repair; _ }) ->
       if
         not
           (List.mem
              (String.split_lines
                 (git ~process_mgr ~dir
                    [ "diff"; "--name-only"; "--diff-filter=U" ]))
              "dep.txt" ~equal:String.equal)
       then failwith "test10: remote replay omitted the dependency conflict"
   | None
   | Some
       ( B.Preparing | Integrating | Publishing | Confirming | Waiting _
       | Recovering | Settled | Intervention _
       | Repairing { mode = B.History_recovery _; _ } ) ->
       failwith
         ("test10: unproven dependency ownership must attempt remote replay: "
         ^ Yojson.Safe.to_string (B.yojson_of_t result.state)));
   (match B.operation result.state with
   | Some { remote_integration = Some capture; _ }
     when String.equal
            (B.Commit.to_string capture.source_revision)
            result.before ->
       ()
   | Some _ | None -> failwith "test10: remote replay lost its captured source");
   assert_eq "test10: inferred ownership does not authorize publication"
     result.before
     (Git_env.git_capture
        ~cwd:(Stdlib.Filename.concat dir ".git/fixture-origin.git")
        [ "rev-parse"; "feat" ]);
   if
     not
       (List.mem
          (String.split_lines
             (git ~process_mgr ~dir
                [
                  "for-each-ref";
                  "--format=%(objectname)";
                  "refs/onton/reconcile/fixture/";
                ]))
          result.before ~equal:String.equal)
   then failwith "test10: original drifted work must stay pinned";
   let base_receipt =
     match B.integrations result.state with
     | [ receipt ] -> receipt
     | [] | _ :: _ :: _ ->
         failwith "test10: missing original base integration receipt"
   in
   let preserved = B.Commit.to_string base_receipt.integrated_revision in
   assert_eq "test10: own change survives base replay" "mine"
     (git ~process_mgr ~dir [ "show"; preserved ^ ":mine.txt" ]);
   assert_eq "test10: base replay uses merged trunk version" "dep-v1"
     (git ~process_mgr ~dir [ "show"; preserved ^ ":dep.txt" ]);
   assert_eq "test10: conflict retains merged trunk version" "dep-v1"
     (git ~process_mgr ~dir [ "show"; ":2:dep.txt" ]);
   assert_eq "test10: conflict retains drifted remote version" "dep-v1-drift"
     (git ~process_mgr ~dir [ "show"; ":3:dep.txt" ]);
   (match base_receipt.capture.replay_boundary with
   | B.Subject_inferred boundary
     when String.equal (B.Commit.to_string boundary) patch1_sha ->
       ()
   | B.Subject_inferred _ | B.Recorded _ | B.Inferred _ | B.Reconstructed _
   | B.Patch_equivalent _ | B.Plain ->
       failwith "test10: base receipt lost its inferred provenance");
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 11: dirty worktree is preserved for intervention ──────────── *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"f.txt" ~content:"committed" ~msg:"F"
   |> ignore;
   let original_head = git ~process_mgr ~dir [ "rev-parse"; "HEAD" ] in
   git ~process_mgr ~dir [ "checkout"; "main" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"b.txt" ~content:"b" ~msg:"B"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "feat" ] |> ignore;
   let dirty_path = Stdlib.Filename.concat dir "f.txt" in
   let oc = Stdlib.open_out dirty_path in
   Stdlib.output_string oc "unstaged";
   Stdlib.close_out oc;
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "main")
       ~project_name:"" ~ancestor_ids:[] ()
   in
   assert_rebase_uncommitted "test11: dirty worktree" result;
   if not (String.is_substring result.dirty ~substring:"f.txt") then
     failwith "test11: dirty status omitted file";
   assert_eq "test11: HEAD unchanged" original_head
     (git ~process_mgr ~dir [ "rev-parse"; "HEAD" ]);
   let rebase_merge = Stdlib.Filename.concat dir ".git/rebase-merge" in
   let rebase_apply = Stdlib.Filename.concat dir ".git/rebase-apply" in
   if Stdlib.Sys.file_exists rebase_merge || Stdlib.Sys.file_exists rebase_apply
   then failwith "test11: rebase should not be in progress";
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 12: untracked files are preserved for intervention ────────── *)
  (let dir = init_repo () in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"f.txt" ~content:"committed" ~msg:"F"
   |> ignore;
   let original_head = git ~process_mgr ~dir [ "rev-parse"; "HEAD" ] in
   git ~process_mgr ~dir [ "checkout"; "main" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"b.txt" ~content:"b" ~msg:"B"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "feat" ] |> ignore;
   let untracked_path = Stdlib.Filename.concat dir "scratch.txt" in
   let oc = Stdlib.open_out untracked_path in
   Stdlib.output_string oc "untracked";
   Stdlib.close_out oc;
   let result =
     reconcile ~process_mgr ~clock:(Eio.Stdenv.clock env) ~path:dir
       ~target:(Types.Branch.of_string "main")
       ~project_name:"" ~ancestor_ids:[] ()
   in
   assert_rebase_uncommitted "test12: dirty worktree" result;
   if not (String.is_substring result.dirty ~substring:"?? scratch.txt") then
     failwith "test12: dirty status omitted file";
   assert_eq "test12: HEAD unchanged" original_head
     (git ~process_mgr ~dir [ "rev-parse"; "HEAD" ]);
   let rebase_merge = Stdlib.Filename.concat dir ".git/rebase-merge" in
   let rebase_apply = Stdlib.Filename.concat dir ".git/rebase-apply" in
   if Stdlib.Sys.file_exists rebase_merge || Stdlib.Sys.file_exists rebase_apply
   then failwith "test12: rebase should not be in progress";
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  Stdlib.print_endline "All owner-driven rebase integration tests passed."

(* ───────────────────────────────────────────────────────────────────────
   Pure property tests for the remaining [Worktree_parser] decision APIs
   ([normalize_path], [parse_commit_count], [push_gate_from_count],
   [classify_push_result], [parse_push_porcelain]) referenced via the
   core module directly.
   ─────────────────────────────────────────────────────────────────────── *)

let () =
  let open QCheck2 in
  (* normalize_path: an absolute path is left as-is up to trailing-slash /
     trailing "/." normalization, and the result never ends in a bare "/"
     (for inputs longer than one char) nor in "/.". *)
  let prop_normalize_path_idempotent =
    (* Segments of lowercase letters joined by single slashes, then an
       absolute root. A second normalization pass is a no-op. *)
    Test.make ~name:"normalize_path: idempotent over absolute paths" ~count:300
      Gen.(
        list_size (int_range 1 5)
          (string_size ~gen:(char_range 'a' 'z') (int_range 1 6)))
      (fun segs ->
        let path = "/" ^ String.concat ~sep:"/" segs in
        let once = Worktree_parser.normalize_path ~cwd:"/tmp" path in
        let twice = Worktree_parser.normalize_path ~cwd:"/tmp" once in
        String.equal once twice && String.equal once path)
  in
  (* parse_commit_count: nonzero exit -> None; exit 0 with an integer body ->
     Some that integer (stripped). *)
  let prop_parse_commit_count =
    Test.make ~name:"parse_commit_count: code/body contract" ~count:300
      Gen.(pair (int_range 0 5) (int_range 0 1000))
      (fun (code, n) ->
        let stdout = Printf.sprintf "  %d \n" n in
        match Worktree_parser.parse_commit_count ~code ~stdout with
        | None -> code <> 0
        | Some parsed -> code = 0 && parsed = n)
  in
  (* push_gate_from_count: Some 0 -> Skip_no_commits; everything else (None or
     positive/negative count) -> Proceed. *)
  let prop_push_gate_from_count =
    Test.make ~name:"push_gate_from_count: only Some 0 skips" ~count:300
      Gen.(option int)
      (fun count ->
        match Worktree_parser.push_gate_from_count count with
        | Worktree_parser.Skip_no_commits -> (
            match count with Some 0 -> true | _ -> false)
        | Worktree_parser.Proceed -> (
            match count with Some 0 -> false | _ -> true))
  in
  (* classify_push_result: exit 0 with a '=' porcelain flag is Push_up_to_date;
     exit 0 otherwise is Push_ok. Uses generated stdout via the flag char. *)
  let prop_classify_push_result_ok =
    Test.make ~name:"classify_push_result: exit 0 maps via porcelain flag"
      ~count:300
      Gen.(oneof_list [ '='; '*'; ' '; '+' ])
      (fun flag ->
        let stdout =
          Printf.sprintf
            "To github.com:o/r.git\n\
             %c\trefs/heads/b:refs/heads/b\t[summary]\n\
             Done\n"
            flag
        in
        match
          Worktree_parser.classify_push_result ~code:0 ~stdout ~stderr:""
        with
        | Worktree_parser.Push_up_to_date -> Char.equal flag '='
        | Worktree_parser.Push_ok -> not (Char.equal flag '=')
        | Worktree_parser.Push_rejected _ | Worktree_parser.Push_error _ ->
            false)
  in
  let prop_classify_push_result_rejection =
    Test.make
      ~name:"classify_push_result: porcelain rejection reason reaches caller"
      Gen.unit (fun () ->
        let stdout =
          "To github.com:o/r.git\n\
           !\trefs/heads/b:refs/heads/b\t[rejected] (remote ref updated since \
           checkout)\n\
           Done\n"
        in
        let stderr =
          "error: failed to push some refs to 'github.com:o/r.git'"
        in
        Worktree_parser.equal_push_result
          (Worktree_parser.classify_push_result ~code:1 ~stdout ~stderr)
          (Worktree_parser.Push_rejected
             (Push_reject_classify.Unknown
                "[rejected] (remote ref updated since checkout)")))
  in
  (* parse_push_porcelain: feeding a single porcelain line returns that line's
     leading flag char. *)
  let prop_parse_push_porcelain_flag =
    Test.make ~name:"parse_push_porcelain: returns the leading flag char"
      ~count:300
      Gen.(oneof_list [ '+'; '!'; '='; '*' ])
      (fun flag ->
        let stdout =
          Printf.sprintf "To x\n%c\trefs/heads/b:refs/heads/b\t[s]\nDone\n" flag
        in
        match Worktree_parser.parse_push_porcelain stdout with
        | Some c -> Char.equal c flag
        | None -> false)
  in
  let suite =
    [
      prop_normalize_path_idempotent;
      prop_parse_commit_count;
      prop_push_gate_from_count;
      prop_classify_push_result_ok;
      prop_classify_push_result_rejection;
      prop_parse_push_porcelain_flag;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

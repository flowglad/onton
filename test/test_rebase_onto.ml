open Base
open Onton

(* ───────────────────────────────────────────────────────────────────────
   Pure tests for [Worktree.oldest_unique_commit]
   ─────────────────────────────────────────────────────────────────────── *)

let () =
  let open QCheck2 in
  let prop_empty =
    Test.make ~name:"oldest_unique_commit: empty -> Error" ~count:1 Gen.unit
      (fun () ->
        match Worktree.oldest_unique_commit "" with
        | Result.Error _ -> true
        | Result.Ok _ -> false)
  in
  let prop_whitespace_only =
    Test.make ~name:"oldest_unique_commit: whitespace-only -> Error" ~count:1
      Gen.unit (fun () ->
        match Worktree.oldest_unique_commit "  \n  \n" with
        | Result.Error _ -> true
        | Result.Ok _ -> false)
  in
  let prop_single_sha =
    Test.make ~name:"oldest_unique_commit: single SHA -> that SHA" ~count:1
      Gen.unit (fun () ->
        match Worktree.oldest_unique_commit "abc123\n" with
        | Result.Ok sha -> String.equal sha "abc123"
        | Result.Error _ -> false)
  in
  let prop_multiple_shas =
    Test.make ~name:"oldest_unique_commit: multiple -> last line (oldest)"
      ~count:1 Gen.unit (fun () ->
        let output = "newest111\nmiddle222\noldest333\n" in
        match Worktree.oldest_unique_commit output with
        | Result.Ok sha -> String.equal sha "oldest333"
        | Result.Error _ -> false)
  in
  let prop_trailing_whitespace =
    Test.make ~name:"oldest_unique_commit: trailing whitespace stripped"
      ~count:1 Gen.unit (fun () ->
        match Worktree.oldest_unique_commit "abc123  \n  " with
        | Result.Ok sha -> String.equal sha "abc123"
        | Result.Error _ -> false)
  in
  (* Property: for any non-empty list of SHAs, returns the last one *)
  let sha_gen =
    Gen.string_size ~gen:(Gen.char_range 'a' 'f') (Gen.int_range 6 40)
  in
  let prop_always_last =
    Test.make ~name:"oldest_unique_commit: always returns last line" ~count:200
      Gen.(list_size (int_range 1 20) sha_gen)
      (fun shas ->
        try
          let output = String.concat ~sep:"\n" shas ^ "\n" in
          match Worktree.oldest_unique_commit output with
          | Result.Ok sha -> String.equal sha (List.last_exn shas)
          | Result.Error _ -> false
        with _ -> false)
  in
  let suite =
    [
      prop_empty;
      prop_whitespace_only;
      prop_single_sha;
      prop_multiple_shas;
      prop_trailing_whitespace;
      prop_always_last;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

(* ───────────────────────────────────────────────────────────────────────
   Pure tests for [Worktree.parse_push_porcelain]
   ─────────────────────────────────────────────────────────────────────── *)

let () =
  let open QCheck2 in
  let prop_forced_update =
    Test.make ~name:"parse_push_porcelain: forced update -> Some '+'" ~count:1
      Gen.unit (fun () ->
        let output =
          "To github.com:owner/repo.git\n\
           +\trefs/heads/branch:refs/heads/branch\t...forced update\n\
           Done\n"
        in
        Option.equal Char.equal
          (Worktree.parse_push_porcelain output)
          (Some '+'))
  in
  let prop_rejected =
    Test.make ~name:"parse_push_porcelain: rejected -> Some '!'" ~count:1
      Gen.unit (fun () ->
        let output =
          "To github.com:owner/repo.git\n\
           !\trefs/heads/branch:refs/heads/branch\t[rejected] (stale info)\n\
           Done\n"
        in
        Option.equal Char.equal
          (Worktree.parse_push_porcelain output)
          (Some '!'))
  in
  let prop_empty =
    Test.make ~name:"parse_push_porcelain: empty -> None" ~count:1 Gen.unit
      (fun () -> Option.is_none (Worktree.parse_push_porcelain ""))
  in
  let prop_up_to_date =
    Test.make ~name:"parse_push_porcelain: up-to-date -> Some '='" ~count:1
      Gen.unit (fun () ->
        let output =
          "To github.com:owner/repo.git\n\
           =\trefs/heads/branch:refs/heads/branch\t[up to date]\n\
           Done\n"
        in
        Option.equal Char.equal
          (Worktree.parse_push_porcelain output)
          (Some '='))
  in
  let prop_to_line_only =
    Test.make ~name:"parse_push_porcelain: only To line -> None" ~count:1
      Gen.unit (fun () ->
        Option.is_none
          (Worktree.parse_push_porcelain "To github.com:owner/repo.git\n"))
  in
  let suite =
    [
      prop_forced_update;
      prop_rejected;
      prop_up_to_date;
      prop_empty;
      prop_to_line_only;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

(* ───────────────────────────────────────────────────────────────────────
   Pure tests for [Worktree.classify_fetch_result]
   ─────────────────────────────────────────────────────────────────────── *)

let () =
  let open QCheck2 in
  let prop_exit_zero_is_ok =
    Test.make ~name:"classify_fetch_result: exit 0 -> Ok ()" ~count:200
      Gen.string (fun stderr ->
        match Worktree.classify_fetch_result ~code:0 ~stderr with
        | Result.Ok () -> true
        | Result.Error _ -> false)
  in
  let prop_nonzero_is_error =
    Test.make ~name:"classify_fetch_result: exit != 0 -> Error" ~count:200
      Gen.(pair (int_range (-128) 255) string)
      (fun (code, stderr) ->
        if code = 0 then true
        else
          match Worktree.classify_fetch_result ~code ~stderr with
          | Result.Error _ -> true
          | Result.Ok () -> false)
  in
  let prop_error_message_includes_code =
    Test.make ~name:"classify_fetch_result: Error message embeds exit code"
      ~count:200
      Gen.(pair (int_range 1 255) string)
      (fun (code, stderr) ->
        match Worktree.classify_fetch_result ~code ~stderr with
        | Result.Error msg ->
            String.is_substring msg ~substring:(Printf.sprintf "exit %d" code)
        | Result.Ok () -> false)
  in
  let prop_error_message_strips_stderr =
    Test.make ~name:"classify_fetch_result: stderr is stripped in Error message"
      ~count:1 Gen.unit (fun () ->
        let stderr = "  oops  \n" in
        match Worktree.classify_fetch_result ~code:1 ~stderr with
        | Result.Error msg ->
            String.is_substring msg ~substring:"oops"
            && not (String.is_substring msg ~substring:"oops  \n")
        | Result.Ok () -> false)
  in
  let prop_ref_lock_error_surfaces_verbatim =
    Test.make
      ~name:
        "classify_fetch_result: real 'cannot lock ref' stderr surfaces in Error"
      ~count:1 Gen.unit (fun () ->
        (* Regression: this was the stderr observed in the outcome-tracking
           run. The classifier should surface it so downstream log/telemetry
           can still identify the race. *)
        let stderr =
          "error: cannot lock ref 'refs/remotes/origin/main': is at \
           11ea3d8d67b9c481e7c8ddec7a6e1d46f2db1ba8 but expected \
           d97cc64a88e05401a2f8fdf3624b79dbfb16671d\n\
           From github.com:flowglad/review-service\n\
          \ ! d97cc64..11ea3d8  main       -> origin/main  (unable to update \
           local ref)"
        in
        match Worktree.classify_fetch_result ~code:1 ~stderr with
        | Result.Error msg ->
            String.is_substring msg ~substring:"cannot lock ref"
            && String.is_substring msg ~substring:"exit 1"
        | Result.Ok () -> false)
  in
  let prop_total_no_raise =
    (* Totality: the classifier never raises for any (code, stderr). *)
    Test.make ~name:"classify_fetch_result: total (never raises)" ~count:500
      Gen.(pair (int_range (-256) 512) string)
      (fun (code, stderr) ->
        try
          let _ = Worktree.classify_fetch_result ~code ~stderr in
          true
        with _ -> false)
  in
  let suite =
    [
      prop_exit_zero_is_ok;
      prop_nonzero_is_error;
      prop_error_message_includes_code;
      prop_error_message_strips_stderr;
      prop_ref_lock_error_surfaces_verbatim;
      prop_total_no_raise;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

(* ───────────────────────────────────────────────────────────────────────
   Integration tests for [Worktree.rebase_onto]

   Each test creates a temporary git repo with a realistic branch topology,
   then calls rebase_onto and checks the result.
   ─────────────────────────────────────────────────────────────────────── *)

(** Strip GIT_DIR etc. so tests are not affected when run inside a git hook. *)
let clean_git_env () =
  Unix.environment () |> Array.to_list
  |> List.filter ~f:(fun s ->
      (not (String.is_prefix s ~prefix:"GIT_DIR="))
      && (not (String.is_prefix s ~prefix:"GIT_WORK_TREE="))
      && not (String.is_prefix s ~prefix:"GIT_INDEX_FILE="))
  |> Array.of_list

(** Run a git command in [dir], fail on non-zero exit. *)
let git ~process_mgr ~dir args =
  Eio.Switch.run @@ fun sw ->
  let stdout_buf = Buffer.create 64 in
  let stderr_buf = Buffer.create 64 in
  let env = clean_git_env () in
  let child =
    Eio.Process.spawn ~sw process_mgr ~env
      ~stdout:(Eio.Flow.buffer_sink stdout_buf)
      ~stderr:(Eio.Flow.buffer_sink stderr_buf)
      ([ "git"; "-C"; dir ] @ args)
  in
  (match Eio.Process.await child with
  | `Exited 0 -> ()
  | `Exited n ->
      failwith
        (Printf.sprintf "git %s failed (exit %d): %s"
           (String.concat ~sep:" " args)
           n
           (Buffer.contents stderr_buf))
  | `Signaled s -> failwith (Printf.sprintf "git signaled %d" s));
  String.strip (Buffer.contents stdout_buf)

(** Create a fresh git repo in a temp dir with an initial commit on main. *)
let init_repo ~process_mgr =
  let dir = Stdlib.Filename.temp_dir "onton_rebase_test_" "" in
  git ~process_mgr ~dir [ "init"; "-b"; "main" ] |> ignore;
  git ~process_mgr ~dir [ "config"; "user.email"; "test@test.com" ] |> ignore;
  git ~process_mgr ~dir [ "config"; "user.name"; "Test" ] |> ignore;
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

let assert_rebase_ok label = function
  | Worktree.Ok -> ()
  | Worktree.Noop | Worktree.Conflict | Worktree.Error _ ->
      failwith (Printf.sprintf "%s: expected Ok" label)

let assert_rebase_noop label = function
  | Worktree.Noop -> ()
  | Worktree.Ok | Worktree.Conflict | Worktree.Error _ ->
      failwith (Printf.sprintf "%s: expected Noop" label)

let assert_rebase_conflict label = function
  | Worktree.Conflict -> ()
  | Worktree.Ok | Worktree.Noop | Worktree.Error _ ->
      failwith (Printf.sprintf "%s: expected Conflict" label)

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
  (let dir = init_repo ~process_mgr in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   commit_file ~process_mgr ~dir ~filename:"b.txt" ~content:"b" ~msg:"B"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat"; "HEAD~1" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"c.txt" ~content:"c" ~msg:"C"
   |> ignore;
   let result =
     Worktree.rebase_onto ~process_mgr ~path:dir
       ~target:(Types.Branch.of_string "main")
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

     rebase_onto feat onto main should produce:
     main: A -- S -- F1'            (only F1 replayed, not D1/D2) *)
  (let dir = init_repo ~process_mgr in
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
     Worktree.rebase_onto ~process_mgr ~path:dir
       ~target:(Types.Branch.of_string "main")
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
  (let dir = init_repo ~process_mgr in
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
     Worktree.rebase_onto ~process_mgr ~path:dir
       ~target:(Types.Branch.of_string "main")
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
  (let dir = init_repo ~process_mgr in
   commit_file ~process_mgr ~dir ~filename:"a.txt" ~content:"a" ~msg:"A"
   |> ignore;
   git ~process_mgr ~dir [ "checkout"; "-b"; "feat" ] |> ignore;
   commit_file ~process_mgr ~dir ~filename:"f.txt" ~content:"f" ~msg:"F"
   |> ignore;
   let result =
     Worktree.rebase_onto ~process_mgr ~path:dir
       ~target:(Types.Branch.of_string "main")
   in
   assert_rebase_noop "test4: already up-to-date" result;
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 5: conflict during rebase → Conflict, working dir clean ─ *)
  (* Both main and feat modify the same file differently after dep merge *)
  (let dir = init_repo ~process_mgr in
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
     Worktree.rebase_onto ~process_mgr ~path:dir
       ~target:(Types.Branch.of_string "main")
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
  (let dir = init_repo ~process_mgr in
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
     Worktree.rebase_onto ~process_mgr ~path:dir
       ~target:(Types.Branch.of_string "dep2")
   in
   assert_rebase_noop "test6: dep2 already ancestor" result;
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 7: dep merged via real merge commit (not squash) ───────── *)
  (* Verifies --onto still works when dep is merge-committed (the dep
     commits ARE in main's history, so cherry-pick filtering should
     identify only feat's own commits). *)
  (let dir = init_repo ~process_mgr in
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
     Worktree.rebase_onto ~process_mgr ~path:dir
       ~target:(Types.Branch.of_string "main")
   in
   (* With a real merge, D1 is in main's history so cherry-pick should
      identify only F1 as unique. Result should be Ok or Noop depending
      on whether main is already an ancestor. *)
   (match result with
   | Worktree.Ok | Worktree.Noop -> ()
   | Worktree.Conflict | Worktree.Error _ ->
       failwith "test7: expected Ok or Noop");
   let log = git ~process_mgr ~dir [ "log"; "--oneline"; "--format=%s" ] in
   let lines = String.split_lines log in
   (* F1 should be the HEAD *)
   assert_eq "test7: head is F1" "F1" (List.hd_exn lines);
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 8: feat has no unique commits (all are dep's) ──────────── *)
  (* Edge case: feat branch = dep branch exactly. After dep squash-merge,
     rebasing feat onto main should ideally be a noop or produce an empty
     branch (all commits are duplicates). *)
  (let dir = init_repo ~process_mgr in
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
     Worktree.rebase_onto ~process_mgr ~path:dir
       ~target:(Types.Branch.of_string "main")
   in
   (* find_old_base returns Error "no unique commits found" so we fall
      back to plain rebase. Plain rebase should produce Ok or conflict
      since D1 content overlaps with squash. Either way it shouldn't crash. *)
   (match result with
   | Worktree.Ok | Worktree.Noop | Worktree.Conflict -> ()
   | Worktree.Error msg ->
       failwith (Printf.sprintf "test8: unexpected error: %s" msg));
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  (* ── Test 9: feat modifies a dep file, squash-merge, no conflict ── *)
  (* dep:  creates file X with "v1"
     feat: modifies X to "v2", adds own file
     After dep squash-merge, rebase should apply feat's changes cleanly *)
  (let dir = init_repo ~process_mgr in
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
     Worktree.rebase_onto ~process_mgr ~path:dir
       ~target:(Types.Branch.of_string "main")
   in
   assert_rebase_ok "test9: modify dep file" result;
   let log = git ~process_mgr ~dir [ "log"; "--oneline"; "--format=%s" ] in
   let lines = String.split_lines log in
   assert_eq "test9: head" "F2" (List.hd_exn lines);
   assert_eq "test9: F1" "F1" (List.nth_exn lines 1);
   assert_eq "test9: squash" "squash-merge dep" (List.nth_exn lines 2);
   (* x.txt should contain feat's version *)
   let content = read_file ~dir ~filename:"x.txt" in
   assert_eq "test9: x.txt content" "v2" content;
   Stdlib.Sys.command (Printf.sprintf "rm -rf %s" dir) |> ignore);

  Stdlib.print_endline "All rebase_onto integration tests passed."

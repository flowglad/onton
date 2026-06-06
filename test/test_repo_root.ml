(* @archlint.module test
   @archlint.domain repo-root *)

(** Tests for [Onton.Repo_root.normalize]: running [onton] from any directory
    inside a git repository — including a worktree — should resolve to the main
    working tree, never to a worktree path. *)

open Onton
module Git_env = Onton_test_support.Git_env

let ( // ) = Filename.concat

let with_main_and_worktree f =
  Git_env.with_temp_repo (fun main ->
      Git_env.run_git ~cwd:main
        [ "commit"; "-q"; "--allow-empty"; "-m"; "init" ];
      let wt_parent = Filename.temp_dir "onton-repo-root-wt-" "" in
      let wt = wt_parent // "patch-1" in
      Fun.protect
        ~finally:(fun () ->
          ignore
            (Sys.command
               (Printf.sprintf "rm -rf %s" (Filename.quote wt_parent))))
        (fun () ->
          Git_env.run_git ~cwd:main [ "worktree"; "add"; "-b"; "feat"; wt ];
          f ~main ~wt))

let with_chdir dir f =
  let saved = Sys.getcwd () in
  Fun.protect
    ~finally:(fun () -> try Unix.chdir saved with _ -> ())
    (fun () ->
      Unix.chdir dir;
      f ())

let real_path p =
  (* Resolve symlinks — macOS /tmp → /private/tmp — so path comparisons hold
     regardless of how [Sys.getcwd] vs the [git] binary render the prefix. *)
  let ic =
    Unix.open_process_in (Printf.sprintf "cd %s && pwd -P" (Filename.quote p))
  in
  let line = input_line ic in
  ignore (Unix.close_process_in ic);
  line

let assert_eq ~ctx expected actual =
  if String.equal expected actual then ()
  else
    failwith (Printf.sprintf "[%s] expected=%s actual=%s" ctx expected actual)

let test_main_from_main_repo () =
  with_main_and_worktree (fun ~main ~wt:_ ->
      with_chdir main (fun () ->
          let got = Repo_root.normalize "." in
          assert_eq ~ctx:"main-from-main" (real_path main) got))

let test_main_from_worktree () =
  with_main_and_worktree (fun ~main ~wt ->
      with_chdir wt (fun () ->
          let got = Repo_root.normalize "." in
          assert_eq ~ctx:"main-from-worktree" (real_path main) got))

let test_absolute_worktree_path_resolves () =
  with_main_and_worktree (fun ~main ~wt ->
      let got = Repo_root.normalize wt in
      assert_eq ~ctx:"absolute-worktree" (real_path main) got)

let test_trailing_dot_stripped () =
  with_main_and_worktree (fun ~main ~wt:_ ->
      let got = Repo_root.normalize (main // ".") in
      assert_eq ~ctx:"trailing-dot" (real_path main) got)

let test_trailing_slash_stripped () =
  with_main_and_worktree (fun ~main ~wt:_ ->
      let got = Repo_root.normalize (main ^ "/") in
      assert_eq ~ctx:"trailing-slash" (real_path main) got)

let test_non_repo_passthrough () =
  (* Outside any git repo: return absolute-but-unresolved, so downstream
     error paths surface the non-repo case rather than masking it. *)
  let tmp = Filename.temp_dir "onton-repo-root-nonrepo-" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote tmp))))
    (fun () ->
      let got = Repo_root.normalize tmp in
      (* No git repo here, so the resolved path equals the input. *)
      assert_eq ~ctx:"non-repo" tmp got)

let () =
  test_main_from_main_repo ();
  test_main_from_worktree ();
  test_absolute_worktree_path_resolves ();
  test_trailing_dot_stripped ();
  test_trailing_slash_stripped ();
  test_non_repo_passthrough ();
  print_endline "test_repo_root: OK"

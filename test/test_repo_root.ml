(** Tests for [Onton.Repo_root.normalize]: running [onton] from any directory
    inside a git repository — including a worktree — should resolve to the main
    working tree, never to a worktree path. *)

open Onton

let ( // ) = Filename.concat

(** Create a unique temp directory. *)
let mktempdir prefix =
  let base = Filename.get_temp_dir_name () in
  let rec loop n =
    let dir = base // Printf.sprintf "%s-%d-%d" prefix (Unix.getpid ()) n in
    try
      Unix.mkdir dir 0o755;
      dir
    with Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n + 1)
  in
  loop 0

(** Remove [dir] recursively. Best-effort; swallows errors. *)
let rm_rf dir =
  let cmd = Printf.sprintf "rm -rf %s" (Filename.quote dir) in
  ignore (Sys.command cmd)

(** Run a command from [cwd] and assert it succeeded. *)
let sh_at cwd cmd =
  let full =
    Printf.sprintf "cd %s && %s >/dev/null 2>&1" (Filename.quote cwd) cmd
  in
  match Sys.command full with
  | 0 -> ()
  | n -> failwith (Printf.sprintf "command failed (%d): %s" n cmd)

(** Initialize a git repo at [dir] with a single commit. *)
let init_repo dir =
  sh_at dir "git init -q -b main";
  sh_at dir "git config user.email test@example.com";
  sh_at dir "git config user.name test";
  sh_at dir "git commit -q --allow-empty -m init"

let with_main_and_worktree f =
  let main = mktempdir "onton-repo-root-main" in
  let wt_parent = mktempdir "onton-repo-root-wt" in
  let wt = wt_parent // "patch-1" in
  Fun.protect
    ~finally:(fun () ->
      rm_rf main;
      rm_rf wt_parent)
    (fun () ->
      init_repo main;
      sh_at main
        (Printf.sprintf "git worktree add -b feat %s" (Filename.quote wt));
      f ~main ~wt)

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
  let tmp = mktempdir "onton-repo-root-nonrepo" in
  Fun.protect
    ~finally:(fun () -> rm_rf tmp)
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

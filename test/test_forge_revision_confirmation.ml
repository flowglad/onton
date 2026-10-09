(* @archlint.module test
   @archlint.domain branch-reconcile *)
open Onton
open Onton_core
module E = Branch_reconcile_executor
module Git = Onton_test_support.Git_env

let commit dir label =
  Git.run_git ~cwd:dir [ "commit"; "--allow-empty"; "-qm"; label ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let text = function
  | Ok (Some revision) -> Some (Branch_reconcile.Commit.to_string revision)
  | Ok None -> None
  | Error reason -> failwith reason

let () =
  Eio_main.run @@ fun env ->
  Git.with_temp_repo (fun fetch_remote ->
      let base = commit fetch_remote "fetch base" in
      Git.with_temp_repo (fun push_remote ->
          let head = commit push_remote "push head" in
          Git.run_git ~cwd:push_remote [ "branch"; "patch" ];
          Git.with_temp_repo (fun local ->
              Git.run_git ~cwd:local [ "remote"; "add"; "origin"; fetch_remote ];
              Git.run_git ~cwd:local
                [ "config"; "remote.origin.pushurl"; push_remote ];
              Git.run_git ~cwd:local [ "fetch"; "-q"; "origin" ];
              let refs () =
                Git.git_capture ~cwd:local
                  [ "for-each-ref"; "--format=%(refname) %(objectname)" ]
              in
              let before = refs () in
              let io =
                E.make_io
                  ~process_mgr:(Eio.Stdenv.process_mgr env)
                  ~clock:(Eio.Stdenv.clock env) ~path:local
              in
              assert (text (E.remote_base io ~branch:"main") = Some base);
              assert (text (E.remote_head io ~branch:"patch") = Some head);
              let moved = commit fetch_remote "base advanced" in
              assert (text (E.remote_base io ~branch:"main") = Some moved);
              Git.run_git ~cwd:fetch_remote
                [ "update-ref"; "refs/heads/main"; base ];
              assert (text (E.remote_base io ~branch:"main") = Some base);
              assert (E.remote_base io ~branch:"missing" = Ok None);
              assert (refs () = before))));
  let failed = E.{ git = (fun _ -> (128, "", "transport unavailable")) } in
  assert (Result.is_error (E.remote_base failed ~branch:"main"));
  let malformed =
    E.{ git = (fun _ -> (0, "invalid\trefs/heads/main\n", "")) }
  in
  assert (Result.is_error (E.remote_base malformed ~branch:"main"));
  print_endline
    "forge head/base confirmation uses the correct remote without changing \
     refs: OK"

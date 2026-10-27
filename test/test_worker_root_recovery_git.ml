(* @archlint.module test
   @archlint.domain branch-reconcile *)
open Onton
open Onton_core

[@@@warning "-4"]

module F = Onton_test_support.Branch_reconcile_git_fixture
module Git = Onton_test_support.Git_env
module B = Branch_reconcile
module R = Branch_reconcile_runner
module E = Branch_reconcile_executor

let check = F.check
let sha = F.sha
let run runtime persist io event = F.run runtime persist io event

let () =
  F.timed ~name:"root-whole-clone-loss" (fun () ->
      Eio_main.run @@ fun env ->
      let make_io dir =
        E.make_io
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~path:dir
      in
      Git.with_temp_repo (fun remote ->
          let base = F.commit remote "base" "base\n" "base" in
          Git.with_temp_repo (fun old ->
              F.prepare remote old;
              let root_before =
                F.commit old "root" "root content\n" "root work"
              in
              Git.run_git ~cwd:remote [ "checkout"; "-q"; "-b"; "first-child" ];
              let first =
                F.commit remote "child-one" "first sibling\n" "first child"
              in
              Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
              Git.run_git ~cwd:remote [ "checkout"; "-q"; "-b"; "second-child" ];
              let second =
                F.commit remote "child-two" "second sibling\n" "second child"
              in
              Git.run_git ~cwd:remote [ "checkout"; "-q"; "main" ];
              let runtime = F.make_runtime () in
              let checkpoint = Filename.temp_file "root-captured-" ".json" in
              Fun.protect
                ~finally:(fun () -> Sys.remove checkpoint)
                (fun () ->
                  let persist = F.persist checkpoint in
                  let io = make_io old in
                  let request contributor revision =
                    B.Request
                      B.
                        {
                          base = "main";
                          policy = Preserve_ancestry;
                          purpose =
                            Integrate_revision
                              { contributor; revision = sha revision };
                        }
                  in
                  let first_result =
                    run runtime persist io (request "2" first)
                  in
                  check "first captured contribution settled"
                    (first_result = R.Idle);
                  let lost =
                    E.
                      {
                        git =
                          (fun args ->
                            let code, out, err = io.git args in
                            match args with
                            | "push" :: _ when code = 0 ->
                                (128, "", "lost push response")
                            | _ -> (code, out, err));
                      }
                  in
                  let outcome = run runtime persist lost (request "3" second) in
                  check "lost successful push response remains pending"
                    (outcome = R.Waiting);
                  check "origin contains both captured contributions"
                    (Git.git_exit_code ~cwd:remote
                       [ "merge-base"; "--is-ancestor"; first; "patch" ]
                     = 0
                    && Git.git_exit_code ~cwd:remote
                         [ "merge-base"; "--is-ancestor"; second; "patch" ]
                       = 0);
                  check "old root ancestor preserved"
                    (Git.git_exit_code ~cwd:remote
                       [ "merge-base"; "--is-ancestor"; root_before; "patch" ]
                    = 0);
                  (* Remove the entire object database; a new process can use only the
             committed owner snapshot and the observed origin. *)
                  Git.sh ~dir:"/" ("rm -rf " ^ Filename.quote old);
                  check "old clone destroyed" (not (Sys.file_exists old));
                  Git.with_temp_repo (fun fresh ->
                      F.prepare remote fresh;
                      Git.run_git ~cwd:fresh
                        [ "fetch"; "-q"; "origin"; "patch" ];
                      Git.run_git ~cwd:fresh
                        [ "reset"; "--hard"; "origin/patch" ];
                      let restored =
                        Runtime.create ~gameplan:F.gameplan
                          ~main_branch:(Types.Branch.of_string "main")
                          ~snapshot:(F.get (Persistence.load ~path:checkpoint))
                          ()
                      in
                      let fresh_io = make_io fresh in
                      (* Unavailable evidence is not absence. It retains the request. *)
                      let failing =
                        E.
                          {
                            git =
                              (fun _ -> (128, "", "observation unavailable"));
                          }
                      in
                      ignore
                        (run restored persist failing
                           (B.Workspace_lost { request = "replacement-1" }));
                      check "captured contributor intent survives loss"
                        (match B.operation (F.state restored) with
                        | Some op ->
                            op.B.intent.B.purpose
                            = B.Integrate_revision
                                { contributor = "3"; revision = sha second }
                        | None -> false);
                      let outcome = run restored persist fresh_io B.Recover in
                      check "origin observation settles without reintegration"
                        (outcome = R.Idle || outcome = R.Waiting);
                      check "first published receipt survives replacement"
                        (List.length (B.publications (F.state restored)) = 1);
                      check "origin observation identifies published revision"
                        (match B.workspace_outcome (F.state restored) with
                        | Some (B.Workspace_patch _) -> true
                        | _ -> false);
                      ignore (run restored persist fresh_io B.Workspace_resumed);
                      check
                        "lost publication settles without a duplicate \
                         integration"
                        (List.length (B.publications (F.state restored)) = 2);
                      let confirmed =
                        Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                      in
                      ignore
                        (run restored persist fresh_io (request "3" second));
                      check "duplicate captured request does not reintegrate"
                        (List.length (B.publications (F.state restored)) = 2
                        && Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                           = confirmed);
                      check "no second root integration commit"
                        (Git.git_capture ~cwd:remote [ "rev-parse"; "patch" ]
                        = Git.git_capture ~cwd:fresh
                            [ "rev-parse"; "origin/patch" ]);
                      check "fresh object database independent"
                        (not
                           (Sys.file_exists
                              (Filename.concat fresh
                                 ".git/objects/info/alternates")));
                      ignore base)))));
  print_endline
    "root lost-workspace: captured contributors, ancestry, origin evidence"

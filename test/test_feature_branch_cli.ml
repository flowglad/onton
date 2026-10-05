(* @archlint.module test
   @archlint.domain project-store *)
open Base
open Onton
open Onton_core
open Types
module Git = Onton_test_support.Git_env

let check name value = if not value then failwith name

let root_patch =
  Patch.
    {
      id = Patch_id.of_string "1";
      branch = Branch.of_string "feature-root";
      dependencies = [];
      title = "root";
      description = "";
      spec = "";
      acceptance_criteria = [];
      files = [];
      classification = "";
      changes = [];
      test_stubs_introduced = [];
      test_stubs_implemented = [];
      complexity = None;
      precedents = [];
      required_context = [];
    }

let () =
  Eio_main.run (fun env ->
      Git.with_temp_repo (fun dir ->
          let data = dir ^ "/data" in
          let old = Stdlib.Sys.getenv_opt "ONTON_DATA_DIR" in
          Unix.putenv "ONTON_DATA_DIR" data;
          Stdlib.Fun.protect
            ~finally:(fun () ->
              match old with
              | Some x -> Unix.putenv "ONTON_DATA_DIR" x
              | None -> Unix.unsetenv "ONTON_DATA_DIR")
            (fun () ->
              let save name =
                Project_store.save_config ~project_name:name
                  ~github_owner:"test" ~github_repo:"test" ~backend:"claude"
                  ~model:"" ~main_branch:"main" ~poll_interval:30.
                  ~repo_root:dir ~max_concurrency:1 ~max_ci_failures:5
                  ~automerge_timeout:120. ()
              in
              save "ordinary";
              let load name =
                match Project_store.load_config ~project_name:name with
                | Ok c -> c
                | Error e -> failwith e
              in
              check "ordinary mode default"
                (Option.is_none (load "ordinary").Project_store.feature_root);
              let mode =
                match
                  Execution_mode.infer (Graph.of_patches [ root_patch ])
                with
                | Ok m -> m
                | Error e -> failwith e
              in
              save "feature";
              Project_store.save_execution_mode ~project_name:"feature" mode;
              check "root persisted"
                (Option.equal String.equal
                   (load "feature").Project_store.feature_root (Some "1"));
              save "feature";
              check "resolving resume settings preserves mode"
                (Option.equal String.equal
                   (load "feature").Project_store.feature_root (Some "1"));
              let legacy_path = Project_store.config_path "ordinary" in
              let legacy = Yojson.Safe.from_file legacy_path in
              let legacy =
                match legacy with
                | `Assoc fields ->
                    `Assoc
                      (List.Assoc.remove fields "feature_root"
                         ~equal:String.equal)
                | _ -> failwith "config object"
              in
              Yojson.Safe.to_file legacy_path legacy;
              check "legacy absence defaults mainline"
                (Option.is_none (load "ordinary").Project_store.feature_root);
              let executable =
                Stdlib.Filename.concat
                  (Stdlib.Filename.dirname Stdlib.Sys.executable_name)
                  "../bin/main.exe"
              in
              let invoke args =
                let output = Buffer.create 256 in
                let result =
                  Eio.Time.with_timeout (Eio.Stdenv.clock env) 5. (fun () ->
                      let code =
                        Eio.Switch.run (fun sw ->
                            let child =
                              Eio.Process.spawn ~sw
                                (Eio.Stdenv.process_mgr env)
                                ~env:(Git.clean_env ())
                                ~stdin:(Eio.Flow.string_source "")
                                ~stdout:(Eio.Flow.buffer_sink output)
                                ~stderr:(Eio.Flow.buffer_sink output)
                                (executable :: args)
                            in
                            match Eio.Process.await child with
                            | `Exited c -> c
                            | `Signaled _ -> 128)
                      in
                      Ok (code, Buffer.contents output))
                in
                match result with
                | Ok v -> v
                | Error `Timeout -> failwith "CLI timed out"
              in
              let code, help =
                invoke
                  [
                    "--forge"; "github"; "--clone-scheme"; "ssh"; "--help=plain";
                  ]
              in
              check "forge and clone scheme values reach Cmdliner"
                (code = 0
                && String.is_substring help ~substring:"--feature-branch");
              let rejects args message =
                let code, output = invoke args in
                if
                  not
                    (code <> 0 && String.is_substring output ~substring:message)
                then
                  failwith
                    ("CLI rejects: " ^ message ^ " (output: " ^ output ^ ")")
              in
              rejects
                [ "ordinary"; "--feature-branch" ]
                "existing projects cannot switch";
              rejects [ "--feature-branch" ] "requires a GitHub gameplan";
              rejects
                [ "--feature-branch"; "--publish-gameplan" ]
                "requires a GitHub gameplan";
              rejects
                [ "--feature-branch"; "--forge"; "sourcehut" ]
                "requires a GitHub gameplan";
              let gp_path = dir ^ "/gameplan.json" in
              let write patches =
                Yojson.Safe.to_file gp_path
                  (`Assoc
                     [
                       ("projectName", `String "new-feature");
                       ("owner", `String "test");
                       ("repo", `String "test");
                       ("solutionSummary", `String "test");
                       ( "patches",
                         `List
                           (List.map patches ~f:(fun p ->
                                `Assoc
                                  [
                                    ( "number",
                                      `String (Patch_id.to_string p.Patch.id) );
                                    ("title", `String "patch");
                                    ("changes", `List []);
                                  ])) );
                       ( "dependencyGraph",
                         `List
                           (List.map patches ~f:(fun p ->
                                `Assoc
                                  [
                                    ( "patch",
                                      `String (Patch_id.to_string p.Patch.id) );
                                    ("dependsOn", `List []);
                                  ])) );
                     ])
              in
              write [];
              rejects
                [ "--gameplan"; gp_path; "--feature-branch" ]
                "nonempty gameplan";
              rejects
                [
                  "--gameplan";
                  gp_path;
                  "--feature-branch";
                  "--publish-gameplan";
                ]
                "nonempty gameplan";
              write
                [
                  root_patch;
                  {
                    root_patch with
                    Patch.id = Patch_id.of_string "2";
                    Patch.branch = Branch.of_string "other-root";
                  };
                ];
              rejects
                [ "--gameplan"; gp_path; "--feature-branch" ]
                "exactly one dependency root";
              rejects
                [
                  "--gameplan";
                  gp_path;
                  "--feature-branch";
                  "--publish-gameplan";
                ]
                "exactly one dependency root";
              check "invalid mode never persisted"
                (not (Project_store.project_exists "new-feature"));
              write [ root_patch ];
              let managed = Project_store.managed_repo_dir "new-feature" in
              Project_store.ensure_dir managed;
              Git.run_git ~cwd:managed [ "init"; "-b"; "main" ];
              Git.run_git ~cwd:managed [ "config"; "user.name"; "Test" ];
              Git.run_git ~cwd:managed
                [ "config"; "user.email"; "test@example.com" ];
              Git.run_git ~cwd:managed
                [ "config"; "core.sshCommand"; "/usr/bin/false" ];
              Git.run_git ~cwd:managed
                [ "commit"; "--allow-empty"; "-m"; "base" ];
              Git.run_git ~cwd:managed
                [ "remote"; "add"; "origin"; "git@github.com:test/test.git" ];
              (* The existing managed clone refuses fetches locally. Terminal
                 validation stops startup before any forge API requests. *)
              rejects
                [
                  "--gameplan";
                  gp_path;
                  "--feature-branch";
                  "--publish-gameplan";
                  "--token";
                  "unused";
                  "--main-branch";
                  "new-feature/patch-1";
                ]
                "Integration root branch must differ";
              check "combined fresh startup persists publication"
                (Option.is_some
                   (load "new-feature").Project_store.gameplan_publication);
              let publication =
                match
                  Gameplan_publication.create ~directory:"gameplans"
                    ~project_name:"new-feature" ~yaml:false
                    ~content:
                      (Stdlib.In_channel.with_open_bin gp_path
                         Stdlib.In_channel.input_all)
                with
                | Ok p -> p
                | Error e -> failwith e
              in
              Project_store.save_gameplan_source ~project_name:"new-feature"
                ~source_path:gp_path;
              Project_store.save_config ~project_name:"new-feature"
                ~github_owner:"test" ~github_repo:"test" ~backend:"claude"
                ~model:"" ~main_branch:"new-feature/patch-1" ~poll_interval:30.
                ~repo_root:dir ~max_concurrency:1 ~max_ci_failures:5
                ~automerge_timeout:120. ~gameplan_publication:publication ();
              Project_store.save_execution_mode ~project_name:"new-feature" mode;
              (* Stop at terminal validation, before creating any network
                 capabilities. This exercises the real resume/config path. *)
              rejects
                [ "new-feature"; "--token"; "unused" ]
                "Integration root branch must differ";
              rejects
                [ "new-feature"; "--publish-gameplan"; "--token"; "unused" ]
                "Integration root branch must differ";
              check "combined resume preserves implementation root"
                (Option.equal String.equal
                   (load "new-feature").Project_store.feature_root (Some "1"));
              Stdlib.print_endline
                "PASS feature branch CLI and config persistence")))

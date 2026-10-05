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
                "cannot be combined with feature branch mode";
              rejects
                [ "feature"; "--publish-gameplan" ]
                "cannot be combined with feature branch mode";
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
              check "invalid mode never persisted"
                (not (Project_store.project_exists "new-feature"));
              Stdlib.print_endline
                "PASS feature branch CLI and config persistence")))

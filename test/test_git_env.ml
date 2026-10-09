(* @archlint.module test
   @archlint.domain priority *)

open Onton
open Onton_core

let env_list () = Git_env.clean_env () |> Array.to_list

let binding name s =
  let prefix = name ^ "=" in
  String.length s >= String.length prefix
  && String.equal (String.sub s 0 (String.length prefix)) prefix

let value name env =
  let prefix = name ^ "=" in
  List.find_map
    (fun s ->
      if binding name s then
        Some
          (String.sub s (String.length prefix)
             (String.length s - String.length prefix))
      else None)
    env

let count name env = List.length (List.filter (binding name) env)

let restore_env name old_value =
  match old_value with
  | Some value -> Unix.putenv name value
  | None -> Unix.unsetenv name

let assert_true label cond = if not cond then failwith label

let askpass_response env prompt =
  match value "GIT_ASKPASS" env with
  | None -> failwith "GIT_ASKPASS missing"
  | Some path ->
      let input, output, error =
        Unix.open_process_args_full path [| path; prompt |] (Array.of_list env)
      in
      let stdout = In_channel.input_all input |> String.trim in
      let stderr = In_channel.input_all error |> String.trim in
      (match Unix.close_process_full (input, output, error) with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED code ->
          failwith (Printf.sprintf "askpass exited %d: %s" code stderr)
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
          failwith
            (Printf.sprintf "askpass interrupted by signal %d: %s" signal stderr));
      stdout

let test_token_file_auth () =
  let name = "ONTON_GITHUB_TOKEN_FILE" in
  let old_file = Sys.getenv_opt name in
  let old_token = Sys.getenv_opt "GITHUB_TOKEN" in
  let old_gh_token = Sys.getenv_opt "GH_TOKEN" in
  let path = Filename.temp_file "onton-token-" ".txt" in
  Fun.protect
    ~finally:(fun () ->
      restore_env name old_file;
      restore_env "GITHUB_TOKEN" old_token;
      restore_env "GH_TOKEN" old_gh_token;
      Sys.remove path)
    (fun () ->
      let oc = open_out path in
      output_string oc "installation-token\n";
      close_out oc;
      Unix.putenv name path;
      Unix.putenv "GITHUB_TOKEN" "stale-token";
      Unix.putenv "GH_TOKEN" "stale-gh-token";
      assert_true "startup resolves the token file"
        (String.equal (Managed_repo.infer_github_token ()) "installation-token");
      let env = env_list () in
      assert_true "token file removes inherited GitHub tokens"
        (not
           (List.exists
              (fun s -> binding "GITHUB_TOKEN" s || binding "GH_TOKEN" s)
              env));
      assert_true "askpass reads the token file"
        (String.equal
           (askpass_response env "Password for 'https://github.com':")
           "installation-token");
      Unix.putenv name "";
      assert_true "empty token file path fails startup resolution"
        (String.equal (Managed_repo.infer_github_token ()) "");
      let env = env_list () in
      match value "GIT_ASKPASS" env with
      | None -> failwith "GIT_ASKPASS missing"
      | Some askpass ->
          let input, output, error =
            Unix.open_process_args_full askpass
              [| askpass; "Password for 'https://github.com':" |]
              (Array.of_list env)
          in
          let stdout = In_channel.input_all input in
          let _stderr = In_channel.input_all error in
          let status = Unix.close_process_full (input, output, error) in
          assert_true "empty token file path fails askpass"
            (String.equal stdout "" && status <> Unix.WEXITED 0))

let test_clean_env_scrubs_git_and_installs_auth () =
  let inherited_config =
    List.map
      (fun name -> (name, Sys.getenv_opt name))
      [ "GIT_CONFIG_COUNT"; "GIT_CONFIG_KEY_0"; "GIT_CONFIG_VALUE_0" ]
  in
  Unix.putenv "GIT_CONFIG_COUNT" "1";
  Unix.putenv "GIT_CONFIG_KEY_0" "core.hooksPath";
  Unix.putenv "GIT_CONFIG_VALUE_0" "/inherited-hooks";
  let old_git_dir = Sys.getenv_opt "GIT_DIR" in
  let old_gh_token = Sys.getenv_opt "GH_TOKEN" in
  let old_gcm_interactive = Sys.getenv_opt "GCM_INTERACTIVE" in
  let old_gh_prompt_disabled = Sys.getenv_opt "GH_PROMPT_DISABLED" in
  Unix.putenv "GIT_DIR" "/tmp/wrong-repo";
  Unix.putenv "GH_TOKEN" "stale-token";
  Unix.putenv "GCM_INTERACTIVE" "always";
  Unix.putenv "GH_PROMPT_DISABLED" "0";
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (name, old) -> restore_env name old) inherited_config;
      restore_env "GIT_DIR" old_git_dir;
      restore_env "GH_TOKEN" old_gh_token;
      restore_env "GCM_INTERACTIVE" old_gcm_interactive;
      restore_env "GH_PROMPT_DISABLED" old_gh_prompt_disabled)
    (fun () ->
      Git_env.set_github_token " configured-token ";
      let github_env = env_list () in
      assert_true "GIT_DIR scrubbed"
        (not (List.exists (binding "GIT_DIR") github_env));
      assert_true "inherited Git config cannot re-enable hooks"
        (value "GIT_CONFIG_COUNT" github_env = Some "1"
        && value "GIT_CONFIG_KEY_0" github_env = Some "core.hooksPath"
        && value "GIT_CONFIG_VALUE_0" github_env = Some "/dev/null");
      assert_true "stale GH_TOKEN scrubbed"
        (not (List.exists (binding "GH_TOKEN") github_env));
      assert_true "terminal prompts disabled"
        (List.exists (String.equal "GIT_TERMINAL_PROMPT=0") github_env);
      assert_true "credential manager noninteractive"
        (List.exists (String.equal "GCM_INTERACTIVE=never") github_env);
      assert_true "credential manager binding is unique"
        (count "GCM_INTERACTIVE" github_env = 1);
      assert_true "gh prompts disabled"
        (List.exists (String.equal "GH_PROMPT_DISABLED=1") github_env);
      assert_true "gh prompt binding is unique"
        (count "GH_PROMPT_DISABLED" github_env = 1);
      (match value "GIT_ASKPASS" github_env with
      | Some path -> assert_true "askpass script exists" (Sys.file_exists path)
      | None -> failwith "GIT_ASKPASS missing");
      (match value "GITHUB_TOKEN" github_env with
      | Some token ->
          assert_true "configured token trimmed"
            (String.equal token "configured-token")
      | None -> failwith "GITHUB_TOKEN missing");
      assert_true "GitHub askpass returns its configured token"
        (String.equal
           (askpass_response github_env "Password for 'https://github.com':")
           "configured-token");
      Git_env.set_sourcehut_token ~username:" alice " " sourcehut-token ";
      let sourcehut_env = env_list () in
      assert_true "GitHub credentials are not leaked to SourceHut git"
        (not (List.exists (binding "GITHUB_TOKEN") sourcehut_env));
      (match
         (value "SRHT_USERNAME" sourcehut_env, value "SRHT_TOKEN" sourcehut_env)
       with
      | Some username, Some token ->
          assert_true "SourceHut username trimmed"
            (String.equal username "alice");
          assert_true "SourceHut token trimmed"
            (String.equal token "sourcehut-token")
      | (Some _ | None), (Some _ | None) ->
          failwith "SourceHut credentials missing");
      assert_true "SourceHut askpass returns its username"
        (String.equal
           (askpass_response sourcehut_env "Username for 'https://git.sr.ht':")
           "alice");
      assert_true "SourceHut askpass returns its token"
        (String.equal
           (askpass_response sourcehut_env
              "Password for 'https://alice@git.sr.ht':")
           "sourcehut-token");
      assert_true "SourceHut text in a GitHub path does not change the host"
        (String.equal
           (askpass_response sourcehut_env
              "Username for 'https://github.com/alice/git.sr.ht-demo':")
           "x-access-token");
      assert_true "SourceHut hostname prefixes do not match"
        (String.equal
           (askpass_response sourcehut_env
              "Username for 'https://git.sr.ht.example/alice/demo':")
           "x-access-token"))

let test_git_commands_skip_hooks () =
  Eio_main.run @@ fun env ->
  let module Git = Onton_test_support.Git_env in
  Git.with_temp_repo (fun dir ->
      let write name contents =
        let oc = open_out (Filename.concat dir name) in
        Fun.protect
          ~finally:(fun () -> close_out oc)
          (fun () -> output_string oc contents)
      in
      let git = Git.run_git ~cwd:dir in
      let normal_commit () =
        let input, output, error =
          Unix.open_process_args_full "git"
            [| "git"; "-C"; dir; "commit"; "--allow-empty"; "-m"; "normal" |]
            (Unix.environment () |> Array.to_list
            |> List.filter (fun entry ->
                not (String.starts_with ~prefix:"GIT_" entry))
            |> Array.of_list)
        in
        close_out output;
        ignore (In_channel.input_all input);
        ignore (In_channel.input_all error);
        Unix.close_process_full (input, output, error)
      in
      let hooks = Filename.concat dir ".git/custom-hooks" in
      Unix.mkdir hooks 0o755;
      write ".git/custom-hooks/pre-commit"
        "#!/bin/sh\nprintf hook > hook-ran\nexit 97\n";
      Unix.chmod (Filename.concat hooks "pre-commit") 0o755;
      git [ "config"; "core.hooksPath"; hooks ];
      assert_true "normal Git invokes the configured pre-commit hook"
        (normal_commit () = Unix.WEXITED 1
        && Sys.file_exists (Filename.concat dir "hook-ran"));
      Sys.remove (Filename.concat dir "hook-ran");
      write "file" "base\n";
      git [ "add"; "file" ];
      git [ "commit"; "-q"; "-m"; "base" ];
      let publication =
        match
          Gameplan_publication.create ~directory:"plans" ~project_name:"hooks"
            ~yaml:true ~content:"projectName: hooks\n"
        with
        | Ok p -> p
        | Error e -> failwith e
      in
      assert_true "production gameplan commit skips the hook"
        (Worktree.commit_gameplan ~clock:(Eio.Stdenv.clock env)
           ~process_mgr:(Eio.Stdenv.process_mgr env)
           ~path:dir ~publication ~message:"generated gameplan"
        = Ok ());
      git [ "checkout"; "-q"; "-b"; "side" ];
      write "file" "side\n";
      git [ "commit"; "-q"; "-am"; "side" ];
      git [ "checkout"; "-q"; "main" ];
      write "file" "main\n";
      git [ "commit"; "-q"; "-am"; "main" ];
      assert_true "fixture creates a real merge conflict"
        (Git.git_exit_code ~cwd:dir [ "merge"; "side" ] = 1);
      write "file" "resolved\n";
      git [ "add"; "file" ];
      git [ "remote"; "add"; "origin"; dir ];
      let module B = Branch_reconcile in
      let module E = Branch_reconcile_executor in
      let io =
        E.make_io ~clock:(Eio.Stdenv.clock env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~path:dir
      in
      let initial, _ =
        B.step B.empty
          (B.Request
             {
               base = "side";
               policy = B.Preserve_ancestry;
               purpose = B.Reconcile_base;
             })
      in
      let rec continue state remaining =
        if remaining = 0 then failwith "merge continuation was not dispatched";
        match (B.operation state, B.pending state) with
        | Some operation, Some command -> (
            let result =
              E.execute ~io ~prefix:"refs/onton/reconcile/hook-test"
                ~branch:"main" ~operation command
            in
            match command.B.kind with
            | B.Continue _ ->
                assert_true "production merge continuation succeeds"
                  (match result with B.Integrated _ -> true | _ -> false)
            | _ ->
                let state, _ =
                  B.step state
                    (B.Result { token = command.token; at = 100.; result })
                in
                continue state (remaining - 1))
        | _ -> failwith "merge continuation lost its pending command"
      in
      continue initial 5;
      assert_true "Onton commits and merge continuation skip the hook"
        (not (Sys.file_exists (Filename.concat dir "hook-ran")));
      assert_true "repository hooks remain configured for normal Git"
        (normal_commit () = Unix.WEXITED 1
        && Sys.file_exists (Filename.concat dir "hook-ran")));
  print_endline "Git commits and merge continuation skip local hooks: OK"

module Operation_kind = Types.Operation_kind

let all_kinds =
  Operation_kind.
    [
      Uncommitted_changes;
      Rebase;
      Human;
      Merge_conflict;
      Ci;
      Review_comments;
      Pr_body;
      Findings;
    ]

let gen_kind = QCheck2.Gen.oneof_list all_kinds

(* [highest_priority q k] is true iff [k] is enqueued and no member has a
   strictly more-urgent (lower) priority value. Build [q] from generated kinds
   and cross-check against the [peek_highest] view. *)
let highest_priority_matches_peek =
  QCheck2.Test.make ~name:"highest_priority agrees with peek_highest" ~count:300
    QCheck2.Gen.(pair (list_size (int_range 0 6) gen_kind) gen_kind)
    (fun (kinds, k) ->
      let q =
        List.fold_left
          (fun q kind -> Priority.enqueue q kind)
          Priority.empty kinds
      in
      let expected =
        Priority.mem q k
        && Priority.priority k
           = List.fold_left
               (fun acc kind -> min acc (Priority.priority kind))
               max_int (Priority.to_list q)
      in
      Bool.equal (Priority.highest_priority q k) expected)

(* [is_feedback] partitions operation kinds: the feedback set excludes [Rebase]
   (the structural op) — assert against an explicit reference set. *)
let is_feedback_classifies_kinds =
  QCheck2.Test.make ~name:"is_feedback matches reference partition" ~count:200
    gen_kind (fun k ->
      let reference =
        match k with
        | Operation_kind.Rebase -> false
        | Uncommitted_changes | Human | Merge_conflict | Ci | Review_comments
        | Pr_body | Findings ->
            true
      in
      Bool.equal (Priority.is_feedback k) reference)

let () =
  test_clean_env_scrubs_git_and_installs_auth ();
  test_token_file_auth ();
  test_git_commands_skip_hooks ();
  QCheck2.Test.check_exn highest_priority_matches_peek;
  QCheck2.Test.check_exn is_feedback_classifies_kinds;
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"priority public surface is linked"
       QCheck2.Gen.unit (fun () ->
         ignore Priority.highest_priority;
         ignore Priority.is_feedback;
         true))

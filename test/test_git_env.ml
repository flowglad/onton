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

let test_clean_env_scrubs_git_and_installs_auth () =
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
      restore_env "GIT_DIR" old_git_dir;
      restore_env "GH_TOKEN" old_gh_token;
      restore_env "GCM_INTERACTIVE" old_gcm_interactive;
      restore_env "GH_PROMPT_DISABLED" old_gh_prompt_disabled)
    (fun () ->
      Git_env.set_github_token " configured-token ";
      let github_env = env_list () in
      assert_true "GIT_DIR scrubbed"
        (not (List.exists (binding "GIT_DIR") github_env));
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
           "sourcehut-token"))

module Operation_kind = Types.Operation_kind

let all_kinds =
  Operation_kind.
    [ Rebase; Human; Merge_conflict; Ci; Review_comments; Pr_body; Findings ]

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
        | Human | Merge_conflict | Ci | Review_comments | Pr_body | Findings ->
            true
      in
      Bool.equal (Priority.is_feedback k) reference)

let () =
  test_clean_env_scrubs_git_and_installs_auth ();
  QCheck2.Test.check_exn highest_priority_matches_peek;
  QCheck2.Test.check_exn is_feedback_classifies_kinds;
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"priority public surface is linked"
       QCheck2.Gen.unit (fun () ->
         ignore Priority.highest_priority;
         ignore Priority.is_feedback;
         true))

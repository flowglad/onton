open Onton

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

let assert_true label cond = if not cond then failwith label

let test_clean_env_scrubs_git_and_installs_auth () =
  let old_git_dir = Sys.getenv_opt "GIT_DIR" in
  Unix.putenv "GIT_DIR" "/tmp/wrong-repo";
  Fun.protect
    ~finally:(fun () ->
      match old_git_dir with
      | Some value -> Unix.putenv "GIT_DIR" value
      | None -> Unix.putenv "GIT_DIR" "")
    (fun () ->
      Git_env.set_github_token " configured-token ";
      let env = env_list () in
      assert_true "GIT_DIR scrubbed" (not (List.exists (binding "GIT_DIR") env));
      assert_true "terminal prompts disabled"
        (List.exists (String.equal "GIT_TERMINAL_PROMPT=0") env);
      assert_true "credential manager noninteractive"
        (List.exists (String.equal "GCM_INTERACTIVE=never") env);
      assert_true "gh prompts disabled"
        (List.exists (String.equal "GH_PROMPT_DISABLED=1") env);
      (match value "GIT_ASKPASS" env with
      | Some path -> assert_true "askpass script exists" (Sys.file_exists path)
      | None -> failwith "GIT_ASKPASS missing");
      match value "GITHUB_TOKEN" env with
      | Some token ->
          assert_true "configured token trimmed"
            (String.equal token "configured-token")
      | None -> failwith "GITHUB_TOKEN missing")

let () = test_clean_env_scrubs_git_and_installs_auth ()

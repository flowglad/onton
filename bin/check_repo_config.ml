(* @archlint.module shell
   @archlint.domain priority *)

(** One-off: load and pretty-print the per-repo onton config. Useful to verify a
    [reviewBackends] block parses before kicking off a real session.

    Usage: [onton-check-repo-config <owner> <repo>] *)

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [ owner; repo ] -> (
      let known_backends = [ "claude"; "codex"; "gemini"; "opencode"; "pi" ] in
      let config_dir =
        Onton.User_config.config_dir ~github_owner:owner ~github_repo:repo
      in
      Printf.printf "config_dir = %s\n" config_dir;
      match Onton_core.Repo_config.load ~config_dir ~known_backends () with
      | Error msg ->
          Printf.printf "PARSE ERROR: %s\n" msg;
          exit 1
      | Ok c ->
          let show_opt = function Some s -> s | None -> "(unset)" in
          Printf.printf "default.backend   : %s\n" (show_opt c.default_backend);
          Printf.printf "default.model     : %s\n" (show_opt c.default_model);
          Printf.printf "complexity_routes : %d entries\n"
            (List.length c.complexity_routes);
          Printf.printf "review_backends   : %d entries\n"
            (List.length c.review_backends);
          List.iter
            (fun (b : Onton_core.Review_backend.t) ->
              Printf.printf "  - name=%s\n" b.name;
              match b.kind with
              | Onton_core.Review_backend.Review_service { base_url; auth } ->
                  Printf.printf "    baseUrl=%s\n" base_url;
                  Printf.printf "    appId=%s\n" auth.app_id;
                  Printf.printf "    privateKeyPath=%s\n" auth.private_key_path;
                  if Sys.file_exists auth.private_key_path then
                    Printf.printf "    (PEM file exists)\n"
                  else
                    Printf.printf
                      "    WARNING: PEM file does not exist on disk\n")
            c.review_backends)
  | _ ->
      prerr_endline "usage: onton-check-repo-config <owner> <repo>";
      exit 2

(* @archlint.module test
   @archlint.domain resolved-config *)

[@@@warning "-42"]

open Onton
open Onton_core

let valid_config ~max_concurrency : Resolved_config.config =
  {
    Resolved_config.project = Some "project";
    Resolved_config.backend = "claude";
    Resolved_config.model = "sonnet";
    Resolved_config.github_token = "token";
    Resolved_config.github_owner = "owner";
    Resolved_config.github_repo = "repo";
    Resolved_config.main_branch = Types.Branch.of_string "main";
    Resolved_config.poll_interval = 1.0;
    Resolved_config.repo_root = ".";
    Resolved_config.max_concurrency;
    Resolved_config.headless = true;
    Resolved_config.patch_agent_provider = None;
    Resolved_config.patch_agent_effort = None;
    Resolved_config.user_config = { User_config.on_worktree_create = None };
    Resolved_config.repo_config = Repo_config.empty;
  }

let max_concurrency_must_be_positive =
  QCheck2.Test.make ~name:"resolved config rejects non-positive concurrency"
    ~count:200
    QCheck2.Gen.(int_range (-20) 0)
    (fun max_concurrency ->
      match Resolved_config.of_config (valid_config ~max_concurrency) with
      | Ok _ -> false
      | Error errors -> List.length errors >= 1)

let () = QCheck2.Test.check_exn max_concurrency_must_be_positive

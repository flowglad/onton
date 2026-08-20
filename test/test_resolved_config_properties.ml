(* @archlint.module test
   @archlint.domain resolved-config *)

[@@@warning "-42"]

open Onton
open Onton_core

let valid_config ~max_concurrency ~max_ci_failures : Resolved_config.config =
  {
    Resolved_config.project = Some "project";
    forge = "github";
    Resolved_config.backend = "claude";
    Resolved_config.model = "sonnet";
    Resolved_config.github_token = "token";
    Resolved_config.github_owner = "owner";
    Resolved_config.github_repo = "repo";
    Resolved_config.main_branch = Types.Branch.of_string "main";
    Resolved_config.poll_interval = 1.0;
    Resolved_config.repo_root = ".";
    Resolved_config.max_concurrency;
    Resolved_config.max_ci_failures;
    Resolved_config.automerge_timeout =
      Patch_controller.default_automerge_timeout;
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
      match
        Resolved_config.of_config
          (valid_config ~max_concurrency
             ~max_ci_failures:Patch_agent.default_max_ci_failures)
      with
      | Ok _ -> false
      | Error errors -> List.length errors >= 1)

let max_ci_failures_must_be_positive =
  QCheck2.Test.make ~name:"resolved config rejects non-positive CI cap"
    ~count:200
    QCheck2.Gen.(int_range (-20) 0)
    (fun max_ci_failures ->
      match
        Resolved_config.of_config
          (valid_config ~max_concurrency:1 ~max_ci_failures)
      with
      | Ok _ -> false
      | Error errors -> List.length errors >= 1)

let automerge_timeout_must_be_positive =
  QCheck2.Test.make
    ~name:"resolved config rejects non-positive automerge timeout" ~count:200
    QCheck2.Gen.(float_range (-100.) 0.)
    (fun automerge_timeout ->
      let config =
        {
          (valid_config ~max_concurrency:1
             ~max_ci_failures:Patch_agent.default_max_ci_failures)
          with
          Resolved_config.automerge_timeout;
        }
      in
      match Resolved_config.of_config config with
      | Ok _ -> false
      | Error errors -> List.length errors >= 1)

let non_finite_automerge_timeout_is_rejected () =
  List.for_all
    (fun automerge_timeout ->
      let config =
        {
          (valid_config ~max_concurrency:1
             ~max_ci_failures:Patch_agent.default_max_ci_failures)
          with
          Resolved_config.automerge_timeout;
        }
      in
      Result.is_error (Resolved_config.of_config config))
    [ Float.nan; Float.infinity; Float.neg_infinity ]

let positive_automerge_timeout_is_preserved =
  QCheck2.Test.make
    ~name:"resolved config preserves valid automerge timeout values" ~count:500
    QCheck2.Gen.(float_range 0.001 10000.)
    (fun automerge_timeout ->
      let config =
        {
          (valid_config ~max_concurrency:1
             ~max_ci_failures:Patch_agent.default_max_ci_failures)
          with
          Resolved_config.automerge_timeout;
        }
      in
      match Resolved_config.of_config config with
      | Ok resolved ->
          Float.equal resolved.Resolved_config.automerge_timeout
            automerge_timeout
      | Error _ -> false)

let automerge_timeout_resolution_is_total =
  QCheck2.Test.make ~name:"resolved config timeout validation is total"
    ~count:1000 QCheck2.Gen.float (fun automerge_timeout ->
      try
        let config =
          {
            (valid_config ~max_concurrency:1
               ~max_ci_failures:Patch_agent.default_max_ci_failures)
            with
            Resolved_config.automerge_timeout;
          }
        in
        ignore (Resolved_config.of_config config);
        true
      with _ -> false)

let repeated_resolution_order_independent =
  QCheck2.Test.make
    ~name:"repeated resolved config decisions are order independent" ~count:300
    QCheck2.Gen.(list_size (int_range 0 40) (float_range 0.001 10000.))
    (fun timeouts ->
      let resolve automerge_timeout =
        let config =
          {
            (valid_config ~max_concurrency:1
               ~max_ci_failures:Patch_agent.default_max_ci_failures)
            with
            Resolved_config.automerge_timeout;
          }
        in
        match Resolved_config.of_config config with
        | Ok resolved -> Ok resolved.Resolved_config.automerge_timeout
        | Error errors -> Error errors
      in
      let equal_result left right =
        match (left, right) with
        | Ok left, Ok right -> Float.equal left right
        | Error left, Error right -> List.equal String.equal left right
        | Ok _, Error _ | Error _, Ok _ -> false
      in
      let forward = List.map resolve timeouts in
      let reverse_then_restore =
        List.rev (List.map resolve (List.rev timeouts))
      in
      List.equal equal_result forward reverse_then_restore)

let () =
  QCheck2.Test.check_exn max_concurrency_must_be_positive;
  QCheck2.Test.check_exn max_ci_failures_must_be_positive;
  QCheck2.Test.check_exn automerge_timeout_must_be_positive;
  assert (non_finite_automerge_timeout_is_rejected ());
  QCheck2.Test.check_exn positive_automerge_timeout_is_preserved;
  QCheck2.Test.check_exn automerge_timeout_resolution_is_total;
  QCheck2.Test.check_exn repeated_resolution_order_independent

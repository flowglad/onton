(* @archlint.module interface
   @archlint.domain resolved-config *)

open Onton_core.Types

type config = {
  project : string option;
  forge : string;
  backend : string;
  model : string;
  github_token : string;
  github_owner : string;
  github_repo : string;
  main_branch : Branch.t;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
  max_ci_failures : int;
      (** Per-project cap on consecutive CI-failure responses per patch (see
          {!Onton_core.Patch_agent.max_ci_failures}). Resolved from
          [--max-ci-failures] / stored project config / built-in default. *)
  automerge_timeout : float;
      (** Automerge idle window in seconds. Must be finite and greater than
          zero. Resolved from [--automerge-timeout] / [ONTON_AUTOMERGE_TIMEOUT],
          persisted project config, repository config, or the built-in default,
          in that order. *)
  headless : bool;
  patch_agent_provider : string option;
  patch_agent_effort : string option;
  user_config : User_config.t;
  repo_config : Repo_config.t;
      (** Per-repo [config.json], loaded once during config resolution. Already
          consulted as part of the [(backend, model)] merge before this record
          is constructed; carried through so [routing] and [reviewBackends] are
          available to the runtime without re-reading the file. *)
}

type t = {
  project_name : string;
  forge : string;
  backend : string;
  model : string;
  github_token : string;
  github_owner : string;
  github_repo : string;
  main_branch : Branch.t;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
  max_ci_failures : int;
  automerge_timeout : float;
  headless : bool;
  patch_agent_provider : string option;
  patch_agent_effort : string option;
  user_config : User_config.t;
  repo_config : Repo_config.t;
}

val of_config : config -> (t, string list) result

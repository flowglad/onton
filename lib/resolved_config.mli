open Onton_core.Types

type config = {
  project : string option;
  backend : string;
  model : string;
  github_token : string;
  github_owner : string;
  github_repo : string;
  main_branch : Branch.t;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
  headless : bool;
  patch_agent_provider : string option;
  patch_agent_effort : string option;
  user_config : User_config.t;
}

type t = {
  project_name : string;
  backend : string;
  model : string;
  github_token : string;
  github_owner : string;
  github_repo : string;
  main_branch : Branch.t;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
  headless : bool;
  patch_agent_provider : string option;
  patch_agent_effort : string option;
  user_config : User_config.t;
}

val of_config : config -> (t, string list) result

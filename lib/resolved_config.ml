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

let known_backends =
  [ "claude"; "codex"; "opencode"; "pi"; "gemini"; "patch-agent" ]

let known_patch_agent_providers = [ "anthropic"; "openai" ]
let known_patch_agent_efforts = [ "low"; "medium"; "high" ]

let validate_resolved_config ~project_name ~backend ~github_token ~github_owner
    ~github_repo ~main_branch ~poll_interval ~max_concurrency
    ~patch_agent_provider ~patch_agent_effort =
  let errors =
    Base.List.filter_map
      [
        ( Base.String.is_empty (Base.String.strip project_name),
          "project name is required" );
        ( not (Base.List.mem known_backends backend ~equal:String.equal),
          Printf.sprintf "--backend must be one of: %s (got %S)"
            (String.concat ", " known_backends)
            backend );
        ( Base.String.is_empty (Base.String.strip github_token),
          "--token / GITHUB_TOKEN is required" );
        ( Base.String.is_empty (Base.String.strip github_owner),
          "--owner / GITHUB_OWNER is required" );
        ( Base.String.is_empty (Base.String.strip github_repo),
          "--repo / GITHUB_REPO is required" );
        ( Base.String.is_empty (Base.String.strip (Branch.to_string main_branch)),
          "--main-branch cannot be empty" );
        ( Float.compare poll_interval 0.0 <= 0,
          Printf.sprintf "--poll-interval must be > 0 (got %g)" poll_interval );
        ( max_concurrency < 1,
          Printf.sprintf "--max-concurrency must be >= 1 (got %d)"
            max_concurrency );
        ( (match patch_agent_provider with
          | Some provider ->
              not
                (Base.List.mem known_patch_agent_providers provider
                   ~equal:String.equal)
          | None -> false),
          Printf.sprintf
            "--patch-agent-provider / PATCH_AGENT_PROVIDER must be one of: %s"
            (String.concat ", " known_patch_agent_providers) );
        ( (match patch_agent_effort with
          | Some effort ->
              not
                (Base.List.mem known_patch_agent_efforts effort
                   ~equal:String.equal)
          | None -> false),
          Printf.sprintf
            "--patch-agent-effort / PATCH_AGENT_EFFORT must be one of: %s"
            (String.concat ", " known_patch_agent_efforts) );
      ]
      ~f:(fun (cond, msg) -> if cond then Some msg else None)
  in
  match errors with [] -> Ok () | errs -> Error errs

let of_config (config : config) =
  let project_name = Option.value config.project ~default:"" in
  match
    validate_resolved_config ~project_name ~backend:config.backend
      ~github_token:config.github_token ~github_owner:config.github_owner
      ~github_repo:config.github_repo ~main_branch:config.main_branch
      ~poll_interval:config.poll_interval
      ~max_concurrency:config.max_concurrency
      ~patch_agent_provider:config.patch_agent_provider
      ~patch_agent_effort:config.patch_agent_effort
  with
  | Error errs -> Error errs
  | Ok () ->
      Ok
        {
          project_name;
          backend = config.backend;
          model = config.model;
          github_token = config.github_token;
          github_owner = config.github_owner;
          github_repo = config.github_repo;
          main_branch = config.main_branch;
          poll_interval = config.poll_interval;
          repo_root = config.repo_root;
          max_concurrency = config.max_concurrency;
          headless = config.headless;
          patch_agent_provider = config.patch_agent_provider;
          patch_agent_effort = config.patch_agent_effort;
          user_config = config.user_config;
        }

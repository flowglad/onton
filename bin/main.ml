open Onton
open Onton_core
open Onton_core.Types
module Managed_repo = Onton.Managed_repo

(** {1 Configuration} *)

type config = Resolved_config.config

module type STARTUP_RECONCILER = Poller_fiber.STARTUP_RECONCILER

let default_backend = "claude"

let known_backends =
  [ "claude"; "codex"; "opencode"; "pi"; "gemini"; "patch-agent" ]

(** Resolve a CLI [--backend]/[--model] pair (or stored equivalents) into the
    canonical [(backend, model)] tuple used internally. Empty [backend] falls
    back to [default_backend]. An empty [model] is preserved — the backend
    dispatch then omits [--model] from the underlying CLI call so each
    provider's own default applies. *)
let resolve_backend_model ~backend ~model =
  let backend =
    if Base.String.is_empty (Base.String.strip backend) then default_backend
    else Base.String.strip backend
  in
  (backend, Base.String.strip model)

(** {1 PR number registry}

    Maps patch_id -> Pr_number.t. The current data model does not persist PR
    numbers on Patch_agent.t (it only tracks [has_pr : bool]), so we maintain a
    separate table populated when PRs are created and used for polling.

    PR numbers are discovered by querying GitHub for open PRs matching the
    patch's branch name after the backend session completes. *)

module Pr_registry = struct
  type t = { mutex : Eio.Mutex.t; table : (Patch_id.t, Pr_number.t) Hashtbl.t }

  let create () : t = { mutex = Eio.Mutex.create (); table = Hashtbl.create 64 }

  let register (t : t) ~patch_id ~pr_number =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        Hashtbl.replace t.table patch_id pr_number)

  let find (t : t) ~patch_id =
    Eio.Mutex.use_ro t.mutex (fun () -> Hashtbl.find_opt t.table patch_id)

  let unregister (t : t) ~patch_id =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        Hashtbl.remove t.table patch_id)
end

let build_branch_map (gameplan : Gameplan.t) ~default =
  let map =
    Base.List.fold gameplan.Gameplan.patches
      ~init:(Base.Map.empty (module Patch_id))
      ~f:(fun acc (p : Patch.t) ->
        match Base.Map.add acc ~key:p.Patch.id ~data:p.Patch.branch with
        | `Ok acc -> acc
        | `Duplicate ->
            failwith
              (Printf.sprintf "Duplicate patch id in gameplan: %s"
                 (Patch_id.to_string p.Patch.id)))
  in
  fun pid -> Base.Option.value (Base.Map.find map pid) ~default

let log_event runtime ?patch_id msg =
  Runtime_logging.log_event runtime ?patch_id msg

module type FIBER_ENV = sig
  val runtime : Runtime.t
  val clock : float Eio.Time.clock_ty Eio.Time.clock
  val fs : Eio.Fs.dir_ty Eio.Path.t
  val process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t
  val stdout : Eio_unix.sink_ty Eio.Resource.t
  val config : Resolved_config.t
  val project_name : string
  val findings_registry : Findings_registry.t

  val review_clients :
    (module Review_service_client.S
       with type error = Review_service_client.error)
    list

  val transcripts : (Patch_id.t, string) Stdlib.Hashtbl.t
  val event_log : Event_log.t
  val branch_of : Patch_id.t -> Branch.t
  val resolve_routing : complexity:int option -> Backend_routing.decision
  val backend_name : string

  val pick_backend :
    complexity:int option -> Backend_registry.kind * Backend_routing.decision

  val find_pr_number : patch_id:Patch_id.t -> Pr_number.t option
  val register_pr_number : patch_id:Patch_id.t -> pr_number:Pr_number.t -> unit
  val unregister_pr_number : patch_id:Patch_id.t -> unit
  val worktree_mutex : Eio.Mutex.t
  val hook_mutex : Eio.Mutex.t
  val tui_state : Tui_state.t
end

module Make_fibers
    (Forge : Onton.Forge.S with type error = Github.error)
    (W : Worktree.S)
    (Env : FIBER_ENV) =
struct
  module Runner =
    Runner_fiber.Make (Forge) (W)
      (struct
        let runtime = Env.runtime
        let clock = Env.clock
        let fs = Env.fs
        let project_name = Env.project_name
        let user_config = Env.config.user_config
        let worktree_mutex = Env.worktree_mutex
        let hook_mutex = Env.hook_mutex
        let owner = Env.config.github_owner
        let repo = Env.config.github_repo
        let main_branch = Env.config.main_branch
        let max_concurrency = Env.config.max_concurrency
        let patch_agent_provider = Env.config.patch_agent_provider
        let patch_agent_effort = Env.config.patch_agent_effort
        let findings_registry = Env.findings_registry
        let review_clients = Env.review_clients
        let transcripts = Env.transcripts
        let event_log = Env.event_log
        let pick_backend = Env.pick_backend
        let register_pr = Env.register_pr_number
      end)

  module Poller =
    Poller_fiber.Make (Forge) (W)
      (struct
        let runtime = Env.runtime
        let clock = Env.clock
        let fs = Env.fs
        let project_name = Env.project_name
        let user_config = Env.config.user_config
        let worktree_mutex = Env.worktree_mutex
        let hook_mutex = Env.hook_mutex
        let process_mgr = Env.process_mgr
        let github_owner = Env.config.github_owner
        let github_repo = Env.config.github_repo
        let main_branch = Env.config.main_branch
        let poll_interval = Env.config.poll_interval
        let repo_root = Env.config.repo_root
        let find_pr_number = Env.find_pr_number
        let register_pr_number = Env.register_pr_number
        let unregister_pr_number = Env.unregister_pr_number
        let findings_registry = Env.findings_registry
        let review_clients = Env.review_clients
        let event_log = Env.event_log
        let branch_of = Env.branch_of
      end)

  module Tui =
    Tui_fiber.Make (Forge) (W)
      (struct
        let runtime = Env.runtime
        let clock = Env.clock
        let fs = Env.fs
        let project_name = Env.project_name
        let user_config = Env.config.user_config
        let worktree_mutex = Env.worktree_mutex
        let hook_mutex = Env.hook_mutex
        let process_mgr = Env.process_mgr
        let stdout = Env.stdout
        let owner = Env.config.github_owner
        let repo = Env.config.github_repo
        let transcripts = Env.transcripts
        let tui_state = Env.tui_state
        let backend_name = Env.backend_name
        let resolve_routing = Env.resolve_routing
        let find_pr_number = Env.find_pr_number
        let register_pr_number = Env.register_pr_number
        let unregister_pr_number = Env.unregister_pr_number
      end)

  module Headless =
    Headless_fiber.Make (Forge) (W)
      (struct
        let runtime = Env.runtime
        let clock = Env.clock
        let stdout = Env.stdout
      end)

  module Persistence =
    Persistence_fiber.Make (Forge) (W)
      (struct
        let runtime = Env.runtime
        let clock = Env.clock
        let project_name = Env.project_name
        let transcripts = Env.transcripts
      end)
end

(** {1 Main entry point} *)

(** Try to load a persisted snapshot for a project. *)
let load_snapshot ~project_name =
  let path = Project_store.snapshot_path project_name in
  if Stdlib.Sys.file_exists path then
    match Persistence.load ~path with
    | Ok snap -> Ok (Some snap)
    | Error msg -> Error msg
  else Ok None

(** Resolve owner/repo/token with CLI flags, falling back to git remote and
    [gh auth token] when flags are empty. *)
let resolve_github_credentials ~github_token ~repo_root =
  let token =
    let t = Base.String.strip github_token in
    if Base.String.is_empty t then Managed_repo.infer_github_token () else t
  in
  let owner, repo =
    let module Repo = (val Repo_git.make ~repo_root) in
    match Repo.infer_owner_repo () with
    | Some (o, r) -> (o, r)
    | None -> ("", "")
  in
  (token, owner, repo)

(** Attach persisted snapshot to a resolved config, propagating load errors. *)
let with_snapshot_load ~project_name config gameplan =
  match load_snapshot ~project_name with
  | Ok existing_snapshot -> Ok (config, gameplan, existing_snapshot)
  | Error msg ->
      Error
        [
          Printf.sprintf "Error loading snapshot for project %S: %s"
            project_name msg;
        ]

(** Resolve CLI args into a config ready to run.
    - [--gameplan] provided: parse it, persist config + gameplan source, derive
      project name.
    - [PROJECT] only: load stored config + gameplan. CLI flags override stored
      values. *)
let resolve_config ~project ~gameplan_path ~github_token ~backend ~model
    ~main_branch ~poll_interval ~(repo_root : string option) ~max_concurrency
    ~headless =
  let patch_agent_provider =
    match Stdlib.Sys.getenv_opt "PATCH_AGENT_PROVIDER" with
    | Some s ->
        let s = Base.String.strip s in
        if Base.String.is_empty s then None else Some s
    | None -> None
  in
  let patch_agent_effort =
    match Stdlib.Sys.getenv_opt "PATCH_AGENT_EFFORT" with
    | Some s ->
        let s = Base.String.strip s in
        if Base.String.is_empty s then None else Some s
    | None -> None
  in
  let repo_root_for_fresh =
    Repo_root.normalize (Base.Option.value repo_root ~default:".")
  in
  let resolve_branch ~repo_root mb_opt =
    match mb_opt with
    | Some b -> b
    | None ->
        let module Repo = (val Repo_git.make ~repo_root) in
        Repo.infer_default_branch ()
  in
  match (project, gameplan_path) with
  | None, None ->
      let repo_root = repo_root_for_fresh in
      let token, owner, repo =
        resolve_github_credentials ~github_token ~repo_root
      in
      let project_name =
        if Base.String.is_empty owner || Base.String.is_empty repo then "adhoc"
        else Printf.sprintf "%s-%s" owner repo
      in
      let gameplan : Gameplan.t =
        {
          project_name;
          repo_owner = owner;
          repo_name = repo;
          problem_statement = "";
          solution_summary = "";
          final_state_spec = "";
          patches = [];
          current_state_analysis = "";
          explicit_opinions = "";
          acceptance_criteria = [];
          open_questions = [];
          functional_changes = [];
          context_resources = [];
        }
      in
      let backend, model = resolve_backend_model ~backend ~model in
      let main_branch = resolve_branch ~repo_root main_branch in
      Project_store.save_config ~project_name ~github_token:token
        ~github_owner:owner ~github_repo:repo ~backend ~model
        ~main_branch:(Branch.to_string main_branch)
        ~poll_interval ~repo_root ~max_concurrency;
      let config =
        Resolved_config.
          {
            project = Some project_name;
            backend;
            model;
            github_token = token;
            github_owner = owner;
            github_repo = repo;
            main_branch;
            poll_interval;
            repo_root;
            max_concurrency;
            headless;
            patch_agent_provider;
            patch_agent_effort;
            user_config = User_config.load ~github_owner:owner ~github_repo:repo;
          }
      in
      with_snapshot_load ~project_name config gameplan
  | _, Some gp_path -> (
      match Gameplan_parser.parse_file gp_path with
      | Error msg -> Error [ Printf.sprintf "Error parsing gameplan: %s" msg ]
      | Ok parsed -> (
          let gameplan = parsed.Gameplan_parser.gameplan in
          let project_name =
            match project with
            | Some p -> p
            | None -> gameplan.Gameplan.project_name
          in
          let owner = Base.String.strip gameplan.Gameplan.repo_owner in
          let repo = Base.String.strip gameplan.Gameplan.repo_name in
          let target_error =
            if Base.String.is_empty owner || Base.String.is_empty repo then
              Some
                (Printf.sprintf
                   "Gameplan %s is missing required top-level `owner` and/or \
                    `repo`. Every gameplan must declare exactly one repository \
                    — see skills/write-gameplan/SKILL.md §\"One Repo Per \
                    Gameplan\"."
                   gp_path)
            else
              match Github_target.validate_target ~owner ~repo with
              | Ok () -> None
              | Error msg ->
                  Some
                    (Printf.sprintf
                       "Gameplan %s declares an invalid GitHub target: %s"
                       gp_path msg)
          in
          match target_error with
          | Some msg -> Error [ msg ]
          | None -> (
              (* [--repo] is ignored when [--gameplan] is passed: the gameplan
               itself is the source of truth for which repo to operate on, and
               onton manages its own checkout under the project data dir. *)
              (match repo_root with
              | Some user_repo when not (Base.String.is_empty user_repo) ->
                  Printf.eprintf
                    "onton: --repo %s ignored when --gameplan is set; using \
                     onton-managed checkout for %s/%s\n\
                     %!"
                    user_repo owner repo
              | _ -> ());
              let token =
                let t = Base.String.strip github_token in
                if Base.String.is_empty t then
                  Managed_repo.infer_github_token ()
                else t
              in
              match
                Managed_repo.ensure_managed_repo ~project_name ~token ~owner
                  ~repo
              with
              | Error msg ->
                  Error
                    [
                      Printf.sprintf
                        "Could not prepare onton-managed checkout for %s/%s: %s"
                        owner repo msg;
                    ]
              | Ok repo_root ->
                  let backend, model = resolve_backend_model ~backend ~model in
                  let main_branch = resolve_branch ~repo_root main_branch in
                  Project_store.save_config ~project_name ~github_token:token
                    ~github_owner:owner ~github_repo:repo ~backend ~model
                    ~main_branch:(Branch.to_string main_branch)
                    ~poll_interval ~repo_root ~max_concurrency;
                  Project_store.save_gameplan_source ~project_name
                    ~source_path:gp_path;
                  let config =
                    Resolved_config.
                      {
                        project = Some project_name;
                        backend;
                        model;
                        github_token = token;
                        github_owner = owner;
                        github_repo = repo;
                        main_branch;
                        poll_interval;
                        repo_root;
                        max_concurrency;
                        headless;
                        patch_agent_provider;
                        patch_agent_effort;
                        user_config =
                          User_config.load ~github_owner:owner ~github_repo:repo;
                      }
                  in
                  with_snapshot_load ~project_name config gameplan)))
  | Some proj, None -> (
      if not (Project_store.project_exists proj) then
        Error
          [
            Printf.sprintf
              "No stored project %S. Use --gameplan to start a new project."
              proj;
          ]
      else
        let stored_gp_path = Project_store.stored_gameplan_path proj in
        if not (Stdlib.Sys.file_exists stored_gp_path) then
          Error
            [ Printf.sprintf "Stored gameplan not found for project %S." proj ]
        else
          match Gameplan_parser.parse_file stored_gp_path with
          | Error msg ->
              Error [ Printf.sprintf "Error parsing stored gameplan: %s" msg ]
          | Ok parsed -> (
              let gameplan = parsed.Gameplan_parser.gameplan in
              match Project_store.load_config ~project_name:proj with
              | Error msg ->
                  Error [ Printf.sprintf "Error loading config: %s" msg ]
              | Ok stored ->
                  (* CLI flags override stored config; stored config overrides
                     git-remote inference *)
                  let merge_cli_stored cli stored_val =
                    let c = Base.String.strip cli in
                    if Base.String.is_empty c then stored_val else c
                  in
                  let resolved_backend_str =
                    merge_cli_stored backend stored.Project_store.backend
                  in
                  let resolved_model_str =
                    merge_cli_stored model stored.Project_store.model
                  in
                  let backend, model =
                    resolve_backend_model ~backend:resolved_backend_str
                      ~model:resolved_model_str
                  in
                  let token_from_stored =
                    merge_cli_stored github_token
                      stored.Project_store.github_token
                  in
                  (* Always route through [Repo_root.normalize] — including
                     the stored value — so legacy configs that persisted a
                     worktree path (or a trailing [/.]) self-heal on load. *)
                  let repo_root =
                    match repo_root with
                    | Some rr -> Repo_root.normalize rr
                    | None -> Repo_root.normalize stored.Project_store.repo_root
                  in
                  let token, inferred_owner, inferred_repo =
                    resolve_github_credentials ~github_token:token_from_stored
                      ~repo_root
                  in
                  (* Precedence on resume: gameplan > stored config > inferred
                     from git remote. Gameplan-authored sessions have non-empty
                     owner/repo in the parsed gameplan and they are the
                     source-of-truth. Legacy sessions have empty values there
                     and fall through to the stored config (backfill path),
                     then to inference. *)
                  let pick_owner_repo gp stored_v inferred_v =
                    let s = Base.String.strip gp in
                    if not (Base.String.is_empty s) then s
                    else
                      let s = Base.String.strip stored_v in
                      if Base.String.is_empty s then inferred_v else s
                  in
                  let owner =
                    pick_owner_repo gameplan.Gameplan.repo_owner
                      stored.Project_store.github_owner inferred_owner
                  in
                  let repo =
                    pick_owner_repo gameplan.Gameplan.repo_name
                      stored.Project_store.github_repo inferred_repo
                  in
                  (* If the stored repo_root is the onton-managed checkout for
                     this project, refresh it from origin before continuing.
                     Best-effort: an offline resume should still proceed. *)
                  (if
                     String.equal repo_root
                       (Project_store.managed_repo_dir proj)
                   then
                     if
                       Base.String.is_empty (Base.String.strip owner)
                       || Base.String.is_empty (Base.String.strip repo)
                     then
                       Printf.eprintf
                         "onton: warning: stored project %S has no GitHub \
                          owner/repo; skipping managed checkout refresh\n\
                          %!"
                         proj
                     else
                       match
                         Managed_repo.ensure_managed_repo ~project_name:proj
                           ~token ~owner ~repo
                       with
                       | Ok _ -> ()
                       | Error msg ->
                           Printf.eprintf
                             "onton: warning: %s (resuming with local state)\n\
                              %!"
                             msg);
                  let branch =
                    match main_branch with
                    | Some b -> b
                    | None -> Branch.of_string stored.Project_store.main_branch
                  in
                  (* Persist the resolved config so the next launch without
                     CLI overrides picks up the current values. *)
                  Project_store.save_config ~project_name:proj
                    ~github_token:token ~github_owner:owner ~github_repo:repo
                    ~backend ~model ~main_branch:(Branch.to_string branch)
                    ~poll_interval:stored.Project_store.poll_interval ~repo_root
                    ~max_concurrency:stored.Project_store.max_concurrency;
                  let config =
                    Resolved_config.
                      {
                        project = Some proj;
                        backend;
                        model;
                        github_token = token;
                        github_owner = owner;
                        github_repo = repo;
                        main_branch = branch;
                        poll_interval = stored.Project_store.poll_interval;
                        repo_root;
                        max_concurrency = stored.Project_store.max_concurrency;
                        headless;
                        patch_agent_provider;
                        patch_agent_effort;
                        user_config =
                          User_config.load ~github_owner:owner ~github_repo:repo;
                      }
                  in
                  with_snapshot_load ~project_name:proj config gameplan))

let ok_or_exit = function
  | Ok x -> x
  | Error errs ->
      Base.List.iter errs ~f:(fun e -> Printf.eprintf "Error: %s\n" e);
      Stdlib.exit 1

type runtime_setup = {
  config : Resolved_config.t;
  gameplan : Gameplan.t;
  runtime : Runtime.t;
  clock : float Eio.Time.clock_ty Eio.Time.clock;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  stdout : Eio_unix.sink_ty Eio.Resource.t;
}

type constructed_capabilities = {
  forge : (module Onton.Forge.S with type error = Github.error);
  worktree_client : (module Worktree.S);
  startup_reconciler : (module STARTUP_RECONCILER);
  branch_of : Patch_id.t -> Branch.t;
  pick_backend :
    complexity:int option -> Backend_registry.kind * Backend_routing.decision;
  resolve_routing : complexity:int option -> Backend_routing.decision;
  backend_name : string;
  find_pr_number : patch_id:Patch_id.t -> Pr_number.t option;
  register_pr_number : patch_id:Patch_id.t -> pr_number:Pr_number.t -> unit;
  unregister_pr_number : patch_id:Patch_id.t -> unit;
  findings_registry : Findings_registry.t;
  review_clients :
    (module Review_service_client.S
       with type error = Review_service_client.error)
    list;
  transcripts : (Patch_id.t, string) Stdlib.Hashtbl.t;
  event_log : Event_log.t;
  worktree_mutex : Eio.Mutex.t;
  hook_mutex : Eio.Mutex.t;
  reconciliation_fiber : unit -> unit;
}

type built_fiber_env = (module FIBER_ENV)

let setup_runtime env ~config ~gameplan ~existing_snapshot =
  let { Resolved_config.headless; project_name; main_branch; _ } = config in
  if headless then
    Sys.set_signal Sys.sigint
      (Sys.Signal_handle
         (fun _ ->
           Printf.eprintf "\nInterrupted.\n%!";
           (try Unix.kill 0 Sys.sigterm with Unix.Unix_error _ -> ());
           Stdlib.exit 130));
  let runtime =
    match existing_snapshot with
    | Some snap ->
        Printf.eprintf "Resuming project %S from saved state.\n%!" project_name;
        Runtime.create ~gameplan ~main_branch ~snapshot:snap ()
    | None ->
        Printf.eprintf "Starting new project %S.\n%!" project_name;
        Runtime.create ~gameplan ~main_branch ()
  in
  Unix.putenv "ONTON_SNAPSHOT_PATH" (Project_store.snapshot_path project_name);
  {
    config;
    gameplan;
    runtime;
    clock = Eio.Stdenv.clock env;
    fs = Eio.Stdenv.fs env;
    process_mgr = Eio.Stdenv.process_mgr env;
    stdout = Eio.Stdenv.stdout env;
  }

let construct_capabilities ~net (setup : runtime_setup) =
  let config = setup.config in
  let {
    Resolved_config.github_token;
    github_owner;
    github_repo;
    main_branch;
    repo_root;
    model;
    backend;
    project_name;
    _;
  } =
    config
  in
  let forge =
    Github.make ~net ~clock:setup.clock ~token:github_token ~owner:github_owner
      ~repo:github_repo
  in
  let module Forge = (val forge) in
  let worktree_client =
    Worktree.make ~process_mgr:setup.process_mgr ~repo_root
  in
  let module WorktreeClient = (val worktree_client) in
  (match Forge.check_repo_access () with
  | Ok () -> ()
  | Error err ->
      Printf.eprintf "Error: cannot access GitHub repo %s/%s: %s\n" github_owner
        github_repo (Github.show_error err);
      Stdlib.exit 1);
  let module Reconciler = Startup_reconciler.Make (Forge) (WorktreeClient) in
  let pr_registry = Pr_registry.create () in
  Runtime.read setup.runtime (fun snap ->
      Orchestrator.all_agents snap.Runtime.orchestrator)
  |> Base.List.iter ~f:(fun (agent : Patch_agent.t) ->
      Base.Option.iter agent.Patch_agent.pr_number ~f:(fun pr_number ->
          Pr_registry.register pr_registry ~patch_id:agent.Patch_agent.patch_id
            ~pr_number));
  let branch_of = build_branch_map setup.gameplan ~default:main_branch in
  let session_timeout = 1800.0 in
  let setsid_exec =
    let candidate =
      match Sys.getenv_opt "ONTON_SETSID_EXEC" with
      | Some "" -> None
      | Some p -> Some p
      | None ->
          Some
            (Filename.concat
               (Filename.dirname Sys.executable_name)
               "onton-setsid-exec")
    in
    match candidate with
    | Some p when Sys.file_exists p -> Some p
    | Some p ->
        Eio.traceln
          "onton-setsid-exec not found at %s; grandchildren will reparent to \
           PID 1 on teardown"
          p;
        None
    | None -> None
  in
  let cli_model_opt = if Base.String.is_empty model then None else Some model in
  let repo_config =
    let config_dir = User_config.config_dir ~github_owner ~github_repo in
    match Repo_config.load ~config_dir ~known_backends () with
    | Ok t -> t
    | Error msg ->
        Printf.eprintf "Error: %s\n" msg;
        Stdlib.exit 1
  in
  (match
     Backend_preflight.validate ~default_backend:backend
       ~cli_model:cli_model_opt ~repo_config ()
   with
  | Ok () -> ()
  | Error errs ->
      Base.List.iter errs ~f:(fun e -> Printf.eprintf "Error: %s\n" e);
      Stdlib.exit 1);
  let registry =
    Backend_registry.create ~process_mgr:setup.process_mgr ~clock:setup.clock
      ~timeout:session_timeout ~setsid_exec
  in
  let pick_backend ~complexity =
    let ({ Backend_routing.backend; model } as decision) =
      Backend_routing.decide ~repo_config ~default_backend:backend
        ~cli_model:cli_model_opt ~complexity
    in
    (Backend_registry.get registry ~backend ~model, decision)
  in
  let backend_name = function
    | Backend_registry.Ephemeral backend -> backend.Llm_backend.name
    | Backend_registry.Long_lived (Llm_backend_long_lived.T { name; _ }) -> name
  in
  let resolve_routing ~complexity : Backend_routing.decision =
    let dec : Backend_routing.decision =
      Backend_routing.decide ~repo_config ~default_backend:backend
        ~cli_model:cli_model_opt ~complexity
    in
    {
      dec with
      model =
        Backend_registry.resolve_model ~backend:dec.Backend_routing.backend
          ~model:dec.Backend_routing.model ~complexity;
    }
  in
  let pre_agents =
    Runtime.read setup.runtime (fun snap ->
        Orchestrator.all_agents snap.Runtime.orchestrator)
  in
  let pre_worktrees, pre_wt_error =
    Reconciler.recover_worktrees ~patches:setup.gameplan.Gameplan.patches
  in
  let reconciliation_fiber () =
    let startup =
      Reconciler.reconcile ~patches:setup.gameplan.Gameplan.patches
        ~agents:pre_agents ~pre_recovered_worktrees:pre_worktrees ()
    in
    let errored_ids =
      Base.List.map startup.Startup_reconciler.errors ~f:(fun (patch_id, err) ->
          log_event setup.runtime ~patch_id
            (Printf.sprintf "Startup discovery failed — %s" err);
          patch_id)
      |> Base.Hash_set.of_list (module Patch_id)
    in
    Runtime.read setup.runtime (fun snap ->
        Orchestrator.all_agents snap.Runtime.orchestrator)
    |> Base.List.iter ~f:(fun (agent : Patch_agent.t) ->
        if Base.Hash_set.mem errored_ids agent.Patch_agent.patch_id then
          Base.Option.iter agent.Patch_agent.pr_number ~f:(fun pr_number ->
              Pr_registry.register pr_registry
                ~patch_id:agent.Patch_agent.patch_id ~pr_number));
    let open Startup_reconciler in
    Base.List.iter startup.discovered
      ~f:(fun { pr_number = pr; patch_id = pid; base_branch = base; merged } ->
        Runtime.update_orchestrator setup.runtime (fun orch ->
            match Orchestrator.find_agent orch pid with
            | Some agent when Patch_agent.has_pr agent ->
                Pr_registry.register pr_registry ~patch_id:pid ~pr_number:pr;
                if merged then Orchestrator.mark_merged orch pid else orch
            | Some _ ->
                Pr_registry.register pr_registry ~patch_id:pid ~pr_number:pr;
                let orch =
                  Orchestrator.fire orch (Orchestrator.Start (pid, base))
                in
                let orch = Orchestrator.set_pr_number orch pid pr in
                let orch = Orchestrator.complete orch pid in
                if merged then Orchestrator.mark_merged orch pid else orch
            | None -> orch));
    Base.List.iter startup.reset_pending ~f:(fun patch_id ->
        log_event setup.runtime ~patch_id
          "Reset stale busy agent from crashed session";
        Runtime.update_orchestrator setup.runtime (fun orch ->
            Orchestrator.reset_busy orch patch_id));
    Base.List.iter startup.recovered_worktrees ~f:(fun wr ->
        log_event setup.runtime ~patch_id:wr.worktree_patch_id
          (Printf.sprintf "Recovered worktree at %s" wr.worktree_path));
    Base.List.iter
      (startup.worktree_errors @ Base.Option.to_list pre_wt_error)
      ~f:(fun err ->
        log_event setup.runtime
          (Printf.sprintf "Startup worktree error — %s" err))
  in
  let transcripts =
    let t = Hashtbl.create 16 in
    Runtime.read setup.runtime (fun snap ->
        Base.Hashtbl.iteri snap.Runtime.transcripts ~f:(fun ~key ~data ->
            Hashtbl.replace t key data));
    t
  in
  let event_log =
    Event_log.create ~path:(Project_store.event_log_path project_name)
  in
  Telemetry_dispatch.register_sink (Event_log.sink event_log);
  Telemetry_dispatch.register_sink
    (Activity_log_sink.sink
       ~update:(Runtime.update_activity_log setup.runtime)
       ());
  let review_clients =
    Base.List.map repo_config.Repo_config.review_backends ~f:(fun backend ->
        Review_service_client.make ~net ~clock:setup.clock ~backend)
  in
  let default_backend_pair = pick_backend ~complexity:None in
  {
    forge;
    worktree_client;
    startup_reconciler = (module Reconciler : STARTUP_RECONCILER);
    branch_of;
    pick_backend;
    resolve_routing;
    backend_name = backend_name (fst default_backend_pair);
    find_pr_number = Pr_registry.find pr_registry;
    register_pr_number = Pr_registry.register pr_registry;
    unregister_pr_number = Pr_registry.unregister pr_registry;
    findings_registry = Findings_registry.create ();
    review_clients;
    transcripts;
    event_log;
    worktree_mutex = Eio.Mutex.create ();
    hook_mutex = Eio.Mutex.create ();
    reconciliation_fiber;
  }

let build_fiber_env (setup : runtime_setup) (cap : constructed_capabilities)
    ~(tui_state : Tui_state.t) : built_fiber_env =
  let module Fiber_env : FIBER_ENV = struct
    let runtime = setup.runtime
    let clock = setup.clock
    let fs = setup.fs
    let process_mgr = setup.process_mgr
    let stdout = setup.stdout
    let config = setup.config
    let project_name = setup.config.project_name
    let findings_registry = cap.findings_registry
    let review_clients = cap.review_clients
    let transcripts = cap.transcripts
    let event_log = cap.event_log
    let branch_of = cap.branch_of
    let resolve_routing = cap.resolve_routing
    let backend_name = cap.backend_name
    let pick_backend = cap.pick_backend
    let find_pr_number = cap.find_pr_number
    let register_pr_number = cap.register_pr_number
    let unregister_pr_number = cap.unregister_pr_number
    let worktree_mutex = cap.worktree_mutex
    let hook_mutex = cap.hook_mutex
    let tui_state = tui_state
  end in
  (module Fiber_env : FIBER_ENV)

let run_main_loop (setup : runtime_setup) (cap : constructed_capabilities)
    ((module Fiber_env) : built_fiber_env) ~(tui_state : Tui_state.t) =
  let module Forge = (val cap.forge) in
  let module WorktreeClient = (val cap.worktree_client) in
  let module Fibers = Make_fibers (Forge) (WorktreeClient) (Fiber_env) in
  let { Resolved_config.headless; project_name; _ } = setup.config in
  let common_fibers =
    [
      cap.reconciliation_fiber;
      (fun () -> Fibers.Poller.run cap.startup_reconciler);
      (fun () -> Fibers.Persistence.run ());
    ]
  in
  if headless then
    Eio.Fiber.all
      ((fun () -> Fibers.Headless.run ())
      :: (fun () -> Fibers.Runner.run ())
      :: common_fibers)
  else
    let raw_state = Term.Raw.enter () in
    Fun.protect
      ~finally:(fun () ->
        Term.Raw.clear_suspend_handlers ();
        Term.Raw.leave raw_state;
        Eio.Flow.copy_string (Tui.exit_tui ()) setup.stdout;
        let snap = Runtime.snapshot_unsync setup.runtime in
        ignore
          (Persistence.save
             ~path:(Project_store.snapshot_path project_name)
             snap))
      (fun () ->
        Term.Raw.install_suspend_handlers raw_state;
        try
          Eio.Fiber.all
            ((fun () -> Fibers.Tui.run ())
            :: (fun () -> Fibers.Tui.run_input ())
            :: (fun () -> Fibers.Runner.run ~status_msg:tui_state.status_msg ())
            :: common_fibers)
        with Fibers.Tui.Quit -> ())

let run_with_config ~no_lock (config : config) gameplan existing_snapshot =
  let resolved = Resolved_config.of_config config |> ok_or_exit in
  let {
    Resolved_config.github_token;
    repo_root;
    main_branch;
    max_concurrency;
    project_name;
    _;
  } =
    resolved
  in
  Git_env.set_github_token github_token;
  let module Repo = (val Repo_git.make ~repo_root) in
  (match Repo.validate_branch_resolves ~main_branch with
  | Ok () -> ()
  | Error msg ->
      Printf.eprintf "Error: %s\n" msg;
      Stdlib.exit 1);
  let () =
    let open Onton.Rlimit in
    let required = (max_concurrency * 256) + 512 in
    let cur = get_nofile () in
    if cur.soft < required then begin
      let after = try_raise_nofile_soft ~target:required in
      if after.soft < required then begin
        Printf.eprintf
          "onton: soft FD limit %d is below the required %d (hard cap %d).\n\
          \       Raise it with: ulimit -n %d\n\
          \       macOS system ceiling: sudo launchctl limit maxfiles <soft> \
           <hard>.\n\
           %!"
          after.soft required after.hard required;
        Stdlib.exit 1
      end
    end
  in
  let lock =
    if no_lock then None
    else
      let project_dir = Project_store.project_dir project_name in
      match
        Project_lock.acquire ~project_dir ~on_stale:(fun stale_pid ->
            if stale_pid > 0 then
              Printf.eprintf "onton: reclaiming stale lock from pid %d\n%!"
                stale_pid)
      with
      | Ok l -> Some l
      | Error e ->
          Printf.eprintf "onton: %s\n"
            (Format.asprintf "%a" Project_lock.pp_error e);
          Printf.eprintf
            "       pass --no-lock (or set ONTON_NO_LOCK=1) to bypass.\n%!";
          Stdlib.exit 75
  in
  Stdlib.Fun.protect ~finally:(fun () ->
      Base.Option.iter lock ~f:Project_lock.release)
  @@ fun () ->
  Eio_main.run @@ fun env ->
  let setup = setup_runtime env ~config:resolved ~gameplan ~existing_snapshot in
  let capabilities = construct_capabilities ~net:(Eio.Stdenv.net env) setup in
  let tui_state = Tui_state.create () in
  let fiber_env = build_fiber_env setup capabilities ~tui_state in
  run_main_loop setup capabilities fiber_env ~tui_state

(** {1 Prune}

    Remove every persisted project whose gameplan patches are all marked merged
    in the saved snapshot. Operates only on the per-project data directory
    (snapshot, events, gameplan, config, artifacts) — git worktrees under
    [~/worktrees/<project>/] are left in place because they can contain
    user-visible state (build outputs, untracked files) that the user may want
    to inspect or reuse, and traversing a populated build tree is far slower
    than removing the small data directory. Pruned projects are reported with
    the worktree path so the user can decide whether to clean it up.

    Skips any project whose lock is held by a live process — pruning state out
    from under a running [onton] would invalidate its in-memory view. *)

let run ~project ~gameplan_path ~github_token ~backend ~model
    ~(main_branch : Branch.t option) ~poll_interval ~(repo_root : string option)
    ~max_concurrency ~headless ~no_lock =
  match
    resolve_config ~project ~gameplan_path ~github_token ~backend ~model
      ~main_branch ~poll_interval ~repo_root ~max_concurrency ~headless
  with
  | Error errs ->
      Base.List.iter errs ~f:(fun e -> Printf.eprintf "Error: %s\n" e);
      Stdlib.exit 1
  | Ok (config, gameplan, existing_snapshot) ->
      run_with_config ~no_lock config gameplan existing_snapshot

(** {1 CLI via Cmdliner} *)

let project_arg =
  let open Cmdliner in
  Arg.(
    value
    & pos 0 (some string) None
    & info [] ~docv:"PROJECT"
        ~doc:
          "Project name to resume. If omitted, derived from --gameplan's \
           project name.")

let gameplan_path_arg =
  let open Cmdliner in
  Arg.(
    value
    & opt (some string) None
    & info [ "gameplan" ] ~docv:"GAMEPLAN" ~doc:"Path to the gameplan file.")

let github_token_arg =
  let open Cmdliner in
  Arg.(
    value & opt string ""
    & info [ "token" ] ~docv:"TOKEN" ~doc:"GitHub API token."
        ~env:(Cmd.Env.info "GITHUB_TOKEN"))

let backend_arg =
  let open Cmdliner in
  Arg.(
    value & opt string ""
    & info [ "backend" ] ~docv:"BACKEND"
        ~doc:
          "LLM backend to use: claude, codex, opencode, pi, gemini, or \
           patch-agent.")

let model_arg =
  let open Cmdliner in
  Arg.(
    value & opt string ""
    & info [ "model" ] ~docv:"MODEL"
        ~doc:
          "Model name to pass to the selected backend (e.g. [sonnet], [opus], \
           [sonnet-4-6] for claude; [gpt-5.5] for codex). The literal value \
           [auto] picks a model per patch from the gameplan's [complexity] \
           field (1/2/3 → cheap/standard/strongest tier of the selected \
           backend). The per-backend ladder can be overridden by writing \
           [~/.config/onton/<owner>/<repo>/config.json] with a [routing] map — \
           see lib/repo_config.mli for the schema. When omitted, onton does \
           not pass --model to the underlying CLI, so the backend's own \
           default applies.")

let repo_arg =
  let open Cmdliner in
  Arg.(
    value
    & opt (some string) None
    & info [ "repo" ] ~docv:"PATH"
        ~doc:"Path to the git repository (default: current directory).")

let main_branch_arg =
  let open Cmdliner in
  Arg.(
    value
    & opt (some string) None
    & info [ "main-branch" ] ~docv:"BRANCH"
        ~doc:"Main branch name. Auto-detected from the git remote when omitted.")

let poll_interval_arg =
  let open Cmdliner in
  Arg.(
    value & opt float 30.0
    & info [ "poll-interval" ] ~docv:"SECONDS"
        ~doc:"Polling interval in seconds (default: 30).")

let max_concurrency_arg =
  let open Cmdliner in
  Arg.(
    value & opt int 5
    & info [ "max-concurrency" ] ~docv:"N"
        ~doc:"Maximum number of concurrent backend sessions (default: 5)."
        ~env:(Cmd.Env.info "ONTON_MAX_CONCURRENCY"))

let headless_arg =
  let open Cmdliner in
  Arg.(
    value & flag
    & info [ "headless" ] ~doc:"Run without TUI (plain log output).")

let upload_debug_arg =
  let open Cmdliner in
  Arg.(
    value & flag
    & info [ "upload-debug" ]
        ~doc:
          "Upload project debug state for troubleshooting. Requires a project \
           name.")

let no_lock_arg =
  let open Cmdliner in
  Arg.(
    value & flag
    & info [ "no-lock" ]
        ~doc:
          "Bypass the per-project advisory lock. Only use when a stale lock \
           cannot be reclaimed automatically."
        ~env:(Cmd.Env.info "ONTON_NO_LOCK"))

let prune_arg =
  let open Cmdliner in
  Arg.(
    value & flag
    & info [ "prune" ]
        ~doc:
          "Remove every stored project whose gameplan patches are all merged. \
           Skips projects whose lock is held by a live onton process. Does not \
           require any other arguments.")

let main_cmd =
  let open Cmdliner in
  let run_cmd project gameplan_path github_token backend model main_branch
      poll_interval repo_root max_concurrency headless upload_debug no_lock
      prune =
    if prune then Stdlib.exit (Prune_runner.run_prune ())
    else if upload_debug then (
      match project with
      | None ->
          Printf.eprintf
            "Error: --upload-debug requires a project name.\n\
             Usage: onton PROJECT --upload-debug\n";
          Stdlib.exit 1
      | Some project_name ->
          if not (Project_store.project_exists project_name) then (
            Printf.eprintf "Error: no stored project %S.\n" project_name;
            Printf.eprintf "Known projects: %s\n"
              (String.concat ", " (Project_store.list_projects ()));
            Stdlib.exit 1);
          Eio_main.run @@ fun env ->
          Debug_upload.run ~net:(Eio.Stdenv.net env) ~project_name
            ~version:Version.s)
    else
      let main_branch =
        Base.Option.map main_branch ~f:(fun s ->
            Branch.of_string (Base.String.strip s))
      in
      run ~project ~gameplan_path ~github_token
        ~backend:(Base.String.strip backend)
        ~model:(Base.String.strip model) ~main_branch ~poll_interval ~repo_root
        ~max_concurrency ~headless ~no_lock
  in
  let term =
    Term.(
      const run_cmd $ project_arg $ gameplan_path_arg $ github_token_arg
      $ backend_arg $ model_arg $ main_branch_arg $ poll_interval_arg $ repo_arg
      $ max_concurrency_arg $ headless_arg $ upload_debug_arg $ no_lock_arg
      $ prune_arg)
  in
  let info =
    Cmd.info "onton" ~version:Version.s
      ~doc:
        "Orchestrate parallel patch development with an LLM coding agent.\n\n\
         Usage:\n\
        \  onton [PROJECT] --gameplan GAMEPLAN [OPTIONS]   Start a new project\n\
        \  onton PROJECT [OPTIONS]                         Resume a saved \
         project"
  in
  Cmd.v info term

let () = Stdlib.exit (Cmdliner.Cmd.eval main_cmd)

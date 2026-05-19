(** Project-level data directory management.

    Persists project configuration and gameplan sources to
    [~/.local/share/onton/<project-slug>/] (or [$ONTON_DATA_DIR/<slug>/]).
    Enables resuming a project by name without the original gameplan file. *)

val slugify : string -> string
(** Convert a project name to a filesystem-safe slug. *)

val project_dir : string -> string
(** Absolute path to a project's data directory. *)

val snapshot_path : string -> string
(** Path to the project's persisted snapshot JSON. *)

val managed_repo_dir : string -> string
(** Path to the project's onton-managed git checkout ([<project-dir>/repo]).
    Sessions started via [--gameplan] take their [owner]/[repo] from the
    gameplan as source-of-truth; onton clones the repo into this directory (or
    fetches it on resume) and uses it as the source for all worktrees. Sessions
    started ad-hoc (no [--gameplan]) continue to use a user-provided checkout
    and do not populate this path. *)

val event_log_path : string -> string
(** Path to the project's append-only event log (JSONL). *)

val config_path : string -> string
(** Path to the project's persisted config JSON. *)

val ensure_dir : string -> unit
(** Create a directory and parents if needed. *)

type stored_config = {
  project_name : string;
  github_token : string;
  github_owner : string;
  github_repo : string;
  backend : string;
  model : string;
  main_branch : string;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
}
[@@deriving yojson]

val save_config :
  project_name:string ->
  github_token:string ->
  github_owner:string ->
  github_repo:string ->
  backend:string ->
  model:string ->
  main_branch:string ->
  poll_interval:float ->
  repo_root:string ->
  max_concurrency:int ->
  unit
(** Persist project config to the data directory. Creates the directory if
    needed. *)

val load_config : project_name:string -> (stored_config, string) result
(** Load a previously saved project config. *)

val save_gameplan_source : project_name:string -> source_path:string -> unit
(** Copy the gameplan file into the project data directory. Detects [.json] vs
    markdown by file extension and stores to the appropriate filename. *)

val gameplan_path : string -> string
(** Path to the stored gameplan markdown ([gameplan.md]). *)

val gameplan_json_path : string -> string
(** Path to the stored gameplan JSON ([gameplan.json]). *)

val stored_gameplan_path : string -> string
(** Returns the path to whichever stored gameplan file exists ([gameplan.md]
    takes precedence; falls back to [gameplan.json]). *)

val pr_body_artifact_path :
  project_name:string -> patch_id:Types.Patch_id.t -> string
(** Absolute path the agent writes the LLM-authored PR body to. Lives under the
    project's data directory at [artifacts/<patch_id>/pr-body.md] — outside the
    worktree so it can never be accidentally committed. *)

val findings_wontfix_artifact_path :
  project_name:string -> patch_id:Types.Patch_id.t -> string
(** Absolute path of the [findings_wontfix.json] artifact for a Findings
    session. The agent writes a JSON list of [{id, reason}] for any finding it
    has decided not to fix; the supervisor consumes it post-session and POSTs
    the corresponding resolve verbs to the review backend. Findings not listed
    here default to [addressed]. *)

val project_exists : string -> bool
(** Whether a project data directory with config exists. *)

val list_projects : unit -> string list
(** List all project slugs in the data directory. *)

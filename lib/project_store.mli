(* @archlint.module interface
   @archlint.domain project-store *)

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
  github_owner : string;
  github_repo : string;
  backend : string;
  model : string;
  main_branch : string;
  poll_interval : float;
  repo_root : string;
  max_concurrency : int;
  url_scheme : string option;
      (** Persisted transport for the managed [origin]. [None] on legacy configs
          predating P0-D; gets auto-resolved on the next
          {!Managed_repo.ensure_managed_repo}. Stored as a string
          ([Some "https"] / [Some "ssh"]) so the file remains human-readable. *)
}
[@@deriving yojson]

val save_config :
  project_name:string ->
  github_owner:string ->
  github_repo:string ->
  backend:string ->
  model:string ->
  main_branch:string ->
  poll_interval:float ->
  repo_root:string ->
  max_concurrency:int ->
  ?url_scheme:string option ->
  unit ->
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

val sessions_dir : string -> string
(** Path to the per-session artifact directory root ([sessions/]). *)

val gameplan_artifact_path : string -> string
(** Absolute path of the agent-readable gameplan copy
    ([artifacts/gameplan.json]). Lives in the agent-facing [artifacts/] subtree
    (alongside the per-patch artifact dirs) so patch agents can consult the full
    gameplan on demand without being pointed at the project-dir root, which
    holds [config.json] and its token. A pure function of the project name —
    prompt layers embed it without touching the filesystem. *)

val publish_gameplan_artifact : project_name:string -> unit
(** Copy the stored JSON gameplan ({!gameplan_json_path}) to
    {!gameplan_artifact_path} for patch agents to read. No-op when no stored
    JSON gameplan exists (ad-hoc sessions). Called once at startup, after
    {!save_gameplan_source} / resume validation, so the copy matches the
    gameplan the run was parsed from. *)

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

val ci_artifact_dir : project_name:string -> patch_id:Types.Patch_id.t -> string
(** Absolute directory for per-check CI diagnostics under
    [artifacts/<patch_id>/ci]. Pure path helper for prompt rendering; does not
    touch the filesystem. *)

val ci_check_key : Types.Ci_check.t -> string
(** Stable artifact key for a CI check. CheckRuns use [run-<databaseId>];
    id-less StatusContexts and synthesized checks use [status-<slugified name>].
*)

val ci_check_artifact_dir :
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  check:Types.Ci_check.t ->
  string
(** Absolute directory for one check's CI diagnostic artifacts under
    {!ci_artifact_dir}. Pure path helper for prompt rendering; does not touch
    the filesystem. *)

val publish_ci_check_artifact :
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  check:Types.Ci_check.t ->
  ?head_oid:string ->
  summary_md:string ->
  ?log:string ->
  unit ->
  (string, string) result
(** Publish one check's CI diagnostic artifacts. Writes [check.json] and
    [summary.md], plus [log.txt] exactly when [log] is supplied. Returns the
    check artifact directory, or the first write error. *)

val project_exists : string -> bool
(** Whether a project data directory with config exists. *)

val list_projects : unit -> string list
(** List all project slugs in the data directory. *)

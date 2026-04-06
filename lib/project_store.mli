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

val event_log_path : string -> string
(** Path to the project's append-only event log (JSONL). *)

val ensure_dir : string -> unit
(** Create a directory and parents if needed. *)

type stored_config = {
  project_name : string;
  github_token : string;
  github_owner : string;
  github_repo : string;
  backend : string;
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

val project_exists : string -> bool
(** Whether a project data directory with config exists. *)

val list_projects : unit -> string list
(** List all project slugs in the data directory. *)

open Base

type t = { on_worktree_create : string option }

val load : github_owner:string -> github_repo:string -> t
(** Load user configuration from [~/.config/onton/<owner>/<repo>/]. Returns
    defaults (all [None]) if no config directory exists. *)

val config_dir : github_owner:string -> github_repo:string -> string
(** The config directory path for a given owner/repo. *)

val run_hook :
  process_mgr:_ Eio.Process.mgr ->
  script:string ->
  cwd:_ Eio.Path.t ->
  env:(string * string) list ->
  (unit, string) Result.t
(** Execute a hook script in [cwd] with the given environment variables. Both
    stdout and stderr are captured; on failure the returned [Error msg] embeds
    the underlying exception followed by non-empty stdout/stderr sections so the
    caller can surface them (e.g. to the activity log). *)

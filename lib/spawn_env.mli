val per_patch_env :
  project_name:string -> patch_id:Types.Patch_id.t -> (string * string) list
(** Per-patch config-dir overrides for headless agent CLIs. Creates the backing
    directories under the project store before returning:
    [spawn-envs/<patch_id>/{claude,codex,opencode}]. Also seeds each per-patch
    dir with a symlink to the user's real auth file. *)

val per_patch_env_without_codex_home :
  project_name:string -> patch_id:Types.Patch_id.t -> (string * string) list
(** Like {!per_patch_env}, but omits [CODEX_HOME] so Codex sessions use the
    user's normal Codex home. Codex refresh tokens rotate and are not safe to
    fan out across independent homes. *)

val merge_env :
  base_env:string array -> overrides:(string * string) list -> string array
(** Merge [overrides] into [base_env], replacing any existing entries with the
    same variable name. *)

val per_patch_env :
  project_name:string -> patch_id:Types.Patch_id.t -> (string * string) list
(** Per-patch config-dir overrides for headless agent CLIs. Creates the backing
    directories under the project store before returning:
    [spawn-envs/<patch_id>/{claude,codex,opencode}]. Also seeds each per-patch
    dir with a symlink to the user's real auth file
    ([auth.json]/[.credentials.json]/[opencode.json]) so the backend CLI finds
    its credentials and token rotations propagate back to the user's home. *)

val merge_env :
  base_env:string array -> overrides:(string * string) list -> string array
(** Merge [overrides] into [base_env], replacing any existing entries with the
    same variable name. *)

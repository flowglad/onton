val per_patch_env :
  project_name:string -> patch_id:Types.Patch_id.t -> (string * string) list
(** Per-patch config-dir overrides for headless agent CLIs. Creates the backing
    directories under the project store before returning:
    [spawn-envs/<patch_id>/{claude,codex,opencode}]. *)

val merge_env :
  base_env:string array -> overrides:(string * string) list -> string array
(** Merge [overrides] into [base_env], replacing any existing entries with the
    same variable name. *)

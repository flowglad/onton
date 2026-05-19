val per_patch_env :
  project_name:string -> patch_id:Types.Patch_id.t -> (string * string) list
(** Per-patch config-dir overrides for headless agent CLIs. Creates the backing
    directories under the project store before returning:
    [spawn-envs/<patch_id>/{claude,codex,opencode}]. Also seeds each per-patch
    dir with a symlink to the user's real auth file (a no-op when the user's
    file does not exist — e.g. macOS Claude, where credentials live in the
    Keychain rather than [~/.claude/.credentials.json]).

    On macOS, scoping [CLAUDE_CONFIG_DIR] makes Claude unable to find the
    Keychain-stored OAuth credential, so [per_patch_env] also injects
    [CLAUDE_CODE_OAUTH_TOKEN] when one is available. The token is sourced from
    the parent process env if already set; otherwise read from
    [$XDG_CONFIG_HOME/onton/claude-oauth-token] (or
    [~/.config/onton/claude-oauth-token]). Generate the token once via
    [claude setup-token] and write it to that file (mode 0600). *)

val per_patch_env_without_codex_home :
  project_name:string -> patch_id:Types.Patch_id.t -> (string * string) list
(** Like {!per_patch_env}, but omits [CODEX_HOME] so Codex sessions use the
    user's normal Codex home. Codex refresh tokens rotate and are not safe to
    fan out across independent homes. *)

val merge_env :
  base_env:string array -> overrides:(string * string) list -> string array
(** Merge [overrides] into [base_env], replacing any existing entries with the
    same variable name. *)

val claude_session_jsonl_path :
  project_name:string ->
  patch_id:Types.Patch_id.t ->
  worktree_path:string ->
  session_id:string ->
  string
(** Absolute path to a claude conversation file:
    [<per-patch-CLAUDE_CONFIG_DIR>/projects/<cwd-key>/<session-id>.jsonl], where
    [cwd-key] is [worktree_path] with every ["/"] replaced by ["-"] (claude's
    internal projects-dir keying). Used to clean up stub files left behind when
    [claude --resume] fails with "No conversation found". *)

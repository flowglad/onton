open Base

type t = { on_worktree_create : string option }

val load : github_owner:string -> github_repo:string -> t
(** Load user configuration from [~/.config/onton/<owner>/<repo>/]. Returns
    defaults (all [None]) if no config directory exists. *)

val config_dir : github_owner:string -> github_repo:string -> string
(** The config directory path for a given owner/repo. *)

val run_hook :
  process_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  script:string ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  env:(string * string) list ->
  ?timeout:float ->
  ?fd_limit:int ->
  unit ->
  (unit, string) Result.t
(** Execute a hook script in [cwd] with the given environment variables.

    The hook is invoked through
    [/bin/sh -c "ulimit -n <fd_limit>; exec <script>"] so the child's
    file-descriptor limit is capped independently of the parent. This prevents a
    runaway hook (e.g. [npm install] fanning into hundreds of node subprocesses,
    or [dune build] spawning many compilers) from exhausting the shared host FD
    table — see issue #209.

    A wall-clock [timeout] bounds the hook; on expiry the child is sent
    [SIGKILL] and an [Error] is returned carrying any captured output.

    Defaults (override per call or via env):
    - [timeout]: 600.0 seconds ([ONTON_HOOK_TIMEOUT])
    - [fd_limit]: 256 ([ONTON_HOOK_FD_LIMIT])

    Both stdout and stderr are captured; on failure (non-zero exit, signal,
    timeout) the returned [Error msg] embeds a status summary followed by
    non-empty stdout/stderr sections so the caller can surface them. *)

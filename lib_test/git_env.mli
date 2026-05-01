(** Test helpers for spawning [git] without inheriting a poisoned environment.

    When [dune runtest] runs inside this project's pre-commit hook, git sets
    [GIT_DIR] / [GIT_INDEX_FILE] / etc. on the hook process. Tests that shell
    out to [git init] without scrubbing the env will reinitialize the parent
    [.git] (since [git init] honors [GIT_DIR] over [cwd]) and silently set
    [core.bare = true] in the real config. Always run [git] in tests through the
    helpers below, or pass the result of {!clean_env} as the env array when
    spawning your own process. *)

val clean_env : unit -> string array
(** A copy of [Unix.environment ()] with the env vars that redirect git's
    repository target stripped: [GIT_DIR], [GIT_WORK_TREE], [GIT_INDEX_FILE],
    [GIT_COMMON_DIR], [GIT_OBJECT_DIRECTORY].

    Vars like [GIT_AUTHOR_*] and [GIT_COMMITTER_*] are intentionally preserved —
    they only override identity for new commits, they don't poison the parent's
    config. If you want full hermeticity from the user's [~/.gitconfig] (e.g.
    [commit.gpgsign], [init.defaultBranch]) append [GIT_CONFIG_GLOBAL=/dev/null]
    / [GIT_CONFIG_SYSTEM=/dev/null] yourself. *)

val run_git : cwd:string -> string list -> unit
(** Spawn [git <args>] in [cwd] with {!clean_env}. Waits for exit. Raises
    [Failure] with the command, cwd, exit code, and captured stderr on non-zero
    exit (or signal). Stdout is discarded — for tests that need it, use
    [Unix.create_process_env] directly with {!clean_env}. *)

val init_repo : string -> unit
(** Initialize a fresh git repo at [dir] with a deterministic identity: runs
    [git init -q -b main], then sets local [user.email] and [user.name]. Does
    {b not} create an initial commit — callers that need a HEAD (e.g. before
    [git worktree add]) should follow up with their own [commit --allow-empty],
    and callers that build a precise commit graph (rebase tests) get a clean
    slate. [dir] must already exist. *)

val with_temp_repo : (string -> 'a) -> 'a
(** [with_temp_repo f] creates a fresh temp directory, runs {!init_repo} on it,
    calls [f dir], and best-effort [rm -rf]s the directory afterward (even if
    [f] raises). Returns [f]'s result. *)

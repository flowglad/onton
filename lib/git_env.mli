(** A scrubbed [Unix.environment ()] for spawning [git] subprocesses.

    Any inherited [GIT_DIR] / [GIT_WORK_TREE] / [GIT_INDEX_FILE] /
    [GIT_COMMON_DIR] / [GIT_OBJECT_DIRECTORY] would override the [-C path] flag
    and the cwd we just set, redirecting git at a different repository than the
    caller intended. The most common source of leaked vars is git's own
    pre-commit / post-checkout hook environment, but they can also leak through
    [gh], IDE integrations, or shells started under one of the above.

    Always pass [clean_env ()] (or its result) as the env array when spawning
    git from this codebase. *)

val clean_env : unit -> string array
(** [Unix.environment ()] minus the env vars that redirect git's repository
    target. [GIT_AUTHOR_*] / [GIT_COMMITTER_*] are intentionally preserved —
    they only affect new commits, they don't poison config or override the
    target repo. *)

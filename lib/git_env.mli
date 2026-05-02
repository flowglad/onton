(** A scrubbed [Unix.environment ()] for spawning [git] subprocesses.

    Any inherited [GIT_*] variable can override the [-C path] flag, redirect git
    at a different repository, or change object/index storage. The most common
    source of leaked vars is git's own pre-commit / post-checkout hook
    environment, but they can also leak through [gh], IDE integrations, or
    shells started inside a repo.

    Always pass [clean_env ()] (or its result) as the env array when spawning
    git from this codebase. *)

val clean_env : unit -> string array
(** [Unix.environment ()] with all [GIT_*] variables removed.

    onton only spawns git for repository queries, not for authoring commits, so
    stripping [GIT_AUTHOR_*] / [GIT_COMMITTER_*] along with the rest is harmless
    and keeps the isolation boundary simple. *)

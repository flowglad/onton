(* @archlint.module interface
   @archlint.domain git-env *)

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
    and keeps the isolation boundary simple.

    The returned environment also disables terminal prompts and installs a
    controlled HTTPS askpass helper. For GitHub remotes, the helper supplies the
    token configured by {!set_github_token}, an inherited [GITHUB_TOKEN] or
    [GH_TOKEN], or a noninteractive [gh auth token] result. For SourceHut
    remotes, it supplies the username and token configured by
    {!set_sourcehut_token}, or inherited [SRHT_USERNAME] and [SRHT_TOKEN]
    bindings. *)

val set_github_token : string -> unit
(** [set_github_token token] makes [token] available to future [clean_env ()]
    results for supervised git HTTPS authentication. Empty tokens are ignored.
*)

val github_token : fallback:string -> string
(** Read the renewable installation token when [ONTON_GITHUB_TOKEN_FILE] is set.
    A missing or unreadable file yields an empty token so authentication fails
    closed. Without a token file, return [fallback]. *)

val set_sourcehut_token : username:string -> string -> unit
(** [set_sourcehut_token ~username token] makes the SourceHut HTTPS credentials
    available to future [clean_env ()] results. Empty values are ignored. *)

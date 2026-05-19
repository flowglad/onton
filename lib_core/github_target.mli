(** Pure GitHub forge target logic.

    [owner] and [repo] are generic git-forge concepts; this module captures the
    GitHub-specific rules for validating, formatting, and parsing them — nothing
    else in [onton_core] needs to know GitHub. The functions are total and
    side-effect-free, so they can be exhaustively property-tested without
    spawning subprocesses or hitting the network. *)

val validate_owner : string -> (unit, string) Result.t
(** Validate a string as a GitHub user/org handle: 1–39 characters, alphanumeric
    or dash, first character not a dash. The string is stripped before
    validation, so leading/trailing whitespace is forgiven. *)

val validate_repo : string -> (unit, string) Result.t
(** Validate a string as a GitHub repository name: 1–100 characters,
    alphanumeric or [.] [-] [_], first character not punctuation. The string is
    stripped before validation. *)

val validate_target : owner:string -> repo:string -> (unit, string) Result.t
(** Combined owner + repo validation. Returns the first [Error] encountered
    (owner checked before repo) so a single error suffices to identify the
    offending half. *)

val clone_url : owner:string -> repo:string -> string
(** HTTPS clone URL [https://github.com/<owner>/<repo>.git]. The URL itself
    never carries credentials; authentication is supplied out-of-band via
    [Onton.Git_env.set_github_token]. *)

val infer_owner_repo_from_url : string -> (string * string) option
(** Parse a GitHub remote URL (HTTPS or SSH) and extract [(owner, repo)]. Strips
    a trailing [.git] suffix and trailing slash. Returns [None] for URLs that do
    not name a [github.com] host or whose path does not have the [/owner/repo]
    shape. Total — never raises. *)

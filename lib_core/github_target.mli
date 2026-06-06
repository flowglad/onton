(* @archlint.module interface
   @archlint.domain github-target *)

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
    stripped before validation. Names ending in [.git] are rejected so
    {!clone_url} cannot produce a double-[.git] suffix. *)

val validate_target : owner:string -> repo:string -> (unit, string) Result.t
(** Combined owner + repo validation. Returns the first [Error] encountered
    (owner checked before repo) so a single error suffices to identify the
    offending half. *)

(** Transport scheme for the managed clone's [origin]. [Https] uses GitHub's
    [https://github.com/<owner>/<repo>.git] URL together with the OAuth/PAT
    token injected via {!Onton.Git_env}. [Ssh] uses
    [git@github.com:<owner>/<repo>.git] and relies on the user's ssh-agent and
    [~/.ssh/config] for authentication. SSH is the only transport that bypasses
    the per-OAuth-scope restrictions GitHub enforces (e.g. the [workflow] scope
    for [.github/workflows/*] changes); when SSH is reachable it is preferred,
    with HTTPS as the fallback. *)
type url_scheme = Https | Ssh [@@deriving show, eq, sexp_of, compare, yojson]

val clone_url : scheme:url_scheme -> owner:string -> repo:string -> string
(** Clone URL for the given transport scheme. [Https] returns
    [https://github.com/<owner>/<repo>.git]; [Ssh] returns
    [git@github.com:<owner>/<repo>.git]. The URL itself never carries
    credentials; authentication is supplied out-of-band (HTTPS:
    {!Onton.Git_env.set_github_token}; SSH: the user's ssh-agent). *)

val scheme_of_url : string -> url_scheme option
(** Classify an existing remote URL as [Some Https] / [Some Ssh] / [None]. The
    [None] branch covers junk strings and any non-github.com URL. Total — never
    raises. Used by [lib/managed_repo.ml] to detect the existing managed clone's
    transport when one is already on disk. *)

val resolve_scheme :
  override:url_scheme option -> ssh_available:bool -> url_scheme
(** Pure decision for "what transport should the managed clone use?". Returns
    [override] when [Some] (the [--clone-scheme] CLI flag, or a previously-
    persisted scheme, wins). With [override = None], returns [Ssh] when
    [ssh_available] is [true], otherwise [Https]. The handler in
    [lib/managed_repo.ml] is responsible for probing SSH reachability; this
    function takes the probe result as data so the decision stays purely
    testable. *)

val infer_owner_repo_from_url : string -> (string * string) option
(** Parse a GitHub remote URL (HTTPS or SSH) and extract [(owner, repo)]. Strips
    a trailing [.git] suffix and trailing slash. Returns [None] for URLs that do
    not name a [github.com] host or whose path does not have the [/owner/repo]
    shape. Total — never raises. *)

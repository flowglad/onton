(** Pure analysis of GitHub OAuth token scopes.

    GitHub returns the active scopes for an authenticated request in the
    [X-OAuth-Scopes] response header (e.g. ["repo, workflow, read:org"]).
    Certain pushes require specific scopes — most notably the [workflow] scope
    for any change to a file under [.github/workflows/] — and a token missing
    the required scope is silently rejected with [! [remote rejected]] at push
    time.

    The pre-flight check at startup compares the scopes the configured token
    actually has against the scopes the gameplan {e will need} (based on the
    files each patch declares it will touch). When a required scope is missing,
    onton prints a clear stderr warning together with the suggested
    [gh auth refresh -h github.com -s <scope>] fix.

    All decision logic lives here so the gameplan-derived requirement and the
    header-derived presence are both pure functions over their inputs. The
    effectful piece (the HTTPS GET against [api.github.com/user]) lives in
    [lib/github.ml] and feeds [parse_header] with the header string it receives.
*)

type scope =
  | Repo
  | Workflow
  | Read_org
  | Other of string
      (** Anything we don't have a named constructor for, including the
          GitHub-fine-grained Permission strings that we may want to surface
          verbatim. The string is the scope token as it appears in the
          [X-OAuth-Scopes] header (lowercase, may contain ":"). *)
[@@deriving show, eq, sexp_of, compare]

val parse_header : string -> scope list
(** Parse an [X-OAuth-Scopes] header value (comma-separated scope tokens with
    optional whitespace). Total — never raises. Empty / whitespace-only input
    returns []. Whitespace and surrounding quotes are stripped from each token.
    Recognised tokens map to named constructors; unrecognised tokens map to
    [Other token] so downstream logic can still display them. *)

val required_for_files : paths:string list -> scope list
(** Compute the set of OAuth scopes required to push the union of [paths].
    Returns [[Workflow]] iff any path is under [.github/workflows/]; [[]]
    otherwise. Extension point for future requirements (Pages, Packages, etc.) —
    the rules live entirely in this function so the corresponding properties
    stay co-located. *)

val missing : required:scope list -> present:scope list -> scope list
(** Set difference [required - present], up to scope equality. The result
    preserves the order of [required] and de-duplicates. [missing] is empty when
    [present] is a superset of [required]. *)

val show_scope_short : scope -> string
(** Short label suitable for log lines: ["repo"], ["workflow"], ["read:org"], or
    the [Other] payload verbatim. *)

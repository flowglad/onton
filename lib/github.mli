(** GitHub forge implementation.

    Queries the GitHub GraphQL API for PR/world state. Satisfies {!Forge.S}.

    Pure GitHub-target logic (identifier validation, URL formatting, remote URL
    parsing) lives in {!Onton_core.Github_target} so the rules can be
    property-tested without spawning subprocesses; this module owns the
    effectful HTTP and network surface. *)

type error =
  | Http_error of { meth : string; path : string; status : int; body : string }
  | Json_parse_error of string
  | Graphql_error of string list
  | Timeout of { meth : string; path : string; seconds : float }
  | Transport_error of { meth : string; path : string; msg : string }

val show_error : error -> string
(** Render an error as a human-readable string. Includes the HTTP method and
    path for [Http_error]/[Timeout]/[Transport_error], extracts GitHub's
    "message" field and validation details from the response body, and appends a
    hint about PAT scopes for 401/403/404 so users can fix permission problems
    without guesswork. *)

val response_error_message_contains : string -> substring:string -> bool
(** Returns [true] if any human-meaningful field in a GitHub error response body
    contains [substring] (case-insensitive). Use this to discriminate between
    distinct 422 cases (e.g. "pull request already exists" vs "no commits
    between"). Pure; safe on malformed input. *)

val default_timeout : float
(** Per-request timeout default, in seconds. Without a timeout, a TCP connect
    stuck in [SYN_SENT] (e.g. dropped packets, dead route) can block the calling
    fiber indefinitely. *)

val check_repo_access :
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  ?timeout:float ->
  token:string ->
  owner:string ->
  repo:string ->
  unit ->
  (unit, error) Result.t
(** [check_repo_access ~net ~clock ~token ~owner ~repo] verifies that the
    configured token can access [owner/repo] via [GET /repos/:owner/:repo]. This
    is a startup preflight so missing tokens, expired credentials, SSO gaps, and
    wrong repository names fail before long-running orchestration starts. *)

val parse_response_json :
  owner:string -> Yojson.Safe.t -> (Pr_state.t, error) Result.t
(** Parse a GitHub GraphQL response JSON value into a [Pr_state.t]. Pure
    function — no I/O or string parsing. [~owner] is the configured repository
    owner, used to detect fork PRs. *)

val parse_response : owner:string -> string -> (Pr_state.t, error) Result.t
(** Parse a GitHub GraphQL response body string into a [Pr_state.t]. *)

val parse_rest_pr_list :
  string -> ((Types.Pr_number.t * Types.Branch.t * bool) list, error) Result.t
(** Parse the REST response from [GET /repos/:owner/:repo/pulls]. Returns
    non-CLOSED PRs as [(pr_number, base_branch, merged)]. Pure function. *)

type merge_result =
  | Merge_succeeded
      (** Response body confirmed [merged = true]; the PR is merged. *)
  | Merge_queued of string
      (** Response body had [merged = false]; GitHub accepted the request
          (typically into its auto-merge queue waiting for required checks) but
          has not yet merged. Carries GitHub's [message] for logs. *)
  | Merge_unconfirmed
      (** Response was 2xx but did not include a parseable [merged] field. Treat
          as non-authoritative: don't mark merged, don't count as failure — the
          poller will observe the real PR state next cycle. *)

val make :
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  token:string ->
  owner:string ->
  repo:string ->
  (module Forge.S with type error = error)
(** [make ~net ~clock ~token ~owner ~repo] packages a GitHub-backed forge module
    that closes over the network and time capabilities plus client
    configuration. *)

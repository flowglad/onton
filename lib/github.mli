(* @archlint.module interface
   @archlint.domain github *)

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

val extract_github_message : string -> string
(** Summarize an HTTP error response body for activity-log display. When the
    body is a GitHub JSON error, returns the [message] field (and any
    [errors[].message] details). When the body is not JSON — e.g. the multi-KB
    HTML page GitHub serves on 5xx — truncates to ~200 characters so it does not
    flood the activity stream. *)

val default_timeout : float
(** Per-request timeout default, in seconds. Without a timeout, a TCP connect
    stuck in [SYN_SENT] (e.g. dropped packets, dead route) can block the calling
    fiber indefinitely. *)

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

type enqueue_result =
  | Enqueued of Pr_state.merge_queue_entry
  | Already_enqueued of Pr_state.merge_queue_entry

val parse_enqueue_response :
  string -> (Pr_state.merge_queue_entry, error) Result.t
(** Parse an [enqueuePullRequest] GraphQL response body. Pure helper exposed for
    regression tests. *)

val parse_dequeue_response : string -> (unit, error) Result.t
(** Parse a [dequeuePullRequest] GraphQL response body. Pure helper exposed for
    regression tests. *)

val parse_merge_queue_removal_response :
  string -> (Types.Ci_check.t list, error) Result.t
(** Parse a merge-queue removal-event GraphQL response body into the failing
    checks of the most recent removal's [beforeCommit] (the merge-group commit).
    [Ok []] when there is no removal event / no failing checks. Pure helper
    exposed for regression tests. *)

val parse_contexts_page :
  string -> (Types.Ci_check.t list * bool * string option, error) Result.t
(** Parse one page of the OID-keyed [statusCheckRollup.contexts] pagination
    query into [(checks, has_next_page, end_cursor)]. A missing object/rollup
    yields an empty page with [has_next_page = false]. Pure helper exposed for
    regression tests. *)

val parse_merge_queue_removal_pagination : string -> string option
(** [Some oid] when the latest merge-queue removal event's [beforeCommit] rollup
    is truncated ([hasNextPage]) and carries an OID — the signal that
    [parse_merge_queue_removal_response]'s [Ok []] is hiding further pages that
    the caller should paginate by OID. [None] otherwise. Pure helper exposed for
    regression tests. *)

val parse_merge_group_run_id :
  main_branch:Types.Branch.t ->
  pr_number:Types.Pr_number.t ->
  string ->
  (int option, error) Result.t
(** Parse a REST [GET /actions/runs] response and return the newest failed
    [merge_group] workflow run id for this PR's merge-queue branch, when one is
    present. Pure helper exposed for regression tests. *)

val parse_actions_jobs_response :
  string -> (Types.Ci_check.t list, error) Result.t
(** Parse a REST [GET /actions/runs/:id/jobs] response into failing job checks.
    Job database ids become [Ci_check.id], and [html_url] becomes [details_url].
    Pure helper exposed for regression tests. *)

val parse_check_annotations_response :
  string -> (Ci_log_digest.annotation list, error) Result.t
(** Parse a REST [GET /check-runs/:id/annotations] response into digest
    annotations. Extra fields and explicit [null] path/line values are
    tolerated. Pure helper exposed for regression tests. *)

val is_merge_queue_required_error : error -> bool
(** True only for the REST 405 response GitHub returns when the branch requires
    native merge queue use. *)

val make :
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  token:string ->
  owner:string ->
  repo:string ->
  main_branch:Types.Branch.t ->
  (module Forge.S with type error = error)
(** [make ~net ~clock ~token ~owner ~repo ~main_branch] packages a GitHub-backed
    forge module that closes over the network and time capabilities plus client
    configuration. *)

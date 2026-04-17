(** GitHub forge implementation.

    Queries the GitHub GraphQL API for PR/world state. Satisfies {!Forge.S}. *)

type error =
  | Http_error of { meth : string; path : string; status : int; body : string }
  | Json_parse_error of string
  | Graphql_error of string list
  | Transport_error of { meth : string; path : string; msg : string }

val show_error : error -> string
(** Render an error as a human-readable string. Includes the HTTP method and
    path for [Http_error]/[Transport_error], extracts GitHub's "message" field
    from the response body, and appends a hint about PAT scopes for 401/403/404
    so users can fix permission problems without guesswork. *)

val response_error_message_contains : string -> substring:string -> bool
(** Returns [true] if any [errors[].message] in a GitHub validation response
    body contains [substring] (case-insensitive). Use this to discriminate
    between distinct 422 cases (e.g. "pull request already exists" vs "no
    commits between"). Pure; safe on malformed input. *)

type t

val create : token:string -> owner:string -> repo:string -> t
(** [create ~token ~owner ~repo] creates a GitHub API client. *)

val parse_response_json :
  owner:string -> Yojson.Safe.t -> (Pr_state.t, error) Result.t
(** Parse a GitHub GraphQL response JSON value into a [Pr_state.t]. Pure
    function — no I/O or string parsing. [~owner] is the configured repository
    owner, used to detect fork PRs. *)

val parse_response : owner:string -> string -> (Pr_state.t, error) Result.t
(** Parse a GitHub GraphQL response body string into a [Pr_state.t]. *)

val pr_state :
  net:_ Eio.Net.t -> t -> Types.Pr_number.t -> (Pr_state.t, error) Result.t
(** [pr_state ~net client pr] fetches the current state of a pull request via
    the GitHub GraphQL API over HTTPS. *)

val parse_rest_pr_list :
  string -> ((Types.Pr_number.t * Types.Branch.t * bool) list, error) Result.t
(** Parse the REST response from [GET /repos/:owner/:repo/pulls]. Returns
    non-CLOSED PRs as [(pr_number, base_branch, merged)]. Pure function. *)

val list_prs :
  net:_ Eio.Net.t ->
  t ->
  branch:Types.Branch.t ->
  ?base:Types.Branch.t option ->
  state:[ `Open | `All ] ->
  unit ->
  ((Types.Pr_number.t * Types.Branch.t * bool) list, error) Result.t
(** [list_prs ~net t ~branch ~state ()] lists PRs matching [branch] via the
    GitHub REST API. Returns non-CLOSED PRs as [(number, base, merged)]. *)

val update_pr_body :
  net:_ Eio.Net.t ->
  t ->
  pr_number:Types.Pr_number.t ->
  body:string ->
  (unit, error) Result.t
(** [update_pr_body ~net t ~pr_number ~body] updates the PR description via
    [PATCH /repos/:owner/:repo/pulls/:number]. *)

val create_pull_request :
  net:_ Eio.Net.t ->
  t ->
  title:string ->
  head:Types.Branch.t ->
  base:Types.Branch.t ->
  body:string ->
  draft:bool ->
  (Types.Pr_number.t, error) Result.t
(** [create_pull_request ~net t ~title ~head ~base ~body ~draft] creates a pull
    request via [POST /repos/:owner/:repo/pulls] and returns the new PR number.
*)

val update_pr_base :
  net:_ Eio.Net.t ->
  t ->
  pr_number:Types.Pr_number.t ->
  base:Types.Branch.t ->
  (unit, error) Result.t
(** [update_pr_base ~net t ~pr_number ~base] retargets the PR to [base] via
    [PATCH /repos/:owner/:repo/pulls/:number]. *)

val set_draft :
  net:_ Eio.Net.t ->
  t ->
  pr_number:Types.Pr_number.t ->
  draft:bool ->
  (unit, error) Result.t
(** [set_draft ~net t ~pr_number ~draft] sets draft status via GraphQL mutation.
    REST API does not support changing the draft field. *)

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

val merge_pr :
  net:_ Eio.Net.t ->
  t ->
  pr_number:Types.Pr_number.t ->
  merge_method:[ `Merge | `Squash | `Rebase ] ->
  (merge_result, error) Result.t
(** [merge_pr ~net t ~pr_number ~merge_method] merges a PR via
    [PUT /repos/:owner/:repo/pulls/:number/merge]. A 2xx HTTP status does NOT
    imply the merge completed: the caller must inspect the returned
    [merge_result] to distinguish an actual merge from a queued request or an
    unconfirmed response. Transport errors and 4xx/5xx statuses return an
    [error]; the REST API returns 405 "Pull Request is not mergeable" when the
    PR is not in a mergeable state. *)

val owner : t -> string
(** [owner t] returns the repository owner. *)

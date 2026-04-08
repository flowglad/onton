(** GitHub forge implementation.

    Queries the GitHub GraphQL API for PR/world state. Satisfies {!Forge.S}. *)

type error =
  | Http_error of int * string
  | Json_parse_error of string
  | Graphql_error of string list
  | Transport_error of string
[@@deriving show, eq]

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

val set_draft :
  net:_ Eio.Net.t ->
  t ->
  pr_number:Types.Pr_number.t ->
  draft:bool ->
  (unit, error) Result.t
(** [set_draft ~net t ~pr_number ~draft] sets draft status via GraphQL mutation.
    REST API does not support changing the draft field. *)

val owner : t -> string
(** [owner t] returns the repository owner. *)

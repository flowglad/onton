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

val parse_response_json : Yojson.Safe.t -> (Pr_state.t, error) Result.t
(** Parse a GitHub GraphQL response JSON value into a [Pr_state.t]. Pure
    function — no I/O or string parsing. *)

val parse_response : string -> (Pr_state.t, error) Result.t
(** Parse a GitHub GraphQL response body string into a [Pr_state.t]. *)

val pr_state :
  net:_ Eio.Net.t -> t -> Types.Pr_number.t -> (Pr_state.t, error) Result.t
(** [pr_state ~net client pr] fetches the current state of a pull request via
    the GitHub GraphQL API over HTTPS. *)

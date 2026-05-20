(** Per-patch terminal outcome of a single GitHub poll attempt.

    The effectful poll driver translates [Github.error] and the success case
    into this typed variant before handing off to the pure
    [Poll_cycle.classify]. Modeling timeouts as a normal variant lets the
    planning layer reason about hangs as bounded events.

    See {!Poll_cycle} for the pure consumer and the per-patch isolation
    properties that this typing enables. *)
type t =
  | Ok_pr_state of Pr_state.t
  | Transport_failed of { msg : string }
  | Timed_out of { seconds : float }
  | Http_failed of { status : int; msg : string }
  | Graphql_failed of string list
  | Json_parse_failed of string
[@@deriving show, eq]

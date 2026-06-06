(* @archlint.module value
   @archlint.domain poll-outcome *)

open Base

(** Per-patch terminal outcome of a single GitHub poll attempt.

    The effectful poll driver translates [Github.error] and the success case
    into this typed variant before crossing into the pure planning layer. That
    boundary lets [Poll_cycle.classify] be tested with property interleavings
    without spinning up a real HTTP client.

    The [Timed_out] variant exists specifically to model the failure mode that
    wedged the production poller (TCP [SYN_SENT] never returning): the effectful
    driver must convert hangs to [Timed_out] within a bounded interval, so the
    pure layer never has to reason about infinite waits. *)
type t =
  | Ok_pr_state of Pr_state.t
      (** The GitHub call returned a parsed [Pr_state.t]. Downstream
          classification may still treat this as a no-op (fork, closed). *)
  | Transport_failed of { msg : string }
      (** Network-layer failure (TLS, DNS, connection refused, etc.). *)
  | Timed_out of { seconds : float }
      (** The effectful driver gave up after [seconds]. The TCP connect may
          still be in [SYN_SENT] under the hood — what matters is that the pure
          layer sees a value. *)
  | Http_failed of { status : int; msg : string }
      (** GitHub returned a non-2xx status. *)
  | Graphql_failed of string list
      (** GraphQL response carried [errors[]] messages. *)
  | Json_parse_failed of string
      (** Response body did not parse as the expected JSON shape. *)
[@@deriving show, eq]

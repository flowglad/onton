(* @archlint.module core
   @archlint.domain forge-observation *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type request = { id : string; pr_number : Types.Pr_number.t }
[@@deriving show, eq, compare, sexp_of, yojson]

type t = { request : request; outcome : Poll_outcome.t } [@@deriving show, eq]

let validate ~expected observation =
  if
    String.is_empty expected.id
    || Types.Pr_number.to_int expected.pr_number <= 0
  then Error "invalid_forge_request_identity"
  else if not (equal_request expected observation.request) then
    Error "stale_forge_request"
  else
    match observation.outcome with
    | Poll_outcome.Ok_pr_state state -> (
        if
          not
            (Option.equal Types.Pr_number.equal state.Pr_state.pr_number
               (Some expected.pr_number))
        then Error "unidentified_or_mismatched_forge_pr"
        else
          match state.status with
          | Pr_state.Merged | Closed -> Ok observation.outcome
          | Open ->
              let valid_oid =
                Option.value_map ~default:false ~f:(fun oid ->
                    Git_oid.valid oid)
              in
              if
                valid_oid state.head_oid && valid_oid state.base_oid
                && Option.value_map state.base_branch ~default:false
                     ~f:(fun branch ->
                       not (String.is_empty (Types.Branch.to_string branch)))
              then Ok observation.outcome
              else Error "unidentified_forge_revision_pair")
    | Transport_failed _ | Timed_out _ | Http_failed _ | Graphql_failed _
    | Json_parse_failed _ ->
        Ok observation.outcome

type scope = {
  pr_number : Types.Pr_number.t option;
  branch : string;
  generation : int;
  head : string option;
  base_branch : string option;
}
[@@deriving eq, compare, sexp_of, yojson]

type ticket = { sequence : int; request : request; scope : scope }
[@@deriving eq, compare, sexp_of, yojson]

type fact = {
  ticket : ticket;
  head : string;
  base : string;
  base_branch : string;
  merge_state : Pr_state.merge_state;
  confirmed_head : string option;
  confirmed_base : string option; [@yojson.default None]
}
[@@deriving eq, compare, sexp_of, yojson]

type history = {
  next_sequence : int;
  pending : ticket option;
  latest : fact option;
  conflict : fact option;
}
[@@deriving eq, compare, sexp_of, yojson]

let empty_history =
  { next_sequence = 1; pending = None; latest = None; conflict = None }

let pending_request history = history.pending
let latest_fact history = history.latest
let conflict_fact history = history.conflict

let valid_scope scope =
  scope.generation >= 0
  && (not (String.is_empty scope.branch))
  && Option.value_map scope.pr_number ~default:false ~f:(fun pr ->
      Types.Pr_number.to_int pr > 0)

let valid_ticket ticket =
  ticket.sequence > 0
  && (not (String.is_empty ticket.request.id))
  && valid_scope ticket.scope
  && Option.equal Types.Pr_number.equal ticket.scope.pr_number
       (Some ticket.request.pr_number)

let history_valid history =
  let valid_fact fact =
    valid_ticket fact.ticket
    && fact.ticket.sequence < history.next_sequence
    && Git_oid.valid fact.head && Git_oid.valid fact.base
    && (not (String.is_empty fact.base_branch))
    && Option.for_all fact.confirmed_head ~f:(String.equal fact.head)
    && Option.for_all fact.confirmed_base ~f:(String.equal fact.base)
  in
  history.next_sequence > 0
  && Option.for_all history.pending ~f:(fun ticket ->
      valid_ticket ticket
      && ticket.sequence < history.next_sequence
      && Option.for_all history.latest ~f:(fun fact ->
          fact.ticket.sequence < ticket.sequence))
  && Option.for_all history.latest ~f:valid_fact
  && Option.for_all history.conflict ~f:(fun fact ->
      valid_fact fact
      && Pr_state.equal_merge_state fact.merge_state Pr_state.Conflicting
      && Option.exists history.latest ~f:(fun latest ->
          fact.ticket.sequence <= latest.ticket.sequence))

let begin_request history ~request ~scope =
  if
    (not (history_valid history))
    || history.next_sequence = Int.max_value
    || (not (valid_scope scope))
    || String.is_empty request.id
    || not
         (Option.equal Types.Pr_number.equal scope.pr_number
            (Some request.pr_number))
  then (history, None)
  else
    let ticket = { sequence = history.next_sequence; request; scope } in
    ( {
        history with
        next_sequence = history.next_sequence + 1;
        pending = Some ticket;
      },
      Some ticket )

let accept ?confirmed_base history ~ticket ~current ~confirmed_head observation
    =
  if not (history_valid history) then
    (history, Error "invalid_forge_observation_history")
  else if not (Option.equal equal_ticket history.pending (Some ticket)) then
    (history, Error "stale_forge_request_ticket")
  else if not (equal_scope ticket.scope current) then
    (history, Error "stale_forge_local_context")
  else
    match validate ~expected:ticket.request observation with
    | Error reason -> ({ history with pending = None }, Error reason)
    | Ok outcome -> (
        match outcome with
        | Poll_outcome.Ok_pr_state state -> (
            if Pr_state.is_fork state then
              ({ history with pending = None }, Error "fork_forge_observation")
            else if
              Option.exists state.Pr_state.head_branch ~f:(fun branch ->
                  not
                    (String.equal
                       (Types.Branch.to_string branch)
                       current.branch))
            then
              ( { history with pending = None },
                Error "mismatched_forge_head_branch" )
            else
              match
                (state.status, state.head_oid, state.base_oid, state.base_branch)
              with
              | Pr_state.Open, Some head, Some base, Some base_branch ->
                  let fact =
                    {
                      ticket;
                      head;
                      base;
                      base_branch = Types.Branch.to_string base_branch;
                      merge_state = state.merge_state;
                      confirmed_head =
                        Option.filter confirmed_head ~f:(String.equal head);
                      confirmed_base =
                        Option.filter confirmed_base ~f:(String.equal base);
                    }
                  in
                  ( {
                      history with
                      pending = None;
                      latest = Some fact;
                      conflict =
                        (if
                           Pr_state.equal_merge_state state.merge_state
                             Pr_state.Conflicting
                         then Some fact
                         else history.conflict);
                    },
                    Ok outcome )
              | (Pr_state.Merged | Pr_state.Closed), _, _, _ ->
                  ({ history with pending = None }, Ok outcome)
              | Pr_state.Open, _, _, _ ->
                  ( { history with pending = None },
                    Error "unidentified_forge_revision_pair" ))
        | Poll_outcome.Transport_failed _ | Timed_out _ | Http_failed _
        | Graphql_failed _ | Json_parse_failed _ ->
            ({ history with pending = None }, Ok outcome))

let fact_matches_scope (fact : fact) (scope : scope) =
  Option.equal Types.Pr_number.equal scope.pr_number
    (Some fact.ticket.request.pr_number)
  && String.equal scope.branch fact.ticket.scope.branch
  && Option.equal String.equal scope.head (Some fact.head)
  && Option.equal String.equal scope.base_branch (Some fact.base_branch)

let decode_history json =
  try
    let history = history_of_yojson json in
    if history_valid history then Ok history
    else Error "invalid_forge_observation_history"
  with _ -> Error "malformed_forge_observation_history"

let conflict_for_scope history scope =
  Option.filter history.conflict ~f:(fun conflict ->
      fact_matches_scope conflict scope
      && Option.exists history.latest ~f:(fun latest ->
          fact_matches_scope latest scope
          && String.equal latest.base conflict.base))

let revisions_confirmed (fact : fact) =
  Option.equal String.equal fact.confirmed_head (Some fact.head)
  && Option.equal String.equal fact.confirmed_base (Some fact.base)

let can_apply history outcome =
  match outcome with
  | Poll_outcome.Ok_pr_state state -> (
      match state.Pr_state.status with
      | Pr_state.Merged | Closed -> true
      | Open ->
          Option.exists history.latest ~f:(fun fact ->
              revisions_confirmed fact
              && Option.equal Types.Pr_number.equal state.pr_number
                   (Some fact.ticket.request.pr_number)
              && Option.equal String.equal state.head_oid (Some fact.head)
              && Option.equal String.equal state.base_oid (Some fact.base)
              && Option.equal String.equal
                   (Option.map state.base_branch ~f:Types.Branch.to_string)
                   (Some fact.base_branch)))
  | Transport_failed _ | Timed_out _ | Http_failed _ | Graphql_failed _
  | Json_parse_failed _ ->
      false

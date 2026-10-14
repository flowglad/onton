(* @archlint.module core
   @archlint.domain merge-queue-decision *)

open Base
open Types
open Ppx_yojson_conv_lib.Yojson_conv

type pending =
  | Awaiting_removal of {
      entry : Pr_state.merge_queue_entry;
      head_oid : string option;
    }
  | Failed of { head_oid : string option; checks : Ci_check.t list }
[@@deriving show, eq, sexp_of, compare, yojson]

type application = {
  poll_result : Poller.t;
  merge_queue_ejected : bool;
  pending : pending option;
}
[@@deriving show, eq]

let queue_entry_disappeared ~previous_entry (poll : Poller.t) =
  Option.is_some previous_entry
  && Option.is_none poll.merge_queue_entry
  && (not poll.merged) && not poll.closed

let should_fetch ~pending ~previous_entry poll =
  (not poll.Poller.merged) && (not poll.closed)
  &&
  match pending with
  | Some (Awaiting_removal _) -> true
  | Some (Failed _) -> false
  | None -> queue_entry_disappeared ~previous_entry poll

let failed_checks = function
  | Some (Failed { checks; _ }) -> checks
  | None | Some (Awaiting_removal _) -> []

let repair_completed = function
  | Some (Failed _) -> None
  | (None | Some (Awaiting_removal _)) as pending -> pending

let apply ?(pending = None) ?(previous_head_oid = None) ?removal_checks
    ~previous_entry (poll_result : Poller.t) =
  let pending =
    match (pending, previous_entry) with
    | None, Some entry when queue_entry_disappeared ~previous_entry poll_result
      ->
        Some
          (Awaiting_removal
             {
               entry;
               head_oid =
                 Option.first_some previous_head_oid poll_result.head_oid;
             })
    | _ -> pending
  in
  let pending =
    if poll_result.merged || poll_result.closed then None
    else
      match pending with
      | Some (Awaiting_removal { head_oid; _ }) -> (
          match removal_checks with
          | Some [] -> None
          | Some checks -> Some (Failed { head_oid; checks })
          | None -> pending)
      | None | Some (Failed _) -> pending
  in
  let pending =
    match (pending, poll_result.head_oid) with
    | Some (Failed { head_oid = Some old; _ }), Some current
      when not (String.equal old current) ->
        None
    | None, _ | Some (Awaiting_removal _), _ | Some (Failed _), _ -> pending
  in
  match pending with
  | Some (Failed { checks; _ }) ->
      let marker = Ci_check.merge_queue_failure () in
      let ci_checks =
        List.dedup_and_sort
          ((marker :: checks) @ poll_result.ci_checks)
          ~compare:Ci_check.compare
      in
      {
        pending;
        merge_queue_ejected = true;
        poll_result =
          {
            poll_result with
            queue =
              (if
                 List.mem poll_result.queue Operation_kind.Ci
                   ~equal:Operation_kind.equal
               then poll_result.queue
               else Operation_kind.Ci :: poll_result.queue);
            merge_ready = false;
            checks_passing = false;
            ci_checks;
          };
      }
  | None | Some (Awaiting_removal _) ->
      { pending; poll_result; merge_queue_ejected = false }

(* An empty failure list is definitive only when a removal reason is available
   and does not report failed checks. Missing timeline evidence can lag the PR
   poll; a failed-check reason without its checks must also be retried. *)
let removal_processed ~body ~checks =
  match Json.of_string body with
  | Error _ -> false
  | Ok json -> (
      let nodes =
        Json.field "data" json
        |> Option.bind ~f:(Json.field "repository")
        |> Option.bind ~f:(Json.field "pullRequest")
        |> Option.bind ~f:(Json.field "timelineItems")
        |> Option.bind ~f:(Json.field "nodes")
        |> Option.bind ~f:Json.list
      in
      match Option.bind nodes ~f:List.hd with
      | None -> false
      | Some event -> (
          match Json.string_field "reason" event with
          | None -> not (List.is_empty checks)
          | Some reason ->
              let reason = String.lowercase reason in
              let failed_checks =
                String.is_substring reason ~substring:"fail"
              in
              (not (String.is_empty reason))
              && ((not failed_checks) || not (List.is_empty checks))))

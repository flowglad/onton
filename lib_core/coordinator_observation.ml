(* @archlint.module core
   @archlint.domain coordinator-forge *)
open Base
open Types
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

[@@@warning "-4"]

type scope = {
  ticket : string;
  pr : Pr_number.t;
  head_branch : Branch.t;
  base_branch : Branch.t;
  head_revision : string;
  base_revision : string;
  push_destination : string;
  generation : int;
}
[@@deriving yojson]

type t = {
  scope : scope;
  state : Pr_state.t option;
  diagnostic : string;
  removal_checks : Types.Ci_check.t list option;
  observed_at : float;
}
[@@deriving yojson]

let valid t =
  let s = t.scope in
  (not (String.is_empty s.ticket || String.is_empty s.push_destination))
  && String.length s.ticket <= 200
  && s.generation >= 0
  && String.length t.diagnostic <= 4096
  && Float.is_finite t.observed_at
  && Float.(t.observed_at >= 0.)
  &&
  match t.state with
  | None -> true
  | Some pr ->
      Pr_number.to_int s.pr > 0
      && Option.equal Pr_number.equal pr.pr_number (Some s.pr)
      && (Pr_state.merged pr || Pr_state.closed pr
         || Git_oid.valid s.head_revision
            && Git_oid.valid s.base_revision
            && Option.equal String.equal pr.head_oid (Some s.head_revision)
            && Option.equal String.equal pr.base_oid (Some s.base_revision)
            && Option.equal Branch.equal pr.head_branch (Some s.head_branch)
            && Option.equal Branch.equal pr.base_branch (Some s.base_branch))

let encode t = Yojson.Safe.to_string (yojson_of_t t)

let make ?removal_checks ?(observed_at = 0.) scope state ~diagnostic =
  let state =
    Option.map state ~f:(fun pr ->
        { pr with Pr_state.merge_ready_divergence = None })
  in
  let t = { scope; state; diagnostic; removal_checks; observed_at } in
  if valid t && String.length (encode t) <= 262144 then Ok t
  else Error "invalid observation"

let decode bytes =
  if String.length bytes > 262144 then Error "observation too large"
  else
    try
      let t = t_of_yojson (Yojson.Safe.from_string bytes) in
      if valid t then Ok t else Error "invalid observation"
    with _ -> Error "malformed observation"

let apply agent t =
  let observe agent =
    Patch_agent.observe_scheduler agent ~now:t.observed_at ~pr_base_update:false
      ~pr_draft_update:false
  in
  let defer agent = observe (Patch_agent.defer_forge_revision_pair agent) in
  let s = t.scope in
  let matches =
    Branch.equal agent.Patch_agent.branch s.head_branch
    && agent.generation = s.generation
    && (Option.is_none (Patch_agent.pr_number agent)
       || Option.equal Pr_number.equal (Patch_agent.pr_number agent) (Some s.pr)
       )
    && Option.equal Branch.equal agent.base_branch (Some s.base_branch)
    && Option.value_map
         (Patch_agent.expected_remote_head_oid agent)
         ~default:
           (Option.value_map agent.head_oid ~default:true
              ~f:(String.equal s.head_revision))
         ~f:(String.equal s.head_revision)
  in
  if not matches then defer agent
  else
    match t.state with
    | None -> defer agent
    | Some pr when pr.is_fork -> defer agent
    | Some pr -> (
        let agent =
          if Option.is_none (Patch_agent.pr_number agent) then
            Patch_agent.set_pr_number agent s.pr
          else agent
        in
        let request : Forge_observation.request =
          { id = s.ticket; pr_number = s.pr }
        in
        let agent, ticket =
          Patch_agent.begin_forge_observation agent ~request
        in
        match ticket with
        | None -> defer agent
        | Some ticket -> (
            let agent, accepted =
              Patch_agent.accept_forge_observation
                ~confirmed_base:s.base_revision agent ~ticket
                ~confirmed_head:(Some s.head_revision)
                {
                  Forge_observation.request;
                  outcome = Poll_outcome.Ok_pr_state pr;
                }
            in
            match accepted with
            | Error _ -> defer agent
            | Ok _ ->
                let poll = Poller.poll ~was_merged:agent.merged pr in
                let queue =
                  Merge_queue_decision.apply ~pending:agent.merge_queue_ejection
                    ~previous_head_oid:agent.head_oid
                    ?removal_checks:t.removal_checks
                    ~previous_entry:agent.merge_queue_entry poll
                in
                let poll = queue.poll_result in
                let agent =
                  Patch_agent.set_merge_queue_ejection agent queue.pending
                in
                let agent =
                  agent |> fun a ->
                  Patch_agent.set_ci_checks a poll.ci_checks |> fun a ->
                  Patch_agent.set_merge_ready a poll.merge_ready |> fun a ->
                  Patch_agent.set_head_oid a poll.head_oid |> fun a ->
                  Patch_agent.observe_publication_head
                    ~confirmed_remote_head:s.head_revision a poll.head_oid
                  |> fun a ->
                  Patch_agent.set_review_decision a poll.review_decision
                  |> fun a ->
                  Patch_agent.set_unresolved_comment_count a
                    poll.unresolved_comment_count
                  |> fun a ->
                  Patch_agent.set_mergeability_unknown a
                    (Poller.mergeability_unknown poll)
                  |> fun a ->
                  Automerge_state.observe_merge_queue a
                    ~required:poll.merge_queue_required
                    ~entry:poll.merge_queue_entry
                  |> fun a ->
                  Patch_agent.set_native_stack a pr.native_stack |> fun a ->
                  Patch_agent.set_is_draft a poll.is_draft |> fun a ->
                  Patch_agent.set_checks_passing a poll.checks_passing
                  |> fun a ->
                  Patch_agent.set_merge_commit_sha a poll.merge_commit_sha
                in
                let agent =
                  if poll.checks_passing then
                    Patch_agent.reset_ci_failure_count agent
                  else agent
                in
                let agent =
                  List.fold poll.queue ~init:agent ~f:(fun a kind ->
                      match kind with
                      | Operation_kind.Ci -> (
                          match Patch_decision.on_ci_failure a with
                          | Enqueue_ci -> Patch_agent.enqueue a kind
                          | _ -> a)
                      | Merge_conflict when not (Patch_agent.has_conflict a) ->
                          a
                      | _ -> Patch_agent.enqueue a kind)
                in
                observe
                  (if poll.merged then Patch_agent.mark_merged agent else agent)
            ))

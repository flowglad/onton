(* @archlint.module core
   @archlint.domain session-result *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t =
  | Session_ok
  | Session_process_error of { is_fresh : bool; detail : string option }
  | Session_no_resume
  | Session_timed_out of { session_id : string option; detail : string option }
      (** Deadline interruption: preserve the session and retry without
          consuming the fresh/resume failure budget. [session_id] is the ID
          captured by this attempt, or its resumed ID; [None] clears any stale
          ID from a previous failed attempt. *)
  | Session_failed of { is_fresh : bool; detail : string option }
  | Session_wontdo of string
      (** Explicit pre-commit opt-out. Retains the explanation and pauses work
          until a human reprompt or intervention reset. *)
  | Session_give_up
  | Session_worktree_missing
  | Session_push_failed of Push_reject_classify.rejection option
      (** [Some r] carries a classified server-side rejection (workflow-scope,
          branch-protection, lease, hook, …). [None] reflects a transport/local
          [git push] error (no server message available). *)
  | Session_no_commits
  | Session_context_exhausted
      (** The session exhausted the model's context window
          ([Run_classification.Context_exhausted]). Clears [llm_session_id] so
          the next session starts fresh (resuming the overflowed thread would
          re-overflow) and bumps [context_exhaustion_count]; at [>= 2] the agent
          surfaces for intervention. *)
[@@deriving show, eq, sexp_of, compare, yojson]

type delivery_mode = Start | Respond
[@@deriving show, eq, sexp_of, compare, yojson]

type completion = {
  session_uuid : string;
  delivery_mode : delivery_mode;
  kind : Types.Operation_kind.t option;
  message_id : Types.Message_id.t option;
  result : t;
  head : string option;
  guidance : string list;
  turn_accepted : bool;
}
[@@deriving eq, sexp_of, compare, yojson]

let decode_completion json =
  try Ok (completion_of_yojson json)
  with _ -> Error "invalid session completion"

let publication_intent (completion : completion) ~base ~policy =
  Branch_reconcile.
    {
      base;
      policy;
      purpose =
        (match Option.bind completion.head ~f:Commit.make with
        | Some revision -> Publish_revision revision
        | None -> Publish_session completion.session_uuid);
    }

let resume_start ~delivery_mode ~guidance ~publication completion =
  equal_delivery_mode delivery_mode Start
  && equal_delivery_mode completion.delivery_mode Start
  && equal completion.result Session_ok
  && List.equal String.equal guidance completion.guidance
  && (List.is_empty guidance || completion.turn_accepted)
  && not
       (match Branch_reconcile.operation publication with
       | Some op ->
           Branch_reconcile.equal_phase op.phase Settled
           && Option.is_none op.candidate
           && Branch_reconcile.equal_purpose op.intent.purpose
                (publication_intent completion ~base:op.intent.base
                   ~policy:op.intent.policy)
                  .purpose
       | None -> false)

let after_local_work ~delivery_mode ~branch_changed ~no_work session =
  match session with
  | Session_ok
    when no_work
         || (equal_delivery_mode delivery_mode Respond && not branch_changed) ->
      Session_no_commits
  | Session_ok | Session_process_error _ | Session_no_resume
  | Session_timed_out _ | Session_failed _ | Session_wontdo _ | Session_give_up
  | Session_worktree_missing | Session_push_failed _ | Session_no_commits
  | Session_context_exhausted ->
      session

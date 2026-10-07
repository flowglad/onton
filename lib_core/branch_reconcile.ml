(* @archlint.module core
   @archlint.domain branch-reconcile *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Commit = struct
  type t = string [@@deriving eq, compare, sexp_of, yojson]

  let make s =
    if
      (String.length s = 40 || String.length s = 64)
      && String.for_all s ~f:(function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
    then Some s
    else None

  let to_string t = t
end

module Remote_id = struct
  type t = string [@@deriving eq, compare, sexp_of, yojson]

  let of_destination destination =
    Stdlib.Digest.BLAKE256.(to_hex (string destination))

  let valid value =
    String.length value = 64 && Option.is_some (Commit.make value)
end

type remote_binding =
  | Unobserved_remote
  | Bound_remote of Remote_id.t
  | Reconfirm_remote of Remote_id.t option
[@@deriving eq, compare, sexp_of, yojson]

type materialization = New_branch of Commit.t | Adopted_branch of Commit.t
[@@deriving eq, compare, sexp_of, yojson]

let materialization_head = function New_branch sha | Adopted_branch sha -> sha

let materialization_boundary = function
  | New_branch sha -> Some sha
  | Adopted_branch _ -> None

type policy = Rewrite | Preserve_ancestry
[@@deriving eq, compare, sexp_of, yojson]

type purpose =
  | Reconcile_base
  | Reconcile_request of string
  | Integrate_revision of { contributor : string; revision : Commit.t }
  | Publish_revision of Commit.t
  | Publish_session of string
[@@deriving eq, compare, sexp_of, yojson]

type intent = { base : string; policy : policy; purpose : purpose }
[@@deriving eq, compare, sexp_of, yojson]

type boundary = Recorded of Commit.t | Inferred of Commit.t | Plain
[@@deriving eq, compare, sexp_of, yojson]

type token = { operation : int; command : int }
[@@deriving eq, compare, sexp_of, yojson]

type topology = Equal | Includes | Behind | Diverged | Unproven
[@@deriving eq, compare, sexp_of, yojson]

type observation = {
  destination : Remote_id.t;
  head : Commit.t;
  source : Commit.t;
  target : Commit.t;
  remote : Commit.t option;
  boundary : boundary;
  topology : topology;
  clean : bool;
  sequencer : string option;
  conflicts : int;
  target_included : bool;
  completed_integration : bool;
}
[@@deriving eq, compare, sexp_of, yojson]

type remote_replay = {
  preserved : Commit.t;
  incoming : Commit.t;
  upstream : Commit.t;
}
[@@deriving eq, compare, sexp_of, yojson]

type remote_replay_request = {
  preserved : Commit.t;
  incoming : Commit.t;
  boundaries : boundary list;
}
[@@deriving eq, compare, sexp_of, yojson]

type merge_completion = {
  head : Commit.t;
  target : Commit.t;
  parents : Commit.t list;
  sequencer : string;
  resumed_sequencer : string;
}
[@@deriving eq, compare, sexp_of, yojson]

type merge_progress =
  | Pending_merge of merge_completion
  | Completed_merge of { capture : merge_completion; head : Commit.t }
[@@deriving eq, compare, sexp_of, yojson]

type command_kind =
  | Observe
  | Commit_merge of merge_completion
  | Plan_remote_replay of remote_replay_request
  | Checkout_remote of remote_replay
  | Pin of { source : Commit.t; target : Commit.t }
  | Integrate of {
      source : Commit.t;
      target : Commit.t;
      boundary : boundary;
      policy : policy;
    }
  | Inspect
  | Verify_recovery
  | Continue of { head : Commit.t; target : Commit.t; sequencer : string }
  | Publish of { candidate : Commit.t; expected : Commit.t option }
  | Confirm of Commit.t
[@@deriving eq, compare, sexp_of, yojson]

type command = { token : token; kind : command_kind }
[@@deriving eq, compare, sexp_of, yojson]

type repair_mode =
  | Content_repair
  | History_recovery of { reason : string; baseline : Commit.t option }
[@@deriving eq, compare, sexp_of, yojson]

type repair = {
  mode : repair_mode;
  head : Commit.t;
  sequencer : string;
  conflicts : int;
  attempts_without_progress : int;
  attempt_completed : bool;
  attempt_started : bool;
}
[@@deriving eq, compare, sexp_of, yojson]

type phase =
  | Preparing
  | Integrating
  | Repairing of repair
  | Publishing
  | Confirming
  | Waiting of { until : float; reason : string }
  | Recovering
  | Settled
  | Intervention of string
[@@deriving eq, compare, sexp_of, yojson]

type local_action =
  | Publish_source
  | Integrate_source of policy
  | Prepare_remote_replay of remote_replay_request
[@@deriving eq, compare, sexp_of, yojson]

type integration_capture = {
  base_branch : string option;
  source_revision : Commit.t;
  target_revision : Commit.t;
  replay_boundary : boundary;
  integration_policy : policy;
}
[@@deriving eq, compare, sexp_of, yojson]

type integration_evidence = Executed | Recovered_completion | Verified_history
[@@deriving eq, compare, sexp_of, yojson]

type integration_receipt = {
  operation_id : int;
  capture : integration_capture;
  integrated_revision : Commit.t;
  evidence : integration_evidence;
}
[@@deriving eq, compare, sexp_of, yojson]

type publication_receipt = {
  operation_id : int;
  published_intent : intent;
  published_revision : Commit.t;
}
[@@deriving eq, compare, sexp_of, yojson]

type operation = {
  id : int;
  intent : intent;
  local_action : local_action;
  integration : integration_capture option;
  reobserve_after_completion : bool;
  destination : remote_binding; [@yojson.default Unobserved_remote]
  source : Commit.t option;
  target : Commit.t option;
  boundary : boundary;
  boundary_candidates : boundary list;
  remote_replay : remote_replay option;
  candidate : Commit.t option;
  expected : Commit.t option;
  recovery_revisions : Commit.t list; [@yojson.default []]
  phase : phase;
  pending : command option;
  command_sequence : int;
  failures : int;
  repair : repair option;
  merge_progress : merge_progress option;
}
[@@deriving eq, compare, sexp_of, yojson]

type t = {
  desired : intent option;
  materialized : materialization option;
  next_operation : int;
  active : operation option;
  integrations : integration_receipt list;
  remote_integrations : integration_receipt list;
  publications : publication_receipt list;
}
[@@deriving eq, compare, sexp_of, yojson]

type result =
  | Observed of observation
  | Observed_active of { observation : observation; policy : policy }
  | Pinned
  | Merge_completion_needed of merge_completion
  | Merge_completed of Commit.t
  | Remote_replay_selected of boundary
  | Remote_checked_out
  | Integrated of Commit.t
  | Conflict of { head : Commit.t; sequencer : string; conflicts : int }
  | Recovery_verified of {
      observation : observation;
      source_preserved : bool;
      remote_preserved : bool;
    }
  | Inspected of observation
  | Inspected_active of { observation : observation; ready : bool }
  | Published
  | Remote of { sha : Commit.t option; topology : topology }
  | Retryable of { reason : string; retry_after : float option }
  | Recovery_required of string
  | Permanent of string
[@@deriving eq, compare, sexp_of]

type event =
  | Materialized of materialization
  | Request of intent
  | Result of { token : token; at : float; result : result }
  | Repair_invalidated of { token : token; reason : string }
  | Repair_denied of { token : token; reason : string }
  | Repair_started of token
  | Repair_interrupted of { token : token; at : float; reason : string }
  | Repair_completed of { token : token; at : float }
  | Tick of float
  | Recover
  | Resume
  | Refresh of observation
[@@deriving eq, compare, sexp_of]

type effect_command =
  | Execute of command
  | Repair of token
  | Start_repair of token
  | Completed of int
[@@deriving eq, compare, sexp_of]

let empty =
  {
    desired = None;
    materialized = None;
    next_operation = 1;
    active = None;
    integrations = [];
    remote_integrations = [];
    publications = [];
  }

let operation t = t.active

let execution_policy op =
  match op.local_action with
  | Integrate_source policy -> policy
  | Publish_source | Prepare_remote_replay _ -> op.intent.policy

let materialization t = t.materialized
let publications t = t.publications
let integrations t = t.integrations
let remote_integrations t = t.remote_integrations
let pending t = Option.bind t.active ~f:(fun op -> op.pending)
let phase t = Option.map t.active ~f:(fun op -> op.phase)

let conflict_satisfied t ~head ~base ~base_branch =
  Option.value_map t.active ~default:false ~f:(fun op ->
      equal_phase op.phase Settled
      && Option.equal Commit.equal op.candidate (Some head))
  && List.exists t.integrations ~f:(fun receipt ->
      Commit.equal receipt.integrated_revision head
      && Commit.equal receipt.capture.target_revision base
      && Option.equal String.equal receipt.capture.base_branch
           (Some base_branch))

let completion_proven ~policy ~source_included ~reflog_receipt =
  reflog_receipt
  && match policy with Rewrite -> true | Preserve_ancestry -> source_included

let rewrite_publication_input (op : operation) ~candidate ~expected =
  if
    not
      (equal_phase op.phase Publishing
      && Option.equal Commit.equal op.candidate (Some candidate)
      && Option.equal Commit.equal op.expected (Some expected))
  then None
  else
    match (op.local_action, op.boundary) with
    | Integrate_source Rewrite, Recorded upstream ->
        Option.map op.source ~f:(fun source -> (source, upstream))
    | ( ( Publish_source | Prepare_remote_replay _
        | Integrate_source Preserve_ancestry ),
        _ )
    | Integrate_source Rewrite, (Inferred _ | Plain) ->
        None

let publication_destination urls =
  match List.dedup_and_sort urls ~compare:String.compare with
  | [ url ] when not (String.is_empty (String.strip url)) -> Ok url
  | [] | [ _ ] -> Error "missing_push_destination"
  | _ :: _ :: _ -> Error "multiple_push_destinations"

let check_destination (op : operation) ~observed =
  match op.destination with
  | Bound_remote expected when Remote_id.equal expected observed -> Ok ()
  | Bound_remote _ -> Error "publication_destination_changed"
  | Unobserved_remote when Option.is_none op.source -> Ok ()
  | Unobserved_remote -> Error "publication_destination_unrecorded"
  | Reconfirm_remote _ ->
      if
        equal_phase op.phase Recovering
        && Option.value_map op.pending ~default:false ~f:(fun command ->
            match command.kind with
            | Inspect | Verify_recovery -> true
            | Observe | Pin _ | Integrate _ | Continue _ | Commit_merge _
            | Plan_remote_replay _ | Checkout_remote _ | Publish _ | Confirm _
              ->
                false)
      then Ok ()
      else Error "publication_destination_requires_inspection"

let is_unsettled t =
  match phase t with
  | None | Some Settled -> false
  | Some
      ( Preparing | Integrating | Repairing _ | Publishing | Confirming
      | Waiting _ | Recovering | Intervention _ ) ->
      true

let is_pending t =
  Option.value_map t.active ~default:false ~f:(fun op ->
      match op.phase with
      | Settled | Intervention _ -> false
      | Preparing | Integrating | Repairing _ | Publishing | Confirming
      | Waiting _ | Recovering ->
          true)

let issue t op phase kind =
  let command_sequence = op.command_sequence + 1 in
  let command =
    { token = { operation = op.id; command = command_sequence }; kind }
  in
  ( {
      t with
      active = Some { op with phase; command_sequence; pending = Some command };
    },
    [ Execute command ] )

type boundary_choice = Chosen of boundary | Probe of Commit.t
[@@deriving eq, compare, sexp_of]

let rec choose_boundary ~candidates ~evidence =
  match candidates with
  | [] | Plain :: _ -> Chosen Plain
  | ((Recorded sha | Inferred sha) as boundary) :: rest -> (
      match List.Assoc.find evidence sha ~equal:Commit.equal with
      | Some true -> Chosen boundary
      | Some false -> choose_boundary ~candidates:rest ~evidence
      | None -> Probe sha)

let observation_boundaries (op : operation) =
  if Option.is_none op.source then op.boundary_candidates else [ op.boundary ]

let recorded_boundaries t =
  let revisions =
    List.map t.integrations ~f:(fun receipt -> receipt.capture.target_revision)
    @ Option.to_list (Option.bind t.materialized ~f:materialization_boundary)
  in
  let _, reversed =
    List.fold revisions
      ~init:(Set.empty (module String), [])
      ~f:(fun (seen, boundaries) sha ->
        let key = Commit.to_string sha in
        if Set.mem seen key then (seen, boundaries)
        else (Set.add seen key, Recorded sha :: boundaries))
  in
  List.rev reversed

let start t intent =
  let boundary_candidates = recorded_boundaries t in
  let op =
    {
      id = t.next_operation;
      intent;
      local_action =
        (match intent.purpose with
        | Integrate_revision _ -> Integrate_source Preserve_ancestry
        | Reconcile_base | Reconcile_request _ -> Integrate_source intent.policy
        | Publish_revision _ | Publish_session _ -> Publish_source);
      integration = None;
      reobserve_after_completion = false;
      destination = Unobserved_remote;
      source = None;
      target = None;
      boundary = Option.value (List.hd boundary_candidates) ~default:Plain;
      boundary_candidates;
      remote_replay = None;
      candidate = None;
      expected = None;
      recovery_revisions = [];
      phase = Preparing;
      pending = None;
      command_sequence = 0;
      failures = 0;
      repair = None;
      merge_progress = None;
    }
  in
  issue { t with next_operation = t.next_operation + 1 } op Preparing Observe

let stop t op reason =
  ( {
      t with
      active = Some { op with phase = Intervention reason; pending = None };
    },
    [] )

let recover_with_agent t (op : operation) reason =
  match (op.source, op.target, op.repair) with
  | Some source, Some _, (None | Some { mode = Content_repair; _ }) ->
      let repair =
        {
          mode = History_recovery { reason; baseline = None };
          head = source;
          sequencer = "history-recovery";
          conflicts = 0;
          attempts_without_progress = 0;
          attempt_completed = false;
          attempt_started = false;
        }
      in
      issue t { op with repair = Some repair } Recovering Verify_recovery
  | _, _, Some { mode = History_recovery _; _ } | None, _, _ | _, None, _ ->
      stop t op reason

let inspection op =
  match op.repair with
  | Some { mode = History_recovery _; _ } -> Verify_recovery
  | Some { mode = Content_repair; _ } | None -> Inspect

let finite_time at = if Float.is_finite at then Float.max 0. at else 0.

let retry t op at reason retry_after =
  let failures = Int.min 30 (op.failures + 1) in
  let delay =
    Float.min 300. (5. *. Float.(2. ** of_int (Int.min 6 op.failures)))
  in
  let delay =
    Option.value_map retry_after ~default:delay ~f:(fun d ->
        if Float.is_finite d then Float.max delay d else delay)
  in
  ( {
      t with
      active =
        Some
          {
            op with
            failures;
            pending = None;
            phase = Waiting { until = finite_time at +. delay; reason };
          };
    },
    [] )

let settle t op =
  let t =
    {
      t with
      active = Some { op with phase = Settled; pending = None; failures = 0 };
      publications =
        (match op.candidate with
        | Some candidate when not op.reobserve_after_completion ->
            {
              operation_id = op.id;
              published_intent = op.intent;
              published_revision = candidate;
            }
            :: t.publications
        | Some _ | None -> t.publications);
    }
  in
  match t.desired with
  | Some intent
    when op.reobserve_after_completion || not (equal_intent intent op.intent) ->
      let t, effects = start t intent in
      (t, Completed op.id :: effects)
  | _ -> (t, [ Completed op.id ])

let repair ?(mode = Content_repair) t op head sequencer conflicts =
  let previous =
    Option.filter op.repair ~f:(fun r -> equal_repair_mode r.mode mode)
  in
  let attempts_without_progress =
    match previous with
    | Some r
      when (not (String.equal r.sequencer sequencer)) || conflicts < r.conflicts
      ->
        0
    | Some r when r.attempt_completed -> r.attempts_without_progress + 1
    | Some r when not r.attempt_completed -> r.attempts_without_progress
    | Some _ | None -> 0
  in
  let r =
    {
      mode;
      head;
      sequencer;
      conflicts = Int.max 0 conflicts;
      attempts_without_progress;
      attempt_completed = false;
      attempt_started = false;
    }
  in
  if attempts_without_progress >= 2 then
    match mode with
    | Content_repair -> recover_with_agent t op "repair_no_structural_progress"
    | History_recovery _ -> stop t op "recovery_preservation_unproven"
  else
    let command_sequence = op.command_sequence + 1 in
    let token = { operation = op.id; command = command_sequence } in
    ( {
        t with
        active =
          Some
            {
              op with
              phase = Repairing r;
              repair = Some r;
              pending = None;
              command_sequence;
            };
      },
      [ Repair token ] )

let record_integration t op candidate evidence =
  match op.integration with
  | None -> (t, op)
  | Some capture ->
      let receipt =
        {
          operation_id = op.id;
          capture;
          integrated_revision = candidate;
          evidence;
        }
      in
      ( { t with integrations = receipt :: t.integrations },
        { op with integration = None } )

let publish ?(evidence = Executed) t op candidate =
  let remote_pair =
    match (op.remote_replay, op.local_action) with
    | Some replay, _ -> Some (replay.incoming, replay.preserved)
    | None, Prepare_remote_replay request ->
        Some (request.incoming, request.preserved)
    | None, (Publish_source | Integrate_source _) -> None
  in
  let t =
    match remote_pair with
    | None -> t
    | Some (source_revision, target_revision) ->
        let receipt =
          {
            operation_id = op.id;
            capture =
              {
                base_branch = None;
                source_revision;
                target_revision;
                replay_boundary = op.boundary;
                integration_policy = Rewrite;
              };
            integrated_revision = candidate;
            evidence;
          }
        in
        { t with remote_integrations = receipt :: t.remote_integrations }
  in
  let op = { op with remote_replay = None } in
  let op =
    match op.local_action with
    | Prepare_remote_replay _ ->
        { op with local_action = Integrate_source Rewrite }
    | Publish_source | Integrate_source _ -> op
  in
  let t, op = record_integration t op candidate evidence in
  issue t
    { op with candidate = Some candidate; repair = None; merge_progress = None }
    Publishing
    (Publish { candidate; expected = op.expected })

let integrate_remote t op preserved incoming =
  let begin_operation local_action source target boundary remote_replay kind =
    let successor =
      {
        op with
        id = t.next_operation;
        local_action;
        source = Some source;
        target = Some target;
        boundary;
        boundary_candidates = [];
        remote_replay;
        candidate = None;
        expected = Some incoming;
        command_sequence = 0;
        pending = None;
        repair = None;
        merge_progress = None;
      }
    in
    issue
      { t with next_operation = t.next_operation + 1 }
      successor Integrating kind
  in
  match execution_policy op with
  | Rewrite ->
      let boundaries =
        Option.to_list (Option.map op.expected ~f:(fun sha -> Recorded sha))
        @ recorded_boundaries t
      in
      let request = { preserved; incoming; boundaries } in
      begin_operation (Prepare_remote_replay request) incoming preserved Plain
        None (Plan_remote_replay request)
  | Preserve_ancestry ->
      begin_operation (Integrate_source Preserve_ancestry) preserved incoming
        Plain None
        (Integrate
           {
             source = preserved;
             target = incoming;
             boundary = Plain;
             policy = Preserve_ancestry;
           })

let integrate t op source target boundary policy =
  issue t
    {
      op with
      source = Some source;
      target = Some target;
      boundary;
      local_action = Integrate_source policy;
    }
    Integrating
    (Integrate { source; target; boundary; policy })

let observed t op (o : observation) =
  let op =
    {
      op with
      destination = Bound_remote o.destination;
      source = Some o.source;
      target = Some o.target;
      expected = o.remote;
      boundary = o.boundary;
      integration =
        (match op.intent.purpose with
        | Reconcile_base | Reconcile_request _ ->
            Some
              {
                base_branch = Some op.intent.base;
                source_revision = o.source;
                target_revision = o.target;
                replay_boundary = o.boundary;
                integration_policy = op.intent.policy;
              }
        | Integrate_revision _ | Publish_revision _ | Publish_session _ -> None);
    }
  in
  match (op.intent.purpose, o.sequencer) with
  | Integrate_revision { revision; _ }, _
    when not (Commit.equal revision o.target) ->
      stop t op "integration_target_changed"
  | Publish_revision expected, _ when not (Commit.equal expected o.source) ->
      stop t op "publication_source_changed"
  | (Publish_revision _ | Publish_session _), Some _ ->
      stop t op "publication_during_integration"
  | (Reconcile_base | Reconcile_request _ | Integrate_revision _), Some _ ->
      stop t op "integration_context_missing"
  | (Publish_revision _ | Publish_session _), None
    when Option.is_none o.remote
         && Option.equal Commit.equal
              (Option.bind t.materialized ~f:materialization_boundary)
              (Some o.source) ->
      settle t op
  | (Reconcile_base | Reconcile_request _ | Integrate_revision _), None
    when not o.clean ->
      stop t op "dirty_worktree"
  | (Reconcile_base | Reconcile_request _ | Integrate_revision _), None
    when o.target_included && Option.equal Commit.equal o.remote (Some o.source)
    ->
      let t, op = record_integration t op o.source Recovered_completion in
      issue t
        { op with candidate = Some o.source }
        Preparing
        (Pin { source = o.source; target = o.target })
  | _, None ->
      let boundary = o.boundary in
      let op =
        {
          op with
          source = Some o.source;
          target = Some o.target;
          boundary;
          expected = o.remote;
        }
      in
      issue t op Preparing (Pin { source = o.source; target = o.target })

let adopt_integration t op (o : observation) policy =
  match (op.intent.purpose, o.sequencer) with
  | (Publish_revision _ | Publish_session _), _ ->
      stop t op "publication_during_integration"
  | (Reconcile_base | Reconcile_request _ | Integrate_revision _), None ->
      stop t op "missing_adopted_sequencer"
  | ( (Reconcile_base | Reconcile_request _ | Integrate_revision _),
      Some sequencer )
    when String.is_empty sequencer ->
      stop t op "missing_adopted_sequencer"
  | ( (Reconcile_base | Reconcile_request _ | Integrate_revision _),
      Some sequencer ) ->
      let repair =
        {
          mode = Content_repair;
          head = o.head;
          sequencer;
          conflicts = Int.max 0 o.conflicts;
          attempts_without_progress = 0;
          attempt_completed = false;
          attempt_started = false;
        }
      in
      let op =
        {
          op with
          source = Some o.source;
          target = Some o.target;
          expected = o.remote;
          destination = Bound_remote o.destination;
          boundary = o.boundary;
          local_action = Integrate_source policy;
          reobserve_after_completion = true;
          integration =
            Some
              {
                base_branch = None;
                source_revision = o.source;
                target_revision = o.target;
                replay_boundary = o.boundary;
                integration_policy = policy;
              };
          repair = Some repair;
        }
      in
      issue t op Preparing (Pin { source = o.source; target = o.target })

let recovered_unchanged ~ready t op (o : observation) =
  match op.local_action with
  | Prepare_remote_replay request ->
      if
        Commit.equal o.source request.preserved
        && o.clean && Option.is_none o.sequencer
      then issue t op Integrating (Plan_remote_replay request)
      else recover_with_agent t op "remote_replay_preparation_changed"
  | Publish_source | Integrate_source _ -> (
      match o.sequencer with
      | Some sequencer ->
          if ready && o.conflicts = 0 then
            match op.target with
            | Some target ->
                issue t
                  {
                    op with
                    repair =
                      Option.map op.repair ~f:(fun r ->
                          { r with attempt_started = false });
                  }
                  Integrating
                  (Continue { head = o.head; target; sequencer })
            | None -> stop t op "sequencer_without_pinned_target"
          else repair t op o.head sequencer o.conflicts
      | None -> (
          match op.candidate with
          | Some candidate when Commit.equal candidate o.source ->
              if Option.equal Commit.equal o.remote (Some candidate) then
                settle t op
              else issue t op Confirming (Confirm candidate)
          | Some _ ->
              recover_with_agent t op "local_branch_changed_during_publication"
          | None
            when (not o.completed_integration)
                 && Option.value_map op.remote_replay ~default:false
                      ~f:(fun replay -> Commit.equal replay.preserved o.source)
            -> (
              match op.remote_replay with
              | Some replay when o.clean ->
                  issue t op Integrating (Checkout_remote replay)
              | Some _ -> stop t op "dirty_worktree"
              | None -> stop t op "remote_replay_capture_missing")
          | None -> (
              match (op.source, op.target) with
              | Some source, Some target when Commit.equal source o.source -> (
                  match op.local_action with
                  | Prepare_remote_replay request ->
                      issue t op Integrating (Plan_remote_replay request)
                  | Publish_source -> publish t op source
                  | Integrate_source policy ->
                      if not o.clean then stop t op "dirty_worktree"
                      else integrate t op source target op.boundary policy)
              | Some _, Some _
                when o.target_included && o.completed_integration && o.clean ->
                  publish ~evidence:Recovered_completion t op o.source
              | Some _, Some target when Commit.equal target o.target ->
                  (* The executor must prove completed integration, not merely
                 report a new local SHA. Uncertain changes retain the checkpoint. *)
                  recover_with_agent t op "uncertain_integration_outcome"
              | _ -> observed t op o)))

(* Called only after a matching inspection result has consumed its command. *)
let check_inspected_destination (op : operation) ~observed =
  match op.destination with
  | Reconfirm_remote _ when equal_phase op.phase Recovering -> Ok ()
  | Reconfirm_remote _ | Unobserved_remote | Bound_remote _ ->
      check_destination op ~observed

let recovered ?(ready = false) t op (o : observation) =
  match check_inspected_destination op ~observed:o.destination with
  | Error reason -> stop t op reason
  | Ok () -> (
      let op = { op with destination = Bound_remote o.destination } in
      match op.merge_progress with
      | Some (Pending_merge capture) ->
          if
            Commit.equal o.head capture.head
            && o.clean
            && Option.equal String.equal o.sequencer (Some capture.sequencer)
          then issue t op Integrating (Commit_merge capture)
          else recover_with_agent t op "merge_completion_uncertain"
      | Some (Completed_merge { capture; head })
        when Commit.equal o.head head && o.clean
             && Option.equal String.equal o.sequencer
                  (Some capture.resumed_sequencer) ->
          issue t op Integrating
            (Continue
               {
                 head;
                 target = capture.target;
                 sequencer = capture.resumed_sequencer;
               })
      | None | Some (Completed_merge _) -> (
          match op.repair with
          | Some r when r.attempt_started && not (Commit.equal r.head o.head) ->
              recover_with_agent t op "unexpected_agent_head_change"
          | Some r
            when r.attempt_started
                 && not
                      (Option.equal String.equal o.sequencer (Some r.sequencer))
            ->
              recover_with_agent t op "unexpected_agent_sequencer_change"
          | Some _ | None -> recovered_unchanged ~ready t op o))

let recovery_verified t op (o : observation) source_preserved remote_preserved =
  match check_inspected_destination op ~observed:o.destination with
  | Error reason -> stop t op reason
  | Ok () -> (
      let op = { op with destination = Bound_remote o.destination } in
      match op.repair with
      | Some { mode = History_recovery { reason; baseline }; _ } ->
          let op =
            {
              op with
              recovery_revisions =
                List.dedup_and_sort ~compare:Commit.compare
                  ((o.head :: o.source :: Option.to_list o.remote)
                  @ op.recovery_revisions);
            }
          in
          let target_satisfied =
            match op.local_action with
            | Publish_source -> true
            | Integrate_source _ | Prepare_remote_replay _ -> o.target_included
          in
          if not o.clean then stop t op "history_recovery_dirty_worktree"
          else if
            o.clean && Option.is_none o.sequencer && target_satisfied
            && source_preserved && remote_preserved
          then
            publish ~evidence:Verified_history t
              { op with expected = o.remote }
              o.source
          else
            repair
              ~mode:
                (History_recovery
                   {
                     reason;
                     baseline = Some (Option.value baseline ~default:o.head);
                   })
              t op o.head "history-recovery" o.conflicts
      | Some { mode = Content_repair; _ } | None ->
          stop t op "unexpected_recovery_verification")

let remote t (op : operation) at sha topology =
  match op.candidate with
  | None -> stop t op "publication_without_candidate"
  | Some candidate -> (
      if
        Option.equal Commit.equal sha (Some candidate)
        || (Option.is_some sha && equal_topology topology Behind)
      then settle t op
      else if Option.equal Commit.equal sha op.expected then
        (* A failed or unacknowledged push does not create new remote work.
           In particular, a rewritten candidate can diverge from the captured
           remote while retaining authority to publish against that same lease. *)
        publish t op candidate
      else
        match (sha, topology) with
        | None, _ -> publish t { op with expected = None } candidate
        | Some _, Includes -> publish t { op with expected = sha } candidate
        | Some _, Behind -> settle t op
        | Some remote, Diverged -> integrate_remote t op candidate remote
        | Some _, Equal -> stop t op "contradictory_remote_observation"
        | Some _, Unproven -> retry t op at "remote_topology_unproven" None)

let step t = function
  | Materialized receipt ->
      if Option.is_none t.materialized then
        let t = { t with materialized = Some receipt } in
        let boundary_candidates = recorded_boundaries t in
        let active =
          Option.map t.active ~f:(fun op ->
              if Option.is_none op.source then
                {
                  op with
                  boundary =
                    Option.value (List.hd boundary_candidates) ~default:Plain;
                  boundary_candidates;
                }
              else op)
        in
        ({ t with active }, [])
      else (t, [])
  | Request intent -> (
      let intent =
        match intent.purpose with
        | Integrate_revision _ -> { intent with policy = Preserve_ancestry }
        | Reconcile_base | Reconcile_request _ | Publish_revision _
        | Publish_session _ ->
            intent
      in
      let t = { t with desired = Some intent } in
      match t.active with
      | None -> start t intent
      | Some op -> (
          match op.phase with
          | Settled when not (equal_intent op.intent intent) -> start t intent
          | Preparing | Integrating | Repairing _ | Publishing | Confirming
          | Waiting _ | Recovering | Settled | Intervention _ ->
              (t, [])))
  | Refresh o -> (
      match (t.active, t.desired) with
      | Some ({ phase = Settled; _ } as op), Some intent
        when not
               (Option.equal Commit.equal op.candidate (Some o.source)
               && Option.equal Commit.equal op.target (Some o.target)
               && Option.equal Commit.equal o.remote op.candidate) ->
          start t intent
      | None, _
      | Some _, None
      | ( Some
            {
              phase =
                ( Preparing | Integrating | Repairing _ | Publishing
                | Confirming | Waiting _ | Recovering | Settled | Intervention _
                  );
              _;
            },
          Some _ ) ->
          (t, []))
  | Resume -> (
      match t.active with
      | Some ({ phase = Intervention _; _ } as op) ->
          let repair =
            Option.map op.repair ~f:(fun r ->
                {
                  r with
                  attempts_without_progress = 0;
                  attempt_started = false;
                  attempt_completed = false;
                })
          in
          let destination =
            match op.destination with
            | Bound_remote previous -> Reconfirm_remote (Some previous)
            | Unobserved_remote -> Reconfirm_remote None
            | Reconfirm_remote _ as destination -> destination
          in
          let op = { op with repair; failures = 0; destination } in
          issue t op Recovering (inspection op)
      | None
      | Some
          {
            phase =
              ( Preparing | Integrating | Repairing _ | Publishing | Confirming
              | Waiting _ | Recovering | Settled );
            _;
          } ->
          (t, []))
  | Recover -> (
      match t.active with
      | Some { phase = Recovering; pending = Some command; _ } ->
          (t, [ Execute command ])
      | Some op when is_pending t -> issue t op Recovering (inspection op)
      | None
      | Some
          {
            phase =
              ( Preparing | Integrating | Repairing _ | Publishing | Confirming
              | Waiting _ | Recovering | Settled | Intervention _ );
            _;
          } ->
          (t, []))
  | Tick at -> (
      match t.active with
      | Some ({ phase = Waiting { until; _ }; _ } as op)
        when Float.is_finite at && Float.(at >= until) ->
          issue t op Recovering (inspection op)
      | None
      | Some
          {
            phase =
              ( Preparing | Integrating | Repairing _ | Publishing | Confirming
              | Waiting _ | Recovering | Settled | Intervention _ );
            _;
          } ->
          (t, []))
  | Repair_invalidated { token; reason } -> (
      match t.active with
      | Some ({ phase = Repairing _; _ } as op)
        when equal_token token
               { operation = op.id; command = op.command_sequence } ->
          recover_with_agent t op reason
      | None
      | Some
          {
            phase =
              ( Preparing | Integrating | Repairing _ | Publishing | Confirming
              | Waiting _ | Recovering | Settled | Intervention _ );
            _;
          } ->
          (t, []))
  | Repair_denied { token; reason } -> (
      match t.active with
      | Some ({ phase = Repairing _; _ } as op)
        when equal_token token
               { operation = op.id; command = op.command_sequence } ->
          stop t op reason
      | None
      | Some
          {
            phase =
              ( Preparing | Integrating | Repairing _ | Publishing | Confirming
              | Waiting _ | Recovering | Settled | Intervention _ );
            _;
          } ->
          (t, []))
  | Repair_started token -> (
      match t.active with
      | Some ({ phase = Repairing r; _ } as op)
        when (not r.attempt_started)
             && equal_token token
                  { operation = op.id; command = op.command_sequence } ->
          let r = { r with attempt_started = true } in
          ( {
              t with
              active = Some { op with phase = Repairing r; repair = Some r };
            },
            [ Start_repair token ] )
      | None
      | Some
          {
            phase =
              ( Preparing | Integrating | Repairing _ | Publishing | Confirming
              | Waiting _ | Recovering | Settled | Intervention _ );
            _;
          } ->
          (t, []))
  | Repair_interrupted { token; at; reason } -> (
      match t.active with
      | Some ({ phase = Repairing r; _ } as op)
        when r.attempt_started
             && equal_token token
                  { operation = op.id; command = op.command_sequence } ->
          retry t op at reason None
      | None
      | Some
          {
            phase =
              ( Preparing | Integrating | Repairing _ | Publishing | Confirming
              | Waiting _ | Recovering | Settled | Intervention _ );
            _;
          } ->
          (t, []))
  | Repair_completed { token; at = _ } -> (
      match t.active with
      | Some ({ phase = Repairing r; _ } as op)
        when r.attempt_started
             && equal_token token
                  { operation = op.id; command = op.command_sequence } ->
          issue t
            { op with repair = Some { r with attempt_completed = true } }
            Recovering (inspection op)
      | None
      | Some
          {
            phase =
              ( Preparing | Integrating | Repairing _ | Publishing | Confirming
              | Waiting _ | Recovering | Settled | Intervention _ );
            _;
          } ->
          (t, []))
  | Result { token; at; result } -> (
      match t.active with
      | Some op
        when Option.value_map op.pending ~default:false ~f:(fun c ->
                 equal_token c.token token) -> (
          let command = Option.map op.pending ~f:(fun c -> c.kind) in
          let op = { op with pending = None } in
          match (result, command) with
          | Retryable { reason; retry_after }, _ ->
              retry t op at reason retry_after
          | Permanent reason, _ -> stop t op reason
          | Recovery_required reason, _ -> recover_with_agent t op reason
          | Observed o, Some Observe -> observed t op o
          | Observed o, Some Inspect when Option.is_none op.source ->
              observed t op o
          | Observed_active { observation; policy }, Some Observe ->
              adopt_integration t op observation policy
          | Observed_active { observation; policy }, Some Inspect
            when Option.is_none op.source ->
              adopt_integration t op observation policy
          | Inspected o, Some Inspect when Option.is_none o.sequencer ->
              recovered t op o
          | Inspected_active { observation; ready }, Some Inspect ->
              recovered ~ready t op observation
          | Inspected _, Some Inspect ->
              recover_with_agent t op "unidentified_integration_context"
          | ( Recovery_verified
                { observation; source_preserved; remote_preserved },
              Some Verify_recovery ) ->
              recovery_verified t op observation source_preserved
                remote_preserved
          | ( Merge_completion_needed capture,
              Some (Continue { head; target; sequencer }) )
            when Commit.equal capture.head head
                 && Commit.equal capture.target target
                 && String.equal capture.sequencer sequencer
                 && (not (List.is_empty capture.parents))
                 && (not (String.is_empty capture.resumed_sequencer))
                 && not
                      (String.equal capture.sequencer capture.resumed_sequencer)
            ->
              issue t
                {
                  op with
                  repair = None;
                  merge_progress = Some (Pending_merge capture);
                }
                Integrating (Commit_merge capture)
          | Merge_completed head, Some (Commit_merge _ | Inspect) -> (
              match op.merge_progress with
              | Some (Pending_merge capture) ->
                  issue t
                    {
                      op with
                      repair = None;
                      merge_progress = Some (Completed_merge { capture; head });
                    }
                    Integrating
                    (Continue
                       {
                         head;
                         target = capture.target;
                         sequencer = capture.resumed_sequencer;
                       })
              | None | Some (Completed_merge _) ->
                  stop t op "unexpected_merge_completion")
          | Pinned, Some (Pin { target; _ }) when op.reobserve_after_completion
            -> (
              match op.repair with
              | Some r when r.conflicts = 0 ->
                  issue t op Integrating
                    (Continue { head = r.head; target; sequencer = r.sequencer })
              | Some r -> repair t op r.head r.sequencer r.conflicts
              | None -> stop t op "missing_adopted_repair")
          | Pinned, Some (Pin { source; target }) -> (
              match op.candidate with
              | Some candidate -> issue t op Confirming (Confirm candidate)
              | None -> (
                  match op.local_action with
                  | Integrate_source policy ->
                      integrate t op source target op.boundary policy
                  | Publish_source -> publish t op source
                  | Prepare_remote_replay request ->
                      issue t op Integrating (Plan_remote_replay request)))
          | Remote_replay_selected boundary, Some (Plan_remote_replay request)
            -> (
              match boundary with
              | Plain ->
                  recover_with_agent t op "remote_replay_boundary_missing"
              | Recorded upstream | Inferred upstream ->
                  if
                    match boundary with
                    | Recorded _ ->
                        not
                          (List.mem request.boundaries boundary
                             ~equal:equal_boundary)
                    | Inferred _ | Plain -> false
                  then stop t op "unrecorded_remote_replay_boundary"
                  else
                    let replay =
                      {
                        preserved = request.preserved;
                        incoming = request.incoming;
                        upstream;
                      }
                    in
                    issue t
                      {
                        op with
                        boundary;
                        boundary_candidates = request.boundaries;
                        remote_replay = Some replay;
                        local_action = Integrate_source Rewrite;
                      }
                      Integrating (Checkout_remote replay))
          | Remote_checked_out, Some (Checkout_remote replay) ->
              integrate t op replay.incoming replay.preserved op.boundary
                Rewrite
          | Integrated candidate, Some (Integrate _ | Continue _) ->
              publish t op candidate
          | ( Conflict { head; sequencer; conflicts },
              Some (Integrate _ | Continue _) ) ->
              let merge_progress =
                match op.merge_progress with
                | Some (Completed_merge c)
                  when Commit.equal c.head head
                       && String.equal c.capture.resumed_sequencer sequencer ->
                    op.merge_progress
                | None | Some (Pending_merge _ | Completed_merge _) -> None
              in
              repair t { op with merge_progress } head sequencer conflicts
          | Published, Some (Publish { candidate; _ }) ->
              issue t op Confirming (Confirm candidate)
          | Remote { sha; topology }, Some (Confirm _) ->
              remote t op at sha topology
          | ( ( Observed _ | Observed_active _ | Pinned | Integrated _
              | Conflict _ | Merge_completion_needed _ | Merge_completed _
              | Remote_replay_selected _ | Remote_checked_out | Inspected _
              | Inspected_active _ | Recovery_verified _ | Published | Remote _
                ),
              ( None
              | Some
                  ( Observe | Commit_merge _ | Plan_remote_replay _
                  | Checkout_remote _ | Pin _ | Integrate _ | Inspect
                  | Verify_recovery | Continue _ | Publish _ | Confirm _ ) ) )
            ->
              stop t op "command_result_mismatch")
      | _ -> (t, []))

let integration_result (op : operation) ~candidate ~target_preserved
    ~source_preserved =
  match (op.local_action, op.source, op.target) with
  | Integrate_source _, Some _, Some _ when not target_preserved ->
      Recovery_required "integration_target_not_preserved"
  | Integrate_source Preserve_ancestry, Some _, Some _ when not source_preserved
    ->
      Recovery_required "integration_source_not_preserved"
  | Integrate_source (Rewrite | Preserve_ancestry), Some _, Some _ ->
      Integrated candidate
  | Prepare_remote_replay _, _, _
  | Publish_source, _, _
  | Integrate_source _, None, _
  | Integrate_source _, _, None ->
      Recovery_required "integration_capture_missing"

let check_checkout (command : command) ~branch (checkout : Git_observation.t) =
  let module G = Git_observation in
  let named_head sha =
    Option.equal String.equal checkout.branch (Some branch)
    && String.equal checkout.head (Commit.to_string sha)
    && G.equal_sequencer checkout.sequencer G.None_active
  in
  match command.kind with
  | Commit_merge capture ->
      if
        String.equal checkout.head (Commit.to_string capture.head)
        && String.equal (G.progress_key checkout) capture.sequencer
        && G.equal_continuation (G.continuation checkout) G.Complete_merge
        &&
        match checkout.sequencer with
        | G.Rebase r ->
            Option.is_none checkout.branch
            && String.equal r.head_ref ("refs/heads/" ^ branch)
            && String.equal r.target (Commit.to_string capture.target)
            && List.equal String.equal r.merge_heads
                 (List.map capture.parents ~f:Commit.to_string)
        | G.Merge _ | G.Cherry_pick _ | G.None_active -> false
      then Ok ()
      else Error "merge_completion_precondition_changed"
  | Plan_remote_replay request ->
      if named_head request.preserved && G.clean checkout then Ok ()
      else Error "remote_replay_precondition_changed"
  | Checkout_remote replay ->
      if named_head replay.preserved && G.clean checkout then Ok ()
      else Error "remote_replay_precondition_changed"
  | Integrate { source; _ } ->
      if named_head source && G.clean checkout then Ok ()
      else Error "integration_precondition_changed"
  | Publish { candidate; _ } ->
      if named_head candidate && checkout.valid then Ok ()
      else Error "publication_precondition_changed"
  | Continue { head; target; sequencer } ->
      let target = Commit.to_string target in
      let same_head = String.equal checkout.head (Commit.to_string head) in
      let same_integration =
        match checkout.sequencer with
        | G.Rebase r ->
            String.equal r.target target
            && String.equal r.head_ref ("refs/heads/" ^ branch)
            && Option.is_none checkout.branch
        | G.Merge r ->
            String.equal r.target target
            && Option.equal String.equal checkout.branch (Some branch)
        | G.Cherry_pick r ->
            String.equal r.target target
            && Option.equal String.equal checkout.branch (Some branch)
        | G.None_active -> false
      in
      if
        checkout.valid && same_head && same_integration
        && String.equal sequencer (G.progress_key checkout)
      then Ok ()
      else Error "repair_sequencer_changed"
  | Observe | Pin _ | Inspect | Verify_recovery | Confirm _ -> Ok ()

let inspection_result (op : operation) ~branch (o : observation)
    (checkout : Git_observation.t) =
  let module G = Git_observation in
  let fresh = Option.is_none op.source in
  let ordinary () = if fresh then Observed o else Inspected o in
  let active ~policy ~original ~target ~identity =
    if fresh then
      if
        identity
        && String.equal original (Commit.to_string o.source)
        && String.equal target (Commit.to_string o.target)
      then Observed_active { observation = o; policy }
      else Permanent "adopted_integration_context_changed"
    else
      let valid =
        identity && checkout.valid
        && String.equal checkout.head (Commit.to_string o.head)
        && Option.equal String.equal o.sequencer
             (Some (G.progress_key checkout))
        && Option.equal Commit.equal op.source (Some o.source)
        && Option.value_map op.source ~default:false ~f:(fun source ->
            String.equal original (Commit.to_string source))
        && Option.value_map op.target ~default:false ~f:(fun expected ->
            String.equal target (Commit.to_string expected))
        && equal_local_action op.local_action (Integrate_source policy)
      in
      if valid then
        Inspected_active { observation = o; ready = G.repair_ready checkout }
      else Recovery_required "active_integration_context_changed"
  in
  if not checkout.valid then
    Retryable { reason = "invalid_checkout_observation"; retry_after = None }
  else if
    (not (String.equal checkout.head (Commit.to_string o.head)))
    || (not (Bool.equal o.clean (G.clean checkout)))
    || o.conflicts <> List.length checkout.conflicts
    || not
         (Option.equal String.equal o.sequencer
            (match checkout.sequencer with
            | G.None_active -> None
            | G.Rebase _ | G.Merge _ | G.Cherry_pick _ ->
                Some (G.progress_key checkout)))
  then
    Retryable
      { reason = "inconsistent_checkout_observation"; retry_after = None }
  else
    match checkout.sequencer with
    | G.None_active -> ordinary ()
    | G.Rebase r -> (
        match op.intent.purpose with
        | (Publish_revision _ | Publish_session _) when fresh -> ordinary ()
        | Reconcile_base | Reconcile_request _ | Integrate_revision _
        | Publish_revision _ | Publish_session _ ->
            active ~policy:Rewrite ~original:r.original ~target:r.target
              ~identity:
                (Option.is_none checkout.branch
                && String.equal r.head_ref ("refs/heads/" ^ branch)))
    | G.Merge r -> (
        match op.intent.purpose with
        | (Publish_revision _ | Publish_session _) when fresh -> ordinary ()
        | Reconcile_base | Reconcile_request _ | Integrate_revision _
        | Publish_revision _ | Publish_session _ ->
            active ~policy:Preserve_ancestry ~original:checkout.head
              ~target:r.target
              ~identity:
                (Option.equal String.equal checkout.branch (Some branch)))
    | G.Cherry_pick _ ->
        if fresh then Permanent "unidentified_cherry_pick_requires_recovery"
        else Recovery_required "active_integration_context_changed"

let continuation (op : operation) (checkout : Git_observation.t) =
  match op.merge_progress with
  | Some (Completed_merge { capture; head })
    when String.equal checkout.head (Commit.to_string head)
         && String.equal
              (Git_observation.progress_key checkout)
              capture.resumed_sequencer ->
      if Git_observation.clean checkout then Git_observation.Continue
      else Git_observation.Blocked
  | None | Some (Pending_merge _ | Completed_merge _) ->
      Git_observation.continuation checkout

let decode json =
  let valid_commit sha = Option.is_some (Commit.make sha) in
  let optional = Option.for_all ~f:valid_commit in
  let valid_boundary = function
    | Recorded sha | Inferred sha -> valid_commit sha
    | Plain -> true
  in
  let valid_intent intent =
    match intent.purpose with
    | Integrate_revision { contributor; revision } ->
        (not (String.is_empty contributor))
        && valid_commit revision
        && equal_policy intent.policy Preserve_ancestry
    | Reconcile_base -> true
    | Reconcile_request request -> not (String.is_empty request)
    | Publish_revision sha -> valid_commit sha
    | Publish_session session_uuid -> not (String.is_empty session_uuid)
  in
  let valid_capture (capture : integration_capture) =
    valid_commit capture.source_revision
    && valid_commit capture.target_revision
    && valid_boundary capture.replay_boundary
    && Option.for_all capture.base_branch ~f:(fun base ->
        not (String.is_empty base))
  in
  let valid_repair (r : repair) =
    valid_commit r.head
    && (not (String.is_empty r.sequencer))
    && r.conflicts >= 0
    && r.attempts_without_progress >= 0
    && r.attempts_without_progress < 2
    &&
    match r.mode with
    | Content_repair -> true
    | History_recovery { reason; baseline } ->
        optional baseline
        && (not (String.is_empty (String.strip reason)))
        && String.equal r.sequencer "history-recovery"
  in
  let valid_completion (c : merge_completion) =
    valid_commit c.head && valid_commit c.target
    && (not (List.is_empty c.parents))
    && List.for_all c.parents ~f:valid_commit
    && (not (String.is_empty c.sequencer))
    && (not (String.is_empty c.resumed_sequencer))
    && not (String.equal c.sequencer c.resumed_sequencer)
  in
  let valid_kind = function
    | Commit_merge c -> valid_completion c
    | Observe | Inspect | Verify_recovery -> true
    | Plan_remote_replay r ->
        valid_commit r.preserved && valid_commit r.incoming
        && List.for_all r.boundaries ~f:valid_boundary
    | Checkout_remote r ->
        valid_commit r.preserved && valid_commit r.incoming
        && valid_commit r.upstream
    | Pin { source; target } -> valid_commit source && valid_commit target
    | Integrate { source; target; boundary; policy = _ } ->
        valid_commit source && valid_commit target && valid_boundary boundary
    | Continue { head; target; sequencer } ->
        valid_commit head && valid_commit target
        && not (String.is_empty sequencer)
    | Publish { candidate; expected } ->
        valid_commit candidate && optional expected
    | Confirm candidate -> valid_commit candidate
  in
  let valid_phase = function
    | Waiting { until; reason } ->
        Float.is_finite until && not (String.is_empty reason)
    | Repairing r -> valid_repair r
    | Preparing | Integrating | Publishing | Confirming | Recovering | Settled
    | Intervention _ ->
        true
  in
  let consistent (op : operation) =
    let commit_matches observed sha =
      Option.equal Commit.equal observed (Some sha)
    in
    match (op.pending, op.phase) with
    | Some { kind = Commit_merge capture; _ }, Integrating ->
        Option.equal equal_merge_progress op.merge_progress
          (Some (Pending_merge capture))
        && commit_matches op.target capture.target
    | Some { kind = Plan_remote_replay request; _ }, Integrating ->
        equal_local_action op.local_action (Prepare_remote_replay request)
        && commit_matches op.source request.incoming
        && commit_matches op.target request.preserved
        && Option.is_none op.remote_replay
    | Some { kind = Checkout_remote replay; _ }, Integrating ->
        Option.equal equal_remote_replay op.remote_replay (Some replay)
        && commit_matches op.source replay.incoming
        && commit_matches op.target replay.preserved
        && equal_local_action op.local_action (Integrate_source Rewrite)
    | Some { kind = Observe; _ }, Preparing ->
        Option.is_none op.source && Option.is_none op.target
    | Some { kind = Pin { source; target }; _ }, Preparing ->
        commit_matches op.source source && commit_matches op.target target
    | ( Some { kind = Integrate { source; target; policy; boundary }; _ },
        Integrating ) ->
        commit_matches op.source source
        && commit_matches op.target target
        && equal_boundary op.boundary boundary
        && equal_local_action op.local_action (Integrate_source policy)
    | Some { kind = Continue { target; _ }; _ }, Integrating ->
        commit_matches op.target target
    | Some { kind = Publish { candidate; expected }; _ }, Publishing ->
        commit_matches op.candidate candidate
        && Option.equal Commit.equal op.expected expected
    | Some { kind = Confirm candidate; _ }, Confirming ->
        commit_matches op.candidate candidate
    | Some { kind = Inspect; _ }, Recovering -> true
    | Some { kind = Verify_recovery; _ }, Recovering -> (
        match op.repair with
        | Some { mode = History_recovery _; _ } -> true
        | Some { mode = Content_repair; _ } | None -> false)
    | None, Repairing repair ->
        Option.equal equal_repair op.repair (Some repair)
    | None, (Settled | Waiting _ | Intervention _) -> true
    | ( ( None
        | Some
            {
              kind =
                ( Observe | Commit_merge _ | Plan_remote_replay _
                | Checkout_remote _ | Pin _ | Integrate _ | Inspect
                | Verify_recovery | Continue _ | Publish _ | Confirm _ );
              _;
            } ),
        ( Preparing | Integrating | Repairing _ | Publishing | Confirming
        | Waiting _ | Recovering | Settled | Intervention _ ) ) ->
        false
  in
  match Json.try_of_yojson t_of_yojson json with
  | Error message -> Error message
  | Ok t -> (
      if
        t.next_operation < 1
        || (not
              (Option.for_all t.materialized ~f:(fun m ->
                   valid_commit (materialization_head m))))
        || (not (Option.for_all t.desired ~f:valid_intent))
        || (not
              (List.for_all t.publications ~f:(fun (r : publication_receipt) ->
                   r.operation_id > 0
                   && r.operation_id < t.next_operation
                   && valid_intent r.published_intent
                   && valid_commit r.published_revision)))
        || not
             (List.for_all (t.integrations @ t.remote_integrations) ~f:(fun r ->
                  r.operation_id > 0
                  && r.operation_id < t.next_operation
                  && valid_capture r.capture
                  && valid_commit r.integrated_revision))
      then Error "invalid branch checkpoint"
      else
        match t.active with
        | None -> Ok t
        | Some op
          when op.id > 0 && op.id < t.next_operation && op.command_sequence >= 0
               && Option.for_all op.integration ~f:valid_capture
               && ((not op.reobserve_after_completion)
                  || Option.is_some op.source && Option.is_some op.target
                     &&
                     match op.local_action with
                     | Integrate_source _ -> true
                     | Publish_source | Prepare_remote_replay _ -> false)
               && valid_intent op.intent && op.failures >= 0
               && op.failures <= 30 && optional op.source && optional op.target
               && optional op.candidate && optional op.expected
               && (match op.local_action with
                 | Publish_source | Integrate_source _ -> true
                 | Prepare_remote_replay r ->
                     valid_commit r.preserved && valid_commit r.incoming
                     && List.for_all r.boundaries ~f:valid_boundary
                     && Option.equal Commit.equal op.source (Some r.incoming)
                     && Option.equal Commit.equal op.target (Some r.preserved)
                     && Option.is_none op.candidate
                     && Option.is_none op.remote_replay)
               && Option.for_all op.remote_replay ~f:(fun r ->
                   valid_commit r.preserved && valid_commit r.incoming
                   && valid_commit r.upstream
                   && (match op.boundary with
                     | Recorded sha | Inferred sha ->
                         Commit.equal sha r.upstream
                     | Plain -> false)
                   && Option.equal Commit.equal op.source (Some r.incoming)
                   && Option.equal Commit.equal op.target (Some r.preserved))
               && valid_boundary op.boundary
               && List.for_all op.boundary_candidates ~f:valid_boundary
               && List.for_all op.recovery_revisions ~f:valid_commit
               && (match op.destination with
                 | Unobserved_remote -> true
                 | Bound_remote identity -> Remote_id.valid identity
                 | Reconfirm_remote previous ->
                     Option.for_all previous ~f:Remote_id.valid)
               && Option.for_all op.merge_progress ~f:(function
                 | Pending_merge capture ->
                     valid_completion capture
                     && Option.equal Commit.equal op.target
                          (Some capture.target)
                 | Completed_merge { capture; head } ->
                     valid_completion capture && valid_commit head
                     && Option.equal Commit.equal op.target
                          (Some capture.target))
               && valid_phase op.phase && consistent op
               && Option.for_all op.repair ~f:valid_repair
               && Option.for_all op.pending ~f:(fun c ->
                   c.token.operation = op.id
                   && c.token.command = op.command_sequence
                   && valid_kind c.kind) ->
            Ok t
        | Some _ -> Error "invalid branch operation")

let recovery_prefix ~project ~branch =
  let hex s =
    String.to_list s
    |> List.map ~f:(fun c -> Printf.sprintf "%02x" (Char.to_int c))
    |> String.concat
  in
  "refs/onton/reconcile/p" ^ hex project ^ "/b" ^ hex branch

let wake_event ~at t =
  match phase t with
  | Some (Waiting { until; _ }) ->
      if Float.is_finite at && Float.(at >= until) then Some (Tick at) else None
  | Some (Preparing | Integrating | Publishing | Confirming | Recovering) ->
      Some Recover
  | Some (Repairing _) -> Some Recover
  | Some (Settled | Intervention _) | None -> None

let can_execute_git ~at t = Option.is_some (wake_event ~at t)

type repair_turn = {
  token : token;
  head : Commit.t;
  mode : repair_mode;
  prompt : string;
}

let repair_turn t ~branch token =
  match t.active with
  | Some
      {
        phase = Repairing r;
        id;
        command_sequence;
        source;
        target;
        candidate;
        expected;
        recovery_revisions;
        _;
      }
    when r.attempt_started
         && equal_token token { operation = id; command = command_sequence } ->
      let sha value =
        Option.value_map value ~default:"unknown" ~f:Commit.to_string
      in
      Some
        {
          token;
          head = r.head;
          mode = r.mode;
          prompt =
            (match r.mode with
            | History_recovery { reason; baseline } ->
                Printf.sprintf
                  "Recover managed branch %s after deterministic \
                   reconciliation could not proceed: %s.\n\
                   Preserved source: %s\n\
                   Pinned target: %s\n\
                   Preserved candidate: %s\n\
                   Captured remote: %s\n\
                   Recovery starting revision: %s\n\
                   Additional retained revisions: %s\n\
                   Inspect the checkout, index, sequencer, reflogs and project \
                   recovery refs. Preserve all valid patch and remote work. \
                   You may resolve, stage, commit, and finish or reconstruct \
                   integration against the pinned target. Do not discard \
                   uncommitted work or delete recovery refs. Do not push. \
                   Leave a clean checkout of the managed branch with no active \
                   sequencer. Onton will independently verify preserved \
                   history and publish the captured candidate with an explicit \
                   remote lease. If preservation cannot be established, leave \
                   the work intact and explain the uncertainty.\n"
                  branch reason (sha source) (sha target) (sha candidate)
                  (sha expected) (sha baseline)
                  (String.concat ~sep:", "
                     (List.map recovery_revisions ~f:Commit.to_string))
            | Content_repair ->
                Printf.sprintf
                  "Resolve the active Git integration conflicts on managed \
                   branch %s.\n\
                   Captured source: %s\n\
                   Pinned target: %s\n\
                   Repair step: %s\n\
                   Inspect git status and the unmerged index, preserve valid \
                   patch and upstream changes, edit conflicted files, and \
                   stage the resolved paths. Leave staged resolutions in \
                   place. Do not commit, rebase, merge, reset, continue, skip, \
                   abort, switch branches, or push. Onton owns the sequencer \
                   and publication. A successful repair does not require a new \
                   commit.\n"
                  branch (sha source) (sha target) r.sequencer);
        }
  | None
  | Some
      {
        phase =
          ( Preparing | Integrating | Repairing _ | Publishing | Confirming
          | Waiting _ | Recovering | Settled | Intervention _ );
        _;
      } ->
      None

let repair_head_matches (turn : repair_turn) head =
  Option.equal Commit.equal (Some turn.head) head

let repair_result ~(turn : repair_turn) ~at ~before_head ~after_head ~timed_out
    ~final_result ~detail =
  let token = turn.token in
  match (before_head, after_head) with
  | Some head, _ when not (Commit.equal turn.head head) ->
      Repair_invalidated { token; reason = "unexpected_agent_head_change" }
  | _, Some head
    when equal_repair_mode turn.mode Content_repair
         && not (Commit.equal turn.head head) ->
      Repair_invalidated { token; reason = "unexpected_agent_head_change" }
  | Some _, Some _ when (not timed_out) && final_result ->
      Repair_completed { token; at }
  | Some _, Some _ | None, _ | _, None ->
      let reason =
        if String.is_empty (String.strip detail) then
          "repair_session_interrupted"
        else detail
      in
      Repair_interrupted { token; at; reason }

let stopped_integration_result (checkout : Git_observation.t) ~detail =
  let retry reason = Retryable { reason; retry_after = None } in
  if not checkout.valid then retry "invalid_checkout_observation"
  else
    match (checkout.sequencer, Commit.make checkout.head) with
    | Git_observation.None_active, _ -> retry "integration_sequencer_missing"
    | _, None -> retry "invalid_checkout_head"
    | ( ( Git_observation.Rebase _ | Git_observation.Merge _
        | Git_observation.Cherry_pick _ ),
        Some head ) ->
        if List.is_empty checkout.conflicts && List.is_empty checkout.unstaged
        then
          retry
            (if String.is_empty detail then
               "integration_stopped_without_content_conflict"
             else detail)
        else
          Conflict
            {
              head;
              sequencer = Git_observation.progress_key checkout;
              conflicts = List.length checkout.conflicts;
            }

let diagnostics t =
  match t.active with
  | None -> []
  | Some op -> (
      let phase, reason =
        match op.phase with
        | Preparing -> ("preparing", None)
        | Integrating -> ("integrating", None)
        | Repairing r ->
            ( (match r.mode with
              | Content_repair -> "content repair"
              | History_recovery _ -> "history recovery"),
              match r.mode with
              | Content_repair -> None
              | History_recovery { reason; _ } -> Some reason )
        | Publishing -> ("publishing", None)
        | Confirming -> ("confirming remote", None)
        | Waiting { reason; _ } -> ("waiting", Some reason)
        | Recovering -> ("inspecting recovery", None)
        | Settled -> ("settled", None)
        | Intervention reason -> ("intervention", Some reason)
      in
      let revision = Option.value_map ~default:"unknown" ~f:Commit.to_string in
      [
        ("Reconcile", phase);
        ("Operation", Int.to_string op.id);
        ("Source SHA", revision op.source);
        ("Target SHA", revision op.target);
        ("Candidate", revision op.candidate);
        ( "Remote lease",
          match (op.source, op.expected) with
          | None, _ -> "unknown"
          | Some _, None -> "absent"
          | Some _, Some sha -> Commit.to_string sha );
        ( "Replay proof",
          match op.boundary with
          | Recorded sha -> "recorded " ^ Commit.to_string sha
          | Inferred sha -> "inferred " ^ Commit.to_string sha
          | Plain -> "plain (no proven boundary)" );
      ]
      @ Option.to_list (Option.map reason ~f:(fun reason -> ("Reason", reason)))
      @
      match op.phase with
      | Waiting { until; _ } -> [ ("Retry at", Float.to_string until) ]
      | Preparing | Integrating | Repairing _ | Publishing | Confirming
      | Recovering | Settled | Intervention _ ->
          [])

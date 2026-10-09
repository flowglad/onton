(* @archlint.module core
   @archlint.domain branch-reconcile *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Commit = struct
  type t = string [@@deriving eq, compare, sexp_of, yojson]

  let make s = if Git_oid.valid s then Some s else None
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
  | Provision_checkout of string
  | Reconcile_base
  | Reconcile_request of string
  | Reconcile_scoped of {
      request : string;
      project : string;
      ancestors : Types.Patch_id.t list;
    }
  | Integrate_revision of { contributor : string; revision : Commit.t }
  | Publish_revision of Commit.t
  | Publish_session of string
  | Verify_publication
[@@deriving eq, compare, sexp_of, yojson]

let is_provisioning = function
  | Provision_checkout _ -> true
  | Reconcile_base | Reconcile_request _ | Reconcile_scoped _
  | Integrate_revision _ | Publish_revision _ | Publish_session _
  | Verify_publication ->
      false

type intent = { base : string; policy : policy; purpose : purpose }
[@@deriving eq, compare, sexp_of, yojson]

type boundary =
  | Recorded of Commit.t
  | Inferred of Commit.t
  | Reconstructed of { original : Commit.t; upstream : Commit.t }
  | Subject_inferred of Commit.t
  | Patch_equivalent of Commit.t
  | Plain
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
  base_contains_source : bool;
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

type recovery_task =
  | Finish_local_work
  | Reconstruct_history
  | Repair_publication
[@@deriving eq, compare, sexp_of, yojson]

type repair_mode =
  | Diagnosis of { reason : string }
  | Content_repair
  | History_recovery of {
      reason : string;
      baseline : Commit.t option;
      task : recovery_task; [@yojson.default Reconstruct_history]
    }
[@@deriving eq, compare, sexp_of, yojson]

type repair = {
  mode : repair_mode;
  head : Commit.t option;
  sequencer : string;
  conflicts : int;
  attempts_without_progress : int;
  attempt_completed : bool;
  attempt_started : bool;
  last_turn : token option; [@yojson.default None]
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
  preservation_policy : policy; [@yojson.default Rewrite]
  integration : integration_capture option;
  remote_integration : integration_capture option; [@yojson.default None]
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
  deterministic_failures : int; [@yojson.default 0]
  observation_failures : int; [@yojson.default 0]
  publication_recovery_attempts : int; [@yojson.default 0]
  repair : repair option;
  merge_progress : merge_progress option;
}
[@@deriving eq, compare, sexp_of, yojson]

type publication_identity = { operation_id : int; revision : Commit.t }
[@@deriving eq, compare, sexp_of, yojson]

type t = {
  worktree_hook : Worktree_hook.t; [@yojson.default Worktree_hook.empty]
  forge_observations : Forge_observation.history;
      [@yojson.default Forge_observation.empty_history]
  legacy_revisions : Commit.t list; [@yojson.default []]
  legacy_publication_base : string option; [@yojson.default None]
  desired : intent option;
  materialized : materialization option;
  next_operation : int;
  active : operation option;
  integrations : integration_receipt list;
  remote_integrations : integration_receipt list;
  publications : publication_receipt list;
  observed_publication : publication_identity option; [@default None]
}
[@@deriving eq, compare, sexp_of, yojson]

type result =
  | Checkout_ready
  | Observed of observation
  | Observed_active of { observation : observation; policy : policy }
  | Observed_recovery of { observation : observation; policy : policy }
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
  | Publication_rejected of Push_reject_classify.rejection
  | Attempt_failed of string
  | Recovery_required of string
  | Needs_diagnosis of string
[@@deriving eq, compare, sexp_of]

type event =
  | Worktree_hook of Worktree_hook.event
  | Publication_observed of {
      publication : publication_identity;
      head : Commit.t;
      remote : Commit.t option;
    }
  | Materialized of materialization
  | Request of intent
  | Result of { token : token; at : float; result : result }
  | Repair_invalidated of { token : token; reason : string }
  | Repair_started of token
  | Repair_interrupted of { token : token; at : float; reason : string }
  | Repair_failed of { token : token; at : float; reason : string }
  | Repair_completed of { token : token; at : float }
  | Tick of float
  | Recover
  | Reconfirm_publication
  | Resume
  | Refresh of observation
[@@deriving eq, compare, sexp_of]

type effect_command =
  | Execute of command
  | Repair of token
  | Start_repair of token
  | Completed of int
[@@deriving eq, compare, sexp_of]

let command_allowed_for_purpose purpose kind =
  match purpose with
  | Provision_checkout _ -> (
      match kind with
      | Observe | Inspect -> true
      | Pin _ | Confirm _ | Publish _ | Integrate _ | Continue _
      | Verify_recovery | Commit_merge _ | Plan_remote_replay _
      | Checkout_remote _ ->
          false)
  | Verify_publication -> (
      match kind with
      | Observe | Inspect | Pin _ | Confirm _ -> true
      | Publish _ | Integrate _ | Continue _ | Verify_recovery | Commit_merge _
      | Plan_remote_replay _ | Checkout_remote _ ->
          false)
  | Reconcile_base | Reconcile_request _ | Reconcile_scoped _
  | Integrate_revision _ | Publish_revision _ | Publish_session _ ->
      true

let empty =
  {
    worktree_hook = Worktree_hook.empty;
    forge_observations = Forge_observation.empty_history;
    legacy_revisions = [];
    legacy_publication_base = None;
    desired = None;
    materialized = None;
    next_operation = 1;
    active = None;
    integrations = [];
    remote_integrations = [];
    publications = [];
    observed_publication = None;
  }

let forge_observations t = t.forge_observations
let worktree_hook t = t.worktree_hook

let begin_forge_observation t ~request ~scope =
  let forge_observations, ticket =
    Forge_observation.begin_request t.forge_observations ~request ~scope
  in
  ({ t with forge_observations }, ticket)

let accept_forge_observation ?confirmed_base t ~ticket ~current ~confirmed_head
    observation =
  let forge_observations, result =
    Forge_observation.accept ?confirmed_base t.forge_observations ~ticket
      ~current ~confirmed_head observation
  in
  ({ t with forge_observations }, result)

let import_legacy_anchors ?(revision = `Null) t json =
  let revisions =
    Option.value (Json.list json) ~default:[]
    |> List.filter_map ~f:(fun anchor ->
        Option.bind (Json.string_field "sha" anchor) ~f:Commit.make)
  in
  {
    t with
    legacy_revisions =
      List.dedup_and_sort ~compare:Commit.compare
        (t.legacy_revisions @ revisions
        @ Option.to_list (Option.bind (Json.string revision) ~f:Commit.make));
  }

let operation t = t.active

let stronger_policy left right =
  match (left, right) with
  | Preserve_ancestry, _ | _, Preserve_ancestry -> Preserve_ancestry
  | Rewrite, Rewrite -> Rewrite

let strategy_policy op =
  match op.local_action with
  | Integrate_source policy -> policy
  | Prepare_remote_replay _ -> Rewrite
  | Publish_source -> op.intent.policy

let execution_policy op =
  stronger_policy op.preservation_policy
    (stronger_policy op.intent.policy (strategy_policy op))

let ancestry_requirements op =
  match execution_policy op with
  | Rewrite -> []
  | Preserve_ancestry ->
      List.dedup_and_sort ~compare:Commit.compare
        (Option.to_list op.source @ op.recovery_revisions
        @
        match op.local_action with
        | Publish_source -> []
        | Integrate_source _ | Prepare_remote_replay _ ->
            Option.to_list op.target)

let materialization t = t.materialized
let publications t = t.publications

let publication_identity (receipt : publication_receipt) =
  { operation_id = receipt.operation_id; revision = receipt.published_revision }

let unobserved_publication t =
  Option.filter (List.hd t.publications) ~f:(fun receipt ->
      not
        (Option.equal equal_publication_identity t.observed_publication
           (Some (publication_identity receipt))))

let integrations t = t.integrations
let remote_integrations t = t.remote_integrations

let required_revisions t =
  let option = Option.to_list in
  let boundary = function
    | Recorded sha | Inferred sha | Subject_inferred sha | Patch_equivalent sha
      ->
        [ sha ]
    | Reconstructed { original; upstream } -> [ original; upstream ]
    | Plain -> []
  in
  let intent { base = _; policy = _; purpose } =
    match purpose with
    | Integrate_revision { contributor = _; revision }
    | Publish_revision revision ->
        [ revision ]
    | Provision_checkout _ | Reconcile_base | Reconcile_request _
    | Reconcile_scoped _ | Publish_session _ | Verify_publication ->
        []
  in
  let merge { head; target; parents; sequencer = _; resumed_sequencer = _ } =
    head :: target :: parents
  in
  let replay { preserved; incoming; upstream } =
    [ preserved; incoming; upstream ]
  in
  let replay_request { preserved; incoming; boundaries } =
    preserved :: incoming :: List.concat_map boundaries ~f:boundary
  in
  let repair
      {
        mode;
        head;
        sequencer = _;
        conflicts = _;
        attempts_without_progress = _;
        attempt_completed = _;
        attempt_started = _;
        last_turn = _;
      } =
    option head
    @
    match mode with
    | Diagnosis _ | Content_repair -> []
    | History_recovery { baseline; _ } -> option baseline
  in
  let capture
      {
        base_branch = _;
        source_revision;
        target_revision;
        replay_boundary;
        integration_policy = _;
      } =
    source_revision :: target_revision :: boundary replay_boundary
  in
  let integration
      {
        operation_id = _;
        capture = captured;
        integrated_revision;
        evidence = _;
      } =
    integrated_revision :: capture captured
  in
  let command { token = _; kind } =
    match kind with
    | Observe | Inspect | Verify_recovery -> []
    | Pin { source; target } -> [ source; target ]
    | Integrate { source; target; boundary = b; policy = _ } ->
        source :: target :: boundary b
    | Commit_merge completed -> merge completed
    | Plan_remote_replay request -> replay_request request
    | Checkout_remote captured -> replay captured
    | Continue { head; target; sequencer = _ } -> [ head; target ]
    | Publish { candidate; expected } -> candidate :: option expected
    | Confirm candidate -> [ candidate ]
  in
  let operation
      {
        id = _;
        intent = desired;
        local_action;
        preservation_policy = _;
        integration;
        remote_integration;
        reobserve_after_completion = _;
        destination = _;
        source;
        target;
        boundary = b;
        boundary_candidates;
        remote_replay;
        candidate;
        expected;
        recovery_revisions;
        phase;
        pending;
        command_sequence = _;
        failures = _;
        deterministic_failures = _;
        observation_failures = _;
        publication_recovery_attempts = _;
        repair = repairing;
        merge_progress;
      } =
    intent desired @ option source @ option target @ option candidate
    @ option expected @ boundary b
    @ List.concat_map boundary_candidates ~f:boundary
    @ recovery_revisions
    @ List.concat_map (option integration) ~f:capture
    @ List.concat_map (option remote_integration) ~f:capture
    @ List.concat_map (option remote_replay) ~f:replay
    @ List.concat_map (option pending) ~f:command
    @ List.concat_map (option repairing) ~f:repair
    @ (match local_action with
      | Publish_source | Integrate_source _ -> []
      | Prepare_remote_replay request -> replay_request request)
    @ (match phase with
      | Repairing repairing -> repair repairing
      | Preparing | Integrating | Publishing | Confirming | Waiting _
      | Recovering | Settled | Intervention _ ->
          [])
    @
    match merge_progress with
    | None -> []
    | Some (Pending_merge captured) -> merge captured
    | Some (Completed_merge { capture = captured; head }) ->
        head :: merge captured
  in
  let {
    worktree_hook = _;
    forge_observations = _;
    legacy_revisions;
    legacy_publication_base = _;
    desired;
    materialized;
    next_operation = _;
    active;
    integrations;
    remote_integrations;
    publications;
    observed_publication;
  } =
    t
  in
  List.dedup_and_sort ~compare:Commit.compare
    (legacy_revisions
    @ List.concat_map (option desired) ~f:intent
    @ List.map (option materialized) ~f:materialization_head
    @ List.concat_map (option active) ~f:operation
    @ List.concat_map (integrations @ remote_integrations) ~f:integration
    @ List.concat_map publications
        ~f:(fun { operation_id = _; published_intent; published_revision } ->
          published_revision :: intent published_intent)
    @ List.map (option observed_publication)
        ~f:(fun { operation_id = _; revision } -> revision))

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
      (equal_policy (execution_policy op) Rewrite
      && equal_phase op.phase Publishing
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
    | ( Integrate_source Rewrite,
        ( Inferred _ | Reconstructed _ | Patch_equivalent _ | Subject_inferred _
        | Plain ) ) ->
        None

let initial_publication_candidate (op : operation) ~remote =
  if
    equal_purpose op.intent.purpose Verify_publication
    || Option.is_some op.expected || Option.is_none op.source
  then None
  else
    Option.filter op.candidate ~f:(fun candidate ->
        Option.equal Commit.equal remote (Some candidate))

let initial_tracking_edits ~branch ~base ~fetch_destination ~push_destination
    ~remotes ~merges =
  let remote_key = "branch." ^ branch ^ ".remote" in
  let merge_key = "branch." ^ branch ^ ".merge" in
  let target = "refs/heads/" ^ branch in
  if
    String.is_empty branch
    || String.is_empty fetch_destination
    || (not (String.equal fetch_destination push_destination))
    || (not
          (List.is_empty remotes || List.equal String.equal remotes [ "origin" ]))
    || not
         (List.is_empty merges
         || List.equal String.equal merges [ target ]
         || List.equal String.equal merges [ "refs/heads/" ^ base ])
  then None
  else
    Some
      ((if List.is_empty remotes then [ (remote_key, "origin") ] else [])
      @
      if List.equal String.equal merges [ target ] then []
      else [ (merge_key, target) ])

let publication_destination urls =
  match List.dedup_and_sort urls ~compare:String.compare with
  | [ url ] when not (String.is_empty (String.strip url)) -> Ok url
  | [] | [ _ ] -> Error "missing_push_destination"
  | _ :: _ :: _ -> Error "multiple_push_destinations"

let check_observation_destination (op : operation) ~observed =
  match op.destination with
  | Bound_remote expected when Remote_id.equal expected observed -> Ok ()
  | Bound_remote _ -> Error "publication_destination_changed"
  | Unobserved_remote | Reconfirm_remote _ ->
      Error "publication_destination_unconfirmed"

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

let publication_status t =
  match t.active with
  | Some { phase = Settled; candidate = Some _; _ } -> `Published
  | Some { phase = Settled; candidate = None; _ } -> `No_work
  | None
  | Some
      {
        phase =
          ( Preparing | Integrating | Repairing _ | Publishing | Confirming
          | Waiting _ | Recovering | Intervention _ );
        _;
      } ->
      `Pending

let is_unsettled t =
  Worktree_hook.is_pending t.worktree_hook
  ||
  match phase t with
  | None | Some Settled -> false
  | Some
      ( Preparing | Integrating | Repairing _ | Publishing | Confirming
      | Waiting _ | Recovering | Intervention _ ) ->
      true

let publication_observation_target t =
  let pending =
    if is_unsettled t then
      Option.bind t.active ~f:(fun op ->
          Option.map op.candidate ~f:(fun revision ->
              { operation_id = op.id; revision }))
    else None
  in
  match pending with
  | Some _ -> pending
  | None -> Option.map (List.hd t.publications) ~f:publication_identity

let publication_observation_pending t =
  Option.filter (publication_observation_target t) ~f:(fun publication ->
      not
        (Option.equal equal_publication_identity t.observed_publication
           (Some publication)))

let retains_publication_observation t publication =
  List.exists t.publications ~f:(fun receipt ->
      equal_publication_identity publication (publication_identity receipt))
  || is_unsettled t
     && Option.equal equal_publication_identity
          (publication_observation_target t)
          (Some publication)

let can_observe_publication t ~publication ~head ~remote =
  Option.equal equal_publication_identity
    (publication_observation_target t)
    (Some publication)
  && (Commit.equal head publication.revision
     || Option.equal Commit.equal remote (Some head)
        && List.exists t.publications ~f:(fun receipt ->
            equal_publication_identity publication
              (publication_identity receipt)))

let forge_conflict t ~scope =
  Option.exists
    (Forge_observation.latest_fact t.forge_observations)
    ~f:Forge_observation.revisions_confirmed
  && Option.exists
       (Forge_observation.conflict_for_scope t.forge_observations scope)
       ~f:(fun fact ->
         match
           (Commit.make fact.Forge_observation.head, Commit.make fact.base)
         with
         | Some head, Some base ->
             let publication_allows_head =
               match publication_observation_pending t with
               | None -> true
               | Some publication ->
                   let remote =
                     Option.bind
                       (Forge_observation.latest_fact t.forge_observations)
                       ~f:(fun latest ->
                         Option.bind latest.confirmed_head ~f:Commit.make)
                   in
                   can_observe_publication t ~publication ~head ~remote
             in
             publication_allows_head
             && not
                  (conflict_satisfied t ~head ~base
                     ~base_branch:fact.base_branch)
         | None, _ | _, None -> false)

let has_conflict t ~scope =
  is_unsettled t
  && Option.exists t.active ~f:(fun op ->
      Option.exists op.repair ~f:(fun repair -> repair.conflicts > 0))
  || forge_conflict t ~scope

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
  | (( Recorded sha
     | Inferred sha
     | Patch_equivalent sha
     | Subject_inferred sha
     | Reconstructed { upstream = sha; original = _ } ) as boundary)
    :: rest -> (
      match List.Assoc.find evidence sha ~equal:Commit.equal with
      | Some true -> Chosen boundary
      | Some false -> choose_boundary ~candidates:rest ~evidence
      | None -> Probe sha)

let observation_boundaries (op : operation) =
  if Option.is_none op.source then op.boundary_candidates else [ op.boundary ]

(* Git's right-side cherry marks are observations, not ownership proofs.
   Only a complete linear chain can identify an equivalent prefix to omit. *)
let patch_equivalent_boundary ~source text =
  let parse line =
    match String.split line ~on:'\000' with
    | [ marker; revision; parents ]
      when String.equal marker "=" || String.equal marker ">" -> (
        let parents =
          if String.is_empty parents then Some []
          else
            let parsed =
              String.split parents ~on:' ' |> List.map ~f:Commit.make
            in
            if List.for_all parsed ~f:Option.is_some then
              Some (List.filter_opt parsed)
            else None
        in
        match (Commit.make revision, parents) with
        | Some revision, Some parents ->
            Ok (revision, parents, String.equal marker "=")
        | None, _ | _, None -> Error "malformed patch-equivalence commit")
    | _ -> Error "malformed patch-equivalence observation"
  in
  if String.is_empty text then Ok None
  else if not (String.is_suffix text ~suffix:"\n") then
    Error "incomplete patch-equivalence observation"
  else
    let lines = String.split_lines text in
    match Result.all (List.map lines ~f:parse) with
    | Error _ as error -> error
    | Ok commits ->
        let rec linear expected = function
          | [] -> true
          | (revision, [ parent ], _) :: rest ->
              Commit.equal revision expected && linear parent rest
          | (_, ([] | _ :: _ :: _), _) :: _ -> false
        in
        let rec prefix skipped = function
          | [] -> if skipped then Some source else None
          | (_, _, true) :: rest -> prefix true rest
          | (_, [ parent ], false) :: _ -> if skipped then Some parent else None
          | (_, ([] | _ :: _ :: _), false) :: _ -> None
        in
        Ok
          (if linear source commits then prefix false (List.rev commits)
           else None)

let reconstructed_boundary ~source ~recorded ~history =
  let parse line =
    match String.split line ~on:'\000' with
    | [ revision; parents; tree ] -> (
        let parents =
          if String.is_empty parents then [] else String.split parents ~on:' '
        in
        match (Commit.make revision, Commit.make tree) with
        | Some revision, Some tree
          when List.for_all parents ~f:(fun parent ->
                   Option.is_some (Commit.make parent)) ->
            Ok (revision, List.hd parents, tree)
        | Some _, Some _ | None, _ | _, None ->
            Error "invalid reconstruction object")
    | _ -> Error "malformed reconstruction history"
  in
  if List.is_empty recorded then Ok None
  else if not (String.is_suffix history ~suffix:"\n") then
    Error "incomplete reconstruction history"
  else if
    List.exists recorded ~f:(fun (_, tree) -> Option.is_none (Commit.make tree))
  then Error "invalid recorded boundary tree"
  else
    match Result.all (List.map (String.split_lines history) ~f:parse) with
    | Error _ as error -> error
    | Ok commits ->
        let rec connected expected = function
          | [] -> false
          | [ (revision, None, _) ] ->
              String.equal expected (Commit.to_string revision)
          | (revision, Some parent, _) :: rest ->
              String.equal expected (Commit.to_string revision)
              && connected parent rest
          | (_, None, _) :: _ :: _ -> false
        in
        if not (connected (Commit.to_string source) commits) then
          Error "incomplete or disconnected reconstruction topology"
        else
          Ok
            (List.find_map recorded ~f:(fun (original, tree) ->
                 List.find_map commits ~f:(fun (upstream, _, observed_tree) ->
                     if String.equal tree (Commit.to_string observed_tree) then
                       Some (Reconstructed { original; upstream })
                     else None)))

let subject_scope = function
  | Reconcile_scoped { project; ancestors; request = _ }
    when (not (String.is_empty project)) && not (List.is_empty ancestors) ->
      Some (project, ancestors)
  | Provision_checkout _ | Reconcile_scoped _ | Reconcile_request _
  | Reconcile_base | Integrate_revision _ | Publish_revision _
  | Publish_session _ | Verify_publication ->
      None

let subject_boundary ~source ~project ~ancestors text =
  if not (String.is_empty text || String.is_suffix text ~suffix:"\n") then
    Error "incomplete subject observation"
  else
    let marked =
      List.map (String.split_lines text) ~f:(fun line ->
          match String.split line ~on:'\000' with
          | [ revision; parents; subject ] ->
              let dependency =
                Worktree_parser.is_ancestor_patch_subject ~project_name:project
                  ~ancestor_ids:ancestors subject
              in
              Ok
                (String.concat ~sep:"\000"
                   [ (if dependency then "=" else ">"); revision; parents ]
                ^ "\n")
          | _ -> Error "malformed subject observation")
    in
    match Result.all marked with
    | Error _ as error -> error
    | Ok records -> patch_equivalent_boundary ~source (String.concat records)

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
  let boundary_candidates =
    if equal_purpose intent.purpose Verify_publication then []
    else recorded_boundaries t
  in
  let op =
    {
      id = t.next_operation;
      intent;
      local_action =
        (match intent.purpose with
        | Integrate_revision _ -> Integrate_source Preserve_ancestry
        | Provision_checkout _ | Reconcile_base | Reconcile_request _
        | Reconcile_scoped _ ->
            Integrate_source intent.policy
        | Publish_revision _ | Publish_session _ | Verify_publication ->
            Publish_source);
      preservation_policy = intent.policy;
      integration = None;
      remote_integration = None;
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
      deterministic_failures = 0;
      observation_failures = 0;
      publication_recovery_attempts = 0;
      repair = None;
      merge_progress = None;
    }
  in
  issue { t with next_operation = t.next_operation + 1 } op Preparing Observe

let start_publication_verification t base =
  let intent =
    { base; policy = Preserve_ancestry; purpose = Verify_publication }
  in
  start { t with desired = Some intent; legacy_publication_base = None } intent

let import_legacy_publication t ~base =
  if not (List.is_empty t.publications) then t
  else
    match t.active with
    | None -> fst (start_publication_verification t base)
    | Some op when equal_phase op.phase Settled ->
        fst (start_publication_verification t base)
    | Some op ->
        if
          equal_purpose op.intent.purpose Verify_publication
          || Option.is_some t.legacy_publication_base
        then t
        else { t with legacy_publication_base = Some base }

let stop_after_recovery t op reason =
  ( {
      t with
      active = Some { op with phase = Intervention reason; pending = None };
    },
    [] )

let diagnose t (op : operation) reason =
  let reason =
    if String.is_empty (String.strip reason) then
      "reconciliation_context_unavailable"
    else reason
  in
  let previous =
    Option.filter op.repair ~f:(fun r ->
        match r.mode with
        | Diagnosis _ -> true
        | Content_repair | History_recovery _ -> false)
  in
  let attempts =
    Option.value_map previous ~default:0 ~f:(fun r ->
        r.attempts_without_progress + if r.attempt_completed then 1 else 0)
  in
  let r =
    {
      mode = Diagnosis { reason };
      head = op.source;
      sequencer = "diagnosis";
      conflicts = 0;
      attempts_without_progress = Int.min 2 attempts;
      attempt_completed = false;
      attempt_started = false;
      last_turn = Option.bind previous ~f:(fun r -> r.last_turn);
    }
  in
  let module P = Reconcile_plan in
  match
    P.next
      (P.for_obligation ~authority:P.Observe_only P.Preserve_work
         (P.required ~recovery_failures:attempts P.Agent_required))
  with
  | P.Hold _ ->
      stop_after_recovery t
        { op with repair = Some { r with attempt_completed = true } }
        reason
  | P.Run_agent _ ->
      let sequence = op.command_sequence + 1 in
      let token = { operation = op.id; command = sequence } in
      ( {
          t with
          active =
            Some
              {
                op with
                repair = Some r;
                phase = Repairing r;
                pending = None;
                command_sequence = sequence;
              };
        },
        [ Repair token ] )
  | P.Execute _ | P.Settled | P.Wait | P.Observe | P.Verify_agent _ -> (t, [])

let recover_with_agent ?(task = Reconstruct_history) t (op : operation) reason =
  match (op.source, op.target, op.repair) with
  | Some source, Some _, (None | Some { mode = Content_repair | Diagnosis _; _ })
    ->
      let repair =
        {
          mode = History_recovery { reason; baseline = None; task };
          head = Some source;
          sequencer = "history-recovery";
          conflicts = 0;
          attempts_without_progress = 0;
          attempt_completed = false;
          attempt_started = false;
          last_turn = None;
        }
      in
      issue t { op with repair = Some repair } Recovering Verify_recovery
  | Some _, Some _, Some { mode = History_recovery _; _ } ->
      issue t op Recovering Verify_recovery
  | None, _, _ | _, None, _ -> issue t op Recovering Inspect

let inspection op =
  match op.repair with
  | Some { mode = History_recovery _; _ } -> Verify_recovery
  | Some { mode = Content_repair | Diagnosis _; _ } | None -> Inspect

let finite_time at = if Float.is_finite at then Float.max 0. at else 0.

let retry ?(attempt_failed = false) t op at reason retry_after =
  let op =
    if attempt_failed then
      {
        op with
        deterministic_failures = Int.min 2 (op.deterministic_failures + 1);
      }
    else op
  in
  let reason =
    if String.is_empty reason then "reconciliation_retry" else reason
  in
  let failures = Int.min 30 (op.failures + 1) in
  let delay =
    Float.min 300. (5. *. Float.(2. ** of_int (Int.min 6 op.failures)))
  in
  let delay =
    Option.value_map retry_after ~default:delay ~f:(fun d ->
        if Float.is_finite d then Float.max delay d else delay)
  in
  let obligation =
    if Option.is_some op.candidate then Reconcile_plan.Publish
    else Reconcile_plan.Integrate
  in
  let required =
    Reconcile_plan.required ~deterministic_failures:op.deterministic_failures
      Reconcile_plan.Deterministic
  in
  let plan : Reconcile_plan.t =
    {
      authority =
        (if
           equal_purpose op.intent.purpose Verify_publication
           || is_provisioning op.intent.purpose
         then Reconcile_plan.Observe_only
         else Reconcile_plan.Manage);
      execution = Reconcile_plan.Available;
      preserve_work =
        (if Option.is_some op.source && Option.is_some op.target then
           Reconcile_plan.Satisfied
         else Reconcile_plan.Unobserved);
      finish_work = Reconcile_plan.Satisfied;
      integrate =
        (if Reconcile_plan.equal_obligation obligation Reconcile_plan.Integrate
         then required
         else Reconcile_plan.Satisfied);
      publish =
        (if Reconcile_plan.equal_obligation obligation Reconcile_plan.Publish
         then required
         else Reconcile_plan.Satisfied);
    }
  in
  if
    Reconcile_plan.equal_decision (Reconcile_plan.next plan)
      (Reconcile_plan.Run_agent (obligation, Reconcile_plan.Full_recovery))
    && attempt_failed
    &&
    match op.repair with
    | None | Some { mode = Content_repair | Diagnosis _; _ } -> true
    | Some { mode = History_recovery _; _ } -> false
  then
    recover_with_agent
      ~task:
        (if Option.is_some op.candidate then Repair_publication
         else Reconstruct_history)
      t { op with failures } reason
  else
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

(* Unavailable evidence must not wait forever either. Observation attempts
   have their own route to diagnosis and never consume a mutation budget. *)
let retry_observation t (op : operation) at reason retry_after =
  let module P = Reconcile_plan in
  let observation_failures = Int.min 2 (op.observation_failures + 1) in
  let op = { op with observation_failures } in
  let plan =
    P.for_obligation ~authority:P.Observe_only P.Preserve_work
      (P.required ~deterministic_failures:observation_failures
         P.Observe_evidence)
  in
  match P.next plan with
  | P.Run_agent _ ->
      diagnose t { op with failures = Int.min 30 (op.failures + 1) } reason
  | P.Execute _ | P.Settled | P.Wait | P.Observe | P.Verify_agent _ | P.Hold _
    ->
      retry t op at reason retry_after

let settle t op =
  let t =
    {
      t with
      active = Some { op with phase = Settled; pending = None; failures = 0 };
      publications =
        (match op.candidate with
        | Some candidate
          when (not op.reobserve_after_completion)
               && not
                    (List.exists t.publications ~f:(fun receipt ->
                         receipt.operation_id = op.id
                         && Commit.equal receipt.published_revision candidate))
          ->
            {
              operation_id = op.id;
              published_intent = op.intent;
              published_revision = candidate;
            }
            :: t.publications
        | Some _ | None -> t.publications);
    }
  in
  let t =
    if List.is_empty t.publications then t
    else { t with legacy_publication_base = None }
  in
  match t.desired with
  | Some intent
    when op.reobserve_after_completion || not (equal_intent intent op.intent) ->
      let t, effects = start t intent in
      (t, Completed op.id :: effects)
  | _ -> (
      match t.legacy_publication_base with
      | None -> (t, [ Completed op.id ])
      | Some base ->
          let t, effects = start_publication_verification t base in
          (t, Completed op.id :: effects))

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
      head = Some head;
      sequencer;
      conflicts = Int.max 0 conflicts;
      attempts_without_progress;
      attempt_completed = false;
      attempt_started = false;
      last_turn = Option.bind previous ~f:(fun r -> r.last_turn);
    }
  in
  let module P = Reconcile_plan in
  let task, readiness =
    match mode with
    | Diagnosis _ ->
        ( P.Preserve_work,
          P.required ~recovery_failures:attempts_without_progress
            P.Agent_required )
    | Content_repair ->
        ( P.Integrate,
          P.required ~content_failures:attempts_without_progress
            P.Content_repair )
    | History_recovery { task; _ } ->
        ( (match task with
          | Finish_local_work -> P.Finish_work
          | Reconstruct_history -> P.Integrate
          | Repair_publication -> P.Publish),
          P.required
            ~recovery_failures:
              (match task with
              | Repair_publication ->
                  attempts_without_progress + op.publication_recovery_attempts
              | Finish_local_work | Reconstruct_history ->
                  attempts_without_progress)
            P.Agent_required )
  in
  match P.next (P.for_obligation ~authority:P.Manage task readiness) with
  | P.Run_agent (_, P.Full_recovery) when equal_repair_mode mode Content_repair
    ->
      recover_with_agent t op "repair_no_structural_progress"
  | P.Hold P.Recovery_budget_exhausted ->
      let repair =
        Option.map previous ~f:(fun previous ->
            { previous with attempts_without_progress })
      in
      stop_after_recovery t { op with repair } "recovery_preservation_unproven"
  | P.Execute _ | P.Settled | P.Wait | P.Observe | P.Verify_agent _
  | P.Hold P.User_hold ->
      issue t op Recovering (inspection op)
  | P.Run_agent (_, (P.Content_only | P.Full_recovery | P.Diagnosis)) ->
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

let capture_candidate ?(evidence = Executed) t op candidate =
  let t =
    match op.remote_integration with
    | None -> t
    | Some capture ->
        let receipt =
          {
            operation_id = op.id;
            capture;
            integrated_revision = candidate;
            evidence;
          }
        in
        if
          List.exists t.remote_integrations ~f:(fun saved ->
              saved.operation_id = op.id
              && equal_integration_capture saved.capture capture
              && Commit.equal saved.integrated_revision candidate)
        then t
        else { t with remote_integrations = receipt :: t.remote_integrations }
  in
  let op = { op with remote_replay = None } in
  let op =
    match op.local_action with
    | Prepare_remote_replay _ ->
        { op with local_action = Integrate_source Rewrite }
    | Publish_source | Integrate_source _ -> op
  in
  let t, op = record_integration t op candidate evidence in
  ( t,
    {
      op with
      candidate = Some candidate;
      repair = None;
      merge_progress = None;
      deterministic_failures =
        (if Option.is_none op.candidate then 0 else op.deterministic_failures);
    } )

let publish ?evidence t op candidate =
  let t, op = capture_candidate ?evidence t op candidate in
  issue t op Publishing (Publish { candidate; expected = op.expected })

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
        recovery_revisions =
          List.dedup_and_sort ~compare:Commit.compare
            (op.recovery_revisions @ ancestry_requirements op);
        remote_integration =
          Some
            {
              base_branch = None;
              source_revision = incoming;
              target_revision = preserved;
              replay_boundary = boundary;
              integration_policy = execution_policy op;
            };
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

let clear_diagnosis (op : operation) =
  match op.repair with
  | Some { mode = Diagnosis _; _ } -> { op with repair = None }
  | None | Some { mode = Content_repair | History_recovery _; _ } -> op

let observed ?(deterministic = true) t op (o : observation) =
  let op = clear_diagnosis op in
  if is_provisioning op.intent.purpose then
    diagnose t op "provisioning_result_mismatch"
  else
    let source =
      match op.intent.purpose with
      | Publish_revision expected -> expected
      | Provision_checkout _ | Reconcile_base | Reconcile_request _
      | Reconcile_scoped _ | Integrate_revision _ | Publish_session _
      | Verify_publication ->
          o.source
    in
    let target =
      match op.intent.purpose with
      | Integrate_revision { revision; _ } -> revision
      | Provision_checkout _ | Reconcile_base | Reconcile_request _
      | Reconcile_scoped _ | Publish_revision _ | Publish_session _
      | Verify_publication ->
          o.target
    in
    let integrating =
      match op.local_action with
      | Publish_source -> false
      | Integrate_source _ | Prepare_remote_replay _ -> true
    in
    let deterministic =
      deterministic && Option.is_none o.sequencer
      && Commit.equal source o.source
      && Commit.equal target o.target
    in
    let publication_complete =
      deterministic && (not integrating)
      && (o.base_contains_source
         || Option.is_none o.remote
            && Option.equal Commit.equal
                 (Option.bind t.materialized ~f:materialization_boundary)
                 (Some source))
    in
    let op =
      {
        op with
        destination = Bound_remote o.destination;
        source = Some source;
        target = Some target;
        expected = o.remote;
        boundary = o.boundary;
        reobserve_after_completion = integrating && Option.is_some o.sequencer;
        integration =
          (match op.intent.purpose with
          | Provision_checkout _ | Reconcile_base | Reconcile_request _
          | Reconcile_scoped _ ->
              Some
                {
                  base_branch =
                    (if Option.is_some o.sequencer then None
                     else Some op.intent.base);
                  source_revision = source;
                  target_revision = target;
                  replay_boundary = o.boundary;
                  integration_policy = op.intent.policy;
                }
          | Integrate_revision _ | Publish_revision _ | Publish_session _
          | Verify_publication ->
              None);
      }
    in
    let module P = Reconcile_plan in
    let required =
      P.required (if deterministic then P.Deterministic else P.Agent_required)
    in
    let plan : P.t =
      {
        authority =
          (if equal_purpose op.intent.purpose Verify_publication then
             P.Observe_only
           else P.Manage);
        execution = P.Available;
        preserve_work = P.Satisfied;
        finish_work =
          (if integrating && (not o.clean) && Option.is_none o.sequencer then
             P.required P.Agent_required
           else P.Satisfied);
        integrate = (if integrating then required else P.Satisfied);
        publish = (if publication_complete then P.Satisfied else required);
      }
    in
    match P.next plan with
    | P.Settled -> settle t op
    | P.Run_agent (task, _) ->
        recover_with_agent
          ~task:
            (match task with
            | P.Finish_work -> Finish_local_work
            | P.Preserve_work | P.Integrate | P.Publish -> Reconstruct_history)
          t op "deterministic_preconditions_unmet"
    | P.Execute _ ->
        let t, op =
          if
            integrating && o.target_included
            && Option.equal Commit.equal o.remote (Some source)
          then
            let t, op = record_integration t op source Recovered_completion in
            (t, { op with candidate = Some source })
          else (t, op)
        in
        issue t op Preparing (Pin { source; target })
    | P.Hold _ -> diagnose t op "reconciliation_authority_required"
    | P.Observe | P.Verify_agent _ | P.Wait -> issue t op Recovering Inspect

let adopt_integration t op (o : observation) policy =
  match (op.intent.purpose, o.sequencer) with
  | Provision_checkout _, _ -> diagnose t op "provisioning_result_mismatch"
  | (Publish_revision _ | Publish_session _ | Verify_publication), _ ->
      observed ~deterministic:false t op o
  | ( ( Reconcile_base | Reconcile_request _ | Reconcile_scoped _
      | Integrate_revision _ ),
      None ) ->
      observed ~deterministic:false t op o
  | ( ( Reconcile_base | Reconcile_request _ | Reconcile_scoped _
      | Integrate_revision _ ),
      Some sequencer )
    when String.is_empty sequencer ->
      observed ~deterministic:false t op o
  | ( ( Reconcile_base | Reconcile_request _ | Reconcile_scoped _
      | Integrate_revision _ ),
      Some sequencer ) ->
      let repair =
        {
          mode = Content_repair;
          head = Some o.head;
          sequencer;
          conflicts = Int.max 0 o.conflicts;
          attempts_without_progress = 0;
          attempt_completed = false;
          attempt_started = false;
          last_turn = None;
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
          preservation_policy = stronger_policy (execution_policy op) policy;
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
            | None -> recover_with_agent t op "sequencer_without_pinned_target"
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
              | Some _ ->
                  recover_with_agent ~task:Finish_local_work t op
                    "dirty_worktree"
              | None -> recover_with_agent t op "remote_replay_capture_missing")
          | None -> (
              match (op.source, op.target) with
              | Some source, Some target when Commit.equal source o.source -> (
                  match op.local_action with
                  | Prepare_remote_replay request ->
                      issue t op Integrating (Plan_remote_replay request)
                  | Publish_source -> publish t op source
                  | Integrate_source policy ->
                      if not o.clean then
                        recover_with_agent ~task:Finish_local_work t op
                          "dirty_worktree"
                      else integrate t op source target op.boundary policy)
              | Some _, Some _
                when o.target_included && o.completed_integration && o.clean ->
                  publish ~evidence:Recovered_completion t op o.source
              | Some _, Some target when Commit.equal target o.target ->
                  (* The executor must prove completed integration, not merely
                 report a new local SHA. Uncertain changes retain the checkpoint. *)
                  recover_with_agent t op "uncertain_integration_outcome"
              | Some _, Some _ ->
                  recover_with_agent t op "integration_target_changed"
              | None, _ | _, None -> observed t op o)))

(* Called only after a matching inspection result has consumed its command. *)
let check_inspected_destination (op : operation) ~observed =
  match op.destination with
  | Reconfirm_remote _ when equal_phase op.phase Recovering -> Ok ()
  | Reconfirm_remote _ | Unobserved_remote | Bound_remote _ ->
      check_destination op ~observed

let recovered ?(ready = false) t op (o : observation) =
  match check_inspected_destination op ~observed:o.destination with
  | Error reason -> diagnose t op reason
  | Ok ()
    when Option.exists op.target ~f:(fun target ->
             not (Commit.equal target o.target)) ->
      recover_with_agent t op "integration_target_changed"
  | Ok () -> (
      let op =
        clear_diagnosis { op with destination = Bound_remote o.destination }
      in
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
          | Some r
            when r.attempt_started
                 && not (Option.equal Commit.equal r.head (Some o.head)) ->
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
  | Error reason -> diagnose t op reason
  | Ok () -> (
      let op = { op with destination = Bound_remote o.destination } in
      match op.repair with
      | Some
          ({ mode = History_recovery { reason; baseline; task }; _ } as recovery)
        -> (
          (* [source_preserved] proves all previously retained revisions. A
             newly observed remote adds an obligation; it does not make a
             successful repair of the captured work a failed agent turn. *)
          let captured_remote_retained =
            Option.for_all op.expected ~f:(fun expected ->
                List.mem op.recovery_revisions expected ~equal:Commit.equal)
          in
          let new_remote =
            Option.exists o.remote ~f:(fun remote ->
                not (List.mem op.recovery_revisions remote ~equal:Commit.equal))
          in
          let op =
            {
              op with
              recovery_revisions =
                List.dedup_and_sort ~compare:Commit.compare
                  ((o.head :: o.source :: Option.to_list o.remote)
                  @ Option.to_list op.expected @ op.recovery_revisions);
            }
          in
          let target_matches =
            Option.equal Commit.equal op.target (Some o.target)
          in
          let source_preserved = source_preserved && target_matches in
          let target_satisfied =
            match op.local_action with
            | Publish_source -> true
            | Integrate_source _ | Prepare_remote_replay _ -> o.target_included
          in
          let module P = Reconcile_plan in
          let plan : P.t =
            {
              authority = P.Manage;
              execution = P.Available;
              preserve_work = P.Satisfied;
              finish_work =
                (if o.clean && Option.is_none o.sequencer then P.Satisfied
                 else P.required P.Agent_required);
              integrate =
                (if not source_preserved then P.required P.Agent_required
                 else if target_satisfied then P.Satisfied
                 else
                   match task with
                   | Finish_local_work -> P.required P.Deterministic
                   | Reconstruct_history | Repair_publication ->
                       P.required P.Agent_required);
              publish =
                (if
                   Option.exists op.candidate ~f:(fun candidate ->
                       Commit.equal candidate o.source
                       && Option.equal Commit.equal o.remote (Some candidate))
                 then P.Satisfied
                 else if
                   remote_preserved
                   && ((not (equal_recovery_task task Repair_publication))
                      || recovery.attempt_completed)
                 then P.required P.Deterministic
                 else
                   P.required
                     ~recovery_failures:op.publication_recovery_attempts
                     P.Agent_required);
            }
          in
          let decision = P.next plan in
          let task =
            match decision with
            | P.Run_agent ((P.Preserve_work | P.Integrate | P.Publish), _) ->
                if equal_recovery_task task Repair_publication then task
                else Reconstruct_history
            | P.Run_agent (P.Finish_work, _)
            | P.Execute _ | P.Settled | P.Wait | P.Observe | P.Verify_agent _
            | P.Hold _ ->
                task
          in
          match decision with
          | P.Settled -> settle t op
          | P.Execute P.Integrate -> (
              match op.target with
              | Some target ->
                  integrate t
                    { op with repair = None; candidate = None }
                    o.source target op.boundary (execution_policy op)
              | None -> recover_with_agent t op "recovery_target_missing")
          | P.Execute P.Publish ->
              publish ~evidence:Verified_history t
                {
                  op with
                  expected = o.remote;
                  publication_recovery_attempts =
                    (if equal_recovery_task task Repair_publication then
                       Int.min 2
                         (op.publication_recovery_attempts
                        + recovery.attempts_without_progress + 1)
                     else op.publication_recovery_attempts);
                }
                o.source
          | P.Run_agent (P.Publish, _)
            when captured_remote_retained && new_remote
                 && (not remote_preserved)
                 && not (equal_recovery_task task Repair_publication) -> (
              let t, op =
                capture_candidate ~evidence:Verified_history t op o.source
              in
              match o.remote with
              | Some remote -> integrate_remote t op o.source remote
              | None -> recover_with_agent t op "recovery_remote_missing")
          | P.Hold _ -> stop_after_recovery t op reason
          | P.Run_agent _
          | P.Execute (P.Preserve_work | P.Finish_work)
          | P.Wait | P.Observe | P.Verify_agent _ ->
              repair
                ~mode:
                  (History_recovery
                     {
                       reason;
                       baseline = Some (Option.value baseline ~default:o.head);
                       task;
                     })
                t op o.head "history-recovery" o.conflicts)
      | Some { mode = Content_repair | Diagnosis _; _ } | None ->
          recover_with_agent t op "unexpected_recovery_verification")

let remote t (op : operation) at sha topology =
  match op.candidate with
  | None -> recover_with_agent t op "publication_without_candidate"
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
        | Some _, Equal ->
            recover_with_agent t op "contradictory_remote_observation"
        | Some _, Unproven ->
            retry_observation t op at "remote_topology_unproven" None)

let verification_result t (op : operation) at command result =
  let command_is kind = Option.exists command ~f:(equal_command_kind kind) in
  match result with
  | Retryable { reason; retry_after } ->
      retry_observation t op at reason retry_after
  | Publication_rejected rejection ->
      diagnose t op (Push_reject_classify.short_label rejection)
  | Needs_diagnosis reason | Recovery_required reason | Attempt_failed reason ->
      diagnose t op reason
  | Observed o | Inspected o -> (
      if not (command_is Observe || command_is Inspect) then
        diagnose t op "command_result_mismatch"
      else if
        (not o.clean) || Option.is_some o.sequencer
        || (not (Commit.equal o.head o.source))
        || not (Commit.equal o.target o.source)
      then diagnose t op "legacy_publication_requires_clean_checkout"
      else if not (Option.for_all op.source ~f:(Commit.equal o.source)) then
        diagnose t op "legacy_publication_source_changed"
      else
        match check_inspected_destination op ~observed:o.destination with
        | Error reason -> diagnose t op reason
        | Ok () ->
            let op =
              {
                op with
                destination = Bound_remote o.destination;
                source = Some o.source;
                target = Some o.source;
                candidate = Some o.source;
                expected = o.remote;
                recovery_revisions =
                  List.dedup_and_sort
                    (op.recovery_revisions @ Option.to_list op.expected
                   @ Option.to_list o.remote)
                    ~compare:Commit.compare;
              }
            in
            issue t op Preparing (Pin { source = o.source; target = o.source }))
  | Pinned -> (
      match op.candidate with
      | Some candidate
        when command_is (Pin { source = candidate; target = candidate }) ->
          issue t op Confirming (Confirm candidate)
      | Some _ | None -> diagnose t op "command_result_mismatch")
  | Remote { sha; topology = _ } -> (
      match op.candidate with
      | Some candidate when command_is (Confirm candidate) ->
          if Option.equal Commit.equal sha (Some candidate) then settle t op
          else diagnose t op "legacy_publication_not_confirmed"
      | Some _ | None -> diagnose t op "command_result_mismatch")
  | Checkout_ready | Observed_active _ | Observed_recovery _
  | Inspected_active _ | Integrated _ | Conflict _ | Merge_completion_needed _
  | Merge_completed _ | Remote_replay_selected _ | Remote_checked_out
  | Recovery_verified _ | Published ->
      diagnose t op "legacy_publication_requires_reconciliation"

let valid_intent (intent : intent) =
  let valid_commit sha = Option.is_some (Commit.make sha) in
  let named_base = not (String.is_empty intent.base) in
  match intent.purpose with
  | Integrate_revision { contributor; revision } ->
      (not (String.is_empty contributor))
      && valid_commit revision
      && equal_policy intent.policy Preserve_ancestry
  | Provision_checkout request -> named_base && not (String.is_empty request)
  | Reconcile_base -> named_base
  | Reconcile_scoped { request; project = _; ancestors = _ }
  | Reconcile_request request ->
      named_base && not (String.is_empty request)
  | Publish_revision revision -> valid_commit revision
  | Verify_publication -> equal_policy intent.policy Preserve_ancestry
  | Publish_session session_uuid -> not (String.is_empty session_uuid)

let interrupt_repair ~attempt_completed t token at reason =
  match t.active with
  | Some ({ phase = Repairing r; _ } as op)
    when r.attempt_started
         && equal_token token
              { operation = op.id; command = op.command_sequence } ->
      retry t
        { op with repair = Some { r with attempt_completed } }
        at reason None
  | None
  | Some
      {
        phase =
          ( Preparing | Integrating | Repairing _ | Publishing | Confirming
          | Waiting _ | Recovering | Settled | Intervention _ );
        _;
      } ->
      (t, [])

let step_transition t = function
  | Worktree_hook event ->
      if
        Option.exists t.active ~f:(fun op ->
            Option.is_some op.source
            || equal_purpose op.intent.purpose Verify_publication)
      then (t, [])
      else
        ({ t with worktree_hook = Worktree_hook.step t.worktree_hook event }, [])
  | Publication_observed { publication; head; remote } ->
      if can_observe_publication t ~publication ~head ~remote then
        ({ t with observed_publication = Some publication }, [])
      else (t, [])
  | Materialized receipt ->
      if Option.is_none t.materialized then
        let t = { t with materialized = Some receipt } in
        let boundary_candidates = recorded_boundaries t in
        let active =
          Option.map t.active ~f:(fun op ->
              if
                Option.is_none op.source
                && not (equal_purpose op.intent.purpose Verify_publication)
              then
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
        | Integrate_revision _ | Verify_publication ->
            { intent with policy = Preserve_ancestry }
        | Provision_checkout _ | Reconcile_base | Reconcile_request _
        | Reconcile_scoped _ | Publish_revision _ | Publish_session _ ->
            intent
      in
      if not (valid_intent intent) then (t, [])
      else
        let t = { t with desired = Some intent } in
        match t.active with
        | None -> start t intent
        | Some op -> (
            match op.phase with
            | Settled when not (equal_intent op.intent intent) -> start t intent
            | Preparing | Integrating | Repairing _ | Publishing | Confirming
            | Waiting _ | Recovering | Settled | Intervention _ ->
                (t, [])))
  | Reconfirm_publication -> (
      match t.active with
      | Some ({ phase = Settled; candidate = Some candidate; _ } as op) ->
          issue t op Confirming (Confirm candidate)
      | None
      | Some
          {
            phase =
              ( Preparing | Integrating | Repairing _ | Publishing | Confirming
              | Waiting _ | Recovering | Intervention _ );
            _;
          }
      | Some { phase = Settled; candidate = None; _ } ->
          (t, []))
  | Refresh o -> (
      match (t.active, t.desired) with
      | Some ({ phase = Settled; _ } as op), Some intent
        when (not (is_provisioning intent.purpose))
             && not
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
      | Some ({ phase = Intervention _; _ } as op)
        when equal_purpose op.intent.purpose Verify_publication
             && Option.exists t.desired ~f:(fun intent ->
                 (not (equal_purpose intent.purpose Verify_publication))
                 && not (is_provisioning intent.purpose)) ->
          (* Verification cannot repair an unconfirmed legacy claim. An
             explicitly resumed, newly requested operation may do so, but must
             retain every revision observed by verification. Automatic retries
             and resume without a new intent remain verification-only. *)
          let intent = Option.value t.desired ~default:op.intent in
          let recovery_revisions =
            List.dedup_and_sort ~compare:Commit.compare
              (op.recovery_revisions @ Option.to_list op.source
             @ Option.to_list op.target
              @ Option.to_list op.candidate
              @ Option.to_list op.expected)
          in
          let next, commands = start t intent in
          let active =
            Option.map next.active ~f:(fun successor ->
                {
                  successor with
                  preservation_policy =
                    stronger_policy successor.preservation_policy
                      (execution_policy op);
                  recovery_revisions;
                })
          in
          ({ next with active }, commands)
      | Some ({ phase = Intervention _; _ } as op) ->
          let t =
            { t with worktree_hook = Worktree_hook.resume t.worktree_hook }
          in
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
          let op =
            {
              op with
              repair;
              failures = 0;
              deterministic_failures = 0;
              observation_failures = 0;
              publication_recovery_attempts = 0;
              destination;
            }
          in
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
  | Repair_started token -> (
      match t.active with
      | Some ({ phase = Repairing r; _ } as op)
        when (not r.attempt_started)
             && equal_token token
                  { operation = op.id; command = op.command_sequence } ->
          let r = { r with attempt_started = true; last_turn = Some token } in
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
  | Repair_interrupted { token; at; reason } ->
      interrupt_repair ~attempt_completed:false t token at reason
  | Repair_failed { token; at; reason } ->
      interrupt_repair ~attempt_completed:true t token at reason
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
          let observation_failures =
            match result with
            | Checkout_ready | Observed _ | Observed_active _
            | Observed_recovery _ | Inspected _ | Inspected_active _
            | Recovery_verified _
            | Remote { topology = Equal | Includes | Behind | Diverged; _ } ->
                0
            | Remote { topology = Unproven; _ }
            | Pinned | Merge_completion_needed _ | Merge_completed _
            | Remote_replay_selected _ | Remote_checked_out | Integrated _
            | Conflict _ | Published | Retryable _ | Publication_rejected _
            | Attempt_failed _ | Recovery_required _ | Needs_diagnosis _ ->
                op.observation_failures
          in
          let op = { op with pending = None; observation_failures } in
          if
            equal_purpose op.intent.purpose Verify_publication
            && not (Worktree_hook.is_pending t.worktree_hook)
          then verification_result t op at command result
          else
            match (result, command) with
            | Publication_rejected rejection, Some (Publish _) -> (
                let reason =
                  Push_reject_classify.short_label rejection
                  ^ Option.value_map
                      (Push_reject_classify.detail_excerpt rejection)
                      ~default:"" ~f:(fun detail -> ": " ^ detail)
                in
                match rejection with
                | Push_reject_classify.Workflow_scope_missing
                | Permission_denied ->
                    recover_with_agent ~task:Repair_publication t op reason
                | Lease_violation | Merge_queue_locked ->
                    retry ~attempt_failed:true t op at reason None
                | Branch_protection | Push_pattern_block | Hook_failure _
                | Local_state_unsafe _ | Unknown _ ->
                    recover_with_agent ~task:Repair_publication t op reason)
            | Publication_rejected _, _ ->
                diagnose t op "command_result_mismatch"
            | Attempt_failed reason, _ ->
                retry ~attempt_failed:true t op at reason None
            | Retryable { reason; retry_after }, _ ->
                retry_observation t op at reason retry_after
            | Needs_diagnosis reason, _ -> diagnose t op reason
            | _, _ when Worktree_hook.is_pending t.worktree_hook ->
                diagnose t op "worktree_create_hook_incomplete"
            | Checkout_ready, Some (Observe | Inspect)
              when is_provisioning op.intent.purpose ->
                settle t op
            | _, _ when is_provisioning op.intent.purpose ->
                diagnose t op "provisioning_result_mismatch"
            | ( Recovery_required "remote_work_not_incorporated",
                Some (Publish { candidate; expected = Some incoming }) )
              when Option.is_none op.remote_integration ->
                (* A captured remote can predate publication, base integration,
                 or contributor integration. Give all of them the same
                 deterministic recovery as a remote race. The successor keeps
                 its attempt capture through publication and restart, so a
                 failed preservation check cannot repeat this integration. *)
                integrate_remote t op candidate incoming
            | Recovery_required reason, Some Verify_recovery ->
                retry_observation t op at reason None
            | Recovery_required reason, _ -> recover_with_agent t op reason
            | Observed_recovery { observation; policy }, Some (Observe | Inspect)
              when Option.is_none op.source ->
                observed ~deterministic:false t
                  {
                    op with
                    preservation_policy =
                      stronger_policy (execution_policy op) policy;
                  }
                  observation
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
                        (String.equal capture.sequencer
                           capture.resumed_sequencer) ->
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
                        merge_progress =
                          Some (Completed_merge { capture; head });
                      }
                      Integrating
                      (Continue
                         {
                           head;
                           target = capture.target;
                           sequencer = capture.resumed_sequencer;
                         })
                | None | Some (Completed_merge _) ->
                    recover_with_agent t op "unexpected_merge_completion")
            | Pinned, Some (Pin { target; _ })
              when op.reobserve_after_completion -> (
                match op.repair with
                | Some { head = Some head; conflicts = 0; sequencer; _ } ->
                    issue t op Integrating
                      (Continue { head; target; sequencer })
                | Some { head = Some head; sequencer; conflicts; _ } ->
                    repair t op head sequencer conflicts
                | Some { head = None; _ } ->
                    diagnose t op "repair_head_unavailable"
                | None -> recover_with_agent t op "missing_adopted_repair")
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
                | Recorded upstream
                | Inferred upstream
                | Patch_equivalent upstream
                | Subject_inferred upstream
                | Reconstructed { upstream; original = _ } ->
                    if
                      match boundary with
                      | Recorded _ ->
                          not
                            (List.mem request.boundaries boundary
                               ~equal:equal_boundary)
                      | Inferred _ | Reconstructed _ | Patch_equivalent _
                      | Subject_inferred _ | Plain ->
                          false
                    then
                      recover_with_agent t op
                        "unrecorded_remote_replay_boundary"
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
                          remote_integration =
                            Option.map op.remote_integration ~f:(fun capture ->
                                { capture with replay_boundary = boundary });
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
                         && String.equal c.capture.resumed_sequencer sequencer
                    ->
                      op.merge_progress
                  | None | Some (Pending_merge _ | Completed_merge _) -> None
                in
                repair t { op with merge_progress } head sequencer conflicts
            | Published, Some (Publish { candidate; _ }) ->
                issue t op Confirming (Confirm candidate)
            | Remote { sha; topology }, Some (Confirm _) ->
                remote t op at sha topology
            | ( ( Checkout_ready | Observed _ | Observed_active _
                | Observed_recovery _ | Pinned | Integrated _ | Conflict _
                | Merge_completion_needed _ | Merge_completed _
                | Remote_replay_selected _ | Remote_checked_out | Inspected _
                | Inspected_active _ | Recovery_verified _ | Published
                | Remote _ ),
                ( None
                | Some
                    ( Observe | Commit_merge _ | Plan_remote_replay _
                    | Checkout_remote _ | Pin _ | Integrate _ | Inspect
                    | Verify_recovery | Continue _ | Publish _ | Confirm _ ) ) )
              ->
                diagnose t op "command_result_mismatch")
      | _ -> (t, []))

let step t event =
  let next, effects = step_transition t event in
  (* A pre-confirmation observation is scoped to its candidate. If recovery
     replaces that candidate, retain only acknowledgements backed by confirmed
     publication receipts; otherwise the new candidate needs a fresh observation. *)
  let observed_publication =
    Option.filter next.observed_publication
      ~f:(retains_publication_observation next)
  in
  ({ next with observed_publication }, effects)

let integration_result (op : operation) ~candidate ~target_preserved
    ~source_preserved =
  match (op.local_action, op.source, op.target) with
  | Integrate_source _, Some _, Some _ when not target_preserved ->
      Recovery_required "integration_target_not_preserved"
  | Integrate_source _, Some _, Some _
    when equal_policy (execution_policy op) Preserve_ancestry
         && not source_preserved ->
      Recovery_required "integration_source_not_preserved"
  | Integrate_source (Rewrite | Preserve_ancestry), Some _, Some _ ->
      Integrated candidate
  | Prepare_remote_replay _, _, _
  | Publish_source, _, _
  | Integrate_source _, None, _
  | Integrate_source _, _, None ->
      Recovery_required "integration_capture_missing"

let verification_checkout_valid ~branch ~candidate
    (checkout : Git_observation.t) =
  Git_observation.clean checkout
  && Option.equal String.equal checkout.branch (Some branch)
  && String.equal checkout.head (Commit.to_string candidate)
  && Git_observation.equal_sequencer checkout.sequencer
       Git_observation.None_active

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
      else Observed_recovery { observation = o; policy }
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
    | G.None_active ->
        if Option.equal String.equal checkout.branch (Some branch) then
          ordinary ()
        else if fresh then
          Observed_recovery { observation = o; policy = op.preservation_policy }
        else Recovery_required "checkout_branch_changed"
    | G.Rebase r -> (
        match op.intent.purpose with
        | (Publish_revision _ | Publish_session _ | Verify_publication)
          when fresh ->
            ordinary ()
        | Provision_checkout _ | Reconcile_base | Reconcile_request _
        | Reconcile_scoped _ | Integrate_revision _ | Publish_revision _
        | Publish_session _ | Verify_publication ->
            active ~policy:Rewrite ~original:r.original ~target:r.target
              ~identity:
                (Option.is_none checkout.branch
                && String.equal r.head_ref ("refs/heads/" ^ branch)))
    | G.Merge r -> (
        match op.intent.purpose with
        | (Publish_revision _ | Publish_session _ | Verify_publication)
          when fresh ->
            ordinary ()
        | Provision_checkout _ | Reconcile_base | Reconcile_request _
        | Reconcile_scoped _ | Integrate_revision _ | Publish_revision _
        | Publish_session _ | Verify_publication ->
            active ~policy:Preserve_ancestry ~original:checkout.head
              ~target:r.target
              ~identity:
                (Option.equal String.equal checkout.branch (Some branch)))
    | G.Cherry_pick _ ->
        if fresh then
          Observed_recovery { observation = o; policy = Preserve_ancestry }
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
  let legacy_policy =
    Option.is_none
      (Option.bind (Json.field "active" json)
         ~f:(Json.field "preservation_policy"))
  in
  let valid_commit sha = Option.is_some (Commit.make sha) in
  let optional = Option.for_all ~f:valid_commit in
  let valid_boundary = function
    | Recorded sha | Inferred sha | Patch_equivalent sha | Subject_inferred sha
      ->
        valid_commit sha
    | Reconstructed { original; upstream } ->
        valid_commit original && valid_commit upstream
    | Plain -> true
  in
  let valid_capture (capture : integration_capture) =
    valid_commit capture.source_revision
    && valid_commit capture.target_revision
    && valid_boundary capture.replay_boundary
    && Option.for_all capture.base_branch ~f:(fun base ->
        not (String.is_empty base))
  in
  let valid_repair (r : repair) =
    optional r.head
    && (not (String.is_empty r.sequencer))
    && r.conflicts >= 0
    && r.attempts_without_progress >= 0
    && r.attempts_without_progress <= 2
    && Option.for_all r.last_turn ~f:(fun token ->
        token.operation > 0 && token.command > 0)
    &&
    match r.mode with
    | Diagnosis { reason } ->
        (not (String.is_empty (String.strip reason)))
        && String.equal r.sequencer "diagnosis"
    | Content_repair -> Option.is_some r.head
    | History_recovery { reason; baseline; _ } ->
        Option.is_some r.head && optional baseline
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
    | Repairing r -> valid_repair r && r.attempts_without_progress < 2
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
        | Some { mode = Content_repair | Diagnosis _; _ } | None -> false)
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
  let decoded =
    match Json.try_of_yojson t_of_yojson json with
    | Error message -> Error message
    | Ok t -> (
        let t =
          {
            t with
            active =
              Option.map t.active ~f:(fun op ->
                  if Option.is_some op.remote_integration then op
                  else
                    let capture source_revision target_revision replay_boundary
                        =
                      Some
                        {
                          base_branch = None;
                          source_revision;
                          target_revision;
                          replay_boundary;
                          integration_policy = Rewrite;
                        }
                    in
                    let remote_integration =
                      match (op.remote_replay, op.local_action) with
                      | Some replay, _ ->
                          capture replay.incoming replay.preserved op.boundary
                      | None, Prepare_remote_replay request ->
                          capture request.incoming request.preserved op.boundary
                      | None, (Publish_source | Integrate_source _) ->
                          List.find_map t.remote_integrations ~f:(fun receipt ->
                              if receipt.operation_id = op.id then
                                Some receipt.capture
                              else None)
                    in
                    { op with remote_integration });
          }
        in
        let legacy_downgrade =
          legacy_policy
          && Option.exists t.active ~f:(fun op ->
              equal_policy op.intent.policy Preserve_ancestry
              && equal_policy (strategy_policy op) Rewrite)
        in
        let t =
          if not legacy_policy then t
          else
            {
              t with
              active =
                Option.map t.active ~f:(fun op ->
                    let preservation_policy = execution_policy op in
                    let recovery_revisions =
                      if not legacy_downgrade then op.recovery_revisions
                      else
                        let receipts = t.integrations @ t.remote_integrations in
                        let rec collect seen accumulated = function
                          | [] ->
                              List.dedup_and_sort ~compare:Commit.compare
                                accumulated
                          | revision :: rest ->
                              let key = Commit.to_string revision in
                              if Set.mem seen key then
                                collect seen accumulated rest
                              else
                                let predecessors =
                                  List.concat_map receipts ~f:(fun receipt ->
                                      if
                                        receipt.operation_id <= op.id
                                        && Commit.equal
                                             receipt.integrated_revision
                                             revision
                                      then
                                        [
                                          receipt.capture.source_revision;
                                          receipt.capture.target_revision;
                                        ]
                                      else [])
                                in
                                collect (Set.add seen key)
                                  (revision :: accumulated)
                                  (List.rev_append predecessors rest)
                        in
                        collect
                          (Set.empty (module String))
                          []
                          (op.recovery_revisions @ Option.to_list op.source
                         @ Option.to_list op.target
                          @ Option.to_list op.candidate
                          @ Option.to_list op.expected)
                    in
                    { op with preservation_policy; recovery_revisions });
            }
        in
        if
          t.next_operation < 1
          || (not (Worktree_hook.valid t.worktree_hook))
          || (not (Forge_observation.history_valid t.forge_observations))
          || (not (List.for_all t.legacy_revisions ~f:valid_commit))
          || (not
                (Option.for_all t.materialized ~f:(fun m ->
                     valid_commit (materialization_head m))))
          || (not (Option.for_all t.desired ~f:valid_intent))
          || (not
                (List.for_all t.publications
                   ~f:(fun (r : publication_receipt) ->
                     r.operation_id > 0
                     && r.operation_id < t.next_operation
                     && valid_intent r.published_intent
                     && (not (is_provisioning r.published_intent.purpose))
                     && valid_commit r.published_revision)))
          || (not
                (Option.for_all t.observed_publication
                   ~f:(retains_publication_observation t)))
          || not
               (List.for_all (t.integrations @ t.remote_integrations)
                  ~f:(fun r ->
                    r.operation_id > 0
                    && r.operation_id < t.next_operation
                    && valid_capture r.capture
                    && valid_commit r.integrated_revision))
        then Error "invalid branch checkpoint"
        else
          match t.active with
          | None -> Ok t
          | Some op
            when op.id > 0 && op.id < t.next_operation
                 && op.command_sequence >= 0
                 && equal_policy op.preservation_policy (execution_policy op)
                 && Option.for_all op.pending ~f:(fun command ->
                     command_allowed_for_purpose op.intent.purpose command.kind)
                 && ((not (equal_purpose op.intent.purpose Verify_publication))
                    || equal_local_action op.local_action Publish_source
                       && Option.is_none op.integration
                       && Option.is_none op.remote_integration
                       && Option.is_none op.remote_replay
                       && Option.for_all op.repair ~f:(fun r ->
                           match r.mode with
                           | Diagnosis _ -> true
                           | Content_repair | History_recovery _ -> false)
                       && Option.is_none op.merge_progress)
                 && Option.for_all op.integration ~f:valid_capture
                 && Option.for_all op.remote_integration ~f:(fun capture ->
                     valid_capture capture
                     && Option.is_none capture.base_branch
                     && Option.is_none op.integration
                     && equal_boundary capture.replay_boundary op.boundary
                     && equal_policy capture.integration_policy
                          (strategy_policy op)
                     &&
                     match op.local_action with
                     | Publish_source -> false
                     | Prepare_remote_replay _ | Integrate_source Rewrite ->
                         Option.equal Commit.equal op.source
                           (Some capture.source_revision)
                         && Option.equal Commit.equal op.target
                              (Some capture.target_revision)
                     | Integrate_source Preserve_ancestry ->
                         Option.equal Commit.equal op.source
                           (Some capture.target_revision)
                         && Option.equal Commit.equal op.target
                              (Some capture.source_revision))
                 && ((not (is_provisioning op.intent.purpose))
                    || Option.is_none op.source && Option.is_none op.target
                       && Option.is_none op.candidate
                       && Option.is_none op.expected
                       && Option.is_none op.integration
                       && Option.is_none op.remote_integration
                       && Option.is_none op.remote_replay
                       && Option.for_all op.repair ~f:(fun r ->
                           match r.mode with
                           | Diagnosis _ -> true
                           | Content_repair | History_recovery _ -> false)
                       && Option.is_none op.merge_progress
                       && Option.for_all op.pending ~f:(fun command ->
                           match command.kind with
                           | Observe | Inspect -> true
                           | Commit_merge _ | Plan_remote_replay _
                           | Checkout_remote _ | Pin _ | Integrate _
                           | Verify_recovery | Continue _ | Publish _
                           | Confirm _ ->
                               false))
                 && ((not op.reobserve_after_completion)
                    || Option.is_some op.source && Option.is_some op.target
                       &&
                       match op.local_action with
                       | Integrate_source _ -> true
                       | Publish_source | Prepare_remote_replay _ -> false)
                 && valid_intent op.intent && op.failures >= 0
                 && op.failures <= 30
                 && op.observation_failures >= 0
                 && op.observation_failures <= 2
                 && op.deterministic_failures >= 0
                 && op.deterministic_failures <= 2
                 && op.publication_recovery_attempts >= 0
                 && op.publication_recovery_attempts <= 2
                 && optional op.source && optional op.target
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
                       | Recorded sha
                       | Inferred sha
                       | Patch_equivalent sha
                       | Subject_inferred sha
                       | Reconstructed { upstream = sha; original = _ } ->
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
                 && Option.for_all op.repair ~f:(fun r ->
                     valid_repair r
                     && (r.attempts_without_progress < 2
                        || r.attempt_completed
                           &&
                           match op.phase with
                           | Intervention _ | Recovering | Waiting _ -> true
                           | Preparing | Integrating | Repairing _ | Publishing
                           | Confirming | Settled ->
                               false)
                     && Option.for_all r.last_turn ~f:(fun token ->
                         token.operation = op.id
                         && token.command <= op.command_sequence))
                 && Option.for_all op.pending ~f:(fun c ->
                     c.token.operation = op.id
                     && c.token.command = op.command_sequence
                     && valid_kind c.kind) ->
              if
                legacy_downgrade
                && equal_phase op.phase Settled
                && Option.is_some op.candidate
              then
                if op.command_sequence = Int.max_value then
                  Error
                    "legacy policy checkpoint has exhausted command identities"
                else
                  let t =
                    {
                      t with
                      publications =
                        List.filter t.publications ~f:(fun receipt ->
                            receipt.operation_id <> op.id);
                    }
                  in
                  let t =
                    {
                      t with
                      observed_publication =
                        Option.filter t.observed_publication
                          ~f:(retains_publication_observation t);
                    }
                  in
                  Ok (fst (issue t op Recovering Inspect))
              else Ok t
          | Some _ -> Error "invalid branch operation")
  in
  Result.map decoded ~f:(fun t ->
      match t.active with
      | Some ({ phase = Intervention _; _ } as op)
        when op.command_sequence < Int.max_value
             && op.publication_recovery_attempts < 2
             && not
                  (Option.exists op.repair ~f:(fun r ->
                       match r.mode with
                       | History_recovery _ | Diagnosis _ ->
                           r.attempts_without_progress >= 2
                       | Content_repair -> false)) ->
          (* Older checkpoints stored a diagnostic as the entire recovery decision.
           Re-evaluate every unexhausted stop, including intermediate checkpoints
           that already contain planning counters. Captured destination authority
           and exhausted agent budgets remain binding. *)
          fst (issue t op Recovering (inspection op))
      | None
      | Some
          {
            phase =
              ( Preparing | Integrating | Repairing _ | Publishing | Confirming
              | Waiting _ | Recovering | Settled | Intervention _ );
            _;
          } ->
          t)

let recovery_name_hex s =
  String.to_list s
  |> List.map ~f:(fun c -> Printf.sprintf "%02x" (Char.to_int c))
  |> String.concat

let recovery_project_prefix ~project =
  "refs/onton/reconcile/p" ^ recovery_name_hex project ^ "/"

let recovery_prefix ~project ~branch =
  recovery_project_prefix ~project ^ "b" ^ recovery_name_hex branch

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
  head : Commit.t option;
  mode : repair_mode;
  prompt : string;
}

let repair_turn t ~branch token =
  match t.active with
  | Some
      ({
         phase = Repairing r;
         id;
         command_sequence;
         source;
         target;
         candidate;
         expected;
         recovery_revisions;
         remote_integration;
         boundary;
         _;
       } as op)
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
            (Printf.sprintf "Repair turn: %d/%d\n" token.operation token.command
            ^ (match execution_policy op with
              | Rewrite -> "Preservation policy: verified rewrites permitted.\n"
              | Preserve_ancestry ->
                  "Preservation policy: retain required commits as ancestors; \
                   patch equivalence is insufficient.\n")
            ^ Option.value_map remote_integration
                ~default:"Remote integration attempt: none recorded\n"
                ~f:(fun capture ->
                  Printf.sprintf "Remote integration attempt: %s into %s (%s)\n"
                    (Commit.to_string capture.source_revision)
                    (Commit.to_string capture.target_revision)
                    (match capture.integration_policy with
                    | Rewrite -> "rewrite"
                    | Preserve_ancestry -> "preserve ancestry"))
            ^
            match r.mode with
            | Diagnosis { reason } ->
                Printf.sprintf
                  "Diagnose why the managed operation cannot proceed: %s.\n\
                   Managed branch: %s\n\
                   Captured source: %s\n\
                   Captured target: %s\n\
                   This turn is observation-only, including when pending \
                   guidance asks for edits. Inspect available evidence and \
                   explain the cause and the minimum authorized next step. Do \
                   not modify Git, files, configuration, credentials, \
                   permissions, hooks or repository policies. Do not publish, \
                   rerun creation hooks, or treat a legacy publication claim \
                   as write authorization. Onton will independently re-observe \
                   after this turn.\n"
                  reason branch (sha source) (sha target)
            | History_recovery { reason; baseline; task } ->
                (match task with
                  | Finish_local_work ->
                      "First finish the interrupted local work: inspect the \
                       edits, complete the intended changes and checks, and \
                       commit them. Onton will then integrate the pinned base.\n"
                  | Repair_publication ->
                      "Repair the local content or history responsible for the \
                       publication rejection below. Preserve all required \
                       work. Do not disable hooks, checks or branch \
                       protections, change credentials or permission scopes, \
                       change the destination, or push. Do not remove required \
                       work to evade authorization. If the rejection requires \
                       authority you do not have, explain the required \
                       external action. Onton will verify preservation and \
                       retry publication.\n"
                  | Reconstruct_history ->
                      "Deterministic integration needs recovery. Complete the \
                       integration while preserving the retained history.\n")
                ^ Printf.sprintf
                    "Recover managed branch %s after deterministic \
                     reconciliation could not proceed: %s.\n\
                     Preserved source: %s\n\
                     Pinned target: %s\n\
                     Preserved candidate: %s\n\
                     Captured remote: %s\n\
                     Replay boundary evidence: %s\n\
                     Recovery starting revision: %s\n\
                     Additional retained revisions: %s\n\
                     Inspect the checkout, index, sequencer, reflogs and \
                     project recovery refs. Preserve all valid patch and \
                     remote work. You may resolve, stage, commit, and finish \
                     or reconstruct integration against the pinned target. Do \
                     not discard uncommitted work or delete recovery refs. Do \
                     not push. Leave a clean checkout of the managed branch \
                     with no active sequencer. Onton will independently verify \
                     preserved history and publish the captured candidate with \
                     an explicit remote lease. If preservation cannot be \
                     established, leave the work intact and explain the \
                     uncertainty.\n"
                    branch reason (sha source) (sha target) (sha candidate)
                    (sha expected)
                    (match boundary with
                    | Recorded sha -> "recorded " ^ Commit.to_string sha
                    | Reconstructed { original; upstream } ->
                        "best-effort tree reconstruction "
                        ^ Commit.to_string original ^ " -> "
                        ^ Commit.to_string upstream
                    | Subject_inferred sha ->
                        "best-effort dependency subject " ^ Commit.to_string sha
                    | Patch_equivalent sha ->
                        "best-effort patch equivalence " ^ Commit.to_string sha
                    | Inferred sha ->
                        "best-effort inference " ^ Commit.to_string sha
                    | Plain -> "plain rebase; no proven ownership boundary")
                    (sha baseline)
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

let recovery_prompt ~context ~guidance (turn : repair_turn) =
  context
  ^ (if List.is_empty guidance then ""
     else
       "\nPending human guidance (retained for normal task delivery):\n"
       ^ String.concat ~sep:"\n\n" (List.rev guidance))
  ^ "\nOwned recovery instructions:\n" ^ turn.prompt

let repair_head_matches (turn : repair_turn) head =
  match turn.mode with
  | Diagnosis _ -> true
  | Content_repair | History_recovery _ ->
      Option.equal Commit.equal turn.head head

let repair_event_accepted = function
  | Types.Stream_event.Turn_started | Text_delta _ | Tool_use _ | Final_result _
    ->
      true
  | Types.Stream_event.Error _ | Session_init _ -> false

let repair_result ~(turn : repair_turn) ~turn_accepted ~at ~before_head
    ~after_head ~timed_out ~final_result ~detail =
  let token = turn.token in
  match (before_head, after_head) with
  | _, _
    when (match turn.mode with
           | Diagnosis _ -> true
           | Content_repair | History_recovery _ -> false)
         && not (Option.equal Commit.equal before_head after_head) ->
      Repair_failed { token; at; reason = "diagnostic_checkout_changed" }
  | Some head, _
    when (match turn.mode with
           | Diagnosis _ -> false
           | Content_repair | History_recovery _ -> true)
         && not (Option.equal Commit.equal turn.head (Some head)) ->
      Repair_invalidated { token; reason = "unexpected_agent_head_change" }
  | _, Some head
    when equal_repair_mode turn.mode Content_repair
         && not (Option.equal Commit.equal turn.head (Some head)) ->
      Repair_invalidated { token; reason = "unexpected_agent_head_change" }
  | _, _
    when (match turn.mode with
           | Diagnosis _ -> true
           | Content_repair | History_recovery _ -> false)
         && (not timed_out) && final_result ->
      Repair_completed { token; at }
  | Some _, Some _ when (not timed_out) && final_result ->
      Repair_completed { token; at }
  | Some _, Some _ | None, _ | _, None ->
      let reason =
        if String.is_empty (String.strip detail) then
          "repair_session_interrupted"
        else detail
      in
      if turn_accepted then Repair_failed { token; at; reason }
      else Repair_interrupted { token; at; reason }

let stopped_integration_result (checkout : Git_observation.t) ~detail =
  let retry reason = Retryable { reason; retry_after = None } in
  if not checkout.valid then retry "invalid_checkout_observation"
  else
    match (checkout.sequencer, Commit.make checkout.head) with
    | Git_observation.None_active, _
      when String.is_substring detail
             ~substring:"refusing to merge unrelated histories" ->
        Recovery_required "unrelated_histories"
    | Git_observation.None_active, _ ->
        Attempt_failed
          (if String.is_empty detail then "integration_sequencer_missing"
           else detail)
    | _, None -> retry "invalid_checkout_head"
    | ( ( Git_observation.Rebase _ | Git_observation.Merge _
        | Git_observation.Cherry_pick _ ),
        Some head ) ->
        if List.is_empty checkout.conflicts && List.is_empty checkout.unstaged
        then
          Attempt_failed
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
              | Diagnosis _ -> "diagnosis"
              | Content_repair -> "content repair"
              | History_recovery _ -> "history recovery"),
              match r.mode with
              | Diagnosis { reason } -> Some reason
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
        ( "Deterministic failed attempts",
          Int.to_string op.deterministic_failures );
        ( "Publication recovery attempts",
          Int.to_string op.publication_recovery_attempts );
        ( "Preservation policy",
          match execution_policy op with
          | Rewrite -> "rewrite permitted"
          | Preserve_ancestry -> "preserve ancestry" );
        ( "Forge observation",
          match publication_observation_pending t with
          | Some publication ->
              Printf.sprintf "waiting for %d/%s" publication.operation_id
                (Commit.to_string publication.revision)
          | None ->
              if Option.is_some (publication_observation_target t) then
                "acknowledged"
              else "none" );
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
          | Reconstructed { original; upstream } ->
              "tree reconstruction (best effort) " ^ Commit.to_string original
              ^ " -> " ^ Commit.to_string upstream
          | Subject_inferred sha ->
              "dependency subject (best effort) " ^ Commit.to_string sha
          | Patch_equivalent sha ->
              "patch-equivalent (best effort) " ^ Commit.to_string sha
          | Inferred sha -> "inferred " ^ Commit.to_string sha
          | Plain -> "plain (no proven boundary)" );
      ]
      @ Option.value_map op.remote_integration ~default:[] ~f:(fun capture ->
          [
            ( "Remote integration",
              Printf.sprintf "%s into %s (%s)"
                (Commit.to_string capture.source_revision)
                (Commit.to_string capture.target_revision)
                (match capture.integration_policy with
                | Rewrite -> "rewrite"
                | Preserve_ancestry -> "preserve ancestry") );
          ])
      @ Option.value_map op.repair ~default:[] ~f:(fun r ->
          [
            ( "Repair mode",
              match r.mode with
              | Diagnosis _ -> "diagnosis"
              | Content_repair -> "content repair"
              | History_recovery _ -> "history recovery" );
            ( "Last repair turn",
              Option.value_map r.last_turn ~default:"none" ~f:(fun token ->
                  Printf.sprintf "%d/%d" token.operation token.command) );
            ( "Repair completion",
              if r.attempt_completed then "completed"
              else if r.attempt_started then "started"
              else "awaiting claim" );
            ( "Repair no-progress count",
              Int.to_string r.attempts_without_progress );
          ])
      @ Option.to_list (Option.map reason ~f:(fun reason -> ("Reason", reason)))
      @
      match op.phase with
      | Waiting { until; _ } -> [ ("Retry at", Float.to_string until) ]
      | Preparing | Integrating | Repairing _ | Publishing | Confirming
      | Recovering | Settled | Intervention _ ->
          [])

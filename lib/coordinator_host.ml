(* @archlint.module state
   @archlint.domain coordinator-state *)

open Base
open Types

[@@@warning "-4"]

type error = Store of Coordinator_store.error | Invalid of string

type transition =
  | Applied of Coordinator_state.outcome list
  | Rejected of string

let ( let* ) value f = Result.bind value ~f

type t = {
  store : Coordinator_store.t;
  next_write_id : unit -> string;
  mutable current : Coordinator_store.envelope;
  mutable pending : (string * string) option;
  mutable closed_state : Coordinator_state.t option;
}

let restore ~store ~next_write_id =
  match Coordinator_store.load store with
  | Error e -> Error (Store e)
  | Ok current ->
      Ok { store; next_write_id; current; pending = None; closed_state = None }

let authority t = Coordinator_store.authority t.store

let refresh t =
  match Coordinator_store.load t.store with
  | Ok current
    when String.equal
           (Coordinator_store.encode current)
           (Coordinator_store.encode t.current) ->
      Ok ()
  | Ok _ -> Error (Invalid "resource effect changed opaque coordinator state")
  | Error error -> Error (Store error)

let resource_request t (request : Resource_protocol.request) =
  if
    not
      (String.equal request.Resource_protocol.run_id
         (Coordinator_store.state t.current |> Coordinator_state.run_id))
  then Error (Invalid "resource request belongs to another run")
  else
    let* () = refresh t in
    let run, coordinator, epoch, revision = authority t in
    if Int64.equal revision Int64.max_value then
      Error (Invalid "resource revision exhausted")
    else
      try
        match Yojson.Safe.from_string request.Resource_protocol.bytes with
        | `Assoc fields -> (
            let fields =
              List.filter fields ~f:(fun (k, _) ->
                  not
                    (List.mem
                       [ "run_id"; "coordinator_id"; "owner_epoch"; "revision" ]
                       k ~equal:String.equal))
            in
            match
              Resource_protocol.make_request
                (Yojson.Safe.to_string
                   (`Assoc
                      (fields
                      @ [
                          ("run_id", `String run);
                          ("coordinator_id", `String coordinator);
                          ("owner_epoch", `String (Int64.to_string epoch));
                          ( "revision",
                            `String (Int64.to_string (Int64.succ revision)) );
                        ])))
            with
            | Ok request -> Ok request
            | Error reason -> Error (Invalid reason))
        | _ -> Error (Invalid "invalid resource request")
      with _ -> Error (Invalid "invalid resource request")

let envelope t = t.current

let state t =
  Option.value t.closed_state ~default:(Coordinator_store.state t.current)

let commit_envelope ?(emergency = false) t candidate outcomes =
  if Option.is_some t.closed_state then
    Error (Invalid "run is terminally closed")
  else
    let bytes = Coordinator_store.encode candidate in
    if String.length bytes > (8 * 1024 * 1024) - if emergency then 0 else 65536
    then Error (Invalid "run envelope reserve exhausted")
    else
      let id =
        match t.pending with
        | Some (id, old) when String.equal old bytes -> Ok id
        | Some _ -> Error (Invalid "previous transition unresolved")
        | None ->
            let id = t.next_write_id () in
            t.pending <- Some (id, bytes);
            Ok id
      in
      match id with
      | Error e -> Error e
      | Ok id -> (
          match
            Coordinator_store.commit ~emergency t.store
              ~expected_version:(Coordinator_store.version t.current)
              ~write_id:id candidate
          with
          | Error e -> Error (Store e)
          | Ok receipt ->
              if
                not
                  (Int64.equal receipt.state_version
                     Int64.(succ (Coordinator_store.version t.current)))
              then Error (Invalid "unexpected write receipt")
              else (
                (* No mutable coordinator intent, ACK or resource effect is visible
           before current bytes and the receipt have been confirmed. *)
                t.current <- Coordinator_store.committed candidate receipt;
                t.pending <- None;
                Ok (Applied outcomes)))

let commit t event next outcomes =
  commit_envelope
    ~emergency:(Coordinator_state.emergency_event event)
    t
    (Coordinator_store.with_state t.current next)
    outcomes

let apply t event =
  let previous = state t in
  let next, outcomes = Coordinator_state.step previous event in
  if
    List.exists outcomes ~f:(function
      | Coordinator_state.Rejected _ -> true
      | _ -> false)
  then
    match outcomes with
    | Coordinator_state.Rejected reason :: _ -> Ok (Rejected reason)
    | _ -> Error (Invalid "inconsistent rejection")
  else if
    String.equal
      (Coordinator_state.encode previous)
      (Coordinator_state.encode next)
  then Ok (Applied outcomes)
  else commit t event next outcomes

let resource_binding run_id id bytes =
  match Resource_protocol.make_request bytes with
  | Error _ -> false
  | Ok request when not (String.equal request.run_id run_id) -> false
  | Ok request -> (
      match request.operation with
      | Resource_protocol.Launch | Replace -> (
          try
            match Yojson.Safe.from_string bytes with
            | `Assoc fields -> (
                match
                  List.Assoc.find fields "resource_id" ~equal:String.equal
                with
                | Some (`String resource) -> String.equal resource id
                | _ -> false)
            | _ -> false
          with _ -> false)
      | Stop | Close -> String.equal request.request_id id
      | _ -> false)

let transaction t events =
  let rec collect state outcomes = function
    | [] ->
        commit_envelope
          ~emergency:(List.for_all events ~f:Coordinator_state.emergency_event)
          t
          (Coordinator_store.with_state t.current state)
          outcomes
    | ( Coordinator_state.Owner_receipt _ | Advance_receipt _
      | Result_committed _ | Worker_result _ | Cleanup_confirmed _
      | Stop_confirmed _ | Resource_stopped _ | Input_consumed _
      | Cancel_cleanup_confirmed | Terminal_patch _ )
      :: _ ->
        Error (Invalid "external evidence requires a bound submission")
    | Coordinator_state.Resource_request (id, bytes) :: _
      when not (resource_binding (Coordinator_state.run_id state) id bytes) ->
        Error (Invalid "resource request identity or schema")
    | event :: rest -> (
        let next, effects = Coordinator_state.step state event in
        match
          List.find_map effects ~f:(function
            | Coordinator_state.Rejected s -> Some s
            | _ -> None)
        with
        | Some reason -> Ok (Rejected reason)
        | None -> collect next (outcomes @ effects) rest)
  in
  collect (state t) [] events

let submit t event =
  match event with
  | Coordinator_state.Resource_request (id, bytes)
    when not (resource_binding (Coordinator_state.run_id (state t)) id bytes) ->
      Error (Invalid "resource request identity or schema")
  | Coordinator_state.Owner_receipt _ | Advance_receipt _ | Result_committed _
  | Worker_result _ | Cleanup_confirmed _ | Stop_confirmed _
  | Resource_stopped _ | Input_consumed _ | Cancel_cleanup_confirmed
  | Terminal_patch _ ->
      Error (Invalid "external evidence requires a bound submission")
  | _ -> apply t event

let terminal_from_forge t ~patch ~ticket =
  let current =
    List.find
      (Coordinator_state.controls (state t))
      ~f:(fun c ->
        String.equal c.Coordinator_state.kind "forge_ticket"
        && Option.equal String.equal c.patch_id
             (Some (Patch_id.to_string patch)))
  in
  match
    ( current,
      Coordinator_journal.find
        (Coordinator_state.journal (state t))
        ("observation:" ^ ticket) )
  with
  | Some current, Some { phase = Applied bytes; _ }
    when String.equal current.id ticket -> (
      match Coordinator_observation.decode bytes with
      | Ok { state = Some pr; _ } when Pr_state.merged pr ->
          apply t (Coordinator_state.Terminal_patch patch)
      | _ ->
          Error
            (Invalid "terminal patch requires current committed merge evidence")
      )
  | _ -> Error (Invalid "terminal forge evidence missing or superseded")

let cancel_closed t ~(close_request : Resource_protocol.request)
    ~(response : Resource_protocol.response) =
  let state = state t in
  let persisted =
    Option.value_map
      (List.Assoc.find
         (Coordinator_state.resources state)
         close_request.request_id ~equal:String.equal)
      ~default:false
      ~f:(String.equal close_request.bytes)
  in
  let is_cancelling =
    match Coordinator_state.lifecycle state with
    | Cancelling _ -> true
    | _ -> false
  in
  if
    (not is_cancelling) || (not persisted)
    || (not
          (Poly.equal close_request.operation Resource_protocol.Close
          && Poly.equal response.status Resource_protocol.Ok))
    || not
         (Option.value_map response.run_id ~default:true
            ~f:(String.equal (Coordinator_state.run_id state)))
  then Error (Invalid "close did not prove matching terminal cleanup")
  else
    let next, outcomes =
      Coordinator_state.step state Coordinator_state.Cancel_cleanup_confirmed
    in
    if not (Coordinator_state.cancellation_complete next) then
      Error (Invalid "close preceded retirement")
    else (
      (* Close seals opaque storage. The helper's durable close tombstone is
         the settlement evidence; attempting another state-write here is wrong. *)
      t.closed_state <- Some next;
      Ok (Applied outcomes))

let retired t ~resource_id ~(stop_request : Resource_protocol.request) ~response
    =
  let matches =
    Option.value_map response.Resource_protocol.resource_id ~default:true
      ~f:(String.equal resource_id)
    && Option.value_map response.run_id ~default:true
         ~f:(String.equal (Coordinator_state.run_id (state t)))
    &&
      try
        match Yojson.Safe.from_string stop_request.bytes with
        | `Assoc fields -> (
            match List.Assoc.find fields "resource_id" ~equal:String.equal with
            | Some (`String id) -> String.equal id resource_id
            | _ -> false)
        | _ -> false
      with _ -> false
  in
  if
    (not
       (Resource_protocol.retirement_evidence stop_request.operation
          ~process_exit:0 ~matching:matches response))
    || not
         (Option.value_map
            (List.Assoc.find
               (Coordinator_state.resources (state t))
               stop_request.request_id ~equal:String.equal)
            ~default:false
            ~f:(String.equal stop_request.bytes))
  then Error (Invalid "stop did not prove matching durable retirement")
  else
    match
      List.Assoc.find
        (Coordinator_state.resources (state t))
        resource_id ~equal:String.equal
    with
    | None -> Error (Invalid "unknown resource")
    | Some launch ->
        apply t (Coordinator_state.Resource_stopped (resource_id, launch))

let invalid reason = Error (Invalid reason)
let protocol = function Ok x -> Ok x | Error e -> invalid e
let stream t assignment = Coordinator_store.stream t.current assignment

let owner t assignment =
  match
    List.find
      (Coordinator_state.assignments (state t))
      ~f:(fun a -> String.equal a.id assignment)
  with
  | Some a -> Ok a
  | None -> invalid "unknown assignment"

let live_owner t (a : Coordinator_state.assignment) =
  (not a.retired)
  && (not
        (List.mem
           (Coordinator_state.lost_resources (state t))
           a.resource ~equal:String.equal))
  && not
       (List.exists
          (Coordinator_state.assignments (state t))
          ~f:(fun other ->
            (not (String.equal other.id a.id))
            && Patch_id.equal other.patch a.patch
            && other.reserved && (not other.retired) && not other.cleanup))

let check_scope t (a : Coordinator_state.assignment)
    (identity : Worker_protocol.identity) =
  if
    String.equal identity.run_id (Coordinator_state.run_id (state t))
    && String.equal identity.patch_id (Patch_id.to_string a.patch)
  then Ok ()
  else invalid "worker scope does not match committed assignment"

let check_capsule ~gameplan ~main_branch stream bytes version =
  let* capsule =
    protocol
      (try Worker_checkpoint.decode (Yojson.Safe.from_string bytes)
       with _ -> Error "malformed capsule")
  in
  match Worker_stream.binding stream with
  | Some (Worker_protocol_v3.Bind (Worker_protocol.Bind b, context)) ->
      let* _ =
        protocol
          (Worker_checkpoint.project capsule ~run_id:b.identity.run_id
             ~patch_id:(Patch_id.of_string b.identity.patch_id)
             ~context:b.context ~state_version:version
             ~attention_id:(Worker_checkpoint.attention_id capsule)
             ~gameplan ~main_branch)
      in
      if
        not
          (Poly.equal
             (Worker_checkpoint.v3_context capsule)
             (Some (Worker_protocol_v3.context_bytes context)))
      then invalid "capsule execution context differs from bound context"
      else Ok capsule
  | _ -> invalid "missing stream binding"

let step_state state event =
  let next, outcomes = Coordinator_state.step state event in
  match
    List.find_map outcomes ~f:(function
      | Coordinator_state.Rejected e -> Some e
      | _ -> None)
  with
  | Some e -> invalid e
  | None -> Ok (next, outcomes)

let publish_stream ?(emergency = false) t assignment stream next outcomes frames
    =
  let candidate =
    Coordinator_store.with_stream
      (Coordinator_store.with_state t.current next)
      assignment stream
  in
  if
    String.equal
      (Coordinator_store.encode candidate)
      (Coordinator_store.encode t.current)
  then
    if Option.is_none t.pending || List.is_empty frames then
      Ok (outcomes, frames)
    else invalid "previous transition unresolved"
  else
    let* transition = commit_envelope ~emergency t candidate outcomes in
    match transition with
    | Applied outcomes -> Ok (outcomes, frames)
    | Rejected reason -> invalid reason

let command t ~assignment ~frame =
  let* a = owner t assignment in
  if not (live_owner t a) then invalid "lost, retired or superseded assignment"
  else
    let* message = protocol (Worker_protocol_v3.decode_host frame) in
    let previous, inherited_assignment =
      match stream t assignment with
      | Some s -> (s, None)
      | None -> (
          match message with
          | Worker_protocol_v3.Bind (Worker_protocol.Bind b, _)
            when Worker_protocol.admission_is_fresh b.admission ->
              (Worker_stream.initial, None)
          | _ -> (
              match
                List.find_map
                  (Coordinator_state.assignments (state t))
                  ~f:(fun prior ->
                    if
                      String.equal prior.resource a.resource
                      && Patch_id.equal prior.patch a.patch
                      && prior.cleanup && not prior.retired
                    then
                      match stream t prior.id with
                      | Some s when Worker_stream.acknowledged s ->
                          Some (prior.id, s)
                      | _ -> None
                    else None)
              with
              | Some (id, s) -> (s, Some id)
              | None -> (Worker_stream.initial, None)))
    in
    let* () =
      match message with
      | Worker_protocol_v3.Bind (Worker_protocol.Bind b, _context)
        when not (Worker_protocol.admission_is_fresh b.admission) ->
          invalid "command only accepts a fresh bind; use recover/rebind"
      | Worker_protocol_v3.Bind (Worker_protocol.Bind b, context) ->
          let* () = check_scope t a b.identity in
          let json =
            Yojson.Safe.from_string (Worker_protocol_v3.context_bytes context)
          in
          let field name =
            match json with
            | `Assoc fields -> List.Assoc.find fields name ~equal:String.equal
            | _ -> None
          in
          if
            (not
               (Poly.equal (field "config_id")
                  (Some (`String (Coordinator_state.config_id (state t))))))
            || not
                 (Poly.equal (field "plan_id")
                    (Some (`String (Coordinator_state.plan_id (state t)))))
          then invalid "bind configuration/plan mismatch"
          else if
            List.exists t.current.streams ~f:(fun (id, stream) ->
                if String.equal id assignment then false
                else
                  match Worker_stream.identity stream with
                  | Some identity
                    when String.equal identity.patch_id b.identity.patch_id ->
                      not
                        (List.exists
                           (Coordinator_state.assignments (state t))
                           ~f:(fun prior ->
                             String.equal prior.id id && prior.cleanup
                             && prior.result_committed
                             && Worker_stream.acknowledged stream))
                  | Some _ | None -> false)
          then
            invalid
              "patch has committed protocol history; recover instead of fresh \
               bind"
          else if not (String.equal b.capsule "{}") then
            (* Only [recover] may introduce a retained capsule/receipt pair. *)
            invalid
              "initial stream must start fresh; recover from committed stream"
          else if not (Int64.equal b.state_version 0L) then
            invalid "fresh bind version"
          else Ok ()
      | Execute e ->
          let* () = check_scope t a e.request.identity in
          if
            not
              (String.equal e.request.operation.operation_id a.operation
              && String.equal e.request.operation.attempt_id a.request)
          then invalid "execute identity differs from committed reservation"
          else if a.cleanup || a.result_committed then
            invalid "settled assignment cannot execute"
          else if
            List.exists
              (Coordinator_state.operations (state t))
              ~f:(fun op ->
                String.equal op.id a.operation
                && op.maintenance
                && not (Poly.equal e.work Worker_protocol_v3.Reconcile))
          then invalid "maintenance cannot execute a coding turn"
          else Ok ()
      | Input (identity, input) ->
          let* () = check_scope t a identity in
          let evidence_backed =
            List.for_all input.evidence ~f:(fun evidence ->
                match
                  Coordinator_journal.find
                    (Coordinator_state.journal (state t))
                    ("observation:"
                   ^ evidence.Coordinator_observation.scope.ticket)
                with
                | Some { phase = Applied bytes; _ } ->
                    String.equal bytes (Coordinator_observation.encode evidence)
                | _ -> false)
          in
          let controls_backed =
            List.for_all input.controls ~f:(fun (kind, value) ->
                List.exists
                  (Coordinator_state.controls (state t))
                  ~f:(fun c ->
                    String.equal c.kind kind
                    && String.equal c.payload value
                    && Option.equal String.equal c.patch_id
                         (Some identity.patch_id)
                    && List.exists input.covered ~f:(fun (id, _) ->
                        String.equal id c.id)))
          in
          let* () =
            if evidence_backed && controls_backed then Ok ()
            else invalid "worker evidence/control lacks a committed source"
          in
          let base =
            match Worker_stream.input previous with
            | Some input -> input.selected_base
            | None -> (
                match Worker_stream.binding previous with
                | Some (Worker_protocol_v3.Bind (_, context)) ->
                    Worker_protocol_v3.selected_base context
                | _ -> "")
          in
          let* pending =
            match
              Coordinator_inputs.pending (state t) a.patch ~selected_base:base
            with
            | Ok pending -> Ok pending
            | Error reason -> invalid reason
          in
          let cumulative =
            Option.value_map pending ~default:false ~f:(Poly.equal input)
          in
          if
            cumulative
            || List.exists
                 (Coordinator_state.inputs (state t))
                 ~f:(fun (id, patch, version, payload, _) ->
                   String.equal id input.input_id
                   && String.equal patch identity.patch_id
                   && Int64.equal version input.version
                   && (String.equal payload frame
                      || List.is_empty input.evidence
                         && List.exists
                              (Coordinator_state.controls (state t))
                              ~f:(fun c ->
                                String.equal c.id input.input_id
                                &&
                                match (c.kind, input.controls) with
                                | "guidance", [] ->
                                    String.equal payload input.guidance
                                | ("automerge" | "bump"), [ (kind, value) ] ->
                                    String.equal c.kind kind
                                    && String.equal payload value
                                | _ -> false)))
          then Ok ()
          else invalid "input is not exact committed coordinator intent"
      | Legacy_control (Worker_protocol.Cancel c) ->
          let* () = check_scope t a c.identity in
          if
            Worker_stream.operation previous
            |> Option.value_map ~default:false ~f:(Poly.equal c.operation)
          then Ok ()
          else invalid "cancel does not match current protocol operation"
      | Legacy_control (Worker_protocol.Shutdown identity) ->
          check_scope t a identity
      | _ -> invalid "ACK is derived from inbound worker evidence only"
    in
    let* next, frames = protocol (Worker_stream.command previous frame) in
    let* inherited_state, inherited_outcomes =
      match inherited_assignment with
      | None -> Ok (state t, [])
      | Some predecessor -> (
          match Worker_stream.capsule previous with
          | None | Some "{}" -> Ok (state t, [])
          | Some bytes -> (
              match
                List.find
                  (Coordinator_state.capsules (state t))
                  ~f:(fun capsule ->
                    String.equal capsule.assignment predecessor
                    && String.equal capsule.bytes bytes)
              with
              | Some capsule when Worker_stream.acknowledged previous -> (
                  match Worker_stream.receipt next with
                  | Some receipt ->
                      step_state (state t)
                        (Coordinator_state.Owner_receipt
                           { capsule with assignment; receipt })
                  | None -> invalid "continued stream has no derived receipt")
              | Some _ -> invalid "predecessor receipt is not acknowledged"
              | None ->
                  invalid "stream and coordinator owner artifacts disagree"))
    in
    let* next_state, receipt_outcomes =
      match (Worker_stream.capsule next, Worker_stream.receipt next) with
      | Some bytes, Some receipt when not (String.equal bytes "{}") -> (
          match
            List.find (Coordinator_state.capsules inherited_state)
              ~f:(fun capsule ->
                String.equal capsule.assignment assignment
                && String.equal capsule.bytes bytes)
          with
          | Some capsule when String.equal capsule.receipt receipt ->
              Ok (inherited_state, [])
          | Some capsule ->
              step_state inherited_state
                (Coordinator_state.Advance_receipt
                   ({ capsule with receipt }, capsule.receipt))
          | None -> Ok (inherited_state, []))
      | _ -> Ok (inherited_state, [])
    in
    publish_stream t assignment next next_state
      (inherited_outcomes @ receipt_outcomes)
      frames

let advance_receipt state assignment stream =
  match (Worker_stream.capsule stream, Worker_stream.receipt stream) with
  | Some bytes, Some receipt -> (
      match
        List.find (Coordinator_state.capsules state) ~f:(fun c ->
            String.equal c.assignment assignment && String.equal c.bytes bytes)
      with
      | Some c when not (String.equal c.receipt receipt) ->
          step_state state
            (Coordinator_state.Advance_receipt ({ c with receipt }, c.receipt))
      | _ -> Ok (state, []))
  | _ -> invalid "missing derived capsule/receipt pair"

let receive t ~assignment ~frame ~gameplan ~main_branch ~now =
  let* a = owner t assignment in
  if not (live_owner t a) then invalid "lost, retired or superseded assignment"
  else
    let* previous =
      match stream t assignment with
      | Some s -> Ok s
      | None -> invalid "unbound stream"
    in
    let* message = protocol (Worker_protocol_v3.decode_worker frame) in
    let* next, frames = protocol (Worker_stream.receive previous frame) in
    let* next_state, outcomes =
      match message with
      | Worker_protocol.Observation m ->
          let observation_key =
            String.concat ~sep:"\000"
              [
                assignment;
                m.operation.operation_id;
                m.operation.attempt_id;
                Int64.to_string m.operation.assignment_seq;
                m.text;
              ]
          in
          let id =
            "worker-observation:"
            ^ Stdlib.Digest.to_hex (Stdlib.Digest.string observation_key)
          in
          step_state (state t)
            (Coordinator_state.Control
               {
                 id;
                 run_id = Coordinator_state.run_id (state t);
                 config_id = Coordinator_state.config_id (state t);
                 kind = "worker_observation";
                 patch_id = Some (Patch_id.to_string a.patch);
                 payload = m.text;
               })
      | Worker_protocol.Checkpoint m ->
          let* capsule =
            check_capsule ~gameplan ~main_branch previous m.capsule
              m.state_version
          in
          let consumed = Worker_checkpoint.v3_input capsule in
          let prior_input =
            match Worker_stream.capsule previous with
            | Some bytes -> (
                try
                  match
                    Worker_checkpoint.decode (Yojson.Safe.from_string bytes)
                  with
                  | Ok c -> Worker_checkpoint.v3_input c
                  | Error _ -> None
                with _ -> None)
            | None -> None
          in
          if
            not
              (Poly.equal
                 (Worker_checkpoint.latest_operation capsule)
                 (Some m.operation))
          then invalid "capsule operation does not match checkpoint"
          else if
            Option.value_map consumed ~default:false ~f:(fun input ->
                not (Worker_stream.delivered_input previous input))
            ||
            match (prior_input, consumed) with
            | Some old, Some current -> Int64.(current.version < old.version)
            | Some _, None -> true
            | None, _ -> false
          then invalid "capsule input differs from exact delivered intent"
          else
            let* receipt =
              match Worker_stream.receipt next with
              | Some r -> Ok r
              | None -> invalid "missing derived receipt"
            in
            let* next_state, outcomes =
              step_state (state t)
                (Coordinator_state.Owner_receipt
                   {
                     assignment;
                     version = m.state_version;
                     bytes = m.capsule;
                     receipt;
                   })
            in
            let* next_state, input_outcomes =
              match Worker_checkpoint.v3_input capsule with
              | None -> Ok (next_state, [])
              | Some input ->
                  let covered =
                    match input.covered with
                    | [] -> [ (input.input_id, input.version) ]
                    | xs -> xs
                  in
                  List.fold_result covered ~init:(next_state, [])
                    ~f:(fun (state, outcomes) (id, version) ->
                      let* state, effects =
                        step_state state
                          (Coordinator_state.Input_consumed
                             (assignment, id, version))
                      in
                      Ok (state, outcomes @ effects))
            in
            Ok (next_state, outcomes @ input_outcomes)
      | Result m ->
          let* bytes =
            match Worker_stream.capsule previous with
            | Some c -> Ok c
            | None -> invalid "missing terminal capsule"
          in
          let* capsule =
            check_capsule ~gameplan ~main_branch previous bytes m.state_version
          in
          if
            (not
               (Poly.equal
                  (Worker_checkpoint.latest_operation capsule)
                  (Some m.operation)))
            || not
                 (Poly.equal
                    (Worker_checkpoint.terminal capsule)
                    (Some (m.status, m.diagnostics, m.renewal_id)))
          then invalid "result differs from exact terminal capsule"
          else
            let* state, receipt_outcomes =
              advance_receipt (state t) assignment next
            in
            let* state, result_outcomes =
              step_state state
                (Coordinator_state.Worker_result
                   ( assignment,
                     a.request,
                     Worker_checkpoint.status_name m.status,
                     now ))
            in
            Ok (state, receipt_outcomes @ result_outcomes)
      | _ -> Ok (state t, [])
    in
    (* Terminal adoption is its own confirmed write. No Result ACK or cleanup
     release can escape if the subsequent post-ACK receipt write is uncertain. *)
    let* outcomes, frames =
      publish_stream t assignment next next_state outcomes frames
    in
    match message with
    | Worker_protocol.Result _ ->
        let* acknowledged, ack =
          protocol (Worker_stream.acknowledge_result next)
        in
        let* state, receipt_outcomes =
          advance_receipt (state t) assignment acknowledged
        in
        let* cleaned, cleanup_outcomes =
          step_state state
            (Coordinator_state.Cleanup_confirmed (assignment, a.request))
        in
        let* later, ack =
          publish_stream ~emergency:true t assignment acknowledged cleaned
            (receipt_outcomes @ cleanup_outcomes)
            ack
        in
        Ok (outcomes @ later, frames @ ack)
    | _ -> Ok (outcomes, frames)

let recover t ~predecessor ~assignment ~worker_id ~workspace_id =
  let* old = owner t predecessor in
  let* current = owner t assignment in
  let* previous =
    match stream t predecessor with
    | Some s -> Ok s
    | None -> invalid "missing committed predecessor stream"
  in
  if
    (not old.retired) || current.retired
    || (not (Patch_id.equal old.patch current.patch))
    || String.equal old.resource current.resource
    || (not old.cleanup)
       && not
            (String.equal old.operation current.operation
            && String.equal old.request current.request)
    || (old.cleanup && String.equal old.operation current.operation)
  then
    invalid
      "replacement does not preserve unfinished identity or predecessor \
       retirement"
  else
    let* next, frames =
      protocol (Worker_stream.rebind previous ~worker_id ~workspace_id)
    in
    let* next_state, outcomes =
      match (Worker_stream.capsule next, Worker_stream.receipt next) with
      | Some bytes, Some receipt when not (String.equal bytes "{}") ->
          let* capsule =
            protocol
              (try Worker_checkpoint.decode (Yojson.Safe.from_string bytes)
               with _ -> Error "invalid retained capsule")
          in
          step_state (state t)
            (Coordinator_state.Owner_receipt
               {
                 assignment;
                 version = Worker_checkpoint.version capsule;
                 bytes;
                 receipt;
               })
      | _ -> Ok (state t, [])
    in
    publish_stream t assignment next next_state outcomes frames

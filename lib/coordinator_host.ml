(* @archlint.module state
   @archlint.domain coordinator-host *)

open Base
open Types

[@@@warning "-4"]

type error = Store of Coordinator_store.error | Invalid of string

type transition =
  | Applied of Coordinator_state.outcome list
  | Rejected of string

type t = {
  store : Coordinator_store.t;
  next_write_id : unit -> string;
  mutable current : Coordinator_store.envelope;
  mutable pending : (string * string) option;
}

let restore ~store ~next_write_id =
  match Coordinator_store.load store with
  | Error e -> Error (Store e)
  | Ok current -> Ok { store; next_write_id; current; pending = None }

let envelope t = t.current
let state t = Coordinator_store.state t.current

let commit t next outcomes =
  let candidate = Coordinator_store.with_state t.current next in
  let bytes = Coordinator_state.encode next in
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
        Coordinator_store.commit t.store
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
  else commit t next outcomes

let resource_binding id bytes =
  match Resource_protocol.make_request bytes with
  | Error _ -> false
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
      | Stop -> String.equal request.request_id id
      | _ -> false)

let submit t event =
  match event with
  | Coordinator_state.Resource_request (id, bytes)
    when not (resource_binding id bytes) ->
      Error (Invalid "resource request identity or schema")
  | Coordinator_state.Owner_receipt _ | Result_committed _ | Cleanup_confirmed _
  | Stop_confirmed _ | Resource_stopped _ | Input_consumed _ ->
      Error (Invalid "external evidence requires a bound submission")
  | _ -> apply t event

let validate_receipt ~run_id ~patch_id ~context ~workspace_id ~version
    ~attention_id ~capsule receipt =
  try
    let admission = Yojson.Safe.from_string receipt in
    let id = Patch_id.to_string patch_id in
    let json =
      `Assoc
        [
          ("version", `Int 2);
          ("type", `String "bind");
          ("run_id", `String run_id);
          ("patch_id", `String id);
          ("worker_id", `String "coordinator-validation");
          ("workspace_id", `String workspace_id);
          ("context", `String context);
          ("capsule", `String capsule);
          ("state_version", `String (Int64.to_string version));
          ( "attention_id",
            Option.value_map attention_id ~default:`Null ~f:(fun s -> `String s)
          );
          ("admission", admission);
        ]
    in
    match Worker_protocol.decode (Yojson.Safe.to_string json ^ "\n") with
    | Error _ -> false
    | Ok binding ->
        let _, effects =
          Worker_protocol.step
            (Worker_protocol.initial ())
            (Worker_protocol.Host binding)
        in
        List.exists effects ~f:(function
          | Worker_protocol.Send (Ready _) -> true
          | _ -> false)
  with _ -> false

let checkpoint t ~assignment ~version ~capsule ~receipt ~gameplan ~main_branch
    ~context ~attention_id =
  let owner =
    List.find
      (Coordinator_state.assignments (state t))
      ~f:(fun a -> String.equal a.id assignment)
  in
  match owner with
  | None -> Error (Invalid "unknown assignment")
  | Some a -> (
      let run_id =
        match Yojson.Safe.from_string (Coordinator_state.encode (state t)) with
        | `Assoc fields -> (
            match List.Assoc.find fields "run_id" ~equal:String.equal with
            | Some (`String s) -> s
            | _ -> "")
        | _ -> ""
      in
      let decoded =
        try Worker_checkpoint.decode (Yojson.Safe.from_string capsule)
        with _ -> Error "malformed capsule"
      in
      match decoded with
      | Error e -> Error (Invalid e)
      | Ok owner_capsule -> (
          match
            Worker_checkpoint.project owner_capsule ~run_id ~patch_id:a.patch
              ~context ~state_version:version ~attention_id ~gameplan
              ~main_branch
          with
          | Error e -> Error (Invalid e)
          | Ok _ ->
              if
                not
                  (validate_receipt ~run_id ~patch_id:a.patch ~context
                     ~workspace_id:
                       (Worker_checkpoint.workspace_id owner_capsule)
                     ~version ~attention_id ~capsule receipt)
              then Error (Invalid "receipt does not bind capsule")
              else
                apply t
                  (Coordinator_state.Owner_receipt
                     { assignment; version; bytes = capsule; receipt })))

let retired t ~resource_id ~(stop_request : Resource_protocol.request) ~response
    =
  let matches =
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

let result t ~assignment ~frame =
  let assignment_record =
    List.find
      (Coordinator_state.assignments (state t))
      ~f:(fun a -> String.equal a.id assignment)
  in
  let capsule =
    Coordinator_state.capsules (state t)
    |> List.filter ~f:(fun (c : Coordinator_state.capsule) ->
        String.equal c.assignment assignment)
    |> List.max_elt ~compare:(fun (a : Coordinator_state.capsule) b ->
        Int64.compare a.version b.version)
  in
  match
    (Worker_protocol_v3.decode_worker frame, assignment_record, capsule)
  with
  | Ok (Worker_protocol.Result msg), Some a, Some c -> (
      let decoded =
        try Worker_checkpoint.decode (Yojson.Safe.from_string c.bytes)
        with _ -> Error "invalid terminal capsule"
      in
      match decoded with
      | Error e -> Error (Invalid e)
      | Ok checkpoint -> (
          match
            ( Worker_checkpoint.latest_operation checkpoint,
              Worker_checkpoint.terminal checkpoint )
          with
          | Some op, Some (status, _, renewal)
            when Poly.equal op msg.operation
                 && Poly.equal status msg.status
                 && Poly.equal renewal msg.renewal_id
                 && Int64.equal msg.state_version c.version
                 && String.equal msg.identity.patch_id
                      (Patch_id.to_string a.patch)
                 && String.equal a.id assignment ->
              apply t
                (Coordinator_state.Result_committed (assignment, a.request))
          | _ -> Error (Invalid "result not bound to terminal capsule")))
  | _ -> Error (Invalid "invalid or stale worker result")

let cleanup t ~assignment ~request =
  apply t (Coordinator_state.Cleanup_confirmed (assignment, request))

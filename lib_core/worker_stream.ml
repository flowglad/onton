(* @archlint.module core
   @archlint.domain worker-stream *)

module P = Worker_protocol
module V = Worker_protocol_v3

[@@@warning "-4"]

type action = Command of string | Receive of string | Acknowledge

type t = {
  protocol : V.t;
  identity : P.identity option;
  worker : string option;
  operation : P.operation option;
  capsule : string option;
  terminal : P.worker_message option;
  acknowledged : bool;
  accepted : P.worker_message option;
  input : V.input option;
  history : action list;
}

let initial =
  {
    protocol = V.initial ();
    identity = None;
    worker = None;
    operation = None;
    capsule = None;
    terminal = None;
    acknowledged = false;
    accepted = None;
    history = [];
    input = None;
  }

let capsule t = t.capsule

let receipt t =
  Option.map
    (fun json -> Yojson.Safe.to_string json)
    (V.admission_receipt t.protocol)

let identity t = t.identity
let operation t = t.operation

let ready t =
  List.exists
    (function
      | Receive bytes -> (
          match V.decode_worker bytes with Ok (P.Ready _) -> true | _ -> false)
      | _ -> false)
    t.history

let input t = t.input
let terminal t = t.terminal
let acknowledged t = t.acknowledged

let binding t =
  List.find_map
    (function
      | Command bytes -> (
          match V.decode_host bytes with
          | Ok (V.Bind _ as b) -> Some b
          | _ -> None)
      | _ -> None)
    t.history

let delivered_input t input =
  t.input = Some input
  || List.exists
       (function
         | Command bytes -> (
             match V.decode_host bytes with
             | Ok (V.Input (_, old)) -> old = input
             | Ok (V.Bind (P.Bind b, _)) -> (
                 try
                   match Yojson.Safe.from_string b.capsule with
                   | `Assoc fields -> (
                       match List.assoc_opt "v3_input" fields with
                       | Some json -> V.decode_input json = Ok input
                       | None -> false)
                   | _ -> false
                 with _ -> false)
             | _ -> false)
         | _ -> false)
       t.history

let remember t action = { t with history = action :: t.history }

let valid outputs =
  not
    (List.exists
       (function
         | V.Reject _ | V.Send (P.Protocol_error _) -> true | _ -> false)
       outputs)

let transition protocol event =
  let next, outputs = V.step protocol event in
  if valid outputs then Ok (next, outputs)
  else
    Error
      (Option.value
         (List.find_map
            (function
              | V.Reject why -> Some why
              | V.Send (P.Protocol_error e) -> Some e.reason
              | _ -> None)
            outputs)
         ~default:"invalid protocol transition")

let exact message outputs =
  List.exists
    (function V.Send actual -> actual = message | _ -> false)
    outputs

let control kind (identity : P.identity) (operation : P.operation) fields =
  let json =
    `Assoc
      ([
         ("version", `Int 3);
         ("type", `String kind);
         ("run_id", `String identity.run_id);
         ("patch_id", `String identity.patch_id);
         ("operation_id", `String operation.operation_id);
         ("attempt_id", `String operation.attempt_id);
         ("assignment_seq", `String (Int64.to_string operation.assignment_seq));
       ]
      @ fields)
  in
  let bytes = Yojson.Safe.to_string json ^ "\n" in
  match V.decode_host bytes with
  | Ok message -> Ok (message, bytes)
  | Error e -> Error e

let ( let* ) = Result.bind

let command t bytes =
  let* message = V.decode_host bytes in
  let duplicate_input =
    match message with V.Input (_, input) -> t.input = Some input | _ -> false
  in
  let* () =
    match message with
    | V.Bind (P.Bind _, _) when t.identity = None -> Ok ()
    | V.Execute _ | V.Input _ | V.Legacy_control (P.Cancel _ | P.Shutdown _) ->
        Ok ()
    | _ -> Error "host ACKs and rebinding require a validated barrier"
  in
  let* protocol, outputs = transition t.protocol (V.Host message) in
  let t = { t with protocol } in
  let* t =
    match message with
    | V.Bind (P.Bind b, _) ->
        let* protocol =
          if V.recovery_pending protocol then
            let* next, _ = transition protocol V.Recover_unfinished in
            Ok next
          else Ok protocol
        in
        let fields =
          try
            match Yojson.Safe.from_string b.capsule with
            | `Assoc xs -> xs
            | _ -> []
          with _ -> []
        in
        let* protocol, input =
          match List.assoc_opt "v3_input" fields with
          | None | Some `Null -> Ok (protocol, None)
          | Some (`Assoc _ as json) ->
              let* input = V.decode_input json in
              let* next, _ = transition protocol (V.Restore_input input) in
              Ok (next, Some input)
          | Some _ -> Error "invalid restored input"
        in
        let receipt = V.admission_receipt protocol in
        let entries =
          match receipt with
          | Some (`Assoc xs) -> (
              match List.assoc_opt "entries" xs with
              | Some (`List xs) -> xs
              | _ -> [])
          | _ -> []
        in
        let acknowledged =
          match List.rev entries with
          | `Assoc xs :: _ ->
              List.assoc_opt "result_acknowledged" xs = Some (`Bool true)
          | _ -> false
        in
        let latest =
          match List.rev entries with
          | `Assoc xs :: _ -> (
              match List.assoc_opt "request" xs with
              | Some json -> (
                  match P.decode (Yojson.Safe.to_string json ^ "\n") with
                  | Ok (P.Execute r) -> (
                      match V.decode_host r.guidance with
                      | Ok (V.Execute e) -> Some e
                      | _ -> None)
                  | _ -> None)
              | _ -> None)
          | _ -> None
        in
        let operation, terminal =
          match latest with
          | None -> (None, None)
          | Some e ->
              let _, outputs = V.step protocol (V.Host (V.Execute e)) in
              ( Some e.request.operation,
                List.find_map
                  (function V.Send (P.Result _ as r) -> Some r | _ -> None)
                  outputs )
        in
        Ok
          {
            t with
            protocol;
            identity = Some b.identity;
            worker = Some b.worker_id;
            capsule = Some b.capsule;
            operation;
            terminal;
            input;
            acknowledged;
          }
    | V.Execute e ->
        let accepted =
          List.find_map
            (function V.Send (P.Accepted _ as m) -> Some m | _ -> None)
            outputs
        in
        if accepted = None then Error "execution was not admitted"
        else if t.operation = Some e.request.operation then
          Ok { t with accepted }
        else
          Ok
            {
              t with
              operation = Some e.request.operation;
              accepted;
              terminal = None;
              acknowledged = false;
            }
    | V.Input (_, input) ->
        if duplicate_input then Ok t
        else
          let* protocol, _ =
            transition protocol
              (V.Input_committed (input.input_id, input.version))
          in
          Ok { t with protocol; input = Some input }
    | _ -> Ok t
  in
  Ok ((if duplicate_input then t else remember t (Command bytes)), [ bytes ])

let receive t bytes =
  let* message = V.decode_worker bytes in
  let same identity operation =
    t.identity = Some identity && t.operation = Some operation
  in
  let* next, frames =
    match message with
    | P.Ready m
      when t.identity = Some m.identity
           && t.worker = Some m.worker_id
           && V.state_version t.protocol = Some m.state_version ->
        Ok (t, [])
    | P.Accepted _ when t.accepted = Some message -> Ok (t, [])
    | P.Observation m when same m.identity m.operation && t.terminal = None ->
        Ok (t, [])
    | P.Checkpoint m
      when same m.identity m.operation
           && t.terminal = None
           && V.state_version t.protocol = Some m.state_version
           && t.capsule = Some m.capsule
           && List.exists
                (function
                  | Receive old -> V.decode_worker old = Ok message | _ -> false)
                t.history ->
        let* _, frame =
          control "checkpoint_ack" m.identity m.operation
            [
              ("proposal_id", `String m.proposal_id);
              ("state_version", `String (Int64.to_string m.state_version));
              ("capsule", `String m.capsule);
              ("accepted", `Bool true);
            ]
        in
        Ok (t, [ frame ])
    | P.Checkpoint m
      when same m.identity m.operation
           && t.terminal = None
           && V.state_version t.protocol = Some m.previous_state_version ->
        let* proposed, outputs =
          transition t.protocol
            (V.Worker_event
               (P.Propose_checkpoint
                  { proposal_id = m.proposal_id; capsule = m.capsule }))
        in
        if not (exact message outputs) then
          Error "checkpoint identity/version mismatch"
        else
          let* ack, frame =
            control "checkpoint_ack" m.identity m.operation
              [
                ("proposal_id", `String m.proposal_id);
                ("state_version", `String (Int64.to_string m.state_version));
                ("capsule", `String m.capsule);
                ("accepted", `Bool true);
              ]
          in
          let* protocol, _ = transition proposed (V.Host ack) in
          Ok ({ t with protocol; capsule = Some m.capsule }, [ frame ])
    | P.Result m when same m.identity m.operation -> (
        match t.terminal with
        | Some old when old = message -> Ok (t, [])
        | Some _ -> Error "changed terminal result"
        | None ->
            (* In v3 Result is emitted only after cleanup and a terminal capsule
               ACK. The handler additionally checks that exact capsule's status,
               diagnostics and renewal before committing this attestation. *)
            let* cleaned, _ =
              transition t.protocol (V.Worker_event P.Cleanup_complete)
            in
            let* protocol, outputs =
              transition cleaned
                (V.Worker_event
                   (P.Finish
                      {
                        status = m.status;
                        diagnostics = m.diagnostics;
                        renewal_id = m.renewal_id;
                      }))
            in
            if exact message outputs then
              Ok ({ t with protocol; terminal = Some message }, [])
            else Error "result not bound to current protocol version")
    | _ -> Error "unexpected or stale worker frame"
  in
  let first_ready =
    match message with P.Ready _ -> not (ready t) | _ -> false
  in
  Ok
    ( (if next == t && not first_ready then t else remember next (Receive bytes)),
      frames )

let acknowledge_result t =
  match t.terminal with
  | Some (P.Result m) ->
      let* ack, frame =
        control "result_ack" m.identity m.operation
          [ ("state_version", `String (Int64.to_string m.state_version)) ]
      in
      let* protocol, _ = transition t.protocol (V.Host ack) in
      Ok
        ( (if t.acknowledged then t
           else remember { t with protocol; acknowledged = true } Acknowledge),
          [ frame ] )
  | _ -> Error "no validated terminal result"

let rebind t ~worker_id ~workspace_id =
  match (binding t, receipt t, capsule t, V.state_version t.protocol) with
  | ( Some (V.Bind (P.Bind _, _) as bound),
      Some receipt,
      Some capsule,
      Some version ) -> (
      let* original = V.encode_host bound in
      try
        let fields =
          match Yojson.Safe.from_string original with
          | `Assoc xs -> xs
          | _ -> []
        in
        let admission = Yojson.Safe.from_string receipt in
        let attention =
          match admission with
          | `Assoc xs ->
              Option.value (List.assoc_opt "attention_id" xs) ~default:`Null
          | _ -> `Null
        in
        let fields =
          List.filter
            (fun (k, _) ->
              not
                (List.mem k
                   [
                     "worker_id";
                     "workspace_id";
                     "capsule";
                     "state_version";
                     "attention_id";
                     "admission";
                   ]))
            fields
        in
        let frame =
          Yojson.Safe.to_string
            (`Assoc
               (fields
               @ [
                   ("worker_id", `String worker_id);
                   ("workspace_id", `String workspace_id);
                   ("capsule", `String capsule);
                   ("state_version", `String (Int64.to_string version));
                   ("attention_id", attention);
                   ("admission", admission);
                 ]))
          ^ "\n"
        in
        command initial frame
      with _ -> Error "cannot reconstruct committed binding")
  | _ -> Error "no committed binding to recover"

let encode t =
  let action = function
    | Command bytes -> `List [ `String "command"; `String bytes ]
    | Receive bytes -> `List [ `String "receive"; `String bytes ]
    | Acknowledge -> `List [ `String "acknowledge" ]
  in
  let optional = function None -> `Null | Some bytes -> `String bytes in
  Yojson.Safe.to_string
    (`Assoc
       [
         ("capsule", optional (capsule t));
         ("receipt", optional (receipt t));
         ("history", `List (List.rev_map action t.history));
       ])

let decode bytes =
  if String.length bytes > 8 * 1024 * 1024 then Error "oversized stream"
  else
    try
      let* expected_capsule, expected_receipt, actions =
        match Yojson.Safe.from_string bytes with
        | `Assoc
            [
              ("capsule", capsule);
              ("receipt", receipt);
              ("history", `List actions);
            ] ->
            let optional = function
              | `Null -> Ok None
              | `String bytes -> Ok (Some bytes)
              | _ -> Error "invalid committed pair"
            in
            let* capsule = optional capsule in
            let* receipt = optional receipt in
            Ok (capsule, receipt, actions)
        | _ -> Error "invalid stream history"
      in
      let* restored =
        List.fold_left
          (fun state action ->
            let* state = state in
            let* next, _ =
              match action with
              | `List [ `String "command"; `String bytes ] ->
                  command state bytes
              | `List [ `String "receive"; `String bytes ] ->
                  receive state bytes
              | `List [ `String "acknowledge" ] -> acknowledge_result state
              | _ -> Error "invalid stream action"
            in
            Ok next)
          (Ok initial) actions
      in
      if
        capsule restored = expected_capsule
        && receipt restored = expected_receipt
      then Ok restored
      else Error "committed pair differs from validated protocol history"
    with _ -> Error "malformed stream history"

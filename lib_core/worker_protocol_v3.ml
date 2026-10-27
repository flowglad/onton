(* @archlint.module core
   @archlint.domain worker-protocol-v3 *)

open Stdlib

[@@@warning "-4"]

module P = Worker_protocol

type execution_context = {
  bytes : string;
  run : string;
  patch : string;
  config : string;
  plan : string;
  base : string;
  mode : string;
  role : string;
  admitted : bool;
  generation : int64;
  input_version : int64;
}

let context_bytes c = c.bytes
let selected_base c = c.base
let execution_mode c = c.mode
let assigned_patch c = c.patch
let root_role c = c.role
let message_generation c = c.generation
let input_version c = c.input_version
let field k xs = List.assoc_opt k xs

let keys expected = function
  | `Assoc xs
    when List.length xs = List.length expected
         && List.for_all (fun (k, _) -> List.mem k expected) xs ->
      Some xs
  | _ -> None

let str xs k =
  match field k xs with
  | Some (`String s)
    when String.length s > 0
         && String.length s <= 256
         && not (String.contains s '\000') ->
      Some s
  | _ -> None

let text xs k =
  match field k xs with
  | Some (`String s) when String.length s <= 262144 -> Some s
  | _ -> None

let counter xs k =
  match field k xs with
  | Some (`String s)
    when String.length s > 0
         && (String.length s = 1 || s.[0] <> '0')
         && String.for_all (fun c -> c >= '0' && c <= '9') s -> (
      match Int64.of_string_opt s with
      | Some n when n >= 0L -> Some n
      | _ -> None)
  | _ -> None

let context_keys =
  [
    "version";
    "run_id";
    "patch_id";
    "config_id";
    "plan_id";
    "message_id";
    "generation";
    "dependencies";
    "selected_base";
    "execution_mode";
    "root_role";
    "admitted";
    "evidence_id";
    "guidance_ids";
    "input_version";
  ]

let valid_ids = function
  | `List xs ->
      let ids =
        List.filter_map (function `String s -> Some s | _ -> None) xs
      in
      List.length ids = List.length xs
      && List.length ids = List.length (List.sort_uniq String.compare ids)
      && List.for_all
           (fun s ->
             String.length s > 0
             && String.length s <= 256
             && not (String.contains s '\000'))
           ids
  | _ -> false

let decode_context bytes =
  if String.length bytes > 262144 then Error "context too large"
  else
    match P.parse_frame (bytes ^ "\n") with
    | Error _ -> Error "malformed context"
    | Ok json -> (
        match keys context_keys json with
        | None -> Error "context schema"
        | Some xs -> (
            let dependencies =
              match field "dependencies" xs with
              | Some (`List entries) ->
                  let decoded =
                    List.filter_map
                      (fun j ->
                        match keys [ "patch_id"; "admitted"; "revision" ] j with
                        | Some d -> (
                            match
                              ( str d "patch_id",
                                field "admitted" d,
                                field "revision" d )
                            with
                            | ( Some id,
                                Some (`Bool admitted),
                                Some (`String revision) )
                              when String.length revision <= 256
                                   && ((not admitted) || revision <> "") ->
                                Some id
                            | _ -> None)
                        | None -> None)
                      entries
                  in
                  List.length entries = List.length decoded
                  && List.length decoded
                     = List.length (List.sort_uniq String.compare decoded)
              | _ -> false
            in
            match
              ( field "version" xs,
                str xs "run_id",
                str xs "patch_id",
                str xs "config_id",
                str xs "plan_id",
                str xs "message_id",
                counter xs "generation",
                counter xs "input_version",
                str xs "selected_base",
                field "execution_mode" xs,
                field "root_role" xs,
                field "admitted" xs,
                str xs "evidence_id",
                field "guidance_ids" xs )
            with
            | ( Some (`Int 3),
                Some run,
                Some patch,
                Some config,
                Some plan,
                Some _,
                Some generation,
                Some input_version,
                Some base,
                Some (`String (("feature" | "mainline") as mode)),
                Some (`String (("root" | "patch") as role)),
                Some (`Bool admitted),
                Some _,
                Some ids )
              when dependencies && valid_ids ids
                   && (role <> "root" || mode = "feature") ->
                Ok
                  {
                    bytes = Yojson.Safe.to_string json;
                    run;
                    patch;
                    config;
                    plan;
                    base;
                    mode;
                    role;
                    admitted;
                    generation;
                    input_version;
                  }
            | _ -> Error "incomplete or inconsistent execution context"))

type work =
  | Session of P.phase * Types.Operation_kind.t option * string
  | Reconcile
  | Integrate_revision of { contributor : string; revision : string }

type input = {
  input_id : string;
  version : int64;
  guidance : string;
  selected_base : string;
}

type execute = {
  request : P.execute_request;
  work : work;
  context : execution_context;
  input_version : int64;
}

type host_message =
  | Bind of P.host_message * execution_context
  | Execute of execute
  | Input of P.identity * input
  | Legacy_control of P.host_message

type worker_message = P.worker_message

let put xs k v = (k, v) :: List.remove_assoc k xs
let obj = function `Assoc xs -> xs | _ -> []
let json_line j = Yojson.Safe.to_string j ^ "\n"
let v2 j = `Assoc (put (obj j) "version" (`Int 2))
let v3 j = `Assoc (put (obj j) "version" (`Int 3))

let decode_v2 j =
  match P.decode (json_line (v2 j)) with
  | Ok m -> Ok m
  | Error _ -> Error "invalid shared protocol fields"

let extended_work = function
  | `Assoc xs as j -> (
      match keys [ "type"; "contributor"; "revision" ] j with
      | Some xs when field "type" xs = Some (`String "integrate_revision") -> (
          match (str xs "contributor", str xs "revision") with
          | Some contributor, Some revision ->
              Some (Integrate_revision { contributor; revision })
          | _ -> None)
      | _ ->
          let _ = xs in
          None)
  | _ -> None

let work_of_request = function
  | P.Session { phase; kind; turn } -> Session (phase, kind, turn)
  | P.Reconcile -> Reconcile

let shadow_work = function
  | Integrate_revision _ -> P.Reconcile
  | Session (phase, kind, turn) -> P.Session { phase; kind; turn }
  | Reconcile -> P.Reconcile

let decode_host line =
  match P.parse_frame line with
  | Error _ -> Error "malformed v3 frame"
  | Ok (`Assoc xs as json) when field "version" xs = Some (`Int 3) -> (
      match field "type" xs with
      | Some (`String "bind") -> (
          match
            ( keys
                ("execution_context"
                :: [
                     "version";
                     "type";
                     "run_id";
                     "patch_id";
                     "worker_id";
                     "workspace_id";
                     "context";
                     "capsule";
                     "state_version";
                     "attention_id";
                     "admission";
                   ])
                json,
              field "execution_context" xs )
          with
          | Some _, Some (`String bytes) -> (
              match
                ( decode_context bytes,
                  decode_v2 (`Assoc (List.remove_assoc "execution_context" xs))
                )
              with
              | Ok context, Ok (P.Bind { identity; _ } as bind)
                when context.run = identity.run_id
                     && context.patch = identity.patch_id ->
                  Ok (Bind (bind, context))
              | _ -> Error "bind context mismatch")
          | _ -> Error "bind schema")
      | Some (`String "execute") -> (
          match
            ( keys
                ("execution_context" :: "input_version"
                :: [
                     "version";
                     "type";
                     "run_id";
                     "patch_id";
                     "operation_id";
                     "attempt_id";
                     "assignment_seq";
                     "expected_state_version";
                     "work";
                     "guidance";
                     "recovery_mode";
                     "renewal_id";
                     "renewed_intent";
                   ])
                json,
              field "execution_context" xs,
              counter xs "input_version" )
          with
          | Some _, Some (`String bytes), Some input_version -> (
              match decode_context bytes with
              | Error e -> Error e
              | Ok context -> (
                  let work = field "work" xs in
                  let proxy =
                    match Option.bind work extended_work with
                    | Some _ ->
                        `Assoc
                          (put
                             (obj
                                ( List.remove_assoc "execution_context"
                                    (List.remove_assoc "input_version" xs)
                                |> fun x -> `Assoc x ))
                             "work"
                             (`Assoc [ ("type", `String "reconcile") ]))
                    | None ->
                        `Assoc
                          (List.remove_assoc "execution_context"
                             (List.remove_assoc "input_version" xs))
                  in
                  match (decode_v2 proxy, Option.bind work extended_work) with
                  | Ok (P.Execute request), extended
                    when context.run = request.identity.run_id
                         && context.patch = request.identity.patch_id
                         && context.admitted ->
                      Ok
                        (Execute
                           {
                             request;
                             work =
                               (match extended with
                               | Some w -> w
                               | None -> work_of_request request.work);
                             context;
                             input_version;
                           })
                  | _ -> Error "execute context or work mismatch"))
          | _ -> Error "execute schema")
      | Some (`String "input") -> (
          match
            ( keys
                [
                  "version";
                  "type";
                  "run_id";
                  "patch_id";
                  "input_id";
                  "input_version";
                  "guidance";
                  "selected_base";
                ]
                json,
              str xs "run_id",
              str xs "patch_id",
              str xs "input_id",
              counter xs "input_version",
              field "guidance" xs,
              str xs "selected_base" )
          with
          | ( Some _,
              Some run_id,
              Some patch_id,
              Some input_id,
              Some version,
              Some (`String guidance),
              Some selected_base )
            when String.length guidance <= 262144 ->
              Ok
                (Input
                   ( { run_id; patch_id },
                     { input_id; version; guidance; selected_base } ))
          | _ -> Error "input schema")
      | _ -> (
          match decode_v2 json with
          | Ok m -> Ok (Legacy_control m)
          | Error e -> Error e))
  | Ok _ -> Error "v3 required"

let decode_worker line =
  match P.parse_frame line with
  | Ok (`Assoc xs as json) when field "version" xs = Some (`Int 3) -> (
      (* The shared v2 encoder defines the exact seven worker message schemas.
       Validate by round-tripping a strict inverse, not by trusting extra keys. *)
      let fields = obj (v2 json) in
      let id =
        match (str fields "run_id", str fields "patch_id") with
        | Some run_id, Some patch_id -> Some P.{ run_id; patch_id }
        | _ -> None
      in
      let operation =
        match
          ( str fields "operation_id",
            str fields "attempt_id",
            counter fields "assignment_seq" )
        with
        | Some operation_id, Some attempt_id, Some assignment_seq ->
            Some P.{ operation_id; attempt_id; assignment_seq }
        | _ -> None
      in
      let message =
        match (field "type" fields, id, operation) with
        | Some (`String "ready"), Some identity, _ -> (
            match (str fields "worker_id", counter fields "state_version") with
            | Some worker_id, Some state_version ->
                Some (P.Ready { identity; worker_id; state_version })
            | _ -> None)
        | Some (`String "accepted"), Some identity, Some operation -> (
            match field "replay" fields with
            | Some (`Bool replay) ->
                Some (P.Accepted { identity; operation; replay })
            | _ -> None)
        | Some (`String "observation"), Some identity, Some operation -> (
            match text fields "text" with
            | Some text -> Some (P.Observation { identity; operation; text })
            | _ -> None)
        | Some (`String "checkpoint"), Some identity, Some operation -> (
            match
              ( str fields "proposal_id",
                counter fields "previous_state_version",
                counter fields "state_version",
                text fields "capsule" )
            with
            | ( Some proposal_id,
                Some previous_state_version,
                Some state_version,
                Some capsule ) ->
                Some
                  (P.Checkpoint
                     {
                       identity;
                       operation;
                       proposal_id;
                       previous_state_version;
                       state_version;
                       capsule;
                     })
            | _ -> None)
        | Some (`String "result"), Some identity, Some operation -> (
            let status =
              match field "status" fields with
              | Some (`String "completed_ok") -> Some P.Completed_ok
              | Some (`String "completed_no_commits") ->
                  Some P.Completed_no_commits
              | Some (`String "failed") -> Some P.Failed
              | Some (`String "cancelled") -> Some P.Cancelled
              | Some (`String "retryable") -> Some P.Retryable
              | Some (`String "attention") -> Some P.Attention
              | Some (`String "merged_observed") -> Some P.Merged_observed
              | _ -> None
            in
            match
              ( counter fields "state_version",
                status,
                text fields "diagnostics",
                field "renewal_id" fields )
            with
            | ( Some state_version,
                Some status,
                Some diagnostics,
                Some (`String renewal_id) )
              when status = P.Attention
                   && str fields "renewal_id" = Some renewal_id ->
                Some
                  (P.Result
                     {
                       identity;
                       operation;
                       state_version;
                       status;
                       diagnostics;
                       renewal_id = Some renewal_id;
                     })
            | Some state_version, Some status, Some diagnostics, Some `Null
              when status <> P.Attention ->
                Some
                  (P.Result
                     {
                       identity;
                       operation;
                       state_version;
                       status;
                       diagnostics;
                       renewal_id = None;
                     })
            | _ -> None)
        | Some (`String "protocol_error"), _, _ -> (
            match text fields "reason" with
            | Some reason -> Some (P.Protocol_error { identity = id; reason })
            | _ -> None)
        | _ -> None
      in
      let rec same_json a b =
        match (a, b) with
        | `Assoc a, `Assoc b ->
            List.length a = List.length b
            && List.for_all
                 (fun (k, v) ->
                   match List.assoc_opt k b with
                   | Some w -> same_json v w
                   | None -> false)
                 a
        | `List a, `List b ->
            List.length a = List.length b && List.for_all2 same_json a b
        | _ -> a = b
      in
      match message with
      | Some m when same_json (v3 (Yojson.Safe.from_string (P.encode m))) json
        ->
          Ok m
      | _ -> Error "invalid worker message")
  | _ -> Error "malformed v3 worker frame"

let encode_worker m =
  match P.parse_frame (P.encode m) with
  | Ok j -> (
      let line = json_line (v3 j) in
      if String.length line > 1048576 then Error "frame too large"
      else
        match decode_worker line with
        | Ok validated when validated = m -> Ok line
        | _ -> Error "invalid worker message")
  | Error _ -> Error "invalid worker message"

let encode_host msg =
  let encode_checked msg =
    let json =
      match msg with
      | Bind (bind, context) ->
          `Assoc
            (("execution_context", `String context.bytes)
            :: obj (v3 (Yojson.Safe.from_string (P.encode_host bind))))
      | Execute e ->
          let request = { e.request with work = shadow_work e.work } in
          let base =
            obj (v3 (Yojson.Safe.from_string (P.execute_frame request)))
          in
          let base =
            match e.work with
            | Integrate_revision { contributor; revision } ->
                put base "work"
                  (`Assoc
                     [
                       ("type", `String "integrate_revision");
                       ("contributor", `String contributor);
                       ("revision", `String revision);
                     ])
            | _ -> base
          in
          `Assoc
            (("execution_context", `String e.context.bytes)
            :: ("input_version", `String (Int64.to_string e.input_version))
            :: base)
      | Input (identity, i) ->
          `Assoc
            [
              ("version", `Int 3);
              ("type", `String "input");
              ("run_id", `String identity.run_id);
              ("patch_id", `String identity.patch_id);
              ("input_id", `String i.input_id);
              ("input_version", `String (Int64.to_string i.version));
              ("guidance", `String i.guidance);
              ("selected_base", `String i.selected_base);
            ]
      | Legacy_control host -> v3 (Yojson.Safe.from_string (P.encode_host host))
    in
    let line = json_line json in
    if String.length line > 1048576 then Error "frame too large"
    else match decode_host line with Ok _ -> Ok line | Error e -> Error e
  in
  match msg with
  | Execute e -> (
      match P.execute_message { e.request with work = shadow_work e.work } with
      | None -> Error "invalid execution request"
      | Some _ -> encode_checked msg)
  | _ -> encode_checked msg

type t = {
  protocol : P.t;
  binding : execution_context option;
  assignments : (P.execute_request * execute) list;
  current_input : input option;
  pending_input : input option;
}

let initial () =
  {
    protocol = P.initial ();
    binding = None;
    assignments = [];
    current_input = None;
    pending_input = None;
  }

let state_version t = P.state_version t.protocol

let desired_base t =
  match (t.current_input, t.binding) with
  | Some i, _ -> Some i.selected_base
  | None, Some c -> Some c.base
  | None, None -> None

let admission_receipt t = P.admission_receipt t.protocol
let is_stopped t = P.is_stopped t.protocol
let recovery_pending t = P.recovery_pending t.protocol

(* Bind fixes the assignment scope, not the coordinator's per-operation
   evidence. Only committed inputs advance the desired base and input version.
   Historical executions bypass this check solely through exact receipt replay. *)
let current_context t (bound : execution_context) e =
  let context = e.context in
  let version =
    match t.current_input with
    | Some input -> input.version
    | None -> bound.input_version
  in
  bound.admitted && context.admitted && context.run = bound.run
  && context.patch = bound.patch
  && context.config = bound.config
  && context.plan = bound.plan && context.mode = bound.mode
  && context.role = bound.role
  && context.generation >= bound.generation
  && List.for_all
       (fun (_, old) -> context.generation >= old.context.generation)
       t.assignments
  && Some context.base = desired_base t
  && context.input_version = version
  && e.input_version = version

let shadow e =
  match encode_host (Execute e) with
  | Error err -> Error err
  | Ok bytes ->
      Ok { e.request with work = shadow_work e.work; guidance = bytes }

let restored_assignments binding =
  let json = Yojson.Safe.from_string (P.encode_host binding) in
  match field "admission" (obj json) with
  | Some (`Assoc fields) -> (
      match field "entries" fields with
      | None -> Ok []
      | Some (`List entries) ->
          let rec gather acc = function
            | [] -> Ok acc
            | entry :: rest -> (
                match field "request" (obj entry) with
                | Some json -> (
                    match P.decode (json_line json) with
                    | Ok (P.Execute request) -> (
                        match decode_host request.guidance with
                        | Ok (Execute e) -> (
                            match shadow e with
                            | Ok exact when exact = request ->
                                gather ((request, e) :: acc) rest
                            | _ -> Error "changed predecessor execution binding"
                            )
                        | _ -> Error "missing predecessor execution binding")
                    | _ -> Error "invalid predecessor request")
                | None -> Error "missing predecessor request")
          in
          gather [] entries
      | Some _ -> Error "invalid predecessor entries")
  | _ -> Error "missing admission"

type event =
  | Host of host_message
  | Worker_event of P.event
  | Input_committed of string * int64
  | Restore_input of input
  | Recover_unfinished
  | Host_lost

type output =
  | Send of worker_message
  | Begin of execute
  | Recover of execute
  | Apply_input of input
  | Cancel_active
  | Stop
  | Reject of string

let step t event =
  let reject reason = (t, [ Reject reason ]) in
  let transition t e =
    let protocol, outputs = P.step t.protocol e in
    let t = { t with protocol } in
    let outputs =
      List.map
        (function
          | P.Send msg -> Send msg
          | P.Cancel_active -> Cancel_active
          | P.Stop -> Stop
          | P.Begin (P.Execute req) -> (
              match List.find_opt (fun (r, _) -> r = req) t.assignments with
              | Some (_, e) -> Begin e
              | None -> Reject "missing execution binding")
          | P.Recover (P.Execute req) -> (
              match List.find_opt (fun (r, _) -> r = req) t.assignments with
              | Some (_, e) -> Recover e
              | None -> Reject "missing recovery binding")
          | P.Begin _ | P.Recover _ -> Reject "unexpected host transition")
        outputs
    in
    (t, outputs)
  in
  match event with
  | Host (Bind ((P.Bind { identity; _ } as binding), context)) -> (
      if context.run <> identity.run_id || context.patch <> identity.patch_id
      then reject "binding scope"
      else if t.binding <> None && t.binding <> Some context then
        reject "changed context"
      else
        match restored_assignments binding with
        | Error reason -> reject reason
        | Ok assignments ->
            transition
              { t with binding = Some context; assignments }
              (P.Host binding))
  | Host (Bind _) -> reject "invalid bind"
  | Host (Execute e) -> (
      match t.binding with
      | Some c
        when List.exists (fun (_, old) -> old = e) t.assignments
             || current_context t c e -> (
          match shadow e with
          | Error err -> reject err
          | Ok request -> (
              match P.execute_message request with
              | Some message ->
                  let _, outputs = P.step t.protocol (P.Host message) in
                  let admitted =
                    List.exists
                      (function P.Begin _ -> true | _ -> false)
                      outputs
                  in
                  let t =
                    if admitted then
                      { t with assignments = (request, e) :: t.assignments }
                    else t
                  in
                  transition t (P.Host message)
              | None -> reject "oversized or invalid execution binding"))
      | _ -> reject "unbound or stale execution context")
  | Host (Input (identity, input)) -> (
      match t.binding with
      | Some c
        when c.run = identity.run_id
             && c.patch = identity.patch_id
             && String.length input.guidance <= 262144
             && input.input_id <> "" -> (
          match t.current_input with
          | Some old when old.input_id = input.input_id && old = input -> (t, [])
          | Some old when old.input_id = input.input_id ->
              reject "changed input ID"
          | _ -> (
              match t.pending_input with
              | Some old when old = input -> (t, [])
              | Some _ -> reject "uncommitted input pending"
              | None ->
                  let next =
                    match t.current_input with
                    | None ->
                        c.input_version < Int64.max_int
                        && input.version = Int64.succ c.input_version
                    | Some old ->
                        old.version < Int64.max_int
                        && input.version = Int64.succ old.version
                  in
                  if not next then reject "input version"
                  else ({ t with pending_input = Some input }, [])))
      | _ -> reject "input scope")
  | Host (Legacy_control (P.Bind _ | P.Execute _)) ->
      reject "v2 bind/execute cannot enter v3"
  | Host (Legacy_control msg) -> transition t (P.Host msg)
  | Input_committed (id, version) -> (
      match t.pending_input with
      | Some input when input.input_id = id && input.version = version ->
          ( { t with current_input = Some input; pending_input = None },
            [ Apply_input input ] )
      | _ -> reject "input commit not matching pending bytes")
  | Restore_input input -> (
      match t.binding with
      | Some bound
        when input.input_id <> ""
             && String.length input.guidance <= 262144
             && input.version > bound.input_version
             && t.pending_input = None && t.current_input = None ->
          ({ t with current_input = Some input }, [])
      | Some _ | None -> reject "restored input scope or version")
  | Worker_event (P.Host _) -> reject "host commands require v3 envelope"
  | Worker_event e -> transition t e
  | Recover_unfinished -> transition t P.Recover_unfinished
  | Host_lost -> transition t P.Host_lost

type note =
  | Missing
  | Empty
  | Oversized
  | Unreadable
  | Present of {
      assignment : P.operation;
      publication : string;
      bytes : string;
    }

let note ~assignment ~publication = function
  | Error "missing" -> Missing
  | Error _ -> Unreadable
  | Ok "" -> Empty
  | Ok bytes when String.length bytes > 262144 -> Oversized
  | Ok bytes -> Present { assignment; publication; bytes }

let accept_note ~assignment ~publication = function
  | Present n when n.assignment = assignment && n.publication = publication ->
      Some n.bytes
  | _ -> None

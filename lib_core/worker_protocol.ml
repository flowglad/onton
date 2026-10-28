(* @archlint.module core
   @archlint.domain worker-protocol *)

open Stdlib

type decode_error = Invalid of string | Incompatible of string
type identity = { run_id : string; patch_id : string }

type operation = {
  operation_id : string;
  attempt_id : string;
  assignment_seq : int64;
}

type phase = Start | Respond

type operation_kind =
  | Session of {
      phase : phase;
      kind : Types.Operation_kind.t option;
      turn : string;
    }
  | Reconcile

type recovery_mode = Retained_workspace | Lost_workspace
type renewal = Ordinary | Renewed of string

type result_status =
  | Completed_ok
  | Completed_no_commits
  | Failed
  | Cancelled
  | Retryable
  | Attention
  | Merged_observed

type terminal = {
  state_version : int64;
  status : result_status;
  diagnostics : string;
  renewal_id : string option;
}

type execute_request = {
  identity : identity;
  operation : operation;
  expected_state_version : int64;
  work : operation_kind;
  guidance : string;
  recovery_mode : recovery_mode;
  renewal : renewal;
}

type host_message =
  | Bind of {
      identity : identity;
      worker_id : string;
      workspace_id : string;
      context : string;
      capsule : string;
      state_version : int64;
      attention_id : string option;
      admission : admission;
    }
  | Execute of execute_request
  | Cancel of { identity : identity; operation : operation }
  | Checkpoint_ack of {
      identity : identity;
      operation : operation;
      proposal_id : string;
      state_version : int64;
      capsule : string;
      accepted : bool;
    }
  | Result_ack of {
      identity : identity;
      operation : operation;
      state_version : int64;
    }
  | Shutdown of identity

and admission =
  | Fresh
  | Resume of {
      initial_version : int64;
      initial_attention : string option;
      committed_version : int64;
      committed_capsule : string;
      committed_attention : string option;
      entries : receipt list;
    }

and receipt = {
  request : execute_request;
  terminal : terminal option;
  result_acknowledged : bool;
}

type worker_message =
  | Ready of { identity : identity; worker_id : string; state_version : int64 }
  | Accepted of { identity : identity; operation : operation; replay : bool }
  | Observation of { identity : identity; operation : operation; text : string }
  | Checkpoint of {
      identity : identity;
      operation : operation;
      proposal_id : string;
      previous_state_version : int64;
      state_version : int64;
      capsule : string;
    }
  | Result of {
      identity : identity;
      operation : operation;
      state_version : int64;
      status : result_status;
      diagnostics : string;
      renewal_id : string option;
    }
  | Protocol_error of { identity : identity option; reason : string }

let max_frame = 1024 * 1024
let max_text = 256 * 1024

let valid_utf8 s =
  let len = String.length s in
  let rec loop i =
    if i = len then true
    else
      let byte j = Char.code s.[j] in
      let c = byte i in
      if c < 0x80 then loop (i + 1)
      else
        let n, low, high =
          if c >= 0xc2 && c <= 0xdf then (2, 0x80, 0xbf)
          else if c = 0xe0 then (3, 0xa0, 0xbf)
          else if c >= 0xe1 && c <= 0xec then (3, 0x80, 0xbf)
          else if c = 0xed then (3, 0x80, 0x9f)
          else if c >= 0xee && c <= 0xef then (3, 0x80, 0xbf)
          else if c = 0xf0 then (4, 0x90, 0xbf)
          else if c >= 0xf1 && c <= 0xf3 then (4, 0x80, 0xbf)
          else if c = 0xf4 then (4, 0x80, 0x8f)
          else (0, 0, 0)
        in
        n > 0
        && i + n <= len
        && byte (i + 1) >= low
        && byte (i + 1) <= high
        && (let rec rest j =
              j = n
              || (byte (i + j) >= 0x80 && byte (i + j) <= 0xbf && rest (j + 1))
            in
            rest 2)
        && loop (i + n)
  in
  loop 0

let valid_id s =
  String.length s > 0
  && String.length s <= 256
  && valid_utf8 s
  && not (String.contains s '\000')

let valid_text s =
  String.length s <= max_text && valid_utf8 s && not (String.contains s '\000')

let canonical_counter s =
  let len = String.length s in
  len > 0
  && (len = 1 || s.[0] <> '0')
  && (let rec digits i =
        i = len || (s.[i] >= '0' && s.[i] <= '9' && digits (i + 1))
      in
      digits 0)
  &&
  match Int64.of_string_opt s with
  | Some n -> Int64.compare n 0L >= 0
  | None -> false

let recursive_shape json =
  let rec check = function
    | `Assoc fields ->
        let names = List.map fst fields in
        List.length names = List.length (List.sort_uniq String.compare names)
        && List.for_all (fun (_, v) -> check v) fields
    | `List xs -> List.for_all check xs
    | `Float f -> Float.is_finite f
    | `String s -> valid_utf8 s && not (String.contains s '\000')
    | _ -> true
  in
  check json

let fields expected = function
  | `Assoc xs
    when List.length xs = List.length expected
         && List.for_all (fun (k, _) -> List.mem k expected) xs ->
      Some xs
  | _ -> None

let get xs k = List.assoc_opt k xs

let string xs k =
  match get xs k with Some (`String s) when valid_text s -> Some s | _ -> None

let id xs k =
  match string xs k with Some s when valid_id s -> Some s | _ -> None

let counter xs k =
  match get xs k with
  | Some (`String s) when canonical_counter s -> Int64.of_string_opt s
  | _ -> None

let optional_id xs k =
  match get xs k with
  | Some `Null -> Some None
  | Some (`String s) when valid_id s -> Some (Some s)
  | _ -> None

let optional_text xs k =
  match get xs k with
  | Some `Null -> Some None
  | Some (`String s) when valid_text s -> Some (Some s)
  | _ -> None

let phase_of_string = function
  | "start" -> Some Start
  | "respond" -> Some Respond
  | _ -> None

let phase_string = function Start -> "start" | Respond -> "respond"

let required_identity xs =
  match (id xs "run_id", id xs "patch_id") with
  | Some run_id, Some patch_id -> Some { run_id; patch_id }
  | _ -> None

let required_operation xs =
  match
    (id xs "operation_id", id xs "attempt_id", counter xs "assignment_seq")
  with
  | Some operation_id, Some attempt_id, Some assignment_seq ->
      Some { operation_id; attempt_id; assignment_seq }
  | _ -> None

let base = [ "version"; "type"; "run_id"; "patch_id" ]
let op = [ "operation_id"; "attempt_id"; "assignment_seq" ]
let invalid = Error (Invalid "invalid worker protocol frame")

let parse_frame line =
  if
    String.length line = 0
    || String.length line > max_frame
    || line.[String.length line - 1] <> '\n'
  then invalid
  else if
    (not (valid_utf8 line))
    || String.contains (String.sub line 0 (String.length line - 1)) '\n'
  then invalid
  else
    match
      try
        Some
          (Yojson.Safe.from_string (String.sub line 0 (String.length line - 1)))
      with _ -> None
    with
    | None -> invalid
    | Some json when not (recursive_shape json) -> invalid
    | Some json -> Ok json

let rec decode line =
  match parse_frame line with
  | Error e -> Error e
  | Ok (`Assoc xs as json) -> (
      match get xs "version" with
      | Some (`Int 2) -> (
          match (get xs "type", required_identity xs) with
          | Some (`String "bind"), Some identity -> (
              match
                fields
                  (base
                  @ [
                      "worker_id";
                      "workspace_id";
                      "context";
                      "capsule";
                      "state_version";
                      "attention_id";
                      "admission";
                    ])
                  json
              with
              | Some f -> (
                  match
                    ( id f "worker_id",
                      id f "workspace_id",
                      string f "context",
                      string f "capsule",
                      counter f "state_version",
                      optional_id f "attention_id",
                      decode_admission (get f "admission") )
                  with
                  | ( Some worker_id,
                      Some workspace_id,
                      Some context,
                      Some capsule,
                      Some state_version,
                      Some attention_id,
                      Some admission ) ->
                      Ok
                        (Bind
                           {
                             identity;
                             worker_id;
                             workspace_id;
                             context;
                             capsule;
                             state_version;
                             attention_id;
                             admission;
                           })
                  | _ -> invalid)
              | None -> invalid)
          | Some (`String "execute"), Some identity -> (
              match
                ( fields
                    (base @ op
                    @ [
                        "expected_state_version";
                        "work";
                        "guidance";
                        "recovery_mode";
                        "renewal_id";
                        "renewed_intent";
                      ])
                    json,
                  required_operation xs )
              with
              | Some f, Some operation -> (
                  let work =
                    match get f "work" with
                    | Some (`Assoc _ as j) -> (
                        match fields [ "type"; "phase"; "kind"; "turn" ] j with
                        | Some w -> (
                            match
                              ( get w "type",
                                string w "phase",
                                optional_text w "kind",
                                string w "turn" )
                            with
                            | ( Some (`String "session"),
                                Some phase,
                                Some kind,
                                Some turn ) -> (
                                match
                                  ( phase_of_string phase,
                                    match kind with
                                    | None -> Some None
                                    | Some label ->
                                        Option.map
                                          (fun kind -> Some kind)
                                          (Types.Operation_kind.of_label label)
                                  )
                                with
                                | Some phase, Some kind ->
                                    Some (Session { phase; kind; turn })
                                | _ -> None)
                            | _ -> None)
                        | None -> (
                            match fields [ "type" ] j with
                            | Some w
                              when get w "type" = Some (`String "reconcile") ->
                                Some Reconcile
                            | _ -> None))
                    | _ -> None
                  in
                  let recovery_mode =
                    match get f "recovery_mode" with
                    | Some (`String "retained_workspace") ->
                        Some Retained_workspace
                    | Some (`String "lost_workspace") -> Some Lost_workspace
                    | _ -> None
                  in
                  match
                    ( counter f "expected_state_version",
                      work,
                      string f "guidance",
                      recovery_mode,
                      optional_id f "renewal_id",
                      get f "renewed_intent" )
                  with
                  | ( Some expected_state_version,
                      Some work,
                      Some guidance,
                      Some recovery_mode,
                      Some renewal_id,
                      Some (`Bool renewed_intent) ) -> (
                      match (renewal_id, renewed_intent) with
                      | None, false ->
                          Ok
                            (Execute
                               {
                                 identity;
                                 operation;
                                 expected_state_version;
                                 work;
                                 guidance;
                                 recovery_mode;
                                 renewal = Ordinary;
                               })
                      | Some id, true ->
                          Ok
                            (Execute
                               {
                                 identity;
                                 operation;
                                 expected_state_version;
                                 work;
                                 guidance;
                                 recovery_mode;
                                 renewal = Renewed id;
                               })
                      | None, true | Some _, false -> invalid)
                  | _ -> invalid)
              | _ -> invalid)
          | Some (`String "cancel"), Some identity -> (
              match (fields (base @ op) json, required_operation xs) with
              | Some _, Some operation -> Ok (Cancel { identity; operation })
              | _ -> invalid)
          | Some (`String "checkpoint_ack"), Some identity -> (
              match
                ( fields
                    (base @ op
                    @ [ "proposal_id"; "state_version"; "capsule"; "accepted" ]
                    )
                    json,
                  required_operation xs )
              with
              | Some f, Some operation -> (
                  match
                    ( id f "proposal_id",
                      counter f "state_version",
                      string f "capsule",
                      get f "accepted" )
                  with
                  | ( Some proposal_id,
                      Some state_version,
                      Some capsule,
                      Some (`Bool accepted) ) ->
                      Ok
                        (Checkpoint_ack
                           {
                             identity;
                             operation;
                             proposal_id;
                             state_version;
                             capsule;
                             accepted;
                           })
                  | _ -> invalid)
              | _ -> invalid)
          | Some (`String "result_ack"), Some identity -> (
              match
                ( fields (base @ op @ [ "state_version" ]) json,
                  required_operation xs )
              with
              | Some f, Some operation -> (
                  match counter f "state_version" with
                  | Some state_version ->
                      Ok (Result_ack { identity; operation; state_version })
                  | None -> invalid)
              | _ -> invalid)
          | Some (`String "shutdown"), Some identity -> (
              match fields base json with
              | Some _ -> Ok (Shutdown identity)
              | None -> invalid)
          | _ -> invalid)
      | Some _ -> Error (Incompatible "unsupported worker protocol version")
      | None -> invalid)
  | Ok _ -> invalid

and decode_admission = function
  | Some (`Assoc [ ("mode", `String "fresh") ]) -> Some Fresh
  | Some json -> (
      match
        fields
          [
            "mode";
            "initial_state_version";
            "initial_attention_id";
            "state_version";
            "capsule";
            "attention_id";
            "entries";
          ]
          json
      with
      | Some f when get f "mode" = Some (`String "resume") -> (
          match
            ( counter f "initial_state_version",
              optional_id f "initial_attention_id",
              get f "entries",
              counter f "state_version",
              string f "capsule",
              optional_id f "attention_id" )
          with
          | ( Some initial_version,
              Some initial_attention,
              Some (`List (_ :: _ as entries)),
              Some committed_version,
              Some committed_capsule,
              Some committed_attention ) ->
              let rec loop acc = function
                | [] ->
                    Some
                      (Resume
                         {
                           initial_version;
                           initial_attention;
                           committed_version;
                           committed_capsule;
                           committed_attention;
                           entries = List.rev acc;
                         })
                | json :: rest -> (
                    match
                      fields
                        [ "request"; "terminal"; "result_acknowledged" ]
                        json
                    with
                    | Some f -> (
                        match
                          ( get f "request",
                            decode_terminal (get f "terminal"),
                            get f "result_acknowledged" )
                        with
                        | ( Some (`Assoc request as json),
                            Some terminal,
                            Some (`Bool result_acknowledged) )
                          when get request "type" = Some (`String "execute")
                          -> (
                            match
                              decode (Yojson.Safe.to_string json ^ "\n")
                            with
                            | Ok (Execute request)
                              when terminal <> None || not result_acknowledged
                              ->
                                loop
                                  ({ request; terminal; result_acknowledged }
                                  :: acc)
                                  rest
                            | Ok
                                ( Bind _ | Execute _ | Cancel _
                                | Checkpoint_ack _ | Result_ack _ | Shutdown _
                                  )
                            | Error _ ->
                                None)
                        | _ -> None)
                    | None -> None)
              in
              loop [] entries
          | _ -> None)
      | _ -> None)
  | None -> None

and decode_terminal = function
  | Some `Null -> Some None
  | Some json -> (
      match
        fields [ "state_version"; "status"; "diagnostics"; "renewal_id" ] json
      with
      | Some f -> (
          let status =
            match get f "status" with
            | Some (`String "completed_ok") -> Some Completed_ok
            | Some (`String "completed_no_commits") -> Some Completed_no_commits
            | Some (`String "failed") -> Some Failed
            | Some (`String "cancelled") -> Some Cancelled
            | Some (`String "retryable") -> Some Retryable
            | Some (`String "attention") -> Some Attention
            | Some (`String "merged_observed") -> Some Merged_observed
            | _ -> None
          in
          match
            ( counter f "state_version",
              status,
              string f "diagnostics",
              optional_id f "renewal_id" )
          with
          | Some state_version, Some status, Some diagnostics, Some renewal_id
            when status = Attention = (renewal_id <> None) ->
              Some (Some { state_version; status; diagnostics; renewal_id })
          | _ -> None)
      | None -> None)
  | None -> None

let obj xs = `Assoc xs
let s x = `String x
let n x = s (Int64.to_string x)
let identity_fields i = [ ("run_id", s i.run_id); ("patch_id", s i.patch_id) ]

let operation_fields o =
  [
    ("operation_id", s o.operation_id);
    ("attempt_id", s o.attempt_id);
    ("assignment_seq", n o.assignment_seq);
  ]

let envelope kind identity fields =
  obj
    ([ ("version", `Int 2); ("type", s kind) ]
    @ identity_fields identity @ fields)

let status_name = function
  | Completed_ok -> "completed_ok"
  | Completed_no_commits -> "completed_no_commits"
  | Failed -> "failed"
  | Cancelled -> "cancelled"
  | Retryable -> "retryable"
  | Attention -> "attention"
  | Merged_observed -> "merged_observed"

let opt = function None -> `Null | Some x -> s x

let encode msg =
  let json =
    match msg with
    | Ready { identity; worker_id; state_version } ->
        envelope "ready" identity
          [ ("worker_id", s worker_id); ("state_version", n state_version) ]
    | Accepted { identity; operation; replay } ->
        envelope "accepted" identity
          (operation_fields operation @ [ ("replay", `Bool replay) ])
    | Observation { identity; operation; text } ->
        envelope "observation" identity
          (operation_fields operation @ [ ("text", s text) ])
    | Checkpoint
        {
          identity;
          operation;
          proposal_id;
          previous_state_version;
          state_version;
          capsule;
        } ->
        envelope "checkpoint" identity
          (operation_fields operation
          @ [
              ("proposal_id", s proposal_id);
              ("previous_state_version", n previous_state_version);
              ("state_version", n state_version);
              ("capsule", s capsule);
            ])
    | Result
        { identity; operation; state_version; status; diagnostics; renewal_id }
      ->
        envelope "result" identity
          (operation_fields operation
          @ [
              ("state_version", n state_version);
              ("status", s (status_name status));
              ("diagnostics", s diagnostics);
              ("renewal_id", opt renewal_id);
            ])
    | Protocol_error { identity; reason } -> (
        match identity with
        | Some i -> envelope "protocol_error" i [ ("reason", s reason) ]
        | None ->
            obj
              [
                ("version", `Int 2);
                ("type", s "protocol_error");
                ("reason", s reason);
              ])
  in
  let line = Yojson.Safe.to_string json ^ "\n" in
  if String.length line <= max_frame && valid_utf8 line then line
  else
    "{\"version\":2,\"type\":\"protocol_error\",\"reason\":\"worker output \
     exceeds frame limit\"}\n"

type output =
  | Send of worker_message
  | Begin of host_message
  | Recover of host_message
  | Cancel_active
  | Stop

type pending = { proposal_id : string; capsule : string; version : int64 }

type active = {
  acknowledged : bool;
  last_ack : pending option;
  operation : operation;
  pending : pending option;
  terminal : worker_message option;
  cleanup : bool;
  cancelled : bool;
}

type t = {
  identity : identity option;
  binding : host_message option;
  version : int64;
  active : active option;
  attention : string option;
  consumed_renewals : string list;
  stopped : bool;
  initial_version : int64;
  initial_attention : string option;
  receipts : receipt list;
  capsule : string;
  restored_unfinished : bool;
}

let initial () =
  {
    identity = None;
    binding = None;
    version = 0L;
    active = None;
    attention = None;
    consumed_renewals = [];
    stopped = false;
    initial_version = 0L;
    initial_attention = None;
    receipts = [];
    capsule = "";
    restored_unfinished = false;
  }

let state_version (t : t) =
  match t.identity with None -> None | Some _ -> Some t.version

let is_stopped t = t.stopped
let recovery_pending t = t.restored_unfinished

let error (t : t) reason =
  [ Send (Protocol_error { identity = t.identity; reason }) ]

let bound (t : t) identity = t.identity = Some identity

let active_match t identity operation =
  match t.active with
  | Some a -> bound t identity && a.operation = operation
  | None -> false

let stop t = ({ t with stopped = true }, [ Stop ])

type event =
  | Host of host_message
  | Report_observation of string
  | Propose_checkpoint of { proposal_id : string; capsule : string }
  | Finish of {
      status : result_status;
      diagnostics : string;
      renewal_id : string option;
    }
  | Cleanup_complete
  | Recover_unfinished
  | Host_lost

(* Attention is part of the exact checkpoint capsule, not a later inference
   from Result. Keeping its committed projection at the checkpoint barrier lets
   an interrupted terminal checkpoint bind before Result has been delivered. The
   effectful host separately validates the complete owner capsule. *)
let checkpoint_attention capsule =
  try
    match Yojson.Safe.from_string capsule with
    | `Assoc fields -> (
        match List.assoc_opt "attention_id" fields with
        | Some (`String id) when valid_id id -> Some id
        | _ -> None)
    | _ -> None
  with _ -> None

let rec step t event =
  if t.stopped then (t, [])
  else
    match event with
    | Host
        (Bind
           ({
              identity;
              worker_id;
              state_version;
              attention_id;
              admission;
              capsule;
              _;
            } as binding)) -> (
        match t.identity with
        | None -> (
            let base =
              {
                t with
                identity = Some identity;
                binding = Some (Bind binding);
                version = state_version;
                attention = attention_id;
                capsule;
                initial_version = state_version;
                initial_attention = attention_id;
              }
            in
            let restored =
              match admission with
              | Fresh -> Some base
              | Resume
                  {
                    initial_version;
                    initial_attention;
                    committed_version;
                    committed_capsule;
                    committed_attention;
                    entries;
                  } ->
                  if
                    committed_version = state_version
                    && committed_capsule = capsule
                    && committed_attention = attention_id
                  then restore base initial_version initial_attention entries
                  else None
            in
            match restored with
            | Some t ->
                (t, [ Send (Ready { identity; worker_id; state_version }) ])
            | None ->
                let t, outputs = stop t in
                (t, error t "invalid predecessor receipt" @ outputs))
        | Some old when old = identity && t.binding = Some (Bind binding) ->
            (t, [ Send (Ready { identity; worker_id; state_version }) ])
        | _ ->
            let t, e = stop t in
            (t, error t "binding changed" @ e))
    | Host
        (Execute
           ({ identity; operation; expected_state_version; renewal; _ } as
            request)) -> (
        if not (bound t identity) then (t, error t "unbound identity")
        else
          match
            List.find_opt
              (fun receipt ->
                let old = receipt.request.operation in
                old.assignment_seq = operation.assignment_seq
                || old.operation_id = operation.operation_id
                || old.attempt_id = operation.attempt_id)
              t.receipts
          with
          | Some receipt when receipt.request = request ->
              let replay =
                [ Send (Accepted { identity; operation; replay = true }) ]
              in
              let replay =
                match receipt.terminal with
                | Some terminal ->
                    replay
                    @ [
                        Send
                          (Result
                             {
                               identity;
                               operation;
                               state_version = terminal.state_version;
                               status = terminal.status;
                               diagnostics = terminal.diagnostics;
                               renewal_id = terminal.renewal_id;
                             });
                      ]
                | None -> replay
              in
              (t, replay)
          | Some _ -> (t, error t "identity or sequence reused")
          | None ->
              let next =
                match t.receipts with
                | [] -> operation.assignment_seq = 1L
                | previous :: _ ->
                    let n = previous.request.operation.assignment_seq in
                    Int64.compare n Int64.max_int < 0
                    && operation.assignment_seq = Int64.succ n
              in
              let renewal_ok =
                match (t.attention, renewal) with
                | None, Ordinary -> true
                | Some expected, Renewed supplied ->
                    expected = supplied
                    && not (List.mem supplied t.consumed_renewals)
                | None, Renewed _ | Some _, Ordinary -> false
              in
              if
                t.active <> None || (not next)
                || expected_state_version <> t.version
                || not renewal_ok
              then (t, error t "assignment not admissible")
              else
                let active =
                  {
                    acknowledged = false;
                    last_ack = None;
                    operation;
                    pending = None;
                    terminal = None;
                    cleanup = false;
                    cancelled = false;
                  }
                in
                let t =
                  {
                    t with
                    active = Some active;
                    receipts =
                      { request; terminal = None; result_acknowledged = false }
                      :: t.receipts;
                    attention = None;
                    consumed_renewals =
                      (match renewal with
                      | Ordinary -> t.consumed_renewals
                      | Renewed x -> x :: t.consumed_renewals);
                  }
                in
                ( t,
                  [
                    Send (Accepted { identity; operation; replay = false });
                    Begin (Execute request);
                  ] ))
    | Host (Cancel { identity; operation }) -> (
        match t.active with
        | Some a
          when active_match t identity operation
               && (not a.cancelled) && a.terminal = None ->
            t |> fun t ->
            ( { t with active = Some { a with cancelled = true } },
              [ Cancel_active ] )
        | _ -> (t, []))
    | Host
        (Checkpoint_ack
           {
             identity;
             operation;
             proposal_id;
             state_version;
             capsule;
             accepted;
           }) -> (
        match t.active with
        | Some a when active_match t identity operation -> (
            match a.pending with
            | Some p
              when p.proposal_id = proposal_id
                   && p.version = state_version && p.capsule = capsule ->
                if accepted then
                  ( {
                      t with
                      version = state_version;
                      capsule;
                      attention = checkpoint_attention capsule;
                      active =
                        Some
                          {
                            a with
                            pending = None;
                            last_ack = Some p;
                            acknowledged = true;
                          };
                    },
                    [] )
                else
                  let t, e = stop t in
                  (t, Cancel_active :: e)
            | None
              when a.last_ack
                   = Some { proposal_id; capsule; version = state_version }
                   && accepted ->
                (t, [])
            | _ -> (t, error t "stale checkpoint acknowledgment"))
        | _ -> (t, error t "stale checkpoint acknowledgment"))
    | Host (Result_ack { identity; operation; state_version }) -> (
        let matches =
          match t.receipts with
          | { request; terminal = Some terminal; _ } :: _ ->
              operation = request.operation
              && state_version = terminal.state_version
          | [] | { terminal = None; _ } :: _ -> false
        in
        match t.active with
        | Some a
          when matches && bound t identity && a.operation = operation
               && a.cleanup && a.pending = None ->
            ( {
                t with
                active = None;
                receipts =
                  (match t.receipts with
                  | r :: rest -> { r with result_acknowledged = true } :: rest
                  | [] -> []);
              },
              [] )
        | None when matches && bound t identity -> (t, [])
        | Some _ | None -> (t, error t "stale result acknowledgment"))
    | Host (Shutdown identity) -> (
        if not (bound t identity) then (t, error t "unbound shutdown")
        else
          match t.active with
          | None -> stop t
          | Some a ->
              let t, outputs =
                stop { t with active = Some { a with cancelled = true } }
              in
              (t, Cancel_active :: outputs))
    | Report_observation text -> (
        match (t.identity, t.active) with
        | Some identity, Some a
          when (not t.stopped) && a.terminal = None && valid_text text ->
            ( t,
              [ Send (Observation { identity; operation = a.operation; text }) ]
            )
        | _ -> (t, error t "observation not admissible"))
    | Propose_checkpoint { proposal_id; capsule } -> (
        match (t.identity, t.active) with
        | Some identity, Some a
          when (not t.restored_unfinished)
               && a.pending = None && a.terminal = None && valid_id proposal_id
               && valid_text capsule
               && Int64.compare t.version Int64.max_int < 0 ->
            let version = Int64.succ t.version in
            ( {
                t with
                active =
                  Some
                    { a with pending = Some { proposal_id; capsule; version } };
              },
              [
                Send
                  (Checkpoint
                     {
                       identity;
                       operation = a.operation;
                       proposal_id;
                       previous_state_version = t.version;
                       state_version = version;
                       capsule;
                     });
              ] )
        | _ -> (t, error t "checkpoint not admissible"))
    | Cleanup_complete -> (
        match t.active with
        | None -> (t, [])
        | Some a -> ({ t with active = Some { a with cleanup = true } }, []))
    | Finish { status; diagnostics; renewal_id } -> (
        match (t.identity, t.active) with
        | Some identity, Some a
          when (not t.restored_unfinished)
               && a.pending = None && a.acknowledged && a.cleanup
               && a.terminal = None
               (* Cancellation is a request, not proof that it beat terminal
                  capture. A completed turn may already be awaiting its ACK. *)
               && valid_text diagnostics
               &&
               match (status, renewal_id) with
               | Attention, Some x ->
                   valid_id x && not (List.mem x t.consumed_renewals)
               | Attention, None -> false
               | ( ( Completed_ok | Completed_no_commits | Failed | Cancelled
                   | Retryable | Merged_observed ),
                   None ) ->
                   true
               | ( ( Completed_ok | Completed_no_commits | Failed | Cancelled
                   | Retryable | Merged_observed ),
                   Some _ ) ->
                   false ->
            let result =
              Result
                {
                  identity;
                  operation = a.operation;
                  state_version = t.version;
                  status;
                  diagnostics;
                  renewal_id;
                }
            in
            ( {
                t with
                active = Some { a with terminal = Some result };
                receipts =
                  (match t.receipts with
                  | r :: rest ->
                      {
                        r with
                        terminal =
                          Some
                            {
                              state_version = t.version;
                              status;
                              diagnostics;
                              renewal_id;
                            };
                      }
                      :: rest
                  | [] -> []);
                attention =
                  (match status with
                  | Attention -> renewal_id
                  | Completed_ok | Completed_no_commits | Failed | Cancelled
                  | Retryable | Merged_observed ->
                      None);
              },
              [ Send result ] )
        | _ -> (t, error t "terminal result not admissible"))
    | Recover_unfinished -> (
        match (t.restored_unfinished, t.receipts) with
        | true, { request; terminal = None; _ } :: _ ->
            ( { t with restored_unfinished = false },
              [ Recover (Execute request) ] )
        | _ -> (t, error t "no interrupted assignment to recover"))
    | Host_lost -> (
        let t, e = stop t in
        (t, match t.active with Some _ -> Cancel_active :: e | None -> e))

and restore base initial_version initial_attention entries =
  let start =
    {
      base with
      version = initial_version;
      attention = initial_attention;
      initial_version;
      initial_attention;
    }
  in
  let rec loop (t : t) = function
    | [] ->
        if t.version = base.version && t.attention = base.attention then Some t
        else None
    | receipt :: rest -> (
        let admitted, outputs = step t (Host (Execute receipt.request)) in
        if
          not
            (List.exists
               (function
                 | Begin _ -> true
                 | Send _ | Recover _ | Cancel_active | Stop -> false)
               outputs)
        then None
        else
          match (receipt.terminal, admitted.active) with
          | None, Some _
            when rest = []
                 && (not receipt.result_acknowledged)
                 && Int64.compare base.version admitted.version >= 0
                 && base.attention = checkpoint_attention base.capsule ->
              Some
                {
                  admitted with
                  version = base.version;
                  attention = base.attention;
                  restored_unfinished = true;
                }
          | Some terminal, Some active
            when Int64.compare terminal.state_version admitted.version > 0 ->
              let prepared =
                {
                  admitted with
                  version = terminal.state_version;
                  active =
                    Some { active with acknowledged = true; cleanup = true };
                }
              in
              let finished, outputs =
                step prepared
                  (Finish
                     {
                       status = terminal.status;
                       diagnostics = terminal.diagnostics;
                       renewal_id = terminal.renewal_id;
                     })
              in
              if
                not
                  (List.exists
                     (function
                       | Send (Result _) -> true
                       | Send
                           ( Ready _ | Accepted _ | Observation _ | Checkpoint _
                           | Protocol_error _ )
                       | Begin _ | Recover _ | Cancel_active | Stop ->
                           false)
                     outputs)
              then None
              else if receipt.result_acknowledged then
                let request = receipt.request in
                let acknowledged, _ =
                  step finished
                    (Host
                       (Result_ack
                          {
                            identity = request.identity;
                            operation = request.operation;
                            state_version = terminal.state_version;
                          }))
                in
                loop acknowledged rest
              else if rest = [] then loop finished rest
              else None
          | _ -> None)
  in
  loop start entries

let request_json
    {
      identity;
      operation;
      expected_state_version;
      work;
      guidance;
      recovery_mode;
      renewal;
    } =
  let work =
    match work with
    | Reconcile -> obj [ ("type", s "reconcile") ]
    | Session { phase; kind; turn } ->
        obj
          [
            ("type", s "session");
            ("phase", s (phase_string phase));
            ("kind", opt (Option.map Types.Operation_kind.to_label kind));
            ("turn", s turn);
          ]
  in
  envelope "execute" identity
    (operation_fields operation
    @ [
        ("expected_state_version", n expected_state_version);
        ("work", work);
        ("guidance", s guidance);
        ( "recovery_mode",
          s
            (match recovery_mode with
            | Retained_workspace -> "retained_workspace"
            | Lost_workspace -> "lost_workspace") );
        ( "renewal_id",
          opt (match renewal with Ordinary -> None | Renewed id -> Some id) );
        ( "renewed_intent",
          `Bool (match renewal with Ordinary -> false | Renewed _ -> true) );
      ])

let execute_frame request = Yojson.Safe.to_string (request_json request) ^ "\n"

let execute_message request =
  match decode (Yojson.Safe.to_string (request_json request) ^ "\n") with
  | Ok (Execute decoded) when decoded = request -> Some (Execute decoded)
  | Ok
      ( Bind _ | Execute _ | Cancel _ | Checkpoint_ack _ | Result_ack _
      | Shutdown _ )
  | Error (Invalid _ | Incompatible _) ->
      None

let admission_receipt (t : t) =
  match (t.identity, t.receipts) with
  | None, _ -> None
  | Some _, [] -> Some (obj [ ("mode", s "fresh") ])
  | Some _, receipts ->
      Some
        (obj
           [
             ("mode", s "resume");
             ("initial_state_version", n t.initial_version);
             ("initial_attention_id", opt t.initial_attention);
             ("state_version", n t.version);
             ("capsule", s t.capsule);
             ("attention_id", opt t.attention);
             ( "entries",
               `List
                 (List.rev_map
                    (fun receipt ->
                      obj
                        [
                          ("request", request_json receipt.request);
                          ( "terminal",
                            match receipt.terminal with
                            | None -> `Null
                            | Some terminal ->
                                obj
                                  [
                                    ("state_version", n terminal.state_version);
                                    ("status", s (status_name terminal.status));
                                    ("diagnostics", s terminal.diagnostics);
                                    ("renewal_id", opt terminal.renewal_id);
                                  ] );
                          ( "result_acknowledged",
                            `Bool receipt.result_acknowledged );
                        ])
                    receipts) );
           ])

let encode_host msg =
  let admission_json = function
    | Fresh -> obj [ ("mode", s "fresh") ]
    | Resume r -> (
        let receipt_state =
          {
            (initial ()) with
            identity = Some { run_id = "receipt"; patch_id = "receipt" };
            initial_version = r.initial_version;
            initial_attention = r.initial_attention;
            version = r.committed_version;
            capsule = r.committed_capsule;
            attention = r.committed_attention;
            receipts = List.rev r.entries;
          }
        in
        match admission_receipt receipt_state with
        | Some j -> j
        | None -> obj [ ("mode", s "invalid") ])
  in
  let json =
    match msg with
    | Execute r -> request_json r
    | Bind
        {
          identity;
          worker_id;
          workspace_id;
          context;
          capsule;
          state_version;
          attention_id;
          admission;
        } ->
        envelope "bind" identity
          [
            ("worker_id", s worker_id);
            ("workspace_id", s workspace_id);
            ("context", s context);
            ("capsule", s capsule);
            ("state_version", n state_version);
            ("attention_id", opt attention_id);
            ("admission", admission_json admission);
          ]
    | Cancel { identity; operation } ->
        envelope "cancel" identity (operation_fields operation)
    | Checkpoint_ack
        { identity; operation; proposal_id; state_version; capsule; accepted }
      ->
        envelope "checkpoint_ack" identity
          (operation_fields operation
          @ [
              ("proposal_id", s proposal_id);
              ("state_version", n state_version);
              ("capsule", s capsule);
              ("accepted", `Bool accepted);
            ])
    | Result_ack { identity; operation; state_version } ->
        envelope "result_ack" identity
          (operation_fields operation @ [ ("state_version", n state_version) ])
    | Shutdown identity -> envelope "shutdown" identity []
  in
  Yojson.Safe.to_string json ^ "\n"

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

type operation_kind =
  | Session of { phase : string; kind : string option; turn : string }
  | Reconcile

type recovery_mode = Retained_workspace | Lost_workspace

type host_message =
  | Bind of {
      identity : identity;
      worker_id : string;
      workspace_id : string;
      context : string;
      capsule : string;
      state_version : int64;
      attention_id : string option;
    }
  | Execute of {
      identity : identity;
      operation : operation;
      expected_state_version : int64;
      work : operation_kind;
      guidance : string;
      recovery_mode : recovery_mode;
      renewal_id : string option;
      renewed_intent : bool;
    }
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

type result_status =
  | Completed_ok
  | Completed_no_commits
  | Failed
  | Cancelled
  | Retryable
  | Attention
  | Merged_observed

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

let decode line =
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
    | Some (`Assoc xs as json) -> (
        match get xs "version" with
        | Some (`Int 1) -> (
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
                        optional_id f "attention_id" )
                    with
                    | ( Some worker_id,
                        Some workspace_id,
                        Some context,
                        Some capsule,
                        Some state_version,
                        Some attention_id ) ->
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
                          match
                            fields [ "type"; "phase"; "kind"; "turn" ] j
                          with
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
                                  Some turn )
                                when List.mem phase [ "start"; "respond" ]
                                     &&
                                     match kind with
                                     | None -> true
                                     | Some name ->
                                         List.mem name
                                           [
                                             "uncommitted-changes";
                                             "rebase";
                                             "human";
                                             "merge-conflict";
                                             "ci";
                                             "review-comments";
                                             "pr-body";
                                             "findings";
                                           ] ->
                                  Some (Session { phase; kind; turn })
                              | _ -> None)
                          | None -> (
                              match fields [ "type" ] j with
                              | Some w
                                when get w "type" = Some (`String "reconcile")
                                ->
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
                        Some (`Bool renewed_intent) ) ->
                        Ok
                          (Execute
                             {
                               identity;
                               operation;
                               expected_state_version;
                               work;
                               guidance;
                               recovery_mode;
                               renewal_id;
                               renewed_intent;
                             })
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
                      @ [
                          "proposal_id"; "state_version"; "capsule"; "accepted";
                        ])
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
    | _ -> invalid

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
    ([ ("version", `Int 1); ("type", s kind) ]
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
                ("version", `Int 1);
                ("type", s "protocol_error");
                ("reason", s reason);
              ])
  in
  let line = Yojson.Safe.to_string json ^ "\n" in
  if String.length line <= max_frame && valid_utf8 line then line
  else
    "{\"version\":1,\"type\":\"protocol_error\",\"reason\":\"worker output \
     exceeds frame limit\"}\n"

type output =
  | Send of worker_message
  | Begin of host_message
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
  last_seq : int64 option;
  seen : (operation * host_message) list;
  active : active option;
  last_result : worker_message option;
  attention : string option;
  consumed_renewals : string list;
  stopped : bool;
}

let initial () =
  {
    identity = None;
    binding = None;
    version = 0L;
    last_seq = None;
    seen = [];
    active = None;
    last_result = None;
    attention = None;
    consumed_renewals = [];
    stopped = false;
  }

let state_version t =
  match t.identity with None -> None | Some _ -> Some t.version

let is_stopped t = t.stopped
let error t reason = [ Send (Protocol_error { identity = t.identity; reason }) ]
let bound t identity = t.identity = Some identity

let active_match t identity operation =
  match t.active with
  | Some a -> bound t identity && a.operation = operation
  | None -> false

let stop t = ({ t with stopped = true }, [ Stop ])

type event =
  | Host of host_message
  | Propose_checkpoint of { proposal_id : string; capsule : string }
  | Finish of {
      status : result_status;
      diagnostics : string;
      renewal_id : string option;
    }
  | Cleanup_complete
  | Host_lost

let step t event =
  if t.stopped then (t, [])
  else
    match event with
    | Host
        (Bind
           ({ identity; worker_id; state_version; attention_id; _ } as binding))
      -> (
        match t.identity with
        | None ->
            ( {
                t with
                identity = Some identity;
                binding = Some (Bind binding);
                version = state_version;
                attention = attention_id;
              },
              [ Send (Ready { identity; worker_id; state_version }) ] )
        | Some old when old = identity && t.binding = Some (Bind binding) ->
            (t, [ Send (Ready { identity; worker_id; state_version }) ])
        | _ ->
            let t, e = stop t in
            (t, error t "binding changed" @ e))
    | Host
        (Execute
           ({
              identity;
              operation;
              expected_state_version;
              renewal_id;
              renewed_intent;
              _;
            } as request)) -> (
        if not (bound t identity) then (t, error t "unbound identity")
        else
          match
            List.find_opt
              (fun (op, _) ->
                op = operation
                || op.assignment_seq = operation.assignment_seq
                || op.operation_id = operation.operation_id
                || op.attempt_id = operation.attempt_id)
              t.seen
          with
          | Some (old_op, old_request)
            when old_op = operation && old_request = Execute request ->
              let replay =
                [ Send (Accepted { identity; operation; replay = true }) ]
              in
              let replay =
                match t.last_result with
                | Some (Result { operation = old; _ } as result)
                  when old = operation ->
                    replay @ [ Send result ]
                | Some
                    ( Ready _ | Accepted _ | Observation _ | Checkpoint _
                    | Protocol_error _ | Result _ )
                | None ->
                    replay
              in
              (t, replay)
          | Some _ -> (t, error t "identity or sequence reused")
          | None ->
              let next =
                match t.last_seq with
                | None -> operation.assignment_seq = 1L
                | Some n ->
                    Int64.compare n Int64.max_int < 0
                    && operation.assignment_seq = Int64.succ n
              in
              let renewal_ok =
                match (t.attention, renewal_id) with
                | None, None -> not renewed_intent
                | Some expected, Some supplied ->
                    renewed_intent && expected = supplied
                    && not (List.mem supplied t.consumed_renewals)
                | _ -> false
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
                    last_seq = Some operation.assignment_seq;
                    seen = (operation, Execute request) :: t.seen;
                    last_result = None;
                    attention = None;
                    consumed_renewals =
                      (match renewal_id with
                      | None -> t.consumed_renewals
                      | Some x -> x :: t.consumed_renewals);
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
          match t.last_result with
          | Some (Result { operation = op; state_version = ver; _ }) ->
              operation = op && state_version = ver
          | Some
              ( Ready _ | Accepted _ | Observation _ | Checkpoint _
              | Protocol_error _ )
          | None ->
              false
        in
        match t.active with
        | Some a
          when matches && bound t identity && a.operation = operation
               && a.cleanup && a.pending = None ->
            ({ t with active = None }, [])
        | Some _ | None -> (t, error t "stale result acknowledgment"))
    | Host (Shutdown identity) -> (
        if not (bound t identity) then (t, error t "unbound shutdown")
        else
          match t.active with
          | None -> stop t
          | Some a ->
              ( { t with active = Some { a with cancelled = true } },
                [ Cancel_active ] ))
    | Propose_checkpoint { proposal_id; capsule } -> (
        match (t.identity, t.active) with
        | Some identity, Some a
          when a.pending = None && a.terminal = None && valid_id proposal_id
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
          when a.pending = None && a.acknowledged && a.cleanup
               && a.terminal = None
               && ((not a.cancelled) || status = Cancelled)
               && valid_text diagnostics
               &&
               match (status, renewal_id) with
               | Attention, Some x -> valid_id x
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
                last_result = Some result;
                attention =
                  (match status with
                  | Attention -> renewal_id
                  | Completed_ok | Completed_no_commits | Failed | Cancelled
                  | Retryable | Merged_observed ->
                      None);
              },
              [ Send result ] )
        | _ -> (t, error t "terminal result not admissible"))
    | Host_lost -> (
        let t, e = stop t in
        (t, match t.active with Some _ -> Cancel_active :: e | None -> e))

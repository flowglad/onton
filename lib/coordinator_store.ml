(* @archlint.module state
   @archlint.domain coordinator-store *)

open Base

[@@@warning "-4"]

type envelope = {
  version : int64;
  write_id : string;
  state : Coordinator_state.t;
}

type receipt = { write_id : string; state_version : int64 }

type error =
  | Invalid of string
  | Helper of Resource_client.error
  | Refused of Resource_protocol.response
  | Uncertain of string
  | Conflict of string

type pending = {
  id : string;
  expected : int64;
  payload : string;
  request : Resource_protocol.request;
}

type t = {
  client : Resource_client.t;
  run_id : string;
  coordinator_id : string;
  owner_epoch : int64;
  mutable revision : int64;
  next_id : unit -> string;
  mutable pending : pending option;
}

let create ~client ~run_id ~coordinator_id ~owner_epoch ~revision ~next_id =
  {
    client;
    run_id;
    coordinator_id;
    owner_epoch;
    revision;
    next_id;
    pending = None;
  }

let state e = e.state
let version e = e.version
let write_id (e : envelope) = e.write_id
let initial state = { version = 0L; write_id = "initial"; state }
let with_state e state = { e with state }

let committed e (receipt : receipt) =
  { e with version = receipt.state_version; write_id = receipt.write_id }

let encode e =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("schema", `String "onton-coordinator-1");
         ("version", `String (Int64.to_string e.version));
         ("write_id", `String e.write_id);
         ("state", `String (Coordinator_state.encode e.state));
       ])

let decode bytes =
  if String.length bytes > 8 * 1024 * 1024 then
    Error (Invalid "oversized state")
  else
    try
      match Yojson.Safe.from_string bytes with
      | `Assoc
          [
            ("schema", `String "onton-coordinator-1");
            ("version", `String version);
            ("write_id", `String write_id);
            ("state", `String raw);
          ] -> (
          match Int64.of_string_opt version with
          | None -> Error (Invalid "invalid coordinator version")
          | Some version -> (
              match Coordinator_state.decode raw with
              | Ok state
                when Int64.(version > 0L) && not (String.is_empty write_id) ->
                  Ok { version; write_id; state }
              | Ok _ | Error _ -> Error (Invalid "invalid coordinator envelope")
              ))
      | _ -> Error (Invalid "incompatible coordinator envelope")
    with _ -> Error (Invalid "malformed coordinator envelope")

let request t ~op ~request_id extra =
  let fields =
    [
      ("version", `String "resource-v1");
      ("op", `String op);
      ("run_id", `String t.run_id);
      ("request_id", `String request_id);
      ("coordinator_id", `String t.coordinator_id);
      ("owner_epoch", `String (Int64.to_string t.owner_epoch));
      ("revision", `String (Int64.to_string t.revision));
    ]
  in
  match
    Resource_protocol.make_request
      (Yojson.Safe.to_string (`Assoc (fields @ extra)))
  with
  | Ok req -> Ok req
  | Error reason -> Error (Invalid reason)

let send t req =
  match Resource_client.request t.client req with
  | Error e -> Error (Helper e)
  | Ok r -> Ok r

let query t ~op extra =
  let perform () =
    match request t ~op ~request_id:(t.next_id ()) extra with
    | Error e -> Error e
    | Ok req -> send t req
  in
  match perform () with
  | Ok r
    when Poly.equal r.status Resource_protocol.Rejected
         && Poly.equal r.reason Resource_protocol.Stale_guard
         && Option.value_map r.owner_epoch ~default:false
              ~f:(Int64.equal t.owner_epoch)
         && Option.value_map r.revision ~default:false ~f:(fun n ->
             Int64.(n > t.revision)) ->
      t.revision <- Option.value r.revision ~default:t.revision;
      perform ()
  | other -> other

let load t =
  match query t ~op:"state-read" [] with
  | Error e -> Error e
  | Ok r -> (
      match
        (r.status, r.payload, r.state_version, r.revision, r.owner_epoch)
      with
      | Ok, Some encoded, Some version, Some revision, Some epoch
        when Int64.equal epoch t.owner_epoch -> (
          match Base64.decode encoded with
          | Error _ -> Error (Invalid "invalid state-read base64")
          | Ok bytes -> (
              match decode bytes with
              | Error e -> Error e
              | Ok e when Int64.equal e.version version ->
                  t.revision <- revision;
                  Ok e
              | Ok _ -> Error (Invalid "envelope and pointer version differ")))
      | Ok, _, _, _, _ ->
          Error (Invalid "state-read missing current bytes or guards")
      | Pending, _, _, _, _ | Attention, _, _, _, _ ->
          Error (Uncertain "state-read not confirmed")
      | Rejected, _, _, _, _ -> Error (Refused r))

let confirm t pending =
  (* A known receipt is metadata, not historical bytes. Check current bytes too. *)
  match query t ~op:"write-status" [ ("write_id", `String pending.id) ] with
  | Error _ -> Error (Uncertain "write receipt unavailable")
  | Ok r -> (
      if
        (not (Poly.equal r.status Resource_protocol.Ok))
        || not
             (Option.value_map r.receipt_state_version ~default:false
                ~f:(Int64.equal Int64.(succ pending.expected)))
      then Error (Uncertain "write receipt unconfirmed")
      else
        match load t with
        | Ok current
          when Int64.equal current.version Int64.(succ pending.expected)
               && String.equal current.write_id pending.id
               && String.equal (encode current) pending.payload ->
            t.pending <- None;
            Ok { write_id = pending.id; state_version = current.version }
        | Ok _ -> Error (Conflict "write is not current")
        | Error (Invalid reason) -> Error (Invalid reason)
        | Error _ -> Error (Uncertain "current bytes unavailable"))

let commit t ~expected_version ~write_id e =
  if
    Int64.(expected_version < 0L || expected_version = max_value)
    || String.is_empty write_id
  then Error (Invalid "invalid write identity/version")
  else
    let next = { e with version = Int64.succ expected_version; write_id } in
    let payload = encode next in
    if String.length payload > (8 * 1024 * 1024) - 65536 then
      Error (Invalid "emergency state reserve exhausted")
    else
      let prospective =
        match t.pending with
        | Some p
          when String.equal p.id write_id
               && Int64.equal p.expected expected_version
               && String.equal p.payload payload ->
            Ok p
        | Some _ -> Error (Uncertain "previous write unresolved")
        | None -> (
            if not (Int64.equal e.version expected_version) then
              Error (Conflict "stale expected version")
            else
              match
                request t ~op:"state-write" ~request_id:write_id
                  [
                    ("write_id", `String write_id);
                    ("state_version", `String (Int64.to_string expected_version));
                    ("payload", `String (Base64.encode_string payload));
                  ]
              with
              | Error err -> Error err
              | Ok request ->
                  let p =
                    {
                      id = write_id;
                      expected = expected_version;
                      payload;
                      request;
                    }
                  in
                  t.pending <- Some p;
                  Ok p)
      in
      match prospective with
      | Error e -> Error e
      | Ok p -> (
          match send t p.request with
          | Ok r when Poly.equal r.status Resource_protocol.Rejected -> (
              match confirm t p with
              | Ok receipt -> Ok receipt
              | Error _ -> Error (Refused r))
          | Ok _ | Error _ -> confirm t p)

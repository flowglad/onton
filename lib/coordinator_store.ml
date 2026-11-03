(* @archlint.module state
   @archlint.domain coordinator-state *)

open Base

[@@@warning "-4"]

type serialized = {
  plain : string;
  packed : string Lazy.t;
  stream_bytes : (string * string) list;
  largest_stream : int;
}

type envelope = {
  version : int64;
  write_id : string;
  state : Coordinator_state.t;
  streams : (string * Worker_stream.t) list;
  serialized : serialized Lazy.t;
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
  mutable loaded : (string * envelope) option;
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
    loaded = None;
  }

let authority t = (t.run_id, t.coordinator_id, t.owner_epoch, t.revision)
let state e = e.state
let version e = e.version
let write_id (e : envelope) = e.write_id
let archive_prefix = "onton-coordinator-3\n"

(* The envelope is immutable and opaque, so derived bytes belong to this exact
   value. Every constructor replaces the cache along with its contents. *)
let make_envelope ?previous ~version ~write_id ~state ~streams () =
  let serialized =
    lazy
      (let streams =
         List.map streams ~f:(fun (id, stream) ->
             let reused =
               Option.bind previous ~f:(fun (e : envelope) ->
                   match List.Assoc.find e.streams id ~equal:String.equal with
                   | Some old when phys_equal old stream ->
                       List.Assoc.find (Lazy.force e.serialized).stream_bytes id
                         ~equal:String.equal
                   | Some _ | None -> None)
             in
             ( id,
               match reused with
               | Some bytes -> bytes
               | None -> Worker_stream.encode stream ))
       in
       let plain =
         Yojson.Safe.to_string
           (`Assoc
              [
                ("schema", `String "onton-coordinator-3");
                ("version", `String (Int64.to_string version));
                ("write_id", `String write_id);
                ("state", `String (Coordinator_state.encode state));
                ( "streams",
                  `List
                    (List.map streams ~f:(fun (id, bytes) ->
                         `List [ `String id; `String bytes ])) );
              ])
       in
       {
         plain;
         stream_bytes = streams;
         packed = lazy (archive_prefix ^ Coordinator_archive.encode plain);
         largest_stream =
           List.fold streams ~init:0 ~f:(fun n (_, bytes) ->
               Int.max n (String.length bytes));
       })
  in
  { version; write_id; state; streams; serialized }

let initial state =
  make_envelope ~version:0L ~write_id:"initial" ~state ~streams:[] ()

let streams e = e.streams

let stream e assignment =
  List.Assoc.find e.streams assignment ~equal:String.equal

let with_stream e assignment stream =
  make_envelope ~previous:e ~version:e.version ~write_id:e.write_id
    ~state:e.state
    ~streams:
      ((assignment, stream)
      :: List.Assoc.remove e.streams assignment ~equal:String.equal)
    ()

let with_state e state =
  make_envelope ~previous:e ~version:e.version ~write_id:e.write_id ~state
    ~streams:e.streams ()

let committed e (receipt : receipt) =
  make_envelope ~previous:e ~version:receipt.state_version
    ~write_id:receipt.write_id ~state:e.state ~streams:e.streams ()

let encode e = Lazy.force (Lazy.force e.serialized).packed

let decode ?previous bytes =
  if String.length bytes > 8 * 1024 * 1024 then
    Error (Invalid "oversized state")
  else
    try
      if not (String.is_prefix bytes ~prefix:archive_prefix) then
        failwith "archive schema";
      let packed = String.drop_prefix bytes (String.length archive_prefix) in
      let bytes =
        match
          Coordinator_archive.decode ~max_bytes:(64 * 1024 * 1024) packed
        with
        | Ok bytes -> bytes
        | Error _ -> failwith "invalid archive"
      in
      let fields =
        match Yojson.Safe.from_string bytes with `Assoc xs -> xs | _ -> []
      in
      let streams =
        match List.Assoc.find fields "streams" ~equal:String.equal with
        | None -> failwith "missing streams"
        | Some (`List xs) ->
            List.map xs ~f:(function
              | `List [ `String id; `String bytes ] -> (
                  let cached =
                    Option.bind previous ~f:(fun e ->
                        let serialized = Lazy.force e.serialized in
                        match
                          List.Assoc.find serialized.stream_bytes id
                            ~equal:String.equal
                        with
                        | Some old when String.equal old bytes -> stream e id
                        | Some _ | None -> None)
                  in
                  match
                    match cached with
                    | Some stream -> Ok stream
                    | None -> Worker_stream.decode bytes
                  with
                  | Ok stream -> (id, stream)
                  | Error _ -> failwith "invalid stream")
              | _ -> failwith "invalid stream record")
        | Some _ -> failwith "invalid streams"
      in
      if List.contains_dup (List.map streams ~f:fst) ~compare:String.compare
      then failwith "duplicate stream";
      match List.Assoc.remove fields "streams" ~equal:String.equal with
      | [
       ("schema", `String "onton-coordinator-3");
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
                  let bound =
                    List.for_all streams ~f:(fun (id, stream) ->
                        match
                          ( List.find (Coordinator_state.assignments state)
                              ~f:(fun a -> String.equal a.id id),
                            Worker_stream.identity stream,
                            Worker_stream.capsule stream,
                            Worker_stream.receipt stream )
                        with
                        | ( Some assignment,
                            Some identity,
                            Some bytes,
                            Some receipt ) -> (
                            String.equal identity.run_id
                              (Coordinator_state.run_id state)
                            && String.equal identity.patch_id
                                 (Types.Patch_id.to_string assignment.patch)
                            &&
                            match
                              Coordinator_state.capsules state
                              |> List.filter
                                   ~f:(fun (c : Coordinator_state.capsule) ->
                                     String.equal c.assignment id)
                              |> List.max_elt
                                   ~compare:(fun
                                       (a : Coordinator_state.capsule) b ->
                                     Int64.compare a.version b.version)
                            with
                            | None -> String.equal bytes "{}"
                            | Some capsule ->
                                String.equal capsule.bytes bytes
                                && String.equal capsule.receipt receipt)
                        | _ -> false)
                  in
                  if bound then
                    Ok
                      (make_envelope ?previous ~version ~write_id ~state
                         ~streams ())
                  else
                    Error
                      (Invalid "stream and coordinator owner artifacts disagree")
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
  | Ok r ->
      (* A matching successful reply supplies the next query guard. This is not
         a commit receipt: confirmation still reads the receipt and current bytes. *)
      if
        Poly.equal r.status Resource_protocol.Ok
        && Option.value_map r.owner_epoch ~default:false
             ~f:(Int64.equal t.owner_epoch)
        && Option.value_map r.run_id ~default:false ~f:(String.equal t.run_id)
      then
        Option.iter r.revision ~f:(fun revision ->
            if Int64.(revision > t.revision) then t.revision <- revision);
      Ok r

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

let load_snapshot t =
  match query t ~op:"state-read" [] with
  | Error e -> Error e
  | Ok r -> (
      match
        ( r.status,
          r.run_id,
          r.payload,
          r.state_version,
          r.revision,
          r.owner_epoch )
      with
      | Ok, Some run_id, Some encoded, Some version, Some revision, Some epoch
        when String.equal run_id t.run_id && Int64.equal epoch t.owner_epoch
        -> (
          match Base64.decode encoded with
          | Error _ -> Error (Invalid "invalid state-read base64")
          | Ok bytes -> (
              let decoded =
                match t.loaded with
                | Some (old, e) when String.equal old bytes -> Ok e
                | previous ->
                    decode ?previous:(Option.map previous ~f:snd) bytes
              in
              match decoded with
              | Error e -> Error e
              | Ok e
                when Int64.equal e.version version
                     && String.equal (Coordinator_state.run_id e.state) t.run_id
                ->
                  t.revision <- revision;
                  t.loaded <- Some (bytes, e);
                  Ok (bytes, e)
              | Ok _ -> Error (Invalid "envelope identity/version mismatch")))
      | Ok, _, _, _, _, _ ->
          Error (Invalid "state-read missing current bytes or guards")
      | Pending, _, _, _, _, _ | Attention, _, _, _, _, _ ->
          Error (Uncertain "state-read not confirmed")
      | Rejected, _, _, _, _, _ -> Error (Refused r))

let load t = Result.map (load_snapshot t) ~f:snd

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
        match load_snapshot t with
        | Ok (bytes, current)
          when Int64.equal current.version Int64.(succ pending.expected)
               && String.equal current.write_id pending.id
               && String.equal bytes pending.payload ->
            t.pending <- None;
            Ok { write_id = pending.id; state_version = current.version }
        | Ok _ -> Error (Conflict "write is not current")
        | Error (Invalid reason) -> Error (Invalid reason)
        | Error _ -> Error (Uncertain "current bytes unavailable"))

let commit ?(emergency = false) t ~expected_version ~write_id e =
  if
    Int64.(expected_version < 0L || expected_version = max_value)
    || String.is_empty write_id
  then Error (Invalid "invalid write identity/version")
  else
    let next =
      make_envelope ~previous:e
        ~version:(Int64.succ expected_version)
        ~write_id ~state:e.state ~streams:e.streams ()
    in
    let serialized = Lazy.force next.serialized in
    let plain = serialized.plain in
    let payload =
      if String.length plain > 64 * 1024 * 1024 then ""
      else Lazy.force serialized.packed
    in
    if
      (String.length plain > (64 * 1024 * 1024) - if emergency then 0 else 65536)
      || (String.length payload
         > (8 * 1024 * 1024) - if emergency then 0 else 65536)
      || serialized.largest_stream
         > (8 * 1024 * 1024) - if emergency then 0 else 65536
    then Error (Invalid "emergency state reserve exhausted")
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
              | Error confirmation_error -> Error confirmation_error)
          | Ok _ | Error _ -> confirm t p)

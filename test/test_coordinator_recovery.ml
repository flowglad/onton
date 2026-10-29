(* @archlint.module test
   @archlint.domain coordinator-recovery *)

open Onton_core
open Onton
open Coordinator_state
open Coordinator_store

[@@@warning "-4-42"]

let fail message = failwith message
let check yes message = if not yes then fail message

(* A public resource-v1 fixture: current bytes and known receipt metadata only.
   It deliberately has no resource inventory, historical bytes or reconnect. *)
type fixture = {
  mutable payload : string option;
  mutable version : int64;
  mutable revision : int;
  mutable writes : (string * int64) list;
  mutable drop_write : bool;
  mutable refuse_read : bool;
  mutable refuse_receipt : bool;
}

let get field xs = List.assoc_opt field xs

let string field xs =
  match get field xs with
  | Some (`String s) -> s
  | _ -> fail ("missing " ^ field)

let reply ?(reason = "none") ?run_id ?payload ?state_version
    ?receipt_state_version ?(revision = 1) status =
  let fields =
    [
      ("status", `String status);
      ("reason", `String reason);
      ("owner_epoch", `Int 1);
      ("revision", `Int revision);
    ]
  in
  let optional name value =
    match value with None -> [] | Some v -> [ (name, v) ]
  in
  Yojson.Safe.to_string
    (`Assoc
       (fields
       @ optional "run_id" (Option.map (fun s -> `String s) run_id)
       @ optional "payload"
           (Option.map (fun s -> `String (Base64.encode_string s)) payload)
       @ optional "state_version"
           (Option.map (fun n -> `Intlit (Int64.to_string n)) state_version)
       @ optional "receipt_state_version"
           (Option.map
              (fun n -> `Intlit (Int64.to_string n))
              receipt_state_version)))

let transport (f : fixture) ~argv:_ ~input =
  let fields =
    match Yojson.Safe.from_string input with
    | `Assoc xs -> xs
    | _ -> fail "invalid helper request"
  in
  let op = string "op" fields in
  let stale =
    (String.equal op "state-read" || String.equal op "write-status")
    && int_of_string (string "revision" fields) <> f.revision
  in
  let response =
    if stale then
      Ok (0, reply ~revision:f.revision ~reason:"stale_guard" "rejected", "")
    else
      match op with
      | "state-write" ->
          let write_id = string "write_id" fields in
          let version = Int64.of_string (string "state_version" fields) in
          let bytes =
            match Base64.decode (string "payload" fields) with
            | Ok bytes -> bytes
            | Error _ -> fail "invalid base64"
          in
          if Int64.equal version f.version then (
            f.version <- Int64.succ version;
            f.revision <- f.revision + 1;
            f.payload <- Some bytes;
            f.writes <- (write_id, f.version) :: f.writes);
          if f.drop_write then Error "lost write response"
          else
            Ok (0, reply ~revision:f.revision ~state_version:f.version "ok", "")
      | "state-read" -> (
          if f.refuse_read then Ok (0, reply "pending", "")
          else
            match f.payload with
            | None -> Ok (0, reply ~reason:"not_found" "rejected", "")
            | Some bytes ->
                Ok
                  ( 0,
                    reply ~run_id:(string "run_id" fields) ~revision:f.revision
                      ~payload:bytes ~state_version:f.version "ok",
                    "" ))
      | "write-status" -> (
          if f.refuse_receipt then Ok (0, reply "pending", "")
          else
            match List.assoc_opt (string "write_id" fields) f.writes with
            | None -> Ok (0, reply ~reason:"not_found" "rejected", "")
            | Some v ->
                Ok
                  ( 0,
                    reply ~revision:f.revision ~state_version:f.version
                      ~receipt_state_version:v "ok",
                    "" ))
      | op -> fail ("unexpected op " ^ op)
  in
  response

let fixture () =
  {
    payload = None;
    version = 0L;
    revision = 1;
    writes = [];
    drop_write = false;
    refuse_read = false;
    refuse_receipt = false;
  }

let setup f =
  let counter = ref 0 in
  let next_id () =
    incr counter;
    "request-" ^ string_of_int !counter
  in
  let client =
    Resource_client.create ~helper:"fixture" ~transport:(transport f)
  in
  let store =
    Coordinator_store.create ~client ~run_id:"r" ~coordinator_id:"c"
      ~owner_epoch:1L ~revision:1L ~next_id
  in
  let state =
    Coordinator_state.create ~run_id:"r" ~config_id:"cfg" ~plan_id:"plan"
      ~plan_bytes:"plan" ~config_bytes:"config" ~capacity:2
      ~budget_bytes:((8 * 1024 * 1024) - 65536)
  in
  (store, state, next_id)

let committed = function
  | Ok r -> r
  | Error (Coordinator_store.Invalid why) -> fail ("invalid: " ^ why)
  | Error (Uncertain why) -> fail ("uncertain: " ^ why)
  | Error (Conflict why) -> fail ("conflict: " ^ why)
  | Error (Refused _) -> fail "helper refused"
  | Error (Helper _) -> fail "helper failed"

let test_lost_response () =
  let f = fixture () in
  let store, state, next_id = setup f in
  f.drop_write <- true;
  let r =
    committed
      (Coordinator_store.commit store ~expected_version:0L ~write_id:"write-1"
         (Coordinator_store.initial state))
  in
  check
    (Int64.equal r.state_version 1L)
    "lost response did not read back exact bytes";
  let host =
    match Coordinator_host.restore ~store ~next_write_id:next_id with
    | Ok host -> host
    | Error _ -> fail "restore lost committed envelope"
  in
  check
    (Int64.equal
       (Coordinator_store.version (Coordinator_host.envelope host))
       1L)
    "restored pointer";
  let operation : Coordinator_state.operation =
    {
      id = "logical-1";
      patch = Types.Patch_id.of_string "1";
      used = 0;
      budget = 3;
      retry_after = 0L;
      attention = false;
      unfinished = true;
      maintenance = false;
    }
  in
  (match Coordinator_host.submit host (Coordinator_state.Admit operation) with
  | Ok (Coordinator_host.Applied [ Coordinator_state.Accepted ]) -> ()
  | _ -> fail "durable admission failed");
  let restored =
    match Coordinator_host.restore ~store ~next_write_id:next_id with
    | Ok h -> h
    | Error _ -> fail "admission not restored"
  in
  check
    (List.length
       (Coordinator_state.operations (Coordinator_host.state restored))
    = 1)
    "operation did not survive replacement"

let test_uncertain_blocks () =
  let f = fixture () in
  let store, state, _ = setup f in
  f.drop_write <- true;
  f.refuse_receipt <- true;
  let initial = Coordinator_store.initial state in
  (match
     Coordinator_store.commit store ~expected_version:0L ~write_id:"write-1"
       initial
   with
  | Error (Coordinator_store.Uncertain _) -> ()
  | _ -> fail "unknown write was acknowledged");
  (match
     Coordinator_store.commit store ~expected_version:0L ~write_id:"write-2"
       initial
   with
  | Error (Coordinator_store.Uncertain _) -> ()
  | _ -> fail "overlapping writer was allowed");
  f.refuse_receipt <- false;
  let _ =
    committed
      (Coordinator_store.commit store ~expected_version:0L ~write_id:"write-1"
         initial)
  in
  check (List.length f.writes = 1) "exact retry duplicated storage write"

let test_missing_current_read () =
  let f = fixture () in
  let store, state, _ = setup f in
  f.refuse_read <- true;
  match
    Coordinator_store.commit store ~expected_version:0L ~write_id:"write-1"
      (Coordinator_store.initial state)
  with
  | Error (Coordinator_store.Uncertain _) -> ()
  | _ -> fail "receipt without current bytes was treated as commit"

let test_host_barrier () =
  let f = fixture () in
  let store, initial_state, next_id = setup f in
  let _ =
    committed
      (Coordinator_store.commit store ~expected_version:0L
         ~write_id:"initial-write"
         (Coordinator_store.initial initial_state))
  in
  let host =
    match Coordinator_host.restore ~store ~next_write_id:next_id with
    | Ok h -> h
    | Error _ -> fail "host did not restore"
  in
  let op : Coordinator_state.operation =
    {
      id = "op";
      patch = Types.Patch_id.of_string "1";
      used = 0;
      budget = 3;
      retry_after = 0L;
      attention = false;
      unfinished = true;
      maintenance = false;
    }
  in
  f.refuse_receipt <- true;
  (match Coordinator_host.submit host (Admit op) with
  | Error (Coordinator_host.Store (Uncertain _)) -> ()
  | _ -> fail "admission escaped unconfirmed write");
  check
    (Coordinator_state.operations (Coordinator_host.state host) = [])
    "admission was published in memory before confirmation";
  (match Coordinator_host.submit host (Input ("input", "1", 1L, "new")) with
  | Error (Coordinator_host.Invalid _) -> ()
  | _ -> fail "a second writer overlapped an unresolved transition");
  f.refuse_receipt <- false;
  (match Coordinator_host.submit host (Admit op) with
  | Ok (Coordinator_host.Applied [ Accepted ]) -> ()
  | _ -> fail "same-owner exact pending transition did not recover");
  check
    (List.length (Coordinator_state.operations (Coordinator_host.state host))
    = 1)
    "confirmed intent not published";
  check (List.length f.writes = 2) "retry created a second physical write"

let test_capacity_and_retirement () =
  let f = fixture () in
  let store, state, next_id = setup f in
  let _ =
    committed
      (Coordinator_store.commit store ~expected_version:0L
         ~write_id:"initial-write"
         (Coordinator_store.initial state))
  in
  let host =
    match Coordinator_host.restore ~store ~next_write_id:next_id with
    | Ok h -> h
    | Error _ -> fail "host did not restore"
  in
  let apply event =
    match Coordinator_host.submit host event with
    | Ok (Coordinator_host.Applied outcomes) -> outcomes
    | _ -> fail "unexpected rejected transition"
  in
  let patch n = Types.Patch_id.of_string (string_of_int n) in
  for n = 1 to 5 do
    let id = string_of_int n in
    let operation : Coordinator_state.operation =
      {
        id = "logical-" ^ id;
        patch = patch n;
        used = 0;
        budget = 3;
        retry_after = 0L;
        attention = false;
        unfinished = true;
        maintenance = false;
      }
    in
    ignore (apply (Admit operation));
    let launch =
      Printf.sprintf
        {|{"version":"resource-v1","op":"launch","run_id":"r","request_id":"launch-%s","coordinator_id":"c","owner_epoch":"1","revision":"1","chain_id":"chain-%s","patch_id":"%s","resource_id":"resource-%s","launch_digest":"digest-%s","manifest":{"executable":"worker"}}|}
        id id id id id
    in
    ignore (apply (Resource_request ("resource-" ^ id, launch)));
    ignore (apply (Bind_resource ("resource-" ^ id, patch n)))
  done;
  let assignment n : Coordinator_state.assignment =
    let id = string_of_int n in
    {
      Coordinator_state.id = "assignment-" ^ id;
      operation = "logical-" ^ id;
      patch = patch n;
      resource = "resource-" ^ id;
      request = "execute-" ^ id;
      reserved = true;
      retired = false;
      result_committed = false;
      cleanup = false;
    }
  in
  for n = 1 to 2 do
    check
      (apply (Assign (assignment n, 0L))
      = [ Dispatch ("assignment-" ^ string_of_int n) ])
      "dispatch must follow durable reservation"
  done;
  for n = 3 to 5 do
    match Coordinator_host.submit host (Assign (assignment n, 0L)) with
    | Ok (Coordinator_host.Rejected _) -> ()
    | _ -> fail "third possibly executing assignment admitted"
  done;
  check
    (Coordinator_state.reserved_count (Coordinator_host.state host) = 2)
    "capacity exceeded";
  ignore (apply (Loss ("resource-1", 0L)));
  check
    (Coordinator_state.reserved_count (Coordinator_host.state host) = 2)
    "stream loss released a reservation";
  (match
     Coordinator_host.retired host ~resource_id:"resource-1"
       ~stop_request:
         (match
            Resource_protocol.make_request
              {|{"version":"resource-v1","op":"observe","run_id":"r","request_id":"obs","coordinator_id":"c","owner_epoch":"1","revision":"1","resource_id":"resource-1"}|}
          with
         | Ok r -> r
         | Error e -> fail e)
       ~response:
         (match Resource_protocol.decode_response (reply "ok") with
         | Ok r -> r
         | Error e -> fail e)
   with
  | Error (Coordinator_host.Invalid _) -> ()
  | _ -> fail "observe-ok was treated as stop proof");
  check
    (Coordinator_state.reserved_count (Coordinator_host.state host) = 2)
    "observe-ok released reservation";
  let stop : Resource_protocol.request =
    match
      Resource_protocol.make_request
        {|{"version":"resource-v1","op":"stop","run_id":"r","request_id":"stop-1","coordinator_id":"c","owner_epoch":"1","revision":"1","resource_id":"resource-1"}|}
    with
    | Ok r -> r
    | Error e -> fail e
  in
  let response =
    match Resource_protocol.decode_response (reply "ok") with
    | Ok r -> r
    | Error e -> fail e
  in
  ignore
    (apply
       (Resource_request
          (stop.Resource_protocol.request_id, stop.Resource_protocol.bytes)));
  (match
     Coordinator_host.retired host ~resource_id:"resource-1" ~stop_request:stop
       ~response
   with
  | Ok (Coordinator_host.Applied [ Accepted ]) -> ()
  | _ -> fail "matching stop-success did not release predecessor");
  check
    (Coordinator_state.reserved_count (Coordinator_host.state host) = 1)
    "positive retirement did not release reservation";
  check
    (apply (Assign (assignment 3, 0L)) = [ Dispatch "assignment-3" ])
    "unrelated eligible patch did not progress after retirement";
  (match
     Coordinator_host.submit host
       (Result_committed ("assignment-3", "execute-3"))
   with
  | Error (Coordinator_host.Invalid _) -> ()
  | _ -> fail "unbound result was adopted");
  check
    (Coordinator_state.reserved_count (Coordinator_host.state host) = 2)
    "unbound result released capacity"

let () =
  test_capacity_and_retirement ();
  test_host_barrier ();
  test_lost_response ();
  test_uncertain_blocks ();
  test_missing_current_read ();
  print_endline
    "coordinator storage: confirmed bytes/receipt and uncertain barriers"

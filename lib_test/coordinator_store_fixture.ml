(* @archlint.module exempt
   @archlint.exempt-reason test-support *)

open Onton_core
open Onton
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
  mutable reject_write : bool;
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
    (String.equal op "state-read"
    || String.equal op "write-status"
    || String.equal op "state-write")
    && not
         (match Int64.of_string_opt (string "revision" fields) with
         | Some revision -> Int64.equal revision (Int64.of_int f.revision)
         | None -> false)
  in
  let response =
    if stale then
      Ok (0, reply ~revision:f.revision ~reason:"stale_guard" "rejected", "")
    else
      match op with
      | "state-write" when f.reject_write ->
          Ok (0, reply ~reason:"storage_uncertain" "rejected", "")
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
            f.writes <- (write_id, f.version) :: f.writes;
            if f.drop_write then Error "lost write response"
            else
              Ok
                (0, reply ~revision:f.revision ~state_version:f.version "ok", ""))
          else
            Ok
              ( 0,
                reply ~revision:f.revision ~reason:"state_version" "rejected",
                "" )
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
    reject_write = false;
  }

let setup ?(run_id = "r") ?(plan_bytes = "plan") f =
  let counter = ref 0 in
  let next_id () =
    incr counter;
    "request-" ^ string_of_int !counter
  in
  let client =
    Resource_client.create ~helper:"fixture" ~transport:(transport f)
  in
  let store =
    Coordinator_store.create ~client ~run_id ~coordinator_id:"c" ~owner_epoch:1L
      ~revision:1L ~next_id
  in
  let state =
    Coordinator_state.create ~run_id ~config_id:"cfg" ~plan_id:"plan"
      ~plan_bytes ~config_bytes:"config" ~capacity:2
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

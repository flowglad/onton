(* @archlint.module stateTest
   @archlint.domain coordinator-state *)

open Base
open Onton
open Onton_core

[@@@warning "-4-42"]

type helper_state = {
  mutable bytes : string option;
  mutable version : int64;
  mutable revision : int64;
  mutable writes : (string * int64) list;
}

let helper_response state ?(reason = "none") ?payload ?state_version
    ?receipt_state_version status =
  let optional name value =
    match value with None -> [] | Some value -> [ (name, value) ]
  in
  Yojson.Safe.to_string
    (`Assoc
       ([
          ("status", `String status);
          ("reason", `String reason);
          ("owner_epoch", `Int 1);
          ("revision", `Intlit (Int64.to_string state.revision));
        ]
       @ optional "payload"
           (Option.map payload ~f:(fun value ->
                `String (Base64.encode_string value)))
       @ optional "state_version"
           (Option.map state_version ~f:(fun value ->
                `Intlit (Int64.to_string value)))
       @ optional "receipt_state_version"
           (Option.map receipt_state_version ~f:(fun value ->
                `Intlit (Int64.to_string value)))))

let field key fields = List.Assoc.find fields key ~equal:String.equal

let required_string key fields =
  match field key fields with
  | Some (`String value) -> value
  | _ -> failwith ("missing " ^ key)

let helper_transport state ~argv:_ ~input =
  let fields =
    match Yojson.Safe.from_string input with
    | `Assoc fields -> fields
    | _ -> failwith "invalid helper request"
  in
  let op = required_string "op" fields in
  let response =
    match op with
    | "state-read" -> (
        match state.bytes with
        | None -> helper_response state ~reason:"not_found" "rejected"
        | Some bytes ->
            helper_response state ~payload:bytes ~state_version:state.version
              "ok")
    | "state-write" ->
        let expected =
          Int64.of_string (required_string "state_version" fields)
        in
        let write_id = required_string "write_id" fields in
        if Int64.equal expected state.version then (
          let encoded = required_string "payload" fields in
          let payload =
            match Base64.decode encoded with
            | Ok payload -> payload
            | Error _ -> failwith "invalid helper payload"
          in
          state.version <- Int64.succ state.version;
          state.revision <- Int64.succ state.revision;
          state.bytes <- Some payload;
          state.writes <- (write_id, state.version) :: state.writes;
          helper_response state ~state_version:state.version "ok")
        else helper_response state ~reason:"state_version" "rejected"
    | "write-status" -> (
        let write_id = required_string "write_id" fields in
        match List.Assoc.find state.writes write_id ~equal:String.equal with
        | None -> helper_response state ~reason:"not_found" "rejected"
        | Some receipt_state_version ->
            helper_response state ~state_version:state.version
              ~receipt_state_version "ok")
    | other -> failwith ("unexpected helper operation " ^ other)
  in
  Ok (0, response, "")

let setup () =
  let helper_state =
    { bytes = None; version = 0L; revision = 1L; writes = [] }
  in
  let counter = ref 0 in
  let next_id () =
    Int.incr counter;
    "request-" ^ Int.to_string !counter
  in
  let client =
    Resource_client.create ~helper:"fixture"
      ~transport:(helper_transport helper_state)
  in
  let store =
    Coordinator_store.create ~client ~run_id:"run" ~coordinator_id:"owner"
      ~owner_epoch:1L ~revision:1L ~next_id
  in
  let state =
    Coordinator_state.create ~run_id:"run" ~config_id:"config" ~plan_id:"plan"
      ~plan_bytes:"plan" ~config_bytes:"config" ~capacity:2
      ~budget_bytes:((8 * 1024 * 1024) - 65536)
  in
  let initial = Coordinator_store.initial state in
  (match
     Coordinator_store.commit store ~expected_version:0L ~write_id:"initial"
       initial
   with
  | Ok _ -> ()
  | Error _ -> failwith "initial coordinator envelope did not commit");
  let host =
    match Coordinator_host.restore ~store ~next_write_id:next_id with
    | Ok host -> host
    | Error _ -> failwith "coordinator host did not restore"
  in
  (helper_state, store, next_id, host)

let admit sequence : Coordinator_state.operation =
  Coordinator_state.
    {
      id = "operation-" ^ Int.to_string sequence;
      patch = Types.Patch_id.of_string ("patch-" ^ Int.to_string sequence);
      used = 0;
      budget = 3;
      retry_after = 0L;
      attention = false;
      unfinished = true;
      maintenance = false;
    }

let histories =
  QCheck2.Test.make
    ~name:"coordinator host persists generated transition histories" ~count:100
    QCheck2.Gen.(list_size (int_range 0 30) (int_range 0 2))
    (fun commands ->
      try
        let _, store, next_id, host = setup () in
        let current_host = ref host in
        let latest_operation = ref None in
        let next_operation = ref 0 in
        let accepted = ref 0 in
        let valid = ref true in
        List.iter commands ~f:(fun command ->
            let event =
              match command with
              | 0 ->
                  let operation = admit !next_operation in
                  Int.incr next_operation;
                  latest_operation := Some operation;
                  Coordinator_state.Admit operation
              | 1 -> (
                  match !latest_operation with
                  | Some operation -> Coordinator_state.Admit operation
                  | None -> Coordinator_state.Loss ("unknown-resource", 0L))
              | _ -> Coordinator_state.Loss ("unknown-resource", 0L)
            in
            let pure_next, _ =
              Coordinator_state.step
                (Coordinator_host.state !current_host)
                event
            in
            let before =
              Coordinator_state.encode (Coordinator_host.state !current_host)
            in
            (match Coordinator_host.submit !current_host event with
            | Ok (Coordinator_host.Applied [ Coordinator_state.Accepted ]) ->
                if command = 0 then Int.incr accepted else valid := false
            | Ok (Coordinator_host.Rejected _) ->
                if command = 0 then valid := false
            | Ok (Coordinator_host.Applied _) | Error _ -> valid := false);
            let after =
              Coordinator_state.encode (Coordinator_host.state !current_host)
            in
            if
              (not (String.equal after (Coordinator_state.encode pure_next)))
              || ((not (String.equal before after)) && command <> 0)
            then valid := false;
            let restored =
              match Coordinator_host.restore ~store ~next_write_id:next_id with
              | Ok restored -> restored
              | Error _ ->
                  valid := false;
                  !current_host
            in
            if
              (not
                 (String.equal after
                    (Coordinator_state.encode (Coordinator_host.state restored))))
              || List.length
                   (Coordinator_state.operations
                      (Coordinator_host.state restored))
                 <> !accepted
              || not
                   (Int64.equal
                      (Coordinator_store.version
                         (Coordinator_host.envelope restored))
                      (Coordinator_store.version
                         (Coordinator_host.envelope !current_host)))
            then valid := false;
            current_host := restored);
        !valid
      with _ -> false)

let () = QCheck_base_runner.run_tests_main [ histories ]

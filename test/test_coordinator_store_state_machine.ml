(* @archlint.module stateTest
   @archlint.domain coordinator-store *)

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
  let response =
    match required_string "op" fields with
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
          let payload =
            match Base64.decode (required_string "payload" fields) with
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
    | op -> failwith ("unexpected helper operation " ^ op)
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
  (helper_state, store, state)

let require_commit = function
  | Ok receipt -> receipt
  | Error _ -> failwith "confirmed coordinator-store write failed"

let histories =
  QCheck2.Test.make
    ~name:"coordinator store preserves exact write histories and versions"
    ~count:100
    QCheck2.Gen.(list_size (int_range 1 20) (int_range 0 1000))
    (fun history ->
      try
        let _, store, state = setup () in
        let initial = Coordinator_store.initial state in
        let initial_receipt =
          Coordinator_store.commit store ~expected_version:0L ~write_id:"seed"
            initial
          |> require_commit
        in
        let current =
          ref (Coordinator_store.committed initial initial_receipt)
        in
        let valid = ref true in
        List.iteri history ~f:(fun index _command ->
            let expected = Coordinator_store.version !current in
            let write_id = "write-" ^ Int.to_string index in
            let candidate = Coordinator_store.with_state !current state in
            let receipt : Coordinator_store.receipt =
              Coordinator_store.commit store ~expected_version:expected
                ~write_id candidate
              |> require_commit
            in
            let committed = Coordinator_store.committed candidate receipt in
            if
              (not
                 (Int64.equal receipt.Coordinator_store.state_version
                    (Int64.succ expected)))
              || not (String.equal receipt.Coordinator_store.write_id write_id)
            then valid := false;
            (* Retrying the same logical write with identical bytes must resolve
               from its retained receipt and current payload, not create a new
               write or advance the version again. *)
            let replay : Coordinator_store.receipt =
              Coordinator_store.commit store ~expected_version:expected
                ~write_id candidate
              |> require_commit
            in
            if
              (not
                 (String.equal replay.Coordinator_store.write_id
                    receipt.Coordinator_store.write_id))
              || not
                   (Int64.equal replay.Coordinator_store.state_version
                      receipt.Coordinator_store.state_version)
            then valid := false;
            let restored =
              match Coordinator_store.load store with
              | Ok envelope -> envelope
              | Error _ ->
                  valid := false;
                  committed
            in
            if
              (not
                 (String.equal
                    (Coordinator_store.encode committed)
                    (Coordinator_store.encode restored)))
              || not
                   (Int64.equal
                      (Coordinator_store.version restored)
                      (Int64.succ expected))
            then valid := false;
            current := restored);
        !valid
      with _ -> false)

let () = QCheck_base_runner.run_tests_main [ histories ]

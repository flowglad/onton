(* @archlint.module exempt
   @archlint.exempt-reason effect-facade *)

open Base

[@@@warning "-4"]

type authority = { owner_epoch : int64; revision : int64 }

type ready = {
  store : Coordinator_store.t;
  envelope : Coordinator_store.envelope;
  authority : authority;
}

let ( let* ) value f = Result.bind value ~f
let fail why = Error why

let request ~run_id ~coordinator_id ~request_id ~op ~config_id ?takeover_guards
    () =
  let fields =
    [
      ("version", `String "resource-v1");
      ("op", `String op);
      ("run_id", `String run_id);
      ("request_id", `String request_id);
      ("coordinator_id", `String coordinator_id);
    ]
    @ (if String.equal op "open" then [ ("config_digest", `String config_id) ]
       else [])
    @
    match takeover_guards with
    | Some (epoch, revision) ->
        [
          ("owner_epoch", `String (Int64.to_string epoch));
          ("revision", `String (Int64.to_string revision));
        ]
    | None -> []
  in
  Resource_protocol.make_request (Yojson.Safe.to_string (`Assoc fields))

let start ~client ~run_id ~coordinator_id ~config_id ~plan_id ~plan_bytes
    ~config_bytes ~capacity ~fresh ~takeover_guards ~next_id =
  let valid_id s =
    String.length s > 0
    && String.length s <= 256
    && not
         (String.exists s ~f:(fun c ->
              Char.equal c '\000' || Char.equal c '\r' || Char.equal c '\n'))
  in
  if
    (not
       (List.for_all [ run_id; coordinator_id; config_id; plan_id ] ~f:valid_id))
    || capacity <= 0 || String.is_empty plan_bytes
    || String.is_empty config_bytes
    || String.length plan_bytes + String.length config_bytes > 7 * 1024 * 1024
  then fail "invalid coordinator identity, capacity or bounded configuration"
  else
    let op = if fresh then "open" else "takeover" in
    let* req =
      match (fresh, takeover_guards) with
      | true, None ->
          request ~run_id ~coordinator_id ~request_id:(next_id ()) ~op
            ~config_id ()
      | false, Some (epoch, revision) when Int64.(epoch >= 0L && revision >= 0L)
        ->
          request ~run_id ~coordinator_id ~request_id:(next_id ()) ~op
            ~config_id ~takeover_guards:(epoch, revision) ()
      | _ -> fail "takeover/open guards inconsistent"
    in
    let* response =
      match Resource_client.request client req with
      | Ok r -> Ok r
      | Error _ -> fail "helper open unavailable"
    in
    let* authority =
      match
        ( response.status,
          response.run_id,
          response.owner_epoch,
          response.revision )
      with
      | Resource_protocol.Ok, Some id, Some owner_epoch, Some revision
        when String.equal id run_id
             && Int64.(owner_epoch >= 0L && revision >= 0L) ->
          Ok { owner_epoch; revision }
      | _ -> fail "helper did not confirm run and current owner guards"
    in
    let store =
      Coordinator_store.create ~client ~run_id ~coordinator_id
        ~owner_epoch:authority.owner_epoch ~revision:authority.revision ~next_id
    in
    let* envelope =
      match Coordinator_store.load store with
      | Ok envelope -> Ok envelope
      | Error (Coordinator_store.Refused response)
        when fresh
             && Poly.equal response.status Resource_protocol.Rejected
             && Poly.equal response.reason Resource_protocol.Not_found -> (
          let state =
            Coordinator_state.create ~run_id ~config_id ~plan_id ~plan_bytes
              ~config_bytes ~capacity
              ~budget_bytes:(8 * 1024 * 1024)
          in
          let initial = Coordinator_store.initial state in
          let* _ =
            match
              Coordinator_store.commit store ~expected_version:0L
                ~write_id:(next_id ()) initial
            with
            | Ok receipt -> Ok receipt
            | Error _ -> fail "initial envelope write not confirmed"
          in
          match Coordinator_store.load store with
          | Ok envelope -> Ok envelope
          | Error _ -> fail "initial envelope not readable")
      | Error _ -> fail "current run envelope unavailable or incompatible"
    in
    let state = Coordinator_store.state envelope in
    if
      (not (String.equal (Coordinator_state.run_id state) run_id))
      || (not (String.equal (Coordinator_state.config_id state) config_id))
      || (not (String.equal (Coordinator_state.plan_id state) plan_id))
      || (not (String.equal (Coordinator_state.plan_bytes state) plan_bytes))
      || (not
            (String.equal (Coordinator_state.config_bytes state) config_bytes))
      || Coordinator_state.capacity state <> capacity
    then fail "committed run identity/config/gameplan mismatch"
    else
      let _, _, owner_epoch, revision = Coordinator_store.authority store in
      Ok { store; envelope; authority = { owner_epoch; revision } }

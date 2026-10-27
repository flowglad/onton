(* @archlint.module implementation
   @archlint.domain coordinator *)

open Base

[@@@warning "-4"]

type env = {
  store : Coordinator_store.t;
  client : Resource_client.t;
  next_write_id : unit -> string;
  next_event : unit -> Coordinator_state.event option;
  stop_request :
    resource_id:string -> (Resource_protocol.request, string) Result.t;
  outcome : Coordinator_state.outcome -> unit;
  blocked : string -> unit;
}

let submit = Coordinator_host.submit

let report env = function
  | Ok (Coordinator_host.Applied effects) ->
      List.iter effects ~f:env.outcome;
      true
  | Ok (Rejected reason) ->
      env.blocked reason;
      false
  | Error _ ->
      env.blocked "coordinator commit not confirmed";
      false

let stop env host resource_id =
  (* The persisted request, not a fresh request with updated guards, wins on
     restart. A lost stop response is never a retirement receipt. *)
  let persisted =
    Coordinator_state.resources (Coordinator_host.state host)
    |> List.find_map ~f:(fun (_, bytes) ->
        match Resource_protocol.make_request bytes with
        | Ok request when Poly.equal request.operation Resource_protocol.Stop
          -> (
            try
              match Yojson.Safe.from_string bytes with
              | `Assoc fields -> (
                  match
                    List.Assoc.find fields "resource_id" ~equal:String.equal
                  with
                  | Some (`String id) when String.equal id resource_id ->
                      Some request
                  | _ -> None)
              | _ -> None
            with _ -> None)
        | Ok _ | Error _ -> None)
  in
  let request =
    match persisted with
    | Some r -> Ok r
    | None -> env.stop_request ~resource_id
  in
  match request with
  | Error reason -> env.blocked reason
  | Ok request -> (
      if
        (not (Poly.equal request.operation Resource_protocol.Stop))
        || not
             (report env
                (submit host
                   (Coordinator_state.Resource_request
                      (request.request_id, request.bytes))))
      then env.blocked "stop request not durably authorized"
      else
        match Resource_client.request env.client request with
        | Error _ -> env.blocked "stop response unconfirmed"
        | Ok response ->
            ignore
              (report env
                 (Coordinator_host.retired host ~resource_id
                    ~stop_request:request ~response)))

let restore_inventory env host =
  let inventory = Coordinator_state.resources (Coordinator_host.state host) in
  List.iter inventory ~f:(fun (resource_id, bytes) ->
      match Resource_protocol.make_request bytes with
      | Ok request
        when Poly.equal request.operation Resource_protocol.Launch
             || Poly.equal request.operation Resource_protocol.Replace ->
          if
            not
              (List.mem
                 (Coordinator_state.retired_resources
                    (Coordinator_host.state host))
                 resource_id ~equal:String.equal)
          then
            if
              (* Never reconnect a consumed attachment. Takeover must first
               fence every retained possibly executing predecessor. *)
              report env
                (submit host (Coordinator_state.Loss (resource_id, 0L)))
            then stop env host resource_id
      | Ok _ -> ()
      | Error _ -> env.blocked "corrupt persisted resource inventory")

let run env =
  match
    Coordinator_host.restore ~store:env.store ~next_write_id:env.next_write_id
  with
  | Error _ -> env.blocked "current run envelope unavailable"
  | Ok host ->
      restore_inventory env host;
      let rec loop () =
        match env.next_event () with
        | None -> ()
        | Some event -> if report env (submit host event) then loop ()
      in
      loop ()

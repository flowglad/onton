(* @archlint.module exempt
   @archlint.exempt-reason effect-facade *)

open Base

[@@@warning "-4"]

type connection = {
  run_id : string;
  coordinator_id : string;
  owner_epoch : int64;
  revision : int64;
  request_id : string;
  resource_id : string;
}

type event =
  | Intent of Coordinator_state.event
  | Attach of string * connection
  | Command of string * string
  | Read of string
  | Recover of {
      predecessor : string;
      assignment : string;
      worker_id : string;
      workspace_id : string;
    }

type env = {
  store : Coordinator_store.t;
  client : Resource_client.t;
  next_write_id : unit -> string;
  next_event : unit -> event option;
  now : unit -> int64;
  stream_timeout : float;
  attached : resource_id:string -> readable:(unit -> bool) -> unit;
  gameplan : Types.Gameplan.t;
  main_branch : Types.Branch.t;
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
      let attachments = Hashtbl.create (module String) in
      let close (attachment, _) = Resource_client.disconnect attachment in
      let find_attachment assignment =
        match Hashtbl.find attachments assignment with
        | Some io -> Some io
        | None -> (
            let owners =
              Coordinator_state.assignments (Coordinator_host.state host)
            in
            match
              List.find owners ~f:(fun a -> String.equal a.id assignment)
            with
            | None -> None
            | Some current ->
                Hashtbl.to_alist attachments
                |> List.find_map ~f:(fun (id, io) ->
                    if
                      List.exists owners ~f:(fun prior ->
                          String.equal prior.id id
                          && String.equal prior.resource current.resource
                          && prior.cleanup && not prior.retired)
                    then Some io
                    else None))
      in
      let frames assignment result =
        match result with
        | Error _ ->
            env.blocked "worker protocol/commit barrier not confirmed";
            false
        | Ok (outcomes, frames) -> (
            List.iter outcomes ~f:env.outcome;
            match find_attachment assignment with
            | None ->
                env.blocked "worker stream unavailable; do not retry attachment";
                false
            | Some (_, io) -> (
                match
                  Coordinator_stream_io.send io ~timeout:env.stream_timeout
                    frames
                with
                | Ok () -> true
                | Error reason ->
                    env.blocked reason;
                    false))
      in
      let attach assignment (c : connection) =
        let owner =
          List.find
            (Coordinator_state.assignments (Coordinator_host.state host))
            ~f:(fun a -> String.equal a.id assignment)
        in
        match owner with
        | Some a
          when (not a.retired)
               && String.equal a.resource c.resource_id
               && String.equal c.run_id
                    (Coordinator_state.run_id (Coordinator_host.state host))
               && (not
                     (List.mem
                        (Coordinator_state.lost_resources
                           (Coordinator_host.state host))
                        a.resource ~equal:String.equal))
               && not (Hashtbl.mem attachments assignment) -> (
            match
              Resource_client.connect env.client ~run_id:c.run_id
                ~coordinator_id:c.coordinator_id ~owner_epoch:c.owner_epoch
                ~revision:c.revision ~request_id:c.request_id
                ~resource_id:c.resource_id
            with
            | Error reason ->
                env.blocked reason;
                false
            | Ok attachment -> (
                match
                  Or_error.try_with (fun () ->
                      Coordinator_stream_io.create attachment)
                with
                | Error _ ->
                    Resource_client.disconnect attachment;
                    env.blocked "worker stream setup failed";
                    false
                | Ok io -> (
                    match
                      Hashtbl.add attachments ~key:assignment
                        ~data:(attachment, io)
                    with
                    | `Ok ->
                        env.attached ~resource_id:c.resource_id
                          ~readable:(fun () -> Coordinator_stream_io.ready io);
                        true
                    | `Duplicate ->
                        close (attachment, io);
                        false)))
        | _ ->
            env.blocked "attachment not bound to current live assignment";
            false
      in
      let rec loop () =
        match env.next_event () with
        | None -> ()
        | Some event ->
            let continue =
              match event with
              | Intent event -> report env (submit host event)
              | Attach (assignment, connection) -> attach assignment connection
              | Command (assignment, frame) ->
                  frames assignment
                    (Coordinator_host.command host ~assignment ~frame)
              | Recover r ->
                  frames r.assignment
                    (Coordinator_host.recover host ~predecessor:r.predecessor
                       ~assignment:r.assignment ~worker_id:r.worker_id
                       ~workspace_id:r.workspace_id)
              | Read assignment -> (
                  match find_attachment assignment with
                  | None ->
                      env.blocked "unattached worker";
                      false
                  | Some (_, io) -> (
                      match
                        Coordinator_stream_io.read io
                          ~timeout:env.stream_timeout
                      with
                      | Error reason ->
                          env.blocked reason;
                          false
                      | Ok frame ->
                          frames assignment
                            (Coordinator_host.receive host ~assignment ~frame
                               ~gameplan:env.gameplan
                               ~main_branch:env.main_branch ~now:(env.now ()))))
            in
            if continue then loop ()
      in
      Exn.protect ~finally:(fun () -> Hashtbl.iter attachments ~f:close) ~f:loop

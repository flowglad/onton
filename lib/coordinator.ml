(* @archlint.module shell
   @archlint.domain coordinator-state *)

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
  | Tick of (Coordinator_store.envelope -> event option)
  | Intent_batch of Coordinator_state.event list
  | Stop_resource of string
  | Shutdown
  | Intent of Coordinator_state.event
  | Provision of {
      request : Resource_protocol.request;
      assignment : Coordinator_state.assignment;
      connection : connection;
      recovery : (string * string * string) option;
    }
  | Observe of Types.Patch_id.t * Coordinator_forge.scope
  | Forge_change of {
      patch : Types.Patch_id.t;
      scope : Coordinator_forge.scope;
      effect_id : string;
      view : Scheduler_decision.input;
      renewal_id : string option;
      action : Coordinator_forge.change;
    }
  | Forge_create of {
      patch : Types.Patch_id.t;
      scope : Coordinator_forge.scope;
      effect_id : string;
      renewal_id : string option;
      title : string;
      body : string;
      draft : bool;
    }
  | Read_resource of string
  | Control_line of string * (string -> unit)
  | Close_run of Resource_protocol.request
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
  mono_clock : Eio.Time.Mono.ty Eio.Time.Mono.t;
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
    | None -> (
        match env.stop_request ~resource_id with
        | Error why -> Error why
        | Ok request -> (
            match Coordinator_host.resource_request host request with
            | Ok request -> Ok request
            | Error _ -> Error "stop guard capture failed"))
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
        let response = Resource_client.request env.client request in
        ignore (Coordinator_host.refresh host);
        match response with
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

let run ?forge ?git ?(write_status = fun _ -> ()) env =
  match
    Coordinator_host.restore ~store:env.store ~next_write_id:env.next_write_id
  with
  | Error _ -> env.blocked "current run envelope unavailable"
  | Ok host ->
      restore_inventory env host;
      let attachments = Hashtbl.create (module String) in
      let cancellations = Hash_set.create (module String) in
      let close (attachment, _) =
        match Resource_client.disconnect attachment with
        | Ok () -> true
        | Error reason ->
            env.blocked reason;
            false
      in
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
      let lost assignment reason =
        env.blocked reason;
        match
          List.find
            (Coordinator_state.assignments (Coordinator_host.state host))
            ~f:(fun a -> String.equal a.id assignment)
        with
        | None -> false
        | Some owner ->
            if
              report env
                (submit host
                   (Coordinator_state.Loss (owner.resource, env.now ())))
            then (
              Hashtbl.to_alist attachments
              |> List.iter ~f:(fun (id, io) ->
                  if
                    List.exists
                      (Coordinator_state.assignments
                         (Coordinator_host.state host))
                      ~f:(fun a ->
                        String.equal a.id id
                        && String.equal a.resource owner.resource)
                  then if close io then Hashtbl.remove attachments id);
              stop env host owner.resource;
              true)
            else false
      in
      let frames assignment result =
        match result with
        | Error (Coordinator_host.Invalid reason) ->
            env.blocked
              ("worker protocol/commit barrier not confirmed: " ^ reason);
            false
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
                | Error reason -> lost assignment reason))
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
                      Coordinator_stream_io.create ~mono_clock:env.mono_clock
                        attachment)
                with
                | Error _ ->
                    (match Resource_client.disconnect attachment with
                    | Ok () -> ()
                    | Error reason -> env.blocked reason);
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
                        ignore (close (attachment, io));
                        false)))
        | _ ->
            env.blocked "attachment not bound to current live assignment";
            false
      in
      let deliver_inputs () =
        let state = Coordinator_host.state host in
        List.iter (Coordinator_state.assignments state) ~f:(fun a ->
            if
              (not a.retired)
              && Option.is_some (find_attachment a.id)
              && Option.value_map
                   (List.find (Coordinator_state.assignments state)
                      ~f:(fun other ->
                        String.equal other.resource a.resource
                        && not other.retired))
                   ~default:false
                   ~f:(fun current -> String.equal current.id a.id)
            then
              match Coordinator_host.stream host a.id with
              | None -> ()
              | Some stream -> (
                  let base =
                    match Worker_stream.input stream with
                    | Some i -> i.selected_base
                    | None -> (
                        match Worker_stream.binding stream with
                        | Some (Worker_protocol_v3.Bind (_, c)) ->
                            Worker_protocol_v3.selected_base c
                        | _ -> "")
                  in
                  match
                    Coordinator_inputs.pending state a.patch ~selected_base:base
                  with
                  | Error reason -> env.blocked reason
                  | Ok None -> ()
                  | Ok (Some input) when not (String.is_empty base) -> (
                      let already =
                        Option.value_map (Worker_stream.input stream)
                          ~default:false ~f:(fun old ->
                            Int64.(old.version >= input.version))
                      in
                      if not already then
                        let identity : Worker_protocol.identity =
                          {
                            run_id = Coordinator_state.run_id state;
                            patch_id = Types.Patch_id.to_string a.patch;
                          }
                        in
                        match
                          Worker_protocol_v3.encode_host
                            (Input (identity, input))
                        with
                        | Error why -> env.blocked why
                        | Ok frame ->
                            ignore
                              (frames a.id
                                 (Coordinator_host.command host ~assignment:a.id
                                    ~frame)))
                  | Ok (Some _) -> ()))
      in
      let settle_lifecycle () =
        let state = Coordinator_host.state host in
        match Coordinator_state.lifecycle state with
        | Coordinator_state.Running -> true
        | Draining generation | Sleeping generation | Cancelling generation ->
            (match Coordinator_state.lifecycle state with
            | Cancelling _ ->
                List.iter (Coordinator_state.assignments state) ~f:(fun a ->
                    if
                      a.reserved && (not a.retired)
                      && not (Hash_set.mem cancellations a.id)
                    then
                      match Coordinator_host.stream host a.id with
                      | Some stream -> (
                          match
                            ( Worker_stream.operation stream,
                              Worker_stream.identity stream )
                          with
                          | Some operation, Some identity
                            when String.equal operation.operation_id a.operation
                                 && String.equal operation.attempt_id a.request
                            ->
                              let frame =
                                Yojson.Safe.to_string
                                  (`Assoc
                                     [
                                       ("version", `Int 3);
                                       ("type", `String "cancel");
                                       ("run_id", `String identity.run_id);
                                       ("patch_id", `String identity.patch_id);
                                       ( "operation_id",
                                         `String operation.operation_id );
                                       ( "attempt_id",
                                         `String operation.attempt_id );
                                       ( "assignment_seq",
                                         `String
                                           (Int64.to_string
                                              operation.assignment_seq) );
                                     ])
                                ^ "\n"
                              in
                              if
                                frames a.id
                                  (Coordinator_host.command host
                                     ~assignment:a.id ~frame)
                              then Hash_set.add cancellations a.id
                          | _ -> ())
                      | None -> ())
            | _ -> ());
            let resources = Coordinator_state.resource_owners state in
            List.iter resources ~f:(fun (resource_id, _) ->
                if
                  (not
                     (List.mem
                        (Coordinator_state.retired_resources
                           (Coordinator_host.state host))
                        resource_id ~equal:String.equal))
                  && not
                       (List.exists
                          (Coordinator_state.assignments
                             (Coordinator_host.state host))
                          ~f:(fun a ->
                            String.equal a.resource resource_id
                            && a.reserved
                            &&
                            match Coordinator_state.lifecycle state with
                            | Cancelling _ ->
                                Option.value_map
                                  (Coordinator_host.stream host a.id)
                                  ~default:false ~f:(fun stream ->
                                    Option.value_map
                                      (Worker_stream.operation stream)
                                      ~default:false ~f:(fun operation ->
                                        String.equal operation.operation_id
                                          a.operation
                                        && String.equal operation.attempt_id
                                             a.request))
                            | _ -> true))
                then stop env host resource_id);
            (match
               Coordinator_state.lifecycle (Coordinator_host.state host)
             with
            | Draining current when String.equal current generation -> (
                let next =
                  submit host (Coordinator_state.Sleep_ready generation)
                in
                match next with
                | Ok (Coordinator_host.Rejected _) -> ()
                | _ -> ignore (report env next))
            | _ -> ());
            true
      in
      let rec loop () =
        match env.next_event () with
        | None -> ()
        | Some event ->
            let rec handle = function
              | Tick source -> (
                  match source (Coordinator_host.envelope host) with
                  | None -> true
                  | Some event -> handle event)
              | Intent_batch events ->
                  report env (Coordinator_host.transaction host events)
              | Stop_resource resource ->
                  stop env host resource;
                  true
              | Shutdown -> false
              | Intent event -> report env (submit host event)
              | Provision p -> (
                  match
                    Coordinator_resource.provision ~host ~client:env.client
                      ~request:p.request ~assignment:p.assignment
                      ~now:(env.now ())
                  with
                  | Pending ->
                      env.blocked "resource launch unresolved";
                      true
                  | Attention ->
                      env.blocked "resource launch requires operator attention";
                      true
                  | Blocked why ->
                      env.blocked why;
                      false
                  | Ready -> (
                      let run_id, coordinator_id, owner_epoch, revision =
                        Coordinator_host.authority host
                      in
                      let connection =
                        {
                          p.connection with
                          run_id;
                          coordinator_id;
                          owner_epoch;
                          revision;
                        }
                      in
                      if not (attach p.assignment.id connection) then false
                      else
                        match p.recovery with
                        | None -> true
                        | Some (predecessor, worker_id, workspace_id) ->
                            frames p.assignment.id
                              (Coordinator_host.recover host ~predecessor
                                 ~assignment:p.assignment.id ~worker_id
                                 ~workspace_id)))
              | Observe (patch, scope) -> (
                  match (forge, git) with
                  | Some forge, Some io ->
                      (match
                         Coordinator_forge.observe_committed
                           ~now:(Int64.to_float (env.now ()))
                           forge ~host ~io ~patch ~scope
                       with
                      | Confirmed _ -> ()
                      | Terminal pr -> (
                          if Pr_state.merged pr then
                            match
                              Coordinator_host.terminal_from_forge host ~patch
                                ~ticket:scope.ticket
                            with
                            | Ok _ -> ()
                            | Error _ ->
                                env.blocked "terminal patch awaits settlement")
                      | Unconfirmed why | Unavailable why -> env.blocked why);
                      true
                  | _ ->
                      env.blocked "forge adapter not configured";
                      false)
              | Forge_create c -> (
                  match (forge, git) with
                  | Some forge, Some io ->
                      (match
                         Coordinator_forge.create_or_discover forge ~host ~io
                           ~scope:c.scope ~patch:c.patch ~effect_id:c.effect_id
                           ~renewal_id:c.renewal_id ~title:c.title ~body:c.body
                           ~draft:c.draft
                       with
                      | Found _ -> ()
                      | Pending why | Blocked why -> env.blocked why);
                      true
                  | _ ->
                      env.blocked "forge adapter not configured";
                      false)
              | Forge_change c -> (
                  match (forge, git) with
                  | Some forge, Some io ->
                      (match
                         Coordinator_forge.change forge ~host ~io ~scope:c.scope
                           ~patch:c.patch ~effect_id:c.effect_id
                           ?renewal_id:c.renewal_id ~view:c.view
                           ~now:(Int64.to_float (env.now ()))
                           c.action
                       with
                      | Applied -> ()
                      | Change_pending why | Change_blocked why ->
                          env.blocked why);
                      true
                  | _ ->
                      env.blocked "forge adapter not configured";
                      false)
              | Read_resource resource -> (
                  match
                    List.find
                      (Coordinator_state.assignments
                         (Coordinator_host.state host))
                      ~f:(fun a ->
                        String.equal a.resource resource && not a.retired)
                  with
                  | None -> true
                  | Some a -> (
                      match find_attachment a.id with
                      | Some (_, io) when Coordinator_stream_io.ready io -> (
                          match
                            Coordinator_stream_io.read io
                              ~timeout:env.stream_timeout
                          with
                          | Error why -> lost a.id why
                          | Ok frame ->
                              frames a.id
                                (Coordinator_host.receive host ~assignment:a.id
                                   ~frame ~gameplan:env.gameplan
                                   ~main_branch:env.main_branch
                                   ~now:(env.now ())))
                      | _ -> true))
              | Control_line (bytes, response) ->
                  response (Coordinator_control.handle host bytes);
                  true
              | Close_run template -> (
                  let resources =
                    Coordinator_state.resources (Coordinator_host.state host)
                  in
                  let persisted =
                    match
                      List.Assoc.find resources template.request_id
                        ~equal:String.equal
                    with
                    | Some _ as persisted -> persisted
                    | None ->
                        List.find_map resources ~f:(fun (_, bytes) ->
                            match Resource_protocol.make_request bytes with
                            | Ok request
                              when Poly.equal request.operation
                                     Resource_protocol.Close
                                   && String.equal request.run_id
                                        (Coordinator_state.run_id
                                           (Coordinator_host.state host)) ->
                                Some bytes
                            | Ok _ | Error _ -> None)
                  in
                  let request =
                    match persisted with
                    | Some bytes -> (
                        match Resource_protocol.make_request bytes with
                        | Ok r -> Ok r
                        | Error why -> Error (Coordinator_host.Invalid why))
                    | None -> Coordinator_host.resource_request host template
                  in
                  match request with
                  | Error _ ->
                      env.blocked "close guards unavailable";
                      false
                  | Ok request -> (
                      let state = Coordinator_host.state host in
                      let cancelling =
                        match Coordinator_state.lifecycle state with
                        | Cancelling _ -> true
                        | _ -> false
                      in
                      if
                        (not cancelling)
                        || (not
                              (List.for_all (Coordinator_state.resources state)
                                 ~f:(fun (id, bytes) ->
                                   match
                                     Resource_protocol.make_request bytes
                                   with
                                   | Ok r
                                     when Poly.equal r.operation
                                            Resource_protocol.Stop
                                          || Poly.equal r.operation
                                               Resource_protocol.Close ->
                                       true
                                   | Ok _ | Error _ ->
                                       List.mem
                                         (Coordinator_state.retired_resources
                                            state)
                                         id ~equal:String.equal)))
                        || not
                             (Poly.equal request.operation
                                Resource_protocol.Close)
                      then (
                        env.blocked "cancel close awaits confirmed retirement";
                        false)
                      else if
                        not
                          (report env
                             (submit host
                                (Coordinator_state.Resource_request
                                   (request.request_id, request.bytes))))
                      then false
                      else
                        match Resource_client.request env.client request with
                        | Error _ ->
                            env.blocked "cancel close response unconfirmed";
                            false
                        | Ok response ->
                            report env
                              (Coordinator_host.cancel_closed host
                                 ~close_request:request ~response)))
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
                      | Error reason -> lost assignment reason
                      | Ok frame ->
                          frames assignment
                            (Coordinator_host.receive host ~assignment ~frame
                               ~gameplan:env.gameplan
                               ~main_branch:env.main_branch ~now:(env.now ()))))
            in
            let continue = handle event in
            if continue then (
              deliver_inputs ();
              let continue = settle_lifecycle () in
              Coordinator_headless.emit ~write:write_status
                (Coordinator_host.state host);
              if
                continue
                && not
                     (Coordinator_state.cancellation_complete
                        (Coordinator_host.state host))
              then loop ())
      in
      Exn.protect
        ~finally:(fun () ->
          let failed = ref false in
          Hashtbl.iter attachments ~f:(fun data ->
              if not (close data) then failed := true);
          if !failed then
            failwith "worker supervisor tree was not confirmed reaped")
        ~f:(fun () ->
          Coordinator_headless.emit ~write:write_status
            (Coordinator_host.state host);
          if settle_lifecycle () then loop ())

let serve ?forge ?git ?write_status ~net ~clock ~control_path env =
  let queue = Eio.Stream.create 64 in
  let readers = Hashtbl.create (module String) in
  let submit bytes =
    let promise, resolver = Eio.Promise.create () in
    Eio.Stream.add queue
      ( Some
          (Control_line
             ( bytes,
               fun status ->
                 Eio.Promise.resolve resolver status;
                 Eio.Fiber.yield () )),
        None );
    Eio.Promise.await promise
  in
  let push event =
    let promise, resolver = Eio.Promise.create () in
    Eio.Stream.add queue (event, Some resolver);
    Eio.Promise.await promise
  in
  let rec events () =
    let event = env.next_event () in
    push event;
    if Option.is_some event then events ()
  in
  let rec readable () =
    Eio.Time.sleep clock 0.02;
    List.iter (Hashtbl.to_alist readers) ~f:(fun (resource, ready) ->
        if ready () then push (Some (Read_resource resource)));
    readable ()
  in
  let pending = ref None in
  Eio.Fiber.first
    (fun () ->
      run ?forge ?git ?write_status
        {
          env with
          next_event =
            (fun () ->
              Option.iter !pending ~f:(fun resolver ->
                  Eio.Promise.resolve resolver ());
              let event, resolver = Eio.Stream.take queue in
              pending := resolver;
              event);
          attached =
            (fun ~resource_id ~readable ->
              Hashtbl.set readers ~key:resource_id ~data:readable;
              env.attached ~resource_id ~readable);
        })
    (fun () ->
      Eio.Fiber.all
        [
          (fun () -> Coordinator_control.run ~net ~path:control_path ~submit ());
          events;
          readable;
        ])

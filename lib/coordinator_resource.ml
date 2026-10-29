(* @archlint.module exempt
   @archlint.exempt-reason effect-facade *)
open Base

[@@@warning "-4"]

type result = Ready | Pending | Attention | Blocked of string

let committed = function
  | Ok (Coordinator_host.Applied _) -> true
  | Ok (Rejected _) | Error _ -> false

let provision ~host ~client ~(request : Resource_protocol.request)
    ~(assignment : Coordinator_state.assignment) ~now =
  let state = Coordinator_host.state host in
  let resource = assignment.resource in
  let persisted =
    List.Assoc.find
      (Coordinator_state.resources state)
      resource ~equal:String.equal
  in
  let logical bytes =
    try
      match Yojson.Safe.from_string bytes with
      | `Assoc fields ->
          `Assoc
            (List.filter fields ~f:(fun (k, _) ->
                 not
                   (List.mem
                      [ "owner_epoch"; "revision"; "coordinator_id" ]
                      k ~equal:String.equal))
            |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b))
      | _ -> `Null
    with _ -> `Null
  in
  let prepared =
    match persisted with
    | Some bytes when Poly.equal (logical bytes) (logical request.bytes) ->
        Result.map_error (Resource_protocol.make_request bytes) ~f:(fun s ->
            Coordinator_host.Invalid s)
    | Some _ -> Error (Coordinator_host.Invalid "changed resource intent")
    | None -> Coordinator_host.resource_request host request
  in
  let provision (request : Resource_protocol.request) =
    let field name =
      try
        match Yojson.Safe.from_string request.bytes with
        | `Assoc fields -> (
            match List.Assoc.find fields name ~equal:String.equal with
            | Some (`String s) -> Some s
            | _ -> None)
        | _ -> None
      with _ -> None
    in
    let bound = Coordinator_state.resource_owners state in
    let retired id =
      List.mem
        (Coordinator_state.retired_resources state)
        id ~equal:String.equal
    in
    let request_matches =
      String.equal request.run_id (Coordinator_state.run_id state)
      && Option.equal String.equal (field "resource_id") (Some resource)
      && Option.equal String.equal (field "patch_id")
           (Some (Types.Patch_id.to_string assignment.patch))
      && (match (request.operation, field "predecessor_id") with
        | Resource_protocol.Launch, None ->
            not
              (List.exists bound ~f:(fun (id, patch) ->
                   Types.Patch_id.equal patch assignment.patch
                   && not (String.equal id resource)))
        | Replace, Some predecessor ->
            retired predecessor
            && Option.value_map
                 (List.Assoc.find bound predecessor ~equal:String.equal)
                 ~default:false
                 ~f:(Types.Patch_id.equal assignment.patch)
        | _ -> false)
      && not
           (List.exists bound ~f:(fun (id, patch) ->
                Types.Patch_id.equal patch assignment.patch
                && (not (String.equal id resource))
                && not (retired id)))
    in
    let id = "resource:" ^ resource in
    let journal = Coordinator_state.journal state in
    if not request_matches then
      Blocked "resource scope/predecessor not authorized"
    else
      match Coordinator_journal.find journal id with
      | Some e when not (String.equal e.scope request.bytes) ->
          Blocked "changed resource intent"
      | Some { phase = Coordinator_journal.Applied _; _ } ->
          if
            retired resource
            || List.mem
                 (Coordinator_state.lost_resources state)
                 resource ~equal:String.equal
          then Blocked "resource attachment is fenced"
          else Ready
      | Some { phase = Attention; _ } -> Attention
      | Some { attempts = 3; _ } ->
          ignore
            (Coordinator_host.transaction host
               [
                 Journal_settle (id, None);
                 Operation_attention assignment.operation;
               ]);
          Attention
      | _ -> (
          let prior =
            List.find (Coordinator_state.assignments state) ~f:(fun a ->
                String.equal a.id assignment.id)
          in
          let assignment_events =
            match prior with
            | None -> Ok [ Coordinator_state.Assign (assignment, now) ]
            | Some a when Poly.equal a assignment -> Ok []
            | Some _ -> Error "changed provisioning assignment"
          in
          match assignment_events with
          | Error why -> Blocked why
          | Ok assignment_events -> (
              let events =
                [
                  Coordinator_state.Resource_request (resource, request.bytes);
                  Bind_resource (resource, assignment.patch);
                ]
                @ assignment_events
                @ [ Journal_attempt (id, request.bytes) ]
              in
              if not (committed (Coordinator_host.transaction host events)) then
                Blocked "resource intent commit unconfirmed"
              else
                let response = Resource_client.request client request in
                let refreshed = Coordinator_host.refresh host in
                let ready =
                  match response with
                  | Ok response ->
                      Poly.equal response.status Resource_protocol.Ok
                      && Option.equal String.equal response.resource_id
                           (Some resource)
                      && Option.equal String.equal response.run_id
                           (Some request.run_id)
                  | Error _ -> false
                in
                let ready = ready && Result.is_ok refreshed in
                let result = if ready then Some resource else None in
                let settling =
                  [ Coordinator_state.Journal_settle (id, result) ]
                in
                let settling =
                  match
                    Coordinator_journal.find
                      (Coordinator_state.journal (Coordinator_host.state host))
                      id
                  with
                  | Some { attempts = 3; _ } when not ready ->
                      settling @ [ Operation_attention assignment.operation ]
                  | _ -> settling
                in
                if not (committed (Coordinator_host.transaction host settling))
                then Blocked "resource response commit unconfirmed"
                else if ready then Ready
                else
                  match
                    Coordinator_journal.find
                      (Coordinator_state.journal (Coordinator_host.state host))
                      id
                  with
                  | Some { phase = Attention; _ } -> Attention
                  | _ -> Pending))
  in
  match prepared with
  | Error _ -> Blocked "resource guards/intent unavailable"
  | Ok request -> provision request

(* @archlint.module shell
   @archlint.domain coordinator-forge *)
open Base
open Types

[@@@warning "-4"]

type scope = Coordinator_observation.scope = {
  ticket : string;
  pr : Pr_number.t;
  head_branch : Branch.t;
  base_branch : Branch.t;
  head_revision : string;
  base_revision : string;
  push_destination : string;
  generation : int;
}

type observation =
  | Confirmed of Pr_state.t
  | Unconfirmed of string
  | Unavailable of string

let same_scope a b =
  a.generation = b.generation
  && String.equal a.ticket b.ticket
  && Pr_number.equal a.pr b.pr
  && Branch.equal a.head_branch b.head_branch
  && Branch.equal a.base_branch b.base_branch
  && String.equal a.head_revision b.head_revision
  && String.equal a.base_revision b.base_revision
  && String.equal a.push_destination b.push_destination

let observe (type e) (module F : Forge.S with type error = e) ~io scope =
  if
    String.is_empty scope.ticket
    || String.is_empty scope.push_destination
    || String.is_empty scope.head_revision
    || String.is_empty scope.base_revision
  then Unconfirmed "incomplete scope"
  else
    match F.pr_state scope.pr with
    | Error e -> Unavailable (F.show_error e)
    | Ok pr
      when (Pr_state.merged pr || Pr_state.closed pr)
           && Option.equal Pr_number.equal pr.pr_number (Some scope.pr) ->
        Confirmed pr
    | Ok pr -> (
        if
          Pr_state.merged pr || Pr_state.closed pr
          || (not
                (Option.value_map pr.pr_number ~default:false
                   ~f:(Pr_number.equal scope.pr)))
          || (not
                (Option.value_map pr.head_branch ~default:false
                   ~f:(Branch.equal scope.head_branch)))
          || (not
                (Option.value_map pr.base_branch ~default:false
                   ~f:(Branch.equal scope.base_branch)))
          || (not
                (Option.value_map pr.head_oid ~default:false
                   ~f:(String.equal scope.head_revision)))
          || not
               (Option.value_map pr.base_oid ~default:false
                  ~f:(String.equal scope.base_revision))
        then Unconfirmed "provider revision pair changed"
        else
          let open Branch_reconcile_executor in
          let head =
            remote_head_at io ~destination:scope.push_destination
              ~branch:(Branch.to_string scope.head_branch)
          in
          let base =
            remote_head_at io ~destination:"origin"
              ~branch:(Branch.to_string scope.base_branch)
          in
          match (head, base) with
          | Error e, _ | _, Error e -> Unavailable e
          | Ok (Some h), Ok (Some b)
            when String.equal
                   (Branch_reconcile.Commit.to_string h)
                   scope.head_revision
                 && String.equal
                      (Branch_reconcile.Commit.to_string b)
                      scope.base_revision ->
              Confirmed pr
          | _ -> Unconfirmed "remote revision pair not confirmed")

let observe_committed ?(now = 0.) (type e)
    (module F : Forge.S with type error = e) ~host ~io ~patch ~scope =
  let state = Coordinator_host.state host in
  let payload =
    Yojson.Safe.to_string
      (`List
         [
           `String (Patch_id.to_string patch);
           `Int (Pr_number.to_int scope.pr);
           `String (Branch.to_string scope.head_branch);
           `String (Branch.to_string scope.base_branch);
           `String scope.head_revision;
           `String scope.base_revision;
           `String scope.push_destination;
           `Int scope.generation;
         ])
  in
  let ticket : Coordinator_state.control =
    {
      id = scope.ticket;
      run_id = Coordinator_state.run_id state;
      config_id = Coordinator_state.config_id state;
      patch_id = Some (Patch_id.to_string patch);
      kind = "forge_ticket";
      payload;
    }
  in
  let latest state =
    List.find (Coordinator_state.controls state)
      ~f:(fun (c : Coordinator_state.control) ->
        String.equal c.kind "forge_ticket"
        && Option.value_map c.patch_id ~default:false
             ~f:(String.equal (Patch_id.to_string patch)))
  in
  match Coordinator_host.submit host (Coordinator_state.Control ticket) with
  | Error _ -> Unavailable "observation ticket commit unconfirmed"
  | Ok (Coordinator_host.Rejected _) ->
      Unconfirmed "observation ticket rejected"
  | Ok (Coordinator_host.Applied _) -> (
      match latest (Coordinator_host.state host) with
      | Some current when Poly.equal current ticket -> (
          let id = "observation:" ^ scope.ticket in
          let from_evidence evidence =
            match evidence.Coordinator_observation.state with
            | Some pr -> Confirmed pr
            | None -> Unavailable evidence.diagnostic
          in
          match
            Coordinator_journal.find
              (Coordinator_state.journal (Coordinator_host.state host))
              id
          with
          | Some { phase = Applied bytes; _ } -> (
              match Coordinator_observation.decode bytes with
              | Ok evidence -> from_evidence evidence
              | Error reason -> Unconfirmed reason)
          | Some _ ->
              Unavailable "observation result unresolved; use a fresh ticket"
          | None -> (
              match
                Coordinator_host.transaction host
                  [ Journal_attempt (id, payload) ]
              with
              | Error _ | Ok (Rejected _) ->
                  Unavailable "observation read intent unconfirmed"
              | Ok (Applied _) -> (
                  let observed = observe (module F) ~io scope in
                  let pr, diagnostic =
                    match observed with
                    | Confirmed pr -> (Some pr, "")
                    | Unconfirmed reason | Unavailable reason -> (None, reason)
                  in
                  let removal_checks =
                    match pr with
                    | Some pr when not (Pr_state.enqueued pr) ->
                        Result.ok
                          (F.merge_queue_removal_checks ~pr_number:scope.pr)
                    | _ -> None
                  in
                  match
                    Coordinator_observation.make ?removal_checks
                      ~observed_at:now scope pr ~diagnostic
                  with
                  | Error reason -> Unconfirmed reason
                  | Ok evidence -> (
                      match latest (Coordinator_host.state host) with
                      | Some current when Poly.equal current ticket -> (
                          match
                            Coordinator_host.transaction host
                              (let state = Coordinator_host.state host in
                               let version =
                                 List.fold (Coordinator_state.inputs state)
                                   ~init:0L ~f:(fun v (_, p, n, _, _) ->
                                     if
                                       String.equal p (Patch_id.to_string patch)
                                     then Int64.max v n
                                     else v)
                               in
                               [
                                 Journal_settle
                                   ( id,
                                     Some
                                       (Coordinator_observation.encode evidence)
                                   );
                                 Forge_wait
                                   (patch, Option.is_some evidence.state);
                                 Input
                                   ( "evidence:" ^ scope.ticket,
                                     Patch_id.to_string patch,
                                     (if Int64.equal version Int64.max_value
                                      then version
                                      else Int64.succ version),
                                     Coordinator_observation.encode evidence );
                               ])
                          with
                          | Ok (Applied _) -> observed
                          | Ok (Rejected _) | Error _ ->
                              Unavailable
                                "observation result commit unconfirmed")
                      | _ -> Unconfirmed "observation ticket superseded"))))
      | _ -> Unconfirmed "observation ticket superseded")

let ready ~expected ~current ~policy_ready observation =
  same_scope expected current
  && policy_ready
  && match observation with Confirmed _ -> true | _ -> false

let contribution_credit ~expected ~current ~matching_receipt ~already_credited =
  same_scope expected current && matching_receipt && not already_credited

type create_result =
  | Found of Pr_number.t
  | Pending of string
  | Blocked of string

let create_unsettled (type e) (module F : Forge.S with type error = e) ~host ~io
    ~scope ~patch ~effect_id ~renewal_id ~title ~body ~draft =
  let state = Coordinator_host.state host in
  let bytes =
    Yojson.Safe.to_string
      (`List
         [
           `String scope.ticket;
           `Int (Pr_number.to_int scope.pr);
           `String (Branch.to_string scope.head_branch);
           `String (Branch.to_string scope.base_branch);
           `String scope.head_revision;
           `String scope.base_revision;
           `String scope.push_destination;
           `String title;
           `String body;
           `Bool draft;
         ])
  in
  let control : Coordinator_state.control =
    {
      id = effect_id;
      run_id = Coordinator_state.run_id state;
      config_id = Coordinator_state.config_id state;
      kind = "forge_create";
      patch_id = Some (Patch_id.to_string patch);
      payload = bytes;
    }
  in
  if
    String.is_empty scope.ticket
    || String.is_empty scope.head_revision
    || String.is_empty scope.base_revision
    || String.is_empty scope.push_destination
  then Blocked "incomplete publication scope"
  else
    let prior =
      Coordinator_state.controls state
      |> List.exists ~f:(fun (c : Coordinator_state.control) ->
          String.equal c.kind "forge_create"
          && Option.value_map c.patch_id ~default:false
               ~f:(String.equal (Patch_id.to_string patch))
          && not (String.equal c.id effect_id))
    in
    let renewed =
      Option.value_map renewal_id ~default:false ~f:(fun id ->
          String.equal effect_id ("renewed:" ^ id)
          && List.exists (Coordinator_state.controls state)
               ~f:(fun (c : Coordinator_state.control) ->
                 String.equal c.id id
                 && String.equal c.kind "renewal"
                 && Option.value_map c.patch_id ~default:false
                      ~f:(String.equal (Patch_id.to_string patch))))
    in
    if prior && not renewed then Blocked "explicit renewal required"
    else
      match
        Coordinator_host.submit host (Coordinator_state.Control control)
      with
      | Error _ -> Blocked "publication intent commit unconfirmed"
      | Ok (Coordinator_host.Rejected reason) -> Blocked reason
      | Ok (Coordinator_host.Applied effects) -> (
          let first =
            match effects with
            | [ Coordinator_state.Accepted ] -> true
            | _ -> false
          in
          let replay =
            match effects with
            | [ Coordinator_state.Already_applied ] -> true
            | _ -> false
          in
          if not (first || replay) then
            Blocked "publication intent not committed"
          else
            match
              F.list_prs ~branch:scope.head_branch ~base:scope.base_branch
                ~state:`Open ()
            with
            | Error e -> Pending (F.show_error e)
            | Ok prs -> (
                let confirmed =
                  List.find_map prs ~f:(fun (number, _, _) ->
                      match
                        observe (module F) ~io { scope with pr = number }
                      with
                      | Confirmed _ -> Some number
                      | Unavailable _ | Unconfirmed _ -> None)
                in
                match confirmed with
                | Some number -> Found number
                | None when replay ->
                    Pending
                      "create outcome unresolved; absence is not no-effect \
                       proof"
                | None -> (
                    let head =
                      Branch_reconcile_executor.remote_head_at io
                        ~destination:scope.push_destination
                        ~branch:(Branch.to_string scope.head_branch)
                    in
                    let base =
                      Branch_reconcile_executor.remote_head_at io
                        ~destination:"origin"
                        ~branch:(Branch.to_string scope.base_branch)
                    in
                    let matches revision = function
                      | Ok (Some commit) ->
                          String.equal revision
                            (Branch_reconcile.Commit.to_string commit)
                      | _ -> false
                    in
                    if
                      not
                        (matches scope.head_revision head
                        && matches scope.base_revision base)
                    then Pending "publication pair not directly confirmed"
                    else
                      match
                        F.create_pull_request ~title ~head:scope.head_branch
                          ~base:scope.base_branch ~body ~draft
                      with
                      | Ok number -> Found number
                      | Error e -> Pending (F.show_error e))))

type change =
  | Set_body of string
  | Set_base of Branch.t
  | Set_draft of bool
  | Request_review of string
  | Enqueue
  | Dequeue
  | Merge

type change_result =
  | Applied
  | Change_pending of string
  | Change_blocked of string

let change_unsettled (type e) (module F : Forge.S with type error = e) ~host ~io
    ~scope ~patch ~effect_id ~policy_ready ~expected action =
  let state = Coordinator_host.state host in
  let action_bytes =
    match action with
    | Set_body body -> `List [ `String "body"; `String body ]
    | Set_base base -> `List [ `String "base"; `String (Branch.to_string base) ]
    | Set_draft draft -> `List [ `String "draft"; `Bool draft ]
    | Request_review team -> `List [ `String "review"; `String team ]
    | Enqueue -> `String "enqueue"
    | Dequeue -> `String "dequeue"
    | Merge -> `String "merge"
  in
  let payload =
    Yojson.Safe.to_string
      (`List
         [
           `String scope.ticket;
           `Int (Pr_number.to_int scope.pr);
           `String (Branch.to_string scope.head_branch);
           `String (Branch.to_string scope.base_branch);
           `String scope.head_revision;
           `String scope.base_revision;
           `String scope.push_destination;
           action_bytes;
         ])
  in
  let intent : Coordinator_state.control =
    {
      id = effect_id;
      run_id = Coordinator_state.run_id state;
      config_id = Coordinator_state.config_id state;
      patch_id = Some (Patch_id.to_string patch);
      kind = "forge_effect";
      payload;
    }
  in
  if
    String.is_empty scope.ticket
    || String.is_empty scope.push_destination
    || String.is_empty scope.head_revision
    || String.is_empty scope.base_revision
    ||
    match action with
    | Request_review team -> String.is_empty team
    | _ -> false
  then Change_blocked "incomplete effect scope"
  else if match action with Merge | Enqueue -> not policy_ready | _ -> false
  then Change_blocked "current scheduler policy not ready"
  else
    let pending =
      List.exists (Coordinator_state.controls state)
        ~f:(fun (c : Coordinator_state.control) -> String.equal c.id effect_id)
    in
    let proof =
      if pending then None else Option.map expected ~f:(fun pr -> Confirmed pr)
    in
    match proof with
    | Some (Unavailable reason) -> Change_pending reason
    | Some (Unconfirmed reason) -> Change_blocked reason
    | Some (Confirmed _) | None -> (
        match
          Coordinator_host.submit host (Coordinator_state.Control intent)
        with
        | Error _ -> Change_blocked "effect intent commit unconfirmed"
        | Ok (Coordinator_host.Rejected reason) -> Change_blocked reason
        | Ok (Coordinator_host.Applied effects) -> (
            let first =
              match effects with
              | [ Coordinator_state.Accepted ] -> true
              | _ -> false
            in
            if
              (not first)
              && not
                   (match effects with
                   | [ Coordinator_state.Already_applied ] -> true
                   | _ -> false)
            then Change_blocked "effect intent rejected"
            else
              match F.pr_state scope.pr with
              | Error e -> Change_pending (F.show_error e)
              | Ok pr
                when Option.value_map expected ~default:false ~f:(fun old ->
                         not
                           (Pr_state.equal
                              { pr with merge_ready_divergence = None }
                              { old with merge_ready_divergence = None })) ->
                  Change_pending
                    "provider evidence changed after committed policy proof"
              | Ok pr
                when not
                       (Option.value_map pr.pr_number ~default:false
                          ~f:(Pr_number.equal scope.pr)
                       && Option.value_map pr.head_oid ~default:false
                            ~f:(String.equal scope.head_revision)
                       && Option.value_map pr.head_branch ~default:false
                            ~f:(Branch.equal scope.head_branch)) ->
                  Change_pending "provider PR or contributor revision changed"
              | Ok pr -> (
                  let observed =
                    match action with
                    | Set_base base ->
                        Option.value_map pr.base_branch ~default:false
                          ~f:(Branch.equal base)
                    | Set_draft draft -> Bool.equal pr.is_draft draft
                    | Enqueue -> Pr_state.enqueued pr
                    | Dequeue -> not (Pr_state.enqueued pr)
                    | Merge -> Pr_state.merged pr
                    | Set_body _ | Request_review _ -> false
                  in
                  if observed then Applied
                  else if not first then
                    Change_pending
                      "ambiguous effect awaiting current provider evidence"
                  else
                    match observe (module F) ~io scope with
                    | Unavailable reason -> Change_pending reason
                    | Unconfirmed reason -> Change_blocked reason
                    | Confirmed pr
                      when Option.value_map expected ~default:false
                             ~f:(fun old ->
                               not
                                 (Pr_state.equal
                                    { pr with merge_ready_divergence = None }
                                    { old with merge_ready_divergence = None }))
                      ->
                        Change_pending "provider evidence changed before effect"
                    | Confirmed _ -> (
                        let result =
                          match action with
                          | Set_body body ->
                              F.update_pr_body ~pr_number:scope.pr ~body
                          | Set_base base ->
                              F.update_pr_base ~pr_number:scope.pr ~base
                          | Set_draft draft ->
                              F.set_draft ~pr_number:scope.pr ~draft
                          | Request_review team ->
                              F.request_review ~pr_number:scope.pr
                                ~team_slug:team
                          | Dequeue -> F.dequeue_pr ~pr_number:scope.pr
                          | Enqueue | Merge -> Ok ()
                        in
                        match action with
                        | Enqueue -> (
                            match F.enqueue_pr ~pr_number:scope.pr with
                            | Ok _ -> Applied
                            | Error e -> Change_pending (F.show_error e))
                        | Merge -> (
                            match F.merge_pr ~pr_number:scope.pr with
                            | Ok F.Merge_succeeded -> Applied
                            | Ok (F.Merge_queued _ | F.Merge_unconfirmed) ->
                                Change_pending
                                  "merge awaiting provider evidence"
                            | Error e -> Change_pending (F.show_error e))
                        | _ -> (
                            match result with
                            | Ok () -> Applied
                            | Error e -> Change_pending (F.show_error e))))))

let journal_attempt host id scope =
  match
    Coordinator_journal.find
      (Coordinator_state.journal (Coordinator_host.state host))
      id
  with
  | Some entry when not (String.equal entry.scope scope) ->
      Error "changed effect identity"
  | Some { phase = Applied bytes; _ } -> Ok (Some bytes)
  | Some { phase = Attention; _ } -> Error "effect requires operator attention"
  | Some { attempts = 3; _ } ->
      ignore (Coordinator_host.transaction host [ Journal_settle (id, None) ]);
      Error "effect requires operator attention"
  | Some _ | None -> (
      match
        Coordinator_host.transaction host [ Journal_attempt (id, scope) ]
      with
      | Ok (Applied _) -> Ok None
      | Ok (Rejected reason) -> Error reason
      | Error _ -> Error "effect attempt commit unconfirmed")

let settle_effect host id result =
  match Coordinator_host.transaction host [ Journal_settle (id, result) ] with
  | Ok (Applied _) -> true
  | Ok (Rejected _) | Error _ -> false

let create_or_discover forge ~host ~io ~scope ~patch ~effect_id ~renewal_id
    ~title ~body ~draft =
  let id = "effect:" ^ effect_id in
  let bytes =
    Yojson.Safe.to_string
      (`List
         [
           Coordinator_observation.yojson_of_scope scope;
           `String (Patch_id.to_string patch);
           `String title;
           `String body;
           `Bool draft;
           (match renewal_id with None -> `Null | Some s -> `String s);
         ])
  in
  match journal_attempt host id bytes with
  | Error reason -> Blocked reason
  | Ok (Some bytes) -> (
      match Int.of_string_opt bytes with
      | Some number when number > 0 -> Found (Pr_number.of_int number)
      | _ -> Blocked "invalid committed publication result")
  | Ok None ->
      let result =
        create_unsettled forge ~host ~io ~scope ~patch ~effect_id ~renewal_id
          ~title ~body ~draft
      in
      let stored =
        match result with
        | Found number -> Some (Int.to_string (Pr_number.to_int number))
        | _ -> None
      in
      if settle_effect host id stored then result
      else Blocked "publication result commit unconfirmed"

let change forge ~host ~io ~scope ~patch ~effect_id ?renewal_id ~view ~now
    action =
  let state = Coordinator_host.state host in
  let id = "effect:" ^ effect_id in
  let action_bytes =
    match action with
    | Set_body s -> "body:" ^ s
    | Set_base b -> "base:" ^ Branch.to_string b
    | Set_draft b -> "draft:" ^ Bool.to_string b
    | Request_review s -> "review:" ^ s
    | Enqueue -> "enqueue"
    | Dequeue -> "dequeue"
    | Merge -> "merge"
  in
  let bytes =
    Yojson.Safe.to_string
      (`List
         [
           Coordinator_observation.yojson_of_scope scope;
           `String (Patch_id.to_string patch);
           `String action_bytes;
         ])
  in
  let entry = Coordinator_journal.find (Coordinator_state.journal state) id in
  let replay =
    List.exists (Coordinator_state.controls state) ~f:(fun c ->
        String.equal c.Coordinator_state.id effect_id
        && String.equal c.kind "forge_effect")
  in
  let unresolved =
    List.exists (Coordinator_state.controls state) ~f:(fun c ->
        String.equal c.Coordinator_state.kind "forge_effect"
        && Option.equal String.equal c.patch_id
             (Some (Patch_id.to_string patch))
        && (not (String.equal c.id effect_id))
        &&
        match
          Coordinator_journal.find
            (Coordinator_state.journal state)
            ("effect:" ^ c.id)
        with
        | Some { phase = Applied _; _ } -> false
        | _ -> true)
  in
  let renewed =
    Option.value_map renewal_id ~default:false ~f:(fun renewal ->
        String.equal effect_id ("renewed:" ^ renewal)
        && List.exists (Coordinator_state.controls state) ~f:(fun c ->
            String.equal c.Coordinator_state.id renewal
            && String.equal c.kind "renewal"
            && Option.equal String.equal c.patch_id
                 (Some (Patch_id.to_string patch))))
  in
  if
    (not (Float.is_finite now))
    || Float.(now < 0. || now >= Int64.to_float Int64.max_value)
  then Change_blocked "invalid scheduler clock"
  else if
    Option.value_map entry ~default:false ~f:(fun e ->
        not (String.equal e.scope bytes))
  then Change_blocked "changed effect identity"
  else if unresolved && (not renewed) && not replay then
    Change_blocked "unresolved effect requires explicit renewal"
  else
    match entry with
    | Some { phase = Applied _; _ } -> Applied
    | Some { phase = Attention; _ } ->
        Change_blocked "effect requires operator attention"
    | _ -> (
        let attempt =
          Option.value_map entry ~default:0 ~f:(fun e -> e.attempts)
        in
        let proof_scope =
          {
            scope with
            ticket =
              scope.ticket ^ "/" ^ effect_id ^ "/" ^ Int.to_string attempt ^ "/"
              ^ Float.to_string now;
          }
        in
        let observation =
          if replay then None
          else
            Some
              (observe_committed ~now forge ~host ~io ~patch ~scope:proof_scope)
        in
        let tick =
          Coordinator_host.submit host (Policy_tick (view, Int64.of_float now))
        in
        let view =
          Coordinator_state.scheduling_view (Coordinator_host.state host) view
        in
        let agent =
          List.find view.agents ~f:(fun a ->
              Patch_id.equal a.Patch_agent.patch_id patch)
        in
        let policy_ready =
          match (action, agent) with
          | (Merge | Enqueue), Some a ->
              Scheduler_decision.dependencies_admitted view patch
              && Scheduler_decision.automerge_candidate a ~main:view.main
              && Option.value_map a.automerge_deadline ~default:false
                   ~f:(fun deadline -> Float.(deadline <= now))
          | Set_draft false, _ | Request_review _, _ ->
              Scheduler_decision.ready_for_review view patch
          | Set_base base, _ ->
              Option.equal Branch.equal
                (Scheduler_decision.selected_base view patch)
                (Some base)
          | (Set_body _ | Set_draft true | Dequeue), Some _ -> true
          | _, None -> false
        in
        match tick with
        | Error _ | Ok (Rejected _) ->
            Change_blocked "policy commit unconfirmed"
        | Ok (Applied _) -> (
            if
              (not replay)
              &&
              match observation with
              | Some (Confirmed _) -> not policy_ready
              | _ -> false
            then
              Change_pending
                "current shared scheduler policy/idle window not ready"
            else
              match journal_attempt host id bytes with
              | Error why -> Change_blocked why
              | Ok (Some _) -> Applied
              | Ok None ->
                  let expected =
                    match observation with
                    | Some (Confirmed pr) -> Some pr
                    | _ -> None
                  in
                  let result =
                    match observation with
                    | None | Some (Confirmed _) ->
                        change_unsettled forge ~host ~io ~scope ~patch
                          ~effect_id ~policy_ready:true ~expected action
                    | Some (Unconfirmed why) -> Change_blocked why
                    | Some (Unavailable why) -> Change_pending why
                  in
                  let stored =
                    match result with Applied -> Some "applied" | _ -> None
                  in
                  if settle_effect host id stored then result
                  else Change_blocked "effect result commit unconfirmed"))

let render_notes ~gameplan ~patches ~publications ~projections =
  List.map patches ~f:(fun patch ->
      let publication =
        List.Assoc.find publications patch.Patch.id ~equal:Patch_id.equal
      in
      let note =
        Option.bind publication ~f:(fun expected ->
            List.find_map projections
              ~f:(fun (p : Worker_checkpoint.projection) ->
                if
                  String.equal p.patch_id (Patch_id.to_string patch.id)
                  && Option.value_map p.publication ~default:false
                       ~f:(String.equal expected)
                then p.note_bytes
                else None))
      in
      Feature_pr_body.render_contribution ~patch ~notes:note)
  |> Feature_pr_body.render_contributions ~gameplan
  |> Pr_body_limit.limit

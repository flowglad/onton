(* @archlint.module shell
   @archlint.domain coordinator-forge *)
open Base
open Types

[@@@warning "-4"]

type scope = {
  ticket : string;
  pr : Pr_number.t;
  head_branch : Branch.t;
  base_branch : Branch.t;
  head_revision : string;
  base_revision : string;
  push_destination : string;
}

type observation =
  | Confirmed of Pr_state.t
  | Unconfirmed of string
  | Unavailable of string

let same_scope a b =
  String.equal a.ticket b.ticket
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
    | Ok pr -> (
        if
          Pr_state.merged pr || Pr_state.closed pr
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

let create_or_discover (type e) (module F : Forge.S with type error = e) ~host
    ~io ~scope ~patch ~effect_id ~renewal_id ~title ~body ~draft =
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
                    match
                      F.create_pull_request ~title ~head:scope.head_branch
                        ~base:scope.base_branch ~body ~draft
                    with
                    | Ok number -> Found number
                    | Error e -> Pending (F.show_error e))))

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

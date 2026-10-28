(* @archlint.module core
   @archlint.domain coordinator-state *)

open Base
open Types
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

[@@@warning "-4"]

type control = {
  id : string;
  run_id : string;
  config_id : string;
  payload : string;
  kind : string;
  patch_id : string option;
}
[@@deriving yojson]

type assignment = {
  id : string;
  operation : string;
  patch : Patch_id.t;
  resource : string;
  request : string;
  reserved : bool;
  retired : bool;
  result_committed : bool;
  cleanup : bool;
}
[@@deriving yojson]

type operation = {
  id : string;
  patch : Patch_id.t;
  used : int;
  budget : int;
  retry_after : int64;
  attention : bool;
  unfinished : bool;
  maintenance : bool;
}
[@@deriving yojson]

type capsule = {
  assignment : string;
  version : int64;
  bytes : string;
  receipt : string;
}
[@@deriving yojson]

type lifecycle =
  | Running
  | Draining of string
  | Sleeping of string
  | Cancelling of string
[@@deriving yojson]

type t = {
  run_id : string;
  config_id : string;
  plan_id : string;
  plan_bytes : string;
  config_bytes : string;
  capacity : int;
  budget_bytes : int;
  operations : operation list;
  assignments : assignment list;
  resources : (string * string) list;
  resource_owners : (string * Patch_id.t) list;
  terminal_patches : Patch_id.t list;
  retired_resources : string list;
  lost_resources : string list;
  capsules : capsule list;
  results : (string * string) list;
  controls : control list;
  inputs : (string * string * int64 * string * bool) list;
  input_floors : (string * string * int64) list;
  lifecycle : lifecycle;
  cancel_cleanup : bool;
}
[@@deriving yojson]

type event =
  | Admit of operation
  | Assign of assignment * int64
  | Result_committed of string * string
  | Worker_result of string * string * string * int64
  | Cleanup_confirmed of string * string
  | Stop_confirmed of string * string
  | Resource_stopped of string * string
  | Owner_receipt of capsule
  | Advance_receipt of capsule * string
  | Resource_request of string * string
  | Bind_resource of string * Patch_id.t
  | Terminal_patch of Patch_id.t
  | Loss of string * int64
  | Retry of string * int64
  | Control of control
  | Input of string * string * int64 * string
  | Input_consumed of string * string * int64
  | Sleep_ready of string
  | Cancel_cleanup_confirmed

type outcome =
  | Accepted
  | Rejected of string
  | Already_applied
  | Dispatch of string
  | Recover of string
  | Maintain of Patch_id.t
  | Attention of string
  | Retire of string
  | Ready_to_sleep of string
  | Cancel_complete

let create ~run_id ~config_id ~plan_id ~plan_bytes ~config_bytes ~capacity
    ~budget_bytes =
  {
    run_id;
    config_id;
    plan_id;
    plan_bytes;
    config_bytes;
    capacity = Int.max 0 capacity;
    budget_bytes = Int.max 0 (Int.min 8388608 budget_bytes);
    operations = [];
    assignments = [];
    resources = [];
    resource_owners = [];
    terminal_patches = [];
    retired_resources = [];
    lost_resources = [];
    capsules = [];
    results = [];
    controls = [];
    inputs = [];
    input_floors = [];
    lifecycle = Running;
    cancel_cleanup = false;
  }

let operations t = t.operations
let assignments t = t.assignments
let plan_bytes t = t.plan_bytes
let config_bytes t = t.config_bytes
let resources t = t.resources
let resource_owners t = t.resource_owners
let terminal_patches t = t.terminal_patches
let retired_resources t = t.retired_resources
let lost_resources t = t.lost_resources
let run_id (t : t) = t.run_id
let config_id (t : t) = t.config_id
let plan_id t = t.plan_id
let capsules t = t.capsules
let results t = t.results
let controls t = t.controls
let inputs t = t.inputs
let lifecycle t = t.lifecycle
let reserved_count t = List.count t.assignments ~f:(fun a -> a.reserved)

let all_retired t =
  List.for_all t.assignments ~f:(fun a -> a.retired || a.cleanup)
  && List.for_all t.resources ~f:(fun (id, bytes) ->
      (* A retained stop request is not itself a container. Its matching
         response retires the launch/replace identity, not its request ID. *)
      match Resource_protocol.make_request bytes with
      | Ok request when Poly.equal request.operation Resource_protocol.Stop ->
          true
      | Ok _ | Error _ -> List.mem t.retired_resources id ~equal:String.equal)

let settled_assignments t =
  List.for_all t.assignments ~f:(fun a ->
      a.cleanup
      || not
           (List.exists t.operations ~f:(fun o ->
                String.equal o.id a.operation && o.unfinished)))

let sleep_ready t =
  match t.lifecycle with
  | Sleeping _ -> all_retired t && settled_assignments t
  | Running | Draining _ | Cancelling _ -> false

let cancellation_complete t =
  match t.lifecycle with
  | Cancelling _ -> all_retired t && t.cancel_cleanup
  | _ -> false

let next_retry now =
  if Int64.equal now Int64.max_value then now else Int64.succ now

let can_recover t o ~now =
  o.unfinished && (not o.attention) && o.used < o.budget
  && (not (List.mem t.terminal_patches o.patch ~equal:Patch_id.equal))
  && Int64.(now >= o.retry_after)
  && (match t.lifecycle with
    | Running -> true
    | Draining _ -> not o.maintenance
    | Sleeping _ | Cancelling _ -> false)
  && List.for_all t.assignments ~f:(fun a ->
      (not (String.equal a.operation o.id)) || a.retired || a.cleanup)
  && List.for_all t.lost_resources ~f:(fun id ->
      (not
         (Option.value_map
            (List.Assoc.find t.resource_owners id ~equal:String.equal)
            ~default:false ~f:(Patch_id.equal o.patch)))
      || List.mem t.retired_resources id ~equal:String.equal)

let maintenance_id t patch =
  match List.find t.operations ~f:(fun o -> Patch_id.equal o.patch patch) with
  | Some o when o.maintenance -> o.id
  | _ ->
      let rec unused sequence =
        let id = "maintenance:" ^ Int.to_string sequence in
        if List.exists t.operations ~f:(fun o -> String.equal o.id id) then
          unused (sequence + 1)
        else id
      in
      unused (List.length t.operations)

let valid_id s =
  String.length s > 0
  && String.length s <= 256
  && not (String.contains s '\000')

let control_target_ok (c : control) =
  match (c.kind, c.patch_id) with
  | ("sleep" | "wake" | "cancel"), None -> true
  | ("guidance" | "automerge" | "bump" | "renewal" | "forge_create"), Some id ->
      valid_id id
  | _, _ -> false

let decode_control bytes =
  if String.length bytes > 262144 then Error "control too large"
  else
    match Worker_protocol.parse_frame (bytes ^ "\n") with
    | Error _ -> Error "malformed control"
    | Ok (`Assoc fields) -> (
        let field k = List.Assoc.find fields k ~equal:String.equal in
        let str k =
          match field k with
          | Some (`String s) when valid_id s -> Some s
          | _ -> None
        in
        let patch =
          match field "patch_id" with
          | Some `Null -> Some None
          | Some (`String s) when valid_id s -> Some (Some s)
          | _ -> None
        in
        if
          List.length fields <> 7
          || not
               (List.for_all fields ~f:(fun (k, _) ->
                    List.mem
                      [
                        "version";
                        "id";
                        "run_id";
                        "config_id";
                        "kind";
                        "patch_id";
                        "payload";
                      ]
                      k ~equal:String.equal))
        then Error "control schema"
        else
          match
            ( field "version",
              str "id",
              str "run_id",
              str "config_id",
              str "kind",
              patch,
              field "payload" )
          with
          | ( Some (`Int 3),
              Some id,
              Some run_id,
              Some config_id,
              Some kind,
              Some patch_id,
              Some (`String payload) )
            when String.length payload <= 131072
                 && List.mem
                      [
                        "guidance";
                        "automerge";
                        "bump";
                        "renewal";
                        "sleep";
                        "wake";
                        "cancel";
                      ]
                      kind ~equal:String.equal ->
              let control =
                { id; run_id; config_id; kind; patch_id; payload }
              in
              if control_target_ok control then Ok control
              else Error "invalid control target"
          | _ -> Error "invalid control")
    | Ok _ -> Error "control schema"

let encode t =
  Yojson.Safe.to_string
    (`Assoc [ ("version", `Int 3); ("state", yojson_of_t t) ])

(* Count the encoded envelope, including escaping, field names and history. *)
let fits t = String.length (encode t) <= t.budget_bytes - 65536

let unique ids =
  List.length ids
  = List.length (List.dedup_and_sort ids ~compare:String.compare)

let known_status = function
  | "completed_ok" | "completed_no_commits" | "failed" | "cancelled"
  | "retryable" | "attention" | "merged_observed" ->
      true
  | _ -> false

let valid (t : t) =
  unique (List.map t.results ~f:fst)
  && List.for_all t.results ~f:(fun (id, status) ->
      known_status status
      && List.exists t.assignments ~f:(fun a ->
          String.equal a.id id && a.result_committed))
  && valid_id t.run_id && valid_id t.config_id && valid_id t.plan_id
  && (not (String.is_empty t.plan_bytes))
  && (not (String.is_empty t.config_bytes))
  && t.capacity > 0
  && (match t.lifecycle with
    | Sleeping _ ->
        reserved_count t = 0 && all_retired t && settled_assignments t
    | Running | Draining _ | Cancelling _ -> true)
  && t.budget_bytes <= 8388608
  && String.length (encode t) <= t.budget_bytes
  && reserved_count t <= t.capacity
  && unique (List.map t.operations ~f:(fun o -> o.id))
  && unique (List.map t.assignments ~f:(fun a -> a.id))
  && unique
       (List.filter_map t.assignments ~f:(fun a ->
            if a.reserved then Some a.resource else None))
  && unique
       (List.filter_map t.assignments ~f:(fun a ->
            if a.reserved then Some a.operation else None))
  && unique
       (List.filter_map t.assignments ~f:(fun a ->
            if a.reserved then Some (Patch_id.to_string a.patch) else None))
  && unique
       (List.map t.capsules ~f:(fun c ->
            c.assignment ^ ":" ^ Int64.to_string c.version))
  && List.for_all t.capsules ~f:(fun c ->
      Int64.(c.version >= 0L)
      && (not (String.is_empty c.bytes))
      && (not (String.is_empty c.receipt))
      && List.exists t.assignments ~f:(fun a -> String.equal a.id c.assignment))
  && unique (List.map t.resources ~f:fst)
  && unique (List.map t.resource_owners ~f:fst)
  && unique (List.map t.terminal_patches ~f:Patch_id.to_string)
  && List.for_all t.terminal_patches ~f:(fun patch ->
      (not
         (List.exists t.operations ~f:(fun o ->
              Patch_id.equal o.patch patch && o.unfinished)))
      && not
           (List.exists t.assignments ~f:(fun a ->
                Patch_id.equal a.patch patch && a.reserved)))
  && List.for_all t.resource_owners ~f:(fun (id, _) ->
      Option.is_some (List.Assoc.find t.resources id ~equal:String.equal))
  && unique (List.map t.controls ~f:(fun c -> c.id))
  && unique (List.map t.inputs ~f:(fun (id, _, _, _, _) -> id))
  && unique (List.map t.input_floors ~f:(fun (id, _, _) -> id))
  && List.for_all t.input_floors ~f:(fun (id, assignment, version) ->
      List.exists t.inputs ~f:(fun (i, _, _, _, _) -> String.equal i id)
      && List.exists t.capsules ~f:(fun c ->
          String.equal c.assignment assignment && Int64.equal c.version version))
  && unique t.retired_resources && unique t.lost_resources
  && List.for_all t.resources ~f:(fun (id, request) ->
      valid_id id && not (String.is_empty request))
  && List.for_all (t.retired_resources @ t.lost_resources) ~f:(fun id ->
      Option.is_some (List.Assoc.find t.resources id ~equal:String.equal))
  && List.for_all t.operations ~f:(fun o ->
      (valid_id o.id
      || o.maintenance
         && String.length o.id <= 1024
         && String.length o.id > 0
         && not (String.contains o.id '\000'))
      && ((not o.maintenance) || String.is_prefix o.id ~prefix:"maintenance:")
      && o.used >= 0 && o.budget > 0 && o.used <= o.budget
      && (o.used < o.budget || o.attention))
  && List.for_all t.assignments ~f:(fun a ->
      valid_id a.id && valid_id a.request
      && Option.value_map
           (List.Assoc.find t.resource_owners a.resource ~equal:String.equal)
           ~default:false ~f:(Patch_id.equal a.patch)
      && List.exists t.operations ~f:(fun o ->
          String.equal o.id a.operation && Patch_id.equal o.patch a.patch)
      && ((not a.cleanup) || a.result_committed)
      && ((not a.result_committed)
         || List.exists t.capsules ~f:(fun c -> String.equal c.assignment a.id)
         )
      && ((not a.reserved) || not (a.cleanup || a.retired)))
  && List.for_all t.controls ~f:(fun c ->
      valid_id c.id && control_target_ok c
      && String.equal c.run_id t.run_id
      && String.equal c.config_id t.config_id)

let decode bytes =
  if String.length bytes > 8388608 then Error "state exceeds 8 MiB"
  else
    try
      Result.bind (Json.of_string bytes) ~f:(function
        | `Assoc [ ("version", `Int 3); ("state", state) ]
        | `Assoc [ ("state", state); ("version", `Int 3) ] ->
            Result.bind (Json.try_of_yojson t_of_yojson state) ~f:(fun t ->
                if valid t then Ok t else Error "invalid coordinator inventory")
        | _ -> Error "unsupported coordinator state version")
    with _ -> Error "malformed coordinator state"

let update_op t id ~f =
  {
    t with
    operations =
      List.map t.operations ~f:(fun o ->
          if String.equal o.id id then f o else o);
  }

let update_assignment t id ~f =
  {
    t with
    assignments =
      List.map t.assignments ~f:(fun a ->
          if String.equal a.id id then f a else a);
  }

let reject reason t = (t, [ Rejected reason ])

let emergency_event = function
  | Resource_request (_, bytes) -> (
      match Resource_protocol.make_request bytes with
      | Ok request -> Poly.equal request.operation Resource_protocol.Stop
      | Error _ -> false)
  | Resource_stopped _ | Loss _ | Advance_receipt _ | Cleanup_confirmed _
  | Sleep_ready _ | Cancel_cleanup_confirmed ->
      true
  | Control c -> String.equal c.kind "cancel" || String.equal c.kind "sleep"
  | _ -> false

let commit ~emergency t next effects =
  if
    if emergency then String.length (encode next) <= next.budget_bytes
    else fits next
  then (next, effects)
  else reject "state reserve exhausted" t

let rec step t event =
  let commit = commit ~emergency:(emergency_event event) in
  match event with
  | Resource_request (id, request) -> (
      if (not (valid_id id)) || String.is_empty request then
        reject "invalid resource request" t
      else
        match List.Assoc.find t.resources id ~equal:String.equal with
        | Some old when String.equal old request -> (t, [ Already_applied ])
        | Some _ -> reject "changed resource request" t
        | None ->
            commit t
              { t with resources = (id, request) :: t.resources }
              [ Accepted ])
  | Bind_resource (id, patch) -> (
      match List.Assoc.find t.resources id ~equal:String.equal with
      | None -> reject "resource request missing" t
      | Some _ -> (
          match List.Assoc.find t.resource_owners id ~equal:String.equal with
          | Some old when Patch_id.equal old patch -> (t, [ Already_applied ])
          | Some _ -> reject "changed resource owner" t
          | None when List.mem t.terminal_patches patch ~equal:Patch_id.equal ->
              reject "terminal patch" t
          | None ->
              commit t
                { t with resource_owners = (id, patch) :: t.resource_owners }
                [ Accepted ]))
  | Terminal_patch patch ->
      if List.mem t.terminal_patches patch ~equal:Patch_id.equal then
        (t, [ Already_applied ])
      else if
        List.exists t.operations ~f:(fun o ->
            Patch_id.equal o.patch patch && o.unfinished && not o.maintenance)
        || List.exists t.assignments ~f:(fun a ->
            Patch_id.equal a.patch patch && a.reserved)
      then reject "terminal patch has unfinished assignment" t
      else
        let retire =
          List.filter_map t.resource_owners ~f:(fun (id, owner) ->
              if
                Patch_id.equal owner patch
                && not (List.mem t.retired_resources id ~equal:String.equal)
              then Some (Retire id)
              else None)
        in
        let next =
          {
            t with
            terminal_patches = patch :: t.terminal_patches;
            operations =
              List.map t.operations ~f:(fun o ->
                  if Patch_id.equal o.patch patch && o.maintenance then
                    { o with unfinished = false }
                  else o);
          }
        in
        commit t next (Accepted :: retire)
  | Admit o ->
      if
        (not (valid_id o.id))
        || o.maintenance
        || String.is_prefix o.id ~prefix:"maintenance:"
        || o.used <> 0 || o.budget <= 0 || o.attention || (not o.unfinished)
        || (not (valid_id (Patch_id.to_string o.patch)))
        || List.mem t.terminal_patches o.patch ~equal:Patch_id.equal
        || Option.is_some
             (List.find t.operations ~f:(fun old -> String.equal old.id o.id))
        || match t.lifecycle with Running -> false | _ -> true
      then reject "invalid admission" t
      else
        let prior =
          List.map t.operations ~f:(fun old ->
              if old.maintenance && Patch_id.equal old.patch o.patch then
                { old with unfinished = false }
              else old)
        in
        commit t { t with operations = o :: prior } [ Accepted ]
  | Assign (a, now) ->
      let op =
        List.find t.operations ~f:(fun o -> String.equal o.id a.operation)
      in
      if
        (not (valid_id a.id && valid_id a.resource && valid_id a.request))
        || a.retired || a.cleanup || a.result_committed || (not a.reserved)
        || List.mem t.terminal_patches a.patch ~equal:Patch_id.equal
        || Option.is_some
             (List.find t.assignments ~f:(fun old -> String.equal old.id a.id))
        || reserved_count t >= t.capacity
        || (not
              (Option.value_map
                 (List.Assoc.find t.resource_owners a.resource
                    ~equal:String.equal)
                 ~default:false ~f:(Patch_id.equal a.patch)))
        || List.mem t.retired_resources a.resource ~equal:String.equal
        || List.mem t.lost_resources a.resource ~equal:String.equal
        || List.exists t.assignments ~f:(fun prior ->
            prior.reserved
            && (String.equal prior.resource a.resource
               || Patch_id.equal prior.patch a.patch))
        || Option.value_map op ~default:true ~f:(fun o ->
            (not (Patch_id.equal o.patch a.patch)) || not (can_recover t o ~now))
        ||
        match t.lifecycle with
        | Running -> false
        | Draining _ ->
            not
              (List.exists t.assignments ~f:(fun old ->
                   String.equal old.operation a.operation
                   && (old.retired || old.cleanup)))
        | Sleeping _ | Cancelling _ -> true
      then reject "assignment not authorized" t
      else
        commit t { t with assignments = a :: t.assignments } [ Dispatch a.id ]
  | Worker_result (id, request, status, now) -> (
      if (not (known_status status)) || Int64.(now < 0L) then
        reject "invalid result status/time" t
      else
        match List.Assoc.find t.results id ~equal:String.equal with
        | Some old when not (String.equal old status) ->
            reject "changed result status" t
        | Some _ -> step t (Result_committed (id, request))
        | None -> (
            let next, outcomes = step t (Result_committed (id, request)) in
            if
              List.exists outcomes ~f:(function
                | Rejected _ -> true
                | _ -> false)
            then (t, outcomes)
            else
              match
                List.find next.assignments ~f:(fun a -> String.equal a.id id)
              with
              | None -> reject "missing result assignment" t
              | Some a ->
                  let next =
                    update_op
                      { next with results = (id, status) :: next.results }
                      a.operation
                      ~f:(fun o ->
                        match status with
                        | "completed_ok" | "completed_no_commits"
                        | "merged_observed" ->
                            { o with unfinished = false; attention = false }
                        | "retryable" | "failed" ->
                            let used =
                              if o.used >= o.budget then o.budget
                              else o.used + 1
                            in
                            {
                              o with
                              unfinished = true;
                              used;
                              retry_after = next_retry now;
                              attention = used >= o.budget;
                            }
                        | "attention" ->
                            { o with unfinished = true; attention = true }
                        | "cancelled" ->
                            {
                              o with
                              unfinished =
                                (match t.lifecycle with
                                | Cancelling _ -> false
                                | _ -> true);
                            }
                        | _ -> o)
                  in
                  commit t next outcomes))
  | Advance_receipt (c, previous) -> (
      match
        List.find t.capsules ~f:(fun old ->
            String.equal old.assignment c.assignment
            && Int64.equal old.version c.version)
      with
      | Some old when Poly.equal old c -> (t, [ Already_applied ])
      | Some old
        when String.equal old.bytes c.bytes
             && String.equal old.receipt previous
             && not (String.is_empty c.receipt) ->
          commit t
            {
              t with
              capsules =
                List.map t.capsules ~f:(fun current ->
                    if Poly.equal current old then c else current);
            }
            [ Accepted ]
      | _ -> reject "receipt does not advance an exact committed capsule" t)
  | Owner_receipt (c : capsule) -> (
      if
        (not (valid_id c.assignment))
        || Int64.(c.version < 0L)
        || String.is_empty c.bytes || String.is_empty c.receipt
      then reject "invalid owner artifact" t
      else
        match
          List.find t.assignments ~f:(fun a -> String.equal a.id c.assignment)
        with
        | None -> reject "unknown owner assignment" t
        | Some a ->
            let prior =
              List.filter t.capsules ~f:(fun old ->
                  String.equal old.assignment c.assignment)
            in
            if
              List.exists prior ~f:(fun old ->
                  Int64.equal old.version c.version)
            then
              if List.exists prior ~f:(fun old -> Poly.equal old c) then
                (t, [ Already_applied ])
              else reject "changed owner receipt" t
            else if a.cleanup || a.retired || a.result_committed then
              reject "settled owner" t
            else if
              List.exists prior ~f:(fun old -> Int64.(old.version >= c.version))
            then reject "stale owner receipt" t
            else commit t { t with capsules = c :: t.capsules } [ Accepted ])
  | (Result_committed (id, request) | Cleanup_confirmed (id, request)) as ev
    -> (
      match List.find t.assignments ~f:(fun a -> String.equal a.id id) with
      | None -> reject "unknown assignment" t
      | Some a when not (String.equal a.request request) ->
          reject "stale result" t
      | Some a
        when (match ev with Result_committed _ -> true | _ -> false)
             && not
                  (List.exists t.capsules ~f:(fun c ->
                       String.equal c.assignment a.id)) ->
          reject "result missing exact owner receipt" t
      | Some a
        when List.exists t.assignments ~f:(fun other ->
                 String.equal other.operation a.operation
                 && (not (String.equal other.id a.id))
                 && (not other.retired) && not other.cleanup) ->
          reject "superseded result" t
      | Some a ->
          let result =
            match ev with Result_committed _ -> true | _ -> false
          in
          if (result && a.result_committed) || ((not result) && a.cleanup) then
            (t, [ Already_applied ])
          else if (not result) && not a.result_committed then
            reject "cleanup before result" t
          else
            let next =
              update_assignment t id ~f:(fun a ->
                  if result then { a with result_committed = true }
                  else { a with cleanup = true; reserved = false })
            in
            let next =
              if result || List.Assoc.mem t.results a.id ~equal:String.equal
              then next
              else
                update_op next a.operation ~f:(fun o ->
                    { o with unfinished = false })
            in
            commit t next [ Accepted ])
  | Stop_confirmed (id, request) -> (
      match List.find t.assignments ~f:(fun a -> String.equal a.id id) with
      | None -> reject "unknown predecessor" t
      | Some a when not (String.equal request a.request) ->
          reject "wrong stop binding" t
      | Some a
        when not (List.mem t.retired_resources a.resource ~equal:String.equal)
        ->
          reject "resource stop not confirmed" t
      | Some a when a.retired -> (t, [ Already_applied ])
      | Some _ ->
          commit t
            (update_assignment t id ~f:(fun a ->
                 { a with retired = true; reserved = false }))
            [ Accepted ])
  | Resource_stopped (id, request) -> (
      match List.Assoc.find t.resources id ~equal:String.equal with
      | None -> reject "unknown resource" t
      | Some old when not (String.equal old request) ->
          reject "wrong resource stop binding" t
      | Some _ when List.mem t.retired_resources id ~equal:String.equal ->
          (t, [ Already_applied ])
      | Some _
        when not
               (List.mem t.lost_resources id ~equal:String.equal
               || (match t.lifecycle with
                 | Draining _ | Cancelling _ -> true
                 | Running | Sleeping _ -> false)
               || Option.value_map
                    (List.Assoc.find t.resource_owners id ~equal:String.equal)
                    ~default:false ~f:(fun owner ->
                      List.mem t.terminal_patches owner ~equal:Patch_id.equal))
        ->
          reject "healthy nonterminal worker must be retained" t
      | Some _ ->
          let next =
            {
              t with
              retired_resources = id :: t.retired_resources;
              assignments =
                List.map t.assignments ~f:(fun a ->
                    if String.equal a.resource id then
                      { a with retired = true; reserved = false }
                    else a);
            }
          in
          commit t next [ Accepted ])
  | Loss (resource, now) -> (
      if List.mem t.lost_resources resource ~equal:String.equal then
        (t, [ Already_applied ])
      else if List.mem t.retired_resources resource ~equal:String.equal then
        reject "already retired resource" t
      else
        let prior =
          List.filter t.assignments ~f:(fun a ->
              String.equal a.resource resource)
        in
        match
          List.Assoc.find t.resource_owners resource ~equal:String.equal
        with
        | None -> reject "unknown resource owner" t
        | Some patch -> (
            let active = List.find prior ~f:(fun a -> a.reserved) in
            match active with
            | Some current ->
                let next =
                  update_op
                    { t with lost_resources = resource :: t.lost_resources }
                    current.operation
                    ~f:(fun o ->
                      let used =
                        if o.used >= o.budget then o.budget else o.used + 1
                      in
                      {
                        o with
                        used;
                        retry_after = next_retry now;
                        attention = used >= o.budget;
                      })
                in
                let effects =
                  if
                    List.exists next.operations ~f:(fun o ->
                        String.equal o.id current.operation && o.attention)
                  then [ Retire resource; Attention current.operation ]
                  else [ Retire resource ]
                in
                commit t next effects
            | None ->
                let id = maintenance_id t patch in
                let t =
                  { t with lost_resources = resource :: t.lost_resources }
                in
                let next =
                  if List.exists t.operations ~f:(fun o -> String.equal o.id id)
                  then
                    update_op t id ~f:(fun o ->
                        let used =
                          if o.used >= o.budget then o.budget else o.used + 1
                        in
                        {
                          o with
                          used;
                          retry_after = next_retry now;
                          attention = used >= o.budget;
                          unfinished = true;
                        })
                  else
                    {
                      t with
                      operations =
                        {
                          id;
                          patch;
                          used = 1;
                          budget = 3;
                          retry_after = next_retry now;
                          attention = false;
                          unfinished = true;
                          maintenance = true;
                        }
                        :: t.operations;
                    }
                in
                commit t next
                  (match t.lifecycle with
                  | Running ->
                      if
                        List.exists next.operations ~f:(fun o ->
                            String.equal o.id id && o.attention)
                      then [ Retire resource; Attention id ]
                      else [ Retire resource ]
                  | Draining _ | Sleeping _ | Cancelling _ -> [ Accepted ])))
  | Retry (id, now) -> (
      match List.find t.operations ~f:(fun o -> String.equal o.id id) with
      | None -> reject "unknown operation" t
      | Some o when not (can_recover t o ~now) ->
          reject "recovery fenced or backing off" t
      | Some o ->
          (t, [ (if o.maintenance then Maintain o.patch else Recover id) ]))
  | Control c -> (
      if
        (not (valid_id c.id))
        || String.length c.payload > 131072
        || (not (control_target_ok c))
        || (not
              (List.mem
                 [
                   "guidance";
                   "automerge";
                   "bump";
                   "renewal";
                   "sleep";
                   "wake";
                   "cancel";
                   "forge_create";
                 ]
                 c.kind ~equal:String.equal))
        || (not (String.equal c.run_id t.run_id))
        || not (String.equal c.config_id t.config_id)
      then reject "invalid control scope" t
      else
        match List.find t.controls ~f:(fun old -> String.equal old.id c.id) with
        | Some old when Poly.equal old c -> (t, [ Already_applied ])
        | Some _ -> reject "changed control identity" t
        | None ->
            let lifecycle =
              match (c.kind, t.lifecycle) with
              | "cancel", _ -> Some (Cancelling c.id)
              | _, Cancelling _ -> None
              | "sleep", Running -> Some (Draining c.id)
              | "wake", (Draining _ | Sleeping _) -> Some Running
              | ("sleep" | "wake"), _ -> None
              | _, old -> Some old
            in
            let target = Option.value c.patch_id ~default:"" in
            let known_patch =
              List.exists t.operations ~f:(fun o ->
                  String.equal (Patch_id.to_string o.patch) target)
              || List.exists t.resource_owners ~f:(fun (_, p) ->
                  String.equal (Patch_id.to_string p) target)
            in
            let latest_input =
              List.fold t.inputs ~init:0L ~f:(fun n (_, p, v, _, _) ->
                  if String.equal p target && Int64.(v > n) then v else n)
            in
            let valid_payload =
              match c.kind with
              | "guidance" ->
                  (not (String.is_empty (String.strip c.payload)))
                  && not (Int64.equal latest_input Int64.max_value)
              | "automerge" ->
                  String.equal c.payload "true"
                  || String.equal c.payload "false"
              | "renewal" ->
                  List.exists t.operations ~f:(fun o ->
                      String.equal (Patch_id.to_string o.patch) target
                      && o.attention
                      && String.equal o.id c.payload)
              | "forge_create" ->
                  String.length c.payload > 0
                  && String.length c.payload <= 131072
              | "cancel" ->
                  String.is_empty c.payload || String.equal c.payload "cancel"
              | "bump" | "sleep" | "wake" -> String.is_empty c.payload
              | _ -> false
            in
            if
              Option.is_none lifecycle || (not valid_payload)
              || (Option.is_some c.patch_id && not known_patch)
              || String.equal c.kind "guidance"
                 && List.exists t.inputs ~f:(fun (id, _, _, _, _) ->
                     String.equal id c.id)
            then reject "control not applicable" t
            else
              let lifecycle = Option.value lifecycle ~default:t.lifecycle in
              let inputs, input_floors =
                if String.equal c.kind "guidance" then
                  let floor =
                    List.find t.assignments ~f:(fun (a : assignment) ->
                        String.equal (Patch_id.to_string a.patch) target
                        && a.reserved)
                    |> Option.bind ~f:(fun (a : assignment) ->
                        List.find t.capsules ~f:(fun cap ->
                            String.equal cap.assignment a.id)
                        |> Option.map ~f:(fun cap -> (c.id, a.id, cap.version)))
                  in
                  ( (c.id, target, Int64.succ latest_input, c.payload, false)
                    :: t.inputs,
                    Option.value_map floor ~default:t.input_floors ~f:(fun f ->
                        f :: t.input_floors) )
                else (t.inputs, t.input_floors)
              in
              commit t
                {
                  t with
                  controls = c :: t.controls;
                  lifecycle;
                  inputs;
                  input_floors;
                }
                [ Accepted ])
  | Input (id, patch, version, payload) -> (
      if (not (valid_id id && valid_id patch)) || Int64.(version <= 0L) then
        reject "invalid input" t
      else
        match
          List.find t.inputs ~f:(fun (old, _, _, _, _) -> String.equal old id)
        with
        | Some (old, p, v, bytes, _)
          when String.equal old id && String.equal p patch
               && Int64.equal v version && String.equal bytes payload ->
            (t, [ Already_applied ])
        | Some _ -> reject "changed input identity" t
        | None ->
            let latest =
              List.fold t.inputs ~init:0L ~f:(fun n (_, p, v, _, _) ->
                  if String.equal p patch && Int64.(v > n) then v else n)
            in
            if
              Int64.equal latest Int64.max_value
              || not Int64.(version = succ latest)
            then reject "input sequence" t
            else
              let floor =
                List.find t.assignments ~f:(fun (a : assignment) ->
                    String.equal patch (Patch_id.to_string a.patch)
                    && a.reserved)
                |> Option.bind ~f:(fun (a : assignment) ->
                    List.find t.capsules ~f:(fun c ->
                        String.equal c.assignment a.id)
                    |> Option.map ~f:(fun (c : capsule) ->
                        (id, a.id, c.version)))
              in
              commit t
                {
                  t with
                  inputs = (id, patch, version, payload, false) :: t.inputs;
                  input_floors =
                    Option.value_map floor ~default:t.input_floors
                      ~f:(fun entry -> entry :: t.input_floors);
                }
                [ Accepted ])
  | Input_consumed (assignment_id, id, version) ->
      let input =
        List.find t.inputs ~f:(fun (i, _, v, _, _) ->
            String.equal i id && Int64.equal v version)
      in
      let assignment =
        List.find t.assignments ~f:(fun a -> String.equal a.id assignment_id)
      in
      if
        not
          (Option.value_map input ~default:false ~f:(fun (_, patch, _, _, _) ->
               Option.value_map assignment ~default:false ~f:(fun a ->
                   String.equal patch (Patch_id.to_string a.patch)
                   && (not a.retired) && (not a.cleanup)
                   && (not a.result_committed)
                   && List.exists t.capsules ~f:(fun c ->
                       String.equal c.assignment a.id
                       &&
                       match
                         List.find t.input_floors ~f:(fun (i, _, _) ->
                             String.equal i id)
                       with
                       | Some (_, old, v) when String.equal old a.id ->
                           Int64.(c.version > v)
                       | Some _ | None -> true)
                   && not
                        (List.exists t.assignments ~f:(fun other ->
                             String.equal other.operation a.operation
                             && (not (String.equal other.id a.id))
                             && (not other.retired) && not other.cleanup)))))
      then reject "input not checkpointed by current assignment" t
      else
        commit t
          {
            t with
            inputs =
              List.map t.inputs ~f:(fun (i, p, v, b, consumed) ->
                  ( i,
                    p,
                    v,
                    b,
                    consumed || (String.equal i id && Int64.equal v version) ));
          }
          [ Accepted ]
  | Sleep_ready id -> (
      match t.lifecycle with
      | Draining current
        when String.equal current id
             && reserved_count t = 0
             && all_retired t && settled_assignments t ->
          commit t { t with lifecycle = Sleeping id } [ Ready_to_sleep id ]
      | _ -> reject "sleep not drained" t)
  | Cancel_cleanup_confirmed -> (
      match t.lifecycle with
      | Cancelling _ ->
          let next = { t with cancel_cleanup = true } in
          commit t next
            [
              (if cancellation_complete next then Cancel_complete else Accepted);
            ]
      | _ -> reject "not cancelling" t)

type context_input = {
  run_id : string;
  config_id : string;
  plan_id : string;
  message_id : string;
  generation : int64;
  patch_id : Patch_id.t;
  dependencies : (Patch_id.t * bool * string) list;
  selected_base : Branch.t;
  mode : string;
  root : bool;
  evidence_id : string;
  guidance_ids : string list;
  input_version : int64;
}

let execution_context t view (c : context_input) =
  let id = c.patch_id in
  let dependency_ids =
    if
      List.mem
        (Graph.all_patch_ids view.Scheduler_decision.graph)
        id ~equal:Patch_id.equal
    then Graph.deps view.graph id
    else []
  in
  let actual =
    List.map dependency_ids ~f:(fun d -> Patch_id.to_string d)
    |> List.sort ~compare:String.compare
  in
  let supplied =
    List.map c.dependencies ~f:(fun (d, _, _) -> Patch_id.to_string d)
    |> List.sort ~compare:String.compare
  in
  let selected = Scheduler_decision.selected_base view id in
  let expected_mode =
    if Option.is_some (Execution_mode.root view.mode) then "feature"
    else "mainline"
  in
  let latest_input =
    List.fold t.inputs ~init:0L ~f:(fun n (_, p, v, _, _) ->
        if String.equal p (Patch_id.to_string id) && Int64.(v > n) then v else n)
  in
  let guidance =
    List.filter t.inputs ~f:(fun (i, p, v, _, used) ->
        String.equal p (Patch_id.to_string id)
        && (not used)
        && Int64.(v <= c.input_version)
        && List.mem c.guidance_ids i ~equal:String.equal)
  in
  if
    (not
       (String.equal c.run_id t.run_id && String.equal c.config_id t.config_id))
    || (not (List.equal String.equal actual supplied))
    || (not (Scheduler_decision.dependencies_admitted view id))
    || (not
          (Option.value_map selected ~default:false
             ~f:(Branch.equal c.selected_base)))
    || (not (String.equal c.mode expected_mode))
    || (not (Bool.equal c.root (Execution_mode.is_root view.mode id)))
    || (not
          (String.equal c.plan_id t.plan_id
          && valid_id c.message_id && valid_id c.evidence_id))
    || Int64.(c.generation < 0L || c.input_version <> latest_input)
    || (not
          (Option.value_map
             (List.find t.operations ~f:(fun o ->
                  String.equal o.id c.message_id && Patch_id.equal o.patch id))
             ~default:false
             ~f:(fun o -> o.unfinished && not o.maintenance)))
    || (not
          (Option.value_map
             (List.find view.agents ~f:(fun a ->
                  Patch_id.equal a.Patch_agent.patch_id id))
             ~default:false
             ~f:(fun a -> Int64.equal c.generation (Int64.of_int a.generation))))
    || (not
          (Option.value_map
             (Scheduler_decision.evidence_id view id)
             ~default:false
             ~f:(String.equal c.evidence_id)))
    || List.length guidance <> List.length c.guidance_ids
    || not
         (List.for_all c.dependencies ~f:(fun (dep, admitted, rev) ->
              admitted && valid_id rev
              && Option.value_map
                   (List.find view.agents ~f:(fun a ->
                        Patch_id.equal a.Patch_agent.patch_id dep))
                   ~default:false
                   ~f:(fun a ->
                     let known =
                       if a.merged then a.merge_commit_sha else a.head_oid
                     in
                     Option.value_map known ~default:false ~f:(String.equal rev))))
  then Error "context not supported by current scheduling state"
  else
    let str s = `String s in
    let json =
      `Assoc
        [
          ("version", `Int 3);
          ("run_id", str c.run_id);
          ("patch_id", str (Patch_id.to_string id));
          ("config_id", str c.config_id);
          ("plan_id", str c.plan_id);
          ("message_id", str c.message_id);
          ("generation", str (Int64.to_string c.generation));
          ( "dependencies",
            `List
              (List.map c.dependencies ~f:(fun (d, admitted, revision) ->
                   `Assoc
                     [
                       ("patch_id", str (Patch_id.to_string d));
                       ("admitted", `Bool admitted);
                       ("revision", str revision);
                     ])) );
          ("selected_base", str (Branch.to_string c.selected_base));
          ("execution_mode", str c.mode);
          ("root_role", str (if c.root then "root" else "patch"));
          ("admitted", `Bool true);
          ("evidence_id", str c.evidence_id);
          ("guidance_ids", `List (List.map c.guidance_ids ~f:str));
          ("input_version", str (Int64.to_string c.input_version));
        ]
    in
    Worker_protocol_v3.decode_context (Yojson.Safe.to_string json)

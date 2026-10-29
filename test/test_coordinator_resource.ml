(* @archlint.module test
   @archlint.domain coordinator *)
open Onton_core
open Onton

[@@@warning "-4-40-42"]

module F = Onton_test_support.Coordinator_store_fixture
module R = Coordinator_resource

let check yes why = if not yes then failwith why

let request bytes =
  match Resource_protocol.make_request bytes with
  | Ok r -> r
  | Error why -> failwith why

let patch = Types.Patch_id.of_string

let operation p : Coordinator_state.operation =
  {
    id = "op-" ^ p;
    patch = patch p;
    used = 0;
    budget = 3;
    retry_after = 0L;
    attention = false;
    unfinished = true;
    maintenance = false;
  }

let assignment p resource : Coordinator_state.assignment =
  {
    id = "assignment-" ^ resource;
    operation = "op-" ^ p;
    patch = patch p;
    resource;
    request = "attempt-" ^ p;
    reserved = true;
    retired = false;
    result_committed = false;
    cleanup = false;
  }

let launch ?predecessor p resource =
  request
    (Printf.sprintf
       {|{"version":"resource-v1","op":"%s","run_id":"r","request_id":"request-%s","coordinator_id":"c","owner_epoch":"1","revision":"1","chain_id":"chain-%s","patch_id":"%s","resource_id":"%s","launch_digest":"digest","manifest":{"executable":"worker"}%s}|}
       (if predecessor = None then "launch" else "replace")
       resource p p resource
       (match predecessor with
       | None -> ""
       | Some id -> ",\"predecessor_id\":\"" ^ id ^ "\""))

let applied = function
  | Ok (Coordinator_host.Applied _) -> ()
  | _ -> failwith "commit"

let fixture () =
  let f = F.fixture () in
  let counter = ref 0 in
  let next () =
    incr counter;
    "write-" ^ string_of_int !counter
  in
  let physical = ref []
  and sends = ref 0
  and pending = ref false
  and lose = ref false in
  let sealed = ref false in
  let transport ~argv ~input =
    let fields =
      match Yojson.Safe.from_string input with `Assoc xs -> xs | _ -> []
    in
    let str = F.string in
    match str "op" fields with
    | "state-write" when !sealed ->
        failwith "opaque write after close tombstone"
    | "state-read" when !sealed ->
        Ok (0, F.reply ~reason:"closed" "rejected", "")
    | "close" ->
        check
          (str "revision" fields = string_of_int f.revision)
          "stale close guard";
        sealed := true;
        Ok (0, {|{"status":"ok","reason":"none","run_id":"r"}|}, "")
    | "launch" | "replace" ->
        let resource = str "resource_id" fields in
        let state =
          match f.payload with
          | Some bytes -> (
              let prefix = "onton-coordinator-3\n" in
              let packed =
                String.sub bytes (String.length prefix)
                  (String.length bytes - String.length prefix)
              in
              let bytes =
                match
                  Coordinator_archive.decode
                    ~max_bytes:(64 * 1024 * 1024)
                    packed
                with
                | Ok s -> s
                | Error e -> failwith e
              in
              match Yojson.Safe.from_string bytes with
              | `Assoc fields -> (
                  match Coordinator_state.decode (str "state" fields) with
                  | Ok s -> s
                  | Error e -> failwith e)
              | _ -> failwith "envelope")
          | None -> failwith "effect before any state"
        in
        check
          (List.mem_assoc resource (Coordinator_state.resources state))
          "launch before durable intent";
        check
          (List.exists
             (fun (a : Coordinator_state.assignment) ->
               a.resource = resource && a.reserved)
             (Coordinator_state.assignments state))
          "launch before reservation";
        let id = str "request_id" fields in
        (match List.assoc_opt id !physical with
        | Some bytes -> check (bytes = input) "changed helper replay"
        | None ->
            check
              (str "revision" fields = string_of_int f.revision)
              "first launch has stale pre-commit guard";
            incr sends;
            physical := (id, input) :: !physical;
            f.revision <- f.revision + 1);
        if !lose then (
          lose := false;
          Error "lost launch response")
        else if !pending then
          Ok
            ( 0,
              F.reply ~reason:"launch_uncertain" ~revision:f.revision "pending",
              "" )
        else
          Ok
            ( 0,
              Printf.sprintf
                {|{"status":"ok","reason":"none","run_id":"r","resource_id":"%s","revision":%d,"owner_epoch":1}|}
                resource f.revision,
              "" )
    | "stop" ->
        check
          (str "revision" fields = string_of_int f.revision)
          "stale stop guard";
        f.revision <- f.revision + 1;
        Ok
          ( 0,
            Printf.sprintf
              {|{"status":"ok","reason":"none","run_id":"r","resource_id":"%s","revision":%d,"owner_epoch":1}|}
              (str "resource_id" fields) f.revision,
            "" )
    | _ -> F.transport f ~argv ~input
  in
  let client = Resource_client.create ~helper:"fixture" ~transport in
  let store =
    Coordinator_store.create ~client ~run_id:"r" ~coordinator_id:"c"
      ~owner_epoch:1L ~revision:1L ~next_id:next
  in
  let _, state, _ = F.setup f in
  ignore
    (F.committed
       (Coordinator_store.commit store ~expected_version:0L ~write_id:"initial"
          (Coordinator_store.initial state)));
  let restore () =
    match Coordinator_host.restore ~store ~next_write_id:next with
    | Ok h -> h
    | Error _ -> failwith "restore"
  in
  (f, client, restore, sends, pending, lose)

let () =
  let f, client, restore, sends, _pending, lose = fixture () in
  let host = restore () in
  applied (Coordinator_host.submit host (Admit (operation "one")));
  f.reject_write <- true;
  let req = launch "one" "old" and a = assignment "one" "old" in
  check
    (match R.provision ~host ~client ~request:req ~assignment:a ~now:0L with
    | Blocked _ -> true
    | _ -> false)
    "uncommitted launch accepted";
  check (!sends = 0) "helper called despite rejected intent";
  f.reject_write <- false;
  lose := true;
  check
    (R.provision ~host ~client ~request:req ~assignment:a ~now:0L = R.Pending)
    "lost launch response released intent";
  check (!sends = 1) "one physical launch";
  let host = restore () in
  check
    (R.provision ~host ~client ~request:req ~assignment:a ~now:0L = R.Ready)
    "exact helper replay after restart failed";
  check (!sends = 1) "restart reissued physical launch";
  check
    (R.provision ~host ~client ~request:req ~assignment:a ~now:0L = R.Ready)
    "settled resource result not reusable";
  let replacement = launch ~predecessor:"old" "one" "new" in
  check
    (match
       R.provision ~host ~client ~request:replacement
         ~assignment:(assignment "one" "new") ~now:2L
     with
    | Blocked _ -> true
    | _ -> false)
    "live predecessor replaced";
  applied (Coordinator_host.submit host (Loss ("old", 0L)));
  let stop_template =
    request
      {|{"version":"resource-v1","op":"stop","run_id":"r","request_id":"stop-old","coordinator_id":"c","owner_epoch":"1","revision":"1","resource_id":"old"}|}
  in
  let stop =
    match Coordinator_host.resource_request host stop_template with
    | Ok r -> r
    | Error _ -> failwith "stop guards"
  in
  applied
    (Coordinator_host.submit host
       (Resource_request (stop.request_id, stop.bytes)));
  let reply =
    match Resource_client.request client stop with
    | Ok r -> r
    | Error _ -> failwith "stop"
  in
  check
    (Result.is_ok (Coordinator_host.refresh host))
    "metadata refresh after stop";
  applied
    (Coordinator_host.retired host ~resource_id:"old" ~stop_request:stop
       ~response:reply);
  check
    (R.provision ~host ~client ~request:replacement
       ~assignment:(assignment "one" "new") ~now:2L
    = R.Ready)
    "guarded retired replacement failed";
  let assignments =
    Coordinator_state.assignments (Coordinator_host.state host)
  in
  let recovered =
    Stdlib.List.find_opt
      (fun (assignment : Coordinator_state.assignment) ->
        String.equal assignment.Coordinator_state.id "assignment-new")
      assignments
  in
  check
    (match recovered with
    | Some recovered ->
        recovered.operation = a.operation
        && recovered.request = a.request
        && String.equal recovered.resource "new"
        && recovered.reserved && not recovered.retired
    | None -> false)
    "replacement assignment identity/resource lost";
  check
    (Stdlib.List.exists
       (fun (predecessor : Coordinator_state.assignment) ->
         String.equal predecessor.Coordinator_state.id "assignment-old"
         && String.equal predecessor.resource "old"
         && predecessor.retired)
       assignments)
    "predecessor retirement was not preserved";
  check (!sends = 2) "one replacement";

  let _, client, restore, sends, pending, _ = fixture () in
  let host = restore () in
  applied (Coordinator_host.submit host (Admit (operation "one")));
  pending := true;
  let req = launch "one" "ambiguous" and a = assignment "one" "ambiguous" in
  check
    (R.provision ~host ~client ~request:req ~assignment:a ~now:0L = R.Pending)
    "first ambiguity";
  let host = restore () in
  ignore (R.provision ~host ~client ~request:req ~assignment:a ~now:0L);
  check
    (R.provision ~host ~client ~request:req ~assignment:a ~now:0L = R.Attention)
    "ambiguity budget not durable";
  let before = !sends in
  check
    (R.provision ~host ~client ~request:req ~assignment:a ~now:0L = R.Attention)
    "attention not sticky";
  check (!sends = before) "attention still executes";
  applied (Coordinator_host.submit host (Admit (operation "two")));
  pending := false;
  check
    (R.provision ~host ~client ~request:(launch "two" "other")
       ~assignment:(assignment "two" "other") ~now:0L
    = R.Ready)
    "unrelated slot failed to progress";
  check
    (Coordinator_state.reserved_count (Coordinator_host.state host) = 2)
    "ambiguous launch released its reservation";
  check
    (List.exists
       (fun (o : Coordinator_state.operation) -> o.id = "op-one" && o.attention)
       (Coordinator_state.operations (Coordinator_host.state host)))
    "resource attention absent from operation";
  applied
    (Coordinator_host.submit host
       (Control
          {
            id = "renew";
            run_id = "r";
            config_id = "cfg";
            patch_id = Some "one";
            kind = "renewal";
            payload = "op-one";
          }));
  check
    (List.exists
       (fun (o : Coordinator_state.operation) ->
         o.id = "op-one" && not o.attention)
       (Coordinator_state.operations (Coordinator_host.state host)))
    "explicit operation renewal not scheduled";
  let f, client, restore, _, _, _ = fixture () in
  let host = restore () in
  applied
    (Coordinator_host.submit host
       (Control
          {
            id = "cancel";
            run_id = "r";
            config_id = "cfg";
            patch_id = None;
            kind = "cancel";
            payload = "";
          }));
  let close_template =
    request
      {|{"version":"resource-v1","op":"close","run_id":"r","request_id":"close","coordinator_id":"c","owner_epoch":"1","revision":"1"}|}
  in
  let close =
    match Coordinator_host.resource_request host close_template with
    | Ok r -> r
    | Error _ -> failwith "close guards"
  in
  applied
    (Coordinator_host.submit host
       (Resource_request (close.request_id, close.bytes)));
  let before = List.length f.writes in
  let reply =
    match Resource_client.request client close with
    | Ok r -> r
    | Error _ -> failwith "close"
  in
  applied
    (Coordinator_host.cancel_closed host ~close_request:close ~response:reply);
  check
    (Coordinator_state.cancellation_complete (Coordinator_host.state host))
    "close tombstone not projected";
  check
    (List.length f.writes = before)
    "close attempted a write to sealed storage";
  print_endline
    "resource lifecycle: commit-before-launch, exact replay, metadata guards, \
     retirement/replacement, durable ambiguity and unrelated progress"

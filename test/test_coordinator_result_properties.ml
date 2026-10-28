(* @archlint.module test
   @archlint.domain coordinator-state *)

open Base
open Onton_core
open Types
module S = Coordinator_state

[@@@warning "-4-40-42"]

let patch = Patch_id.of_string "a"

let initial () =
  S.create ~run_id:"run" ~config_id:"config" ~plan_id:"plan"
    ~plan_bytes:"frozen-plan" ~config_bytes:"nonsecret-config" ~capacity:2
    ~budget_bytes:8388608

let operation ?(owner = patch) id : S.operation =
  S.
    {
      id;
      patch = owner;
      used = 0;
      budget = 3;
      retry_after = 0L;
      attention = false;
      unfinished = true;
      maintenance = false;
    }

let assignment ?(owner = patch) id op resource : S.assignment =
  S.
    {
      id;
      operation = op;
      patch = owner;
      resource;
      request = "bind:" ^ id;
      reserved = true;
      retired = false;
      result_committed = false;
      cleanup = false;
    }

let apply t e = fst (S.step t e)

let settled status explicit_cancel =
  let t = initial () |> fun t -> apply t (S.Admit (operation "op")) in
  let t =
    apply t (S.Resource_request ("resource", "launch")) |> fun t ->
    apply t (S.Bind_resource ("resource", patch))
  in
  let t = apply t (S.Assign (assignment "assignment" "op" "resource", 0L)) in
  let capsule : S.capsule =
    {
      assignment = "assignment";
      version = 1L;
      bytes = "exact";
      receipt = "checkpoint";
    }
  in
  let t = apply t (S.Owner_receipt capsule) in
  let t =
    if explicit_cancel then
      apply t
        (S.Control
           {
             id = "cancel";
             run_id = "run";
             config_id = "config";
             kind = "cancel";
             payload = "cancel";
             patch_id = None;
           })
    else t
  in
  let t =
    apply t (S.Worker_result ("assignment", "bind:assignment", status, 100L))
  in
  let t = apply t (S.Cleanup_confirmed ("assignment", "bind:assignment")) in
  t

let property name gen f =
  QCheck2.Test.make ~name ~count:200 gen (fun x -> try f x with _ -> false)

let () =
  QCheck_base_runner.run_tests_main
    [
      property
        "all result statuses preserve explicit retry/attention/cancel intent \
         across restore"
        QCheck2.Gen.(
          pair
            (oneof_list
               [
                 "completed_ok";
                 "completed_no_commits";
                 "failed";
                 "cancelled";
                 "retryable";
                 "attention";
                 "merged_observed";
               ])
            bool)
        (fun (status, cancel) ->
          let t = settled status cancel in
          let restored =
            match S.decode (S.encode t) with Ok t -> t | Error e -> failwith e
          in
          let op =
            match S.operations restored with
            | [ op ] -> op
            | _ -> failwith "operation"
          in
          let retains =
            String.equal status "failed"
            || String.equal status "retryable"
            || String.equal status "attention"
            || (String.equal status "cancelled" && not cancel)
          in
          let repeated =
            apply restored
              (S.Worker_result ("assignment", "bind:assignment", status, 999L))
          in
          Bool.equal op.unfinished retains
          && Bool.equal op.attention (String.equal status "attention")
          && (op.used
             =
             if String.equal status "failed" || String.equal status "retryable"
             then 1
             else 0)
          && S.reserved_count restored = 0
          && List.is_empty (S.terminal_patches restored)
          && String.equal (S.encode restored) (S.encode repeated));
      property "unknown result status is rejected without consuming intent"
        QCheck2.Gen.string (fun status ->
          if
            List.mem
              [
                "completed_ok";
                "completed_no_commits";
                "failed";
                "cancelled";
                "retryable";
                "attention";
                "merged_observed";
              ]
              status ~equal:String.equal
          then true
          else
            let t = settled "retryable" false in
            let next, outcomes =
              S.step t
                (S.Worker_result ("assignment", "bind:assignment", status, 100L))
            in
            String.equal (S.encode t) (S.encode next)
            && List.exists outcomes ~f:(function
              | S.Rejected _ -> true
              | _ -> false));
      property
        "protocol receipt phases cannot replace capsule bytes or skip \
         predecessor"
        QCheck2.Gen.bool (fun change_bytes ->
          let t = settled "completed_ok" false in
          let old =
            match S.capsules t with [ c ] -> c | _ -> failwith "capsule"
          in
          let candidate =
            {
              old with
              receipt = "post-ACK";
              bytes = (if change_bytes then "changed" else old.bytes);
            }
          in
          let next, outcomes =
            S.step t
              (S.Advance_receipt
                 ( candidate,
                   if change_bytes then old.receipt else "wrong predecessor" ))
          in
          String.equal (S.encode t) (S.encode next)
          && List.exists outcomes ~f:(function
            | S.Rejected _ -> true
            | _ -> false));
      property
        "failed/retryable interleavings are bounded and sticky at exhaustion"
        QCheck2.Gen.(list_size (int_range 1 10) bool)
        (fun failed ->
          let t = initial () |> fun t -> apply t (S.Admit (operation "op")) in
          let t =
            apply t (S.Resource_request ("resource", "launch")) |> fun t ->
            apply t (S.Bind_resource ("resource", patch))
          in
          let t, accepted =
            List.foldi failed ~init:(t, 0) ~f:(fun n (t, accepted) failed ->
                let id = "attempt-" ^ Int.to_string n in
                let assigned, outcomes =
                  S.step t
                    (S.Assign
                       (assignment id "op" "resource", Int64.of_int (100 * n)))
                in
                if
                  List.exists outcomes ~f:(function
                    | S.Rejected _ -> true
                    | _ -> false)
                then (t, accepted)
                else
                  let c : S.capsule =
                    {
                      assignment = id;
                      version = 1L;
                      bytes = "exact";
                      receipt = "receipt";
                    }
                  in
                  let t = apply assigned (S.Owner_receipt c) in
                  let status = if failed then "failed" else "retryable" in
                  let t =
                    apply t
                      (S.Worker_result
                         (id, "bind:" ^ id, status, Int64.of_int (100 * n)))
                  in
                  let t = apply t (S.Cleanup_confirmed (id, "bind:" ^ id)) in
                  (t, accepted + 1))
          in
          let op =
            match S.operations t with [ op ] -> op | _ -> failwith "operation"
          in
          accepted = Int.min 3 (List.length failed)
          && op.used = accepted
          && Bool.equal op.attention (accepted = 3)
          && op.unfinished
          && S.reserved_count t = 0);
      property
        "emergency stop and cancel retain headroom after ordinary growth is \
         blocked"
        QCheck2.Gen.(int_range 100000 200000)
        (fun budget ->
          let t =
            S.create ~run_id:"run" ~config_id:"config" ~plan_id:"plan"
              ~plan_bytes:"plan" ~config_bytes:"config" ~capacity:2
              ~budget_bytes:budget
          in
          let padding = budget - 65536 - String.length (S.encode t) - 128 in
          let t =
            apply t (S.Resource_request ("padding", String.make padding 'a'))
          in
          let unchanged, _ =
            S.step t (S.Resource_request ("ordinary", String.make 1000 'b'))
          in
          let stop =
            {|{"version":"resource-v1","op":"stop","run_id":"run","request_id":"stop","coordinator_id":"c","owner_epoch":"1","revision":"1","resource_id":"padding"}|}
          in
          let stopped = apply t (S.Resource_request ("stop", stop)) in
          String.equal (S.encode t) (S.encode unchanged)
          && String.length (S.encode stopped) > budget - 65536
          && Result.is_ok (S.decode (S.encode stopped))
          && Coordinator_state.emergency_event
               (S.Resource_request ("stop", stop)));
      property
        "idle maintenance reserves a slot without coding and keeps its \
         allowance after completion/restart"
        QCheck2.Gen.(int_range 0 10000)
        (fun now ->
          let now = Int64.of_int now in
          let t =
            settled "completed_ok" false |> fun t ->
            apply t (S.Loss ("resource", now))
          in
          let maintenance =
            List.find_exn (S.operations t) ~f:(fun o -> o.maintenance)
          in
          let t = apply t (S.Resource_stopped ("resource", "launch")) in
          let t =
            apply t (S.Resource_request ("replacement", "launch2")) |> fun t ->
            apply t (S.Bind_resource ("replacement", patch))
          in
          let assigned =
            apply t
              (S.Assign
                 ( assignment "maintenance" maintenance.id "replacement",
                   Int64.succ now ))
          in
          let c : S.capsule =
            {
              assignment = "maintenance";
              version = 1L;
              bytes = "exact";
              receipt = "receipt";
            }
          in
          let t =
            apply assigned (S.Owner_receipt c) |> fun t ->
            apply t
              (S.Worker_result
                 ("maintenance", "bind:maintenance", "completed_ok", now))
            |> fun t ->
            apply t (S.Cleanup_confirmed ("maintenance", "bind:maintenance"))
            |> fun t -> apply t (S.Loss ("replacement", Int64.succ now))
          in
          let restored =
            match S.decode (S.encode t) with Ok t -> t | Error e -> failwith e
          in
          let op =
            List.find_exn (S.operations restored) ~f:(fun o -> o.maintenance)
          in
          S.reserved_count assigned = 1
          && op.unfinished && op.used = 2
          && String.equal op.id maintenance.id
          && String.length op.id <= 256);
      property
        "coordinator identity/result projections survive checkpoint decoding"
        QCheck2.Gen.bool (fun cancel ->
          let t = settled "retryable" cancel in
          String.equal (Coordinator_state.run_id t) "run"
          && String.equal (Coordinator_state.config_id t) "config"
          && String.equal (Coordinator_state.plan_id t) "plan"
          && List.is_empty (Coordinator_state.lost_resources t)
          && List.length (Coordinator_state.results t) = 1);
    ]

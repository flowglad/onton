(* @archlint.module test
   @archlint.domain coordinator-state *)

open Base
open Onton_core
open Types
module S = Coordinator_state

[@@@warning "-42"]

let patch = Patch_id.of_string "a"
let patch_b = Patch_id.of_string "b"
let patch_c = Patch_id.of_string "c"

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

let has_rejection =
  List.exists ~f:(function
    | S.Rejected _ -> true
    | S.Accepted | S.Already_applied | S.Dispatch _ | S.Recover _ | S.Maintain _
    | S.Attention _ | S.Retire _ | S.Ready_to_sleep _ | S.Cancel_complete ->
        false)

let () =
  let t =
    initial () |> fun t ->
    apply t (S.Admit (operation "one")) |> fun t ->
    apply t (S.Admit (operation ~owner:patch_b "two")) |> fun t ->
    apply t (S.Admit (operation ~owner:patch_c "three")) |> fun t ->
    apply t (S.Resource_request ("r1", "launch:r1")) |> fun t ->
    apply t (S.Resource_request ("r2", "launch:r2")) |> fun t ->
    apply t (S.Resource_request ("r5", "launch:r5")) |> fun t ->
    apply t (S.Bind_resource ("r1", patch)) |> fun t ->
    apply t (S.Bind_resource ("r2", patch_b)) |> fun t ->
    apply t (S.Bind_resource ("r5", patch_c))
  in
  let t =
    apply t (S.Assign (assignment "a1" "one" "r1", 0L)) |> fun t ->
    apply t (S.Assign (assignment ~owner:patch_b "a2" "two" "r2", 0L))
  in
  assert (S.reserved_count t = 2);
  (match S.decode (S.encode t) with
  | Ok restored -> assert (S.reserved_count restored = 2)
  | Error reason -> failwith reason);
  assert (
    has_rejection
      (snd
         (S.step t (S.Assign (assignment ~owner:patch_c "a3" "three" "r5", 0L)))));
  assert (has_rejection (snd (S.step t (S.Result_committed ("a1", "bind:a1")))));
  let receipt : S.capsule =
    S.
      {
        assignment = "a1";
        version = 1L;
        bytes = "owner-capsule";
        receipt = "exact-host-receipt";
      }
  in
  let t = apply t (S.Owner_receipt receipt) in
  assert (
    has_rejection
      (snd (S.step t (S.Owner_receipt S.{ receipt with bytes = "changed" }))));
  let t = apply t (S.Result_committed ("a1", "bind:a1")) in
  assert (S.reserved_count t = 2);
  let t = apply t (S.Cleanup_confirmed ("a1", "bind:a1")) in
  assert (S.reserved_count t = 1);
  assert (List.length (S.capsules t) = 1);
  assert (has_rejection (snd (S.step t (S.Stop_confirmed ("a2", "bind:a1")))));
  assert (has_rejection (snd (S.step t (S.Stop_confirmed ("a2", "bind:a2")))));
  let sleep : S.control =
    S.
      {
        id = "sleep";
        run_id = "run";
        config_id = "config";
        payload = "";
        kind = "sleep";
        patch_id = None;
      }
  in
  let stranded =
    apply t (S.Control sleep) |> fun t ->
    apply t (S.Resource_stopped ("r2", "launch:r2"))
  in
  assert (S.reserved_count stranded = 0);
  let second : S.capsule =
    S.
      {
        assignment = "a2";
        version = 1L;
        bytes = "second-capsule";
        receipt = "second-receipt";
      }
  in
  let t = apply t (S.Owner_receipt second) in
  let pending = apply t (S.Input ("new", "b", 1L, "new guidance")) in
  assert (
    has_rejection (snd (S.step pending (S.Input_consumed ("a1", "new", 1L)))));
  assert (
    has_rejection (snd (S.step pending (S.Input_consumed ("a2", "new", 1L)))));
  let newer_receipt : S.capsule =
    S.
      {
        second with
        version = 2L;
        bytes = "second-checkpoint";
        receipt = "newer-host-receipt";
      }
  in
  let pending =
    apply pending (S.Owner_receipt newer_receipt) |> fun t ->
    apply t (S.Input_consumed ("a2", "new", 1L))
  in
  assert (
    List.exists (S.inputs pending) ~f:(fun (id, _, _, _, used) ->
        String.equal id "new" && used));
  let t =
    apply t (S.Result_committed ("a2", "bind:a2")) |> fun t ->
    apply t (S.Cleanup_confirmed ("a2", "bind:a2"))
  in
  assert (S.reserved_count t = 0);
  let before_sleep = t in
  let t_input = apply t (S.Input ("guidance-1", "a", 1L, "first")) in
  assert (
    has_rejection
      (snd (S.step t_input (S.Input ("guidance-1", "a", 1L, "changed")))));
  assert (
    has_rejection
      (snd (S.step t_input (S.Input ("guidance-3", "a", 3L, "gap")))));
  assert (
    has_rejection
      (snd (S.step t_input (S.Input_consumed ("a1", "guidance-1", 2L)))));
  assert (
    has_rejection
      (snd (S.step t_input (S.Input_consumed ("a1", "guidance-1", 1L)))));
  let restored_input = Result.ok_or_failwith (S.decode (S.encode t_input)) in
  assert (List.length (S.inputs restored_input) = 1);
  let stranded = apply stranded (S.Resource_stopped ("r1", "launch:r1")) in
  assert (has_rejection (snd (S.step stranded (S.Sleep_ready "sleep"))));
  let t = apply t (S.Control sleep) in
  assert (
    has_rejection
      (snd (S.step t (S.Control S.{ sleep with payload = "changed" }))));
  assert (
    has_rejection
      (snd
         (S.step t (S.Assign (assignment ~owner:patch_c "a3" "three" "r5", 0L)))));
  assert (not (S.sleep_ready (apply t (S.Sleep_ready "sleep"))));
  let t =
    apply t (S.Resource_stopped ("r1", "launch:r1")) |> fun t ->
    apply t (S.Resource_stopped ("r2", "launch:r2")) |> fun t ->
    apply t (S.Resource_stopped ("r5", "launch:r5")) |> fun t ->
    apply t (S.Sleep_ready "sleep")
  in
  assert (S.sleep_ready t);
  (match S.decode (S.encode t) with
  | Ok restored -> assert (S.sleep_ready restored)
  | Error reason -> failwith reason);
  let wake = S.{ sleep with id = "wake"; kind = "wake" } in
  let t = apply t (S.Control wake) in
  assert (not (S.sleep_ready t));
  let cancel = S.{ sleep with id = "cancel"; kind = "cancel" } in
  let cancelling =
    apply before_sleep (S.Control cancel) |> fun t ->
    apply t S.Cancel_cleanup_confirmed
  in
  assert (not (S.cancellation_complete cancelling));
  assert (has_rejection (snd (S.step cancelling (S.Control wake))));
  let cancelling = apply cancelling (S.Resource_stopped ("r1", "launch:r1")) in
  assert (not (S.cancellation_complete cancelling));
  let cancelling = apply cancelling (S.Resource_stopped ("r2", "launch:r2")) in
  assert (not (S.cancellation_complete cancelling));
  let cancelling = apply cancelling (S.Resource_stopped ("r5", "launch:r5")) in
  assert (S.cancellation_complete cancelling);
  let t = apply before_sleep (S.Loss ("r1", 1L)) in
  let maintenance = List.find (S.operations t) ~f:(fun o -> o.S.maintenance) in
  assert (Option.value_map maintenance ~default:false ~f:(fun o -> o.S.used = 1));
  let maintenance_id =
    Option.value_map maintenance ~default:"missing" ~f:(fun (o : S.operation) ->
        o.S.id)
  in
  assert (has_rejection (snd (S.step t (S.Retry (maintenance_id, 2L)))));
  let t = apply t (S.Resource_stopped ("r1", "launch:r1")) in
  let substantive = apply t (S.Admit (operation "next")) in
  let substantive =
    apply substantive (S.Resource_request ("next-r", "launch:next-r"))
    |> fun t ->
    apply t (S.Bind_resource ("next-r", patch)) |> fun t ->
    apply t (S.Loss ("next-r", 3L))
  in
  let maintenance_ops =
    List.filter (S.operations substantive) ~f:(fun o -> o.S.maintenance)
  in
  assert (List.length maintenance_ops = 2);
  assert (
    List.exists maintenance_ops ~f:(fun (o : S.operation) ->
        String.equal o.S.id maintenance_id && o.S.used = 1 && not o.S.unfinished));
  assert (
    List.exists
      (snd (S.step t (S.Retry (maintenance_id, 2L))))
      ~f:(function
        | S.Maintain _ -> true
        | S.Accepted | S.Rejected _ | S.Already_applied | S.Dispatch _
        | S.Recover _ | S.Attention _ | S.Retire _ | S.Ready_to_sleep _
        | S.Cancel_complete ->
            false));
  let t =
    apply t (S.Resource_request ("r3", "launch:r3")) |> fun t ->
    apply t (S.Bind_resource ("r3", patch)) |> fun t ->
    apply t (S.Loss ("r3", 2L))
  in
  let t = Result.ok_or_failwith (S.decode (S.encode t)) in
  let t =
    apply t (S.Resource_request ("r4", "launch:r4")) |> fun t ->
    apply t (S.Bind_resource ("r4", patch))
  in
  let t, outcomes = S.step t (S.Loss ("r4", 3L)) in
  assert (
    List.exists outcomes ~f:(function
      | S.Attention _ -> true
      | S.Accepted | S.Rejected _ | S.Already_applied | S.Dispatch _
      | S.Recover _ | S.Maintain _ | S.Retire _ | S.Ready_to_sleep _
      | S.Cancel_complete ->
          false));
  let t = Result.ok_or_failwith (S.decode (S.encode t)) in
  assert (has_rejection (snd (S.step t (S.Retry (maintenance_id, 5L)))));
  assert (Result.is_error (S.decode "{\"version\":3,\"version\":3}"));
  assert (Result.is_error (S.decode "{\"version\":2,\"state\":{}}"));
  let idle =
    initial () |> fun t ->
    apply t (S.Resource_request ("idle", "launch:idle")) |> fun t ->
    apply t (S.Bind_resource ("idle", patch))
  in
  assert (
    has_rejection
      (snd (S.step idle (S.Resource_stopped ("idle", "launch:idle")))));
  let terminal, outcomes = S.step idle (S.Terminal_patch patch) in
  assert (
    List.exists outcomes ~f:(function
      | S.Retire "idle" -> true
      | S.Accepted | S.Rejected _ | S.Already_applied | S.Dispatch _
      | S.Recover _ | S.Maintain _ | S.Attention _ | S.Retire _
      | S.Ready_to_sleep _ | S.Cancel_complete ->
          false));
  let terminal = apply terminal (S.Resource_stopped ("idle", "launch:idle")) in
  assert (List.mem (S.retired_resources terminal) "idle" ~equal:String.equal);
  let tiny =
    S.create ~run_id:"run" ~config_id:"config" ~plan_id:"plan"
      ~plan_bytes:"frozen-plan" ~config_bytes:"nonsecret-config" ~capacity:2
      ~budget_bytes:65537
  in
  assert (has_rejection (snd (S.step tiny (S.Admit (operation "oversized")))));
  let bounded = initial () in
  assert (
    has_rejection
      (snd
         (S.step bounded (S.Resource_request ("huge", String.make 8400000 'x')))));
  assert (List.is_empty (S.resources bounded));
  let property =
    QCheck2.Test.make
      ~name:
        "five operations: reservations obey capacity and stale IDs do not \
         release slots"
      ~count:150
      (QCheck2.Gen.list_size
         (QCheck2.Gen.int_range 0 70)
         (QCheck2.Gen.int_range 0 7))
      (fun actions ->
        try
          let t = initial () in
          let t =
            List.fold (List.init 5 ~f:Fn.id) ~init:t ~f:(fun t i ->
                let id = "op" ^ Int.to_string i in
                let owner = Patch_id.of_string ("p" ^ Int.to_string i) in
                let t = apply t (S.Admit (operation ~owner id)) in
                let r = "r" ^ Int.to_string i in
                let t =
                  apply t (S.Resource_request (r, "launch" ^ Int.to_string i))
                in
                apply t (S.Bind_resource (r, owner)))
          in
          let t, model =
            List.foldi actions
              ~init:(t, Set.empty (module String))
              ~f:(fun index (t, model) n ->
                let i = n % 5 in
                let id = "a" ^ Int.to_string index in
                let resource = "r" ^ Int.to_string i in
                let op = "op" ^ Int.to_string i in
                let owner = Patch_id.of_string ("p" ^ Int.to_string i) in
                let ev =
                  S.Assign (assignment ~owner id op resource, Int64.of_int index)
                in
                let t', effects = S.step t ev in
                let model =
                  if
                    List.exists effects ~f:(function
                      | S.Dispatch _ -> true
                      | S.Accepted | S.Rejected _ | S.Already_applied
                      | S.Recover _ | S.Maintain _ | S.Attention _ | S.Retire _
                      | S.Ready_to_sleep _ | S.Cancel_complete ->
                          false)
                  then Set.add model id
                  else model
                in
                assert (S.reserved_count t' = Set.length model);
                assert (S.reserved_count t' <= 2);
                (t', model))
          in
          S.reserved_count t = Set.length model
        with _ -> false)
  in
  let decoder =
    QCheck2.Test.make ~name:"arbitrary control bytes decode without raising"
      ~count:250 QCheck2.Gen.string (fun bytes ->
        try
          ignore (S.decode_control bytes);
          true
        with _ -> false)
  in
  let state_decoder =
    QCheck2.Test.make
      ~name:"arbitrary versioned state bytes decode without raising" ~count:250
      QCheck2.Gen.string (fun bytes ->
        try
          ignore (S.decode bytes);
          true
        with _ -> false)
  in
  let rc =
    QCheck_base_runner.run_tests ~verbose:true
      [ property; decoder; state_decoder ]
  in
  if rc <> 0 then Stdlib.exit rc;
  Stdio.printf "coordinator independent lifecycle model: OK\n"

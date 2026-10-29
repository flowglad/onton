(* @archlint.module exempt
   @archlint.exempt-reason effect-facade *)
open Base

let json state =
  let open Coordinator_state in
  let str s = `String s in
  let lifecycle, generation =
    match lifecycle state with
    | Running -> ("executing", `Null)
    | Draining id -> ("draining", str id)
    | Sleeping id -> ("sleeping", str id)
    | Cancelling id -> ("cancelling", str id)
  in
  let assignments = assignments state in
  let operations = operations state in
  let attention =
    List.count operations ~f:(fun o -> o.attention)
    + List.count
        (Coordinator_journal.entries (journal state))
        ~f:(fun e ->
          match e.phase with Attention -> true | Pending | Applied _ -> false)
  in
  let operation_rows =
    List.take operations 128
    |> List.map ~f:(fun (op : operation) ->
        let current =
          List.find assignments ~f:(fun a ->
              String.equal a.operation op.id && not a.retired)
        in
        let status =
          if op.attention then "attention"
          else if
            Option.value_map current ~default:false ~f:(fun a -> a.reserved)
          then "executing"
          else if op.maintenance || (op.used > 0 && op.unfinished) then
            "recovery"
          else if op.unfinished then "capacity_wait"
          else "idle"
        in
        `Assoc
          [
            ("patch_id", str (Types.Patch_id.to_string op.patch));
            ("operation_id", str op.id);
            ("status", str status);
            ("attempts", `Int op.used);
            ("budget", `Int op.budget);
            ( "assignment_id",
              Option.value_map current ~default:`Null ~f:(fun a -> str a.id) );
            ( "resource_id",
              Option.value_map current ~default:`Null ~f:(fun a ->
                  str a.resource) );
          ])
  in
  let executing = reserved_count state in
  let idle =
    List.count (resource_owners state) ~f:(fun (id, _) ->
        (not (List.mem (retired_resources state) id ~equal:String.equal))
        && not
             (List.exists assignments ~f:(fun a ->
                  String.equal a.resource id && a.reserved)))
  in
  `Assoc
    [
      ("run_id", str (run_id state));
      ("config_id", str (config_id state));
      ("lifecycle", str lifecycle);
      ("sleep_generation", generation);
      ("sleep_ready", `Bool (sleep_ready state));
      ("cancel_complete", `Bool (cancellation_complete state));
      ("executing", `Int executing);
      ("idle", `Int idle);
      ("attention", `Int attention);
      ("resource_count", `Int (List.length (resource_owners state)));
      ("operation_count", `Int (List.length operations));
      ("operations", `List operation_rows);
      ("operations_truncated", `Bool (List.length operations > 128));
    ]

let emit ~write state = write (Yojson.Safe.to_string (json state) ^ "\n")

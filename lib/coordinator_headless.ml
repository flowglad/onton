(* @archlint.module shell
   @archlint.domain coordinator *)
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
  let attention = List.count operations ~f:(fun o -> o.attention) in
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
    ]

let emit ~write state = write (Yojson.Safe.to_string (json state) ^ "\n")

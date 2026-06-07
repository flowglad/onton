(* @archlint.module state
   @archlint.domain session-meta *)

open Base

let registry : Telemetry.Sink.t list ref = ref []
let mutex = Stdlib.Mutex.create ()

let register_sink sink =
  Stdlib.Mutex.protect mutex (fun () -> registry := sink :: !registry)

let unregister_sink ~name =
  Stdlib.Mutex.protect mutex (fun () ->
      registry :=
        List.filter !registry ~f:(fun sink ->
            not (String.equal sink.Telemetry.Sink.name name)))

let emit event =
  let snapshot = Stdlib.Mutex.protect mutex (fun () -> !registry) in
  List.iter (Telemetry.route ~sinks:snapshot event) ~f:(fun sink ->
      try sink.Telemetry.Sink.consume event
      with exn ->
        Stdlib.Printf.eprintf "telemetry: sink %s raised %s\n%!"
          sink.Telemetry.Sink.name (Exn.to_string exn))

let replace_registry sinks =
  Stdlib.Mutex.protect mutex (fun () ->
      let old = !registry in
      registry := sinks;
      old)

let restore_registry old =
  Stdlib.Mutex.protect mutex (fun () -> registry := old)

let with_sinks ~sinks body =
  let old = replace_registry sinks in
  Exn.protect ~f:body ~finally:(fun () -> restore_registry old)

let with_sink ~sink body =
  let old =
    Stdlib.Mutex.protect mutex (fun () ->
        let old = !registry in
        registry := sink :: old;
        old)
  in
  Exn.protect ~f:body ~finally:(fun () -> restore_registry old)

let test_event =
  Telemetry.Event.Free_form
    {
      patch_id = Some (Types.Patch_id.of_string "patch-5");
      level = Telemetry.Event.Info;
      message = "hello";
    }

let test_sink ?(consume = fun _ -> ()) name interested_in =
  { Telemetry.Sink.name; interested_in; consume }

let%test "emit dispatches once per interested sink, never to uninterested sinks"
    =
  let interested_count = ref 0 in
  let uninterested_count = ref 0 in
  let interested =
    test_sink "interested"
      (fun _ -> true)
      ~consume:(fun _ -> Int.incr interested_count)
  in
  let uninterested =
    test_sink "uninterested"
      (fun _ -> false)
      ~consume:(fun _ -> Int.incr uninterested_count)
  in
  with_sinks ~sinks:[ interested; uninterested ] (fun () -> emit test_event);
  !interested_count = 1 && !uninterested_count = 0

let%test "emit is a no-op when no sinks are registered" =
  with_sinks ~sinks:[] (fun () -> emit test_event);
  true

let%test "emit is a no-op when no registered sink is interested" =
  let consumed = ref 0 in
  let sink =
    test_sink "uninterested"
      (fun _ -> false)
      ~consume:(fun _ -> Int.incr consumed)
  in
  with_sinks ~sinks:[ sink ] (fun () -> emit test_event);
  !consumed = 0

let%test "sink exceptions in consume do not propagate to emit's caller" =
  let sink =
    test_sink "raising" (fun _ -> true) ~consume:(fun _ -> failwith "boom")
  in
  try
    with_sinks ~sinks:[ sink ] (fun () -> emit test_event);
    true
  with _ -> false

let%test "with_sink restores the exact prior registry" =
  let outer_consumed = ref 0 in
  let inner_consumed = ref 0 in
  let outer =
    test_sink "outer"
      (fun _ -> true)
      ~consume:(fun _ -> Int.incr outer_consumed)
  in
  let inner =
    test_sink "inner"
      (fun _ -> true)
      ~consume:(fun _ -> Int.incr inner_consumed)
  in
  with_sinks ~sinks:[ outer ] (fun () ->
      with_sink ~sink:inner (fun () -> emit test_event);
      emit test_event);
  !outer_consumed = 2 && !inner_consumed = 1

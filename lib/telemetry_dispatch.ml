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

let with_sink ~sink body =
  register_sink sink;
  Exn.protect ~f:body ~finally:(fun () ->
      unregister_sink ~name:sink.Telemetry.Sink.name)

let test_event =
  Telemetry.Event.Free_form
    {
      patch_id = Some (Types.Patch_id.of_string "patch-2");
      level = Info;
      message = "hello";
    }

let test_sink ?(consume = fun _ -> ()) name interested_in =
  { Telemetry.Sink.name; interested_in; consume }

let with_registry_replaced sinks body =
  let old =
    Stdlib.Mutex.protect mutex (fun () ->
        let prev = !registry in
        registry := sinks;
        prev)
  in
  Exn.protect ~f:body ~finally:(fun () ->
      Stdlib.Mutex.protect mutex (fun () -> registry := old))

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
  with_registry_replaced [ interested; uninterested ] (fun () ->
      emit test_event);
  !interested_count = 1 && !uninterested_count = 0

let%test "emit is a no-op when no sinks are registered" =
  with_registry_replaced [] (fun () -> emit test_event);
  true

let%test "emit is a no-op when no registered sink is interested" =
  let consumed = ref 0 in
  let sink =
    test_sink "uninterested"
      (fun _ -> false)
      ~consume:(fun _ -> Int.incr consumed)
  in
  with_registry_replaced [ sink ] (fun () -> emit test_event);
  !consumed = 0

let%test "sink exceptions in consume do not propagate to emit's caller" =
  let sink =
    test_sink "raising" (fun _ -> true) ~consume:(fun _ -> failwith "boom")
  in
  try
    with_registry_replaced [ sink ] (fun () -> emit test_event);
    true
  with _ -> false

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Event = struct
  type level = Trace | Debug | Info | Warning | Error
  [@@deriving show, eq, yojson]

  type json = Yojson.Safe.t

  let pp_json formatter json =
    Stdlib.Format.pp_print_string formatter (Yojson.Safe.to_string json)

  let json_of_yojson json = json
  let yojson_of_json json = json

  type channel = [ `Stdout | `Stderr ] [@@deriving show, yojson]

  type t =
    | Poll of { patch_id : Types.Patch_id.t; payload : json }
    | Action of {
        patch_id : Types.Patch_id.t;
        session_uuid : string option;
        payload : json;
      }
    | Complete of {
        patch_id : Types.Patch_id.t;
        session_uuid : string option;
        subkind : Failure_subkind.t;
        payload : json;
      }
    | Stream of {
        patch_id : Types.Patch_id.t;
        session_uuid : string;
        channel : channel;
        raw : string;
      }
    | Spawn_started of {
        patch_id : Types.Patch_id.t;
        session_uuid : string;
        prompt : string;
        argv : string list;
        env_redacted : string array;
      }
    | Spawn_finalized of {
        patch_id : Types.Patch_id.t;
        session_uuid : string;
        meta : json;
      }
    | Free_form of {
        patch_id : Types.Patch_id.t option;
        level : level;
        message : string;
      }
  [@@deriving show, yojson]
end

module Sink = struct
  type t = {
    name : string;
    interested_in : Event.t -> bool;
    consume : Event.t -> unit;
  }
end

let route ~sinks event =
  List.filter sinks ~f:(fun sink -> sink.Sink.interested_in event)

let registry : Sink.t list ref = ref []
let mutex = Stdlib.Mutex.create ()

let register_sink sink =
  Stdlib.Mutex.protect mutex (fun () -> registry := sink :: !registry)

let unregister_sink ~name =
  Stdlib.Mutex.protect mutex (fun () ->
      registry :=
        List.filter !registry ~f:(fun sink ->
            not (String.equal sink.Sink.name name)))

let emit event =
  let snapshot = Stdlib.Mutex.protect mutex (fun () -> !registry) in
  List.iter (route ~sinks:snapshot event) ~f:(fun sink ->
      try sink.Sink.consume event
      with exn ->
        Stdlib.Printf.eprintf "telemetry: sink %s raised %s\n%!" sink.Sink.name
          (Exn.to_string exn))

let with_sink ~sink body =
  register_sink sink;
  Exn.protect ~f:body ~finally:(fun () -> unregister_sink ~name:sink.Sink.name)

let test_event =
  Event.Free_form
    {
      patch_id = Some (Types.Patch_id.of_string "patch-2");
      level = Info;
      message = "hello";
    }

let test_sink ?(consume = fun _ -> ()) name interested_in =
  { Sink.name; interested_in; consume }

let with_registry_replaced sinks body =
  let old = Stdlib.Mutex.protect mutex (fun () -> !registry) in
  Stdlib.Mutex.protect mutex (fun () -> registry := sinks);
  Exn.protect ~f:body ~finally:(fun () ->
      Stdlib.Mutex.protect mutex (fun () -> registry := old))

let names sinks = List.map sinks ~f:(fun sink -> sink.Sink.name)

let%test "route is pure — same inputs always produce the same output" =
  let calls = ref 0 in
  let sinks =
    [
      test_sink "yes" (fun _ ->
          Int.incr calls;
          true);
      test_sink "no" (fun _ ->
          Int.incr calls;
          false);
    ]
  in
  let first = route ~sinks test_event in
  let second = route ~sinks test_event in
  List.equal String.equal (names first) (names second) && !calls = 4

let%test "route returns exactly the sinks whose interested_in is true" =
  let interested = test_sink "interested" (fun _ -> true) in
  let uninterested = test_sink "uninterested" (fun _ -> false) in
  let sinks = [ uninterested; interested ] in
  List.equal String.equal (names (route ~sinks test_event)) [ "interested" ]

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

let%test "Event.t yojson roundtrip preserves every variant" =
  let patch_id = Types.Patch_id.of_string "patch-2" in
  let events =
    [
      Event.Poll { patch_id; payload = `Assoc [ ("kind", `String "poll") ] };
      Action { patch_id; session_uuid = Some "uuid"; payload = `List [] };
      Complete
        {
          patch_id;
          session_uuid = None;
          subkind = Failure_subkind.Network_error;
          payload = `Bool true;
        };
      Stream { patch_id; session_uuid = "uuid"; channel = `Stdout; raw = "{}" };
      Stream { patch_id; session_uuid = "uuid"; channel = `Stderr; raw = "err" };
      Spawn_started
        {
          patch_id;
          session_uuid = "uuid";
          prompt = "prompt";
          argv = [ "claude"; "--json" ];
          env_redacted = [| "ANTHROPIC_API_KEY=<redacted>" |];
        };
      Spawn_finalized
        {
          patch_id;
          session_uuid = "uuid";
          meta = `Assoc [ ("schema_version", `Int 1) ];
        };
      Free_form { patch_id = None; level = Warning; message = "message" };
    ]
  in
  List.for_all events ~f:(fun event ->
      match Event.t_of_yojson (Event.yojson_of_t event) with
      | event' ->
          phys_equal event event'
          || Yojson.Safe.equal (Event.yojson_of_t event)
               (Event.yojson_of_t event')
      | exception _ -> false)

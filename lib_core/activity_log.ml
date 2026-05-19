open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Transition_entry = struct
  type t = {
    timestamp : float;
    patch_id : Types.Patch_id.t;
    from_status : Display_status.t;
    to_status : Display_status.t;
    action : string;
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  let create ~timestamp ~patch_id ~from_status ~to_status ~action =
    { timestamp; patch_id; from_status; to_status; action }
end

module Event = struct
  type t = {
    timestamp : float;
    patch_id : Types.Patch_id.t option;
    message : string;
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  let create ~timestamp ?patch_id message = { timestamp; patch_id; message }
end

module Stream_entry = struct
  type kind =
    | Tool_use of string * string
    | Text_chunk of string
    | Finished of string
    | Stream_error of string
  [@@deriving show, eq, sexp_of, compare]

  type t = { timestamp : float; patch_id : Types.Patch_id.t; kind : kind }
  [@@deriving show, eq, sexp_of, compare]

  let create ~timestamp ~patch_id ~kind = { timestamp; patch_id; kind }
end

type t = {
  transitions : Transition_entry.t list;
  mutable events : Event.t list;
  mutable stream_entries : Stream_entry.t list;
}
[@@deriving show, eq, sexp_of, compare]

let empty = { transitions = []; events = []; stream_entries = [] }
let add_transition t entry = { t with transitions = entry :: t.transitions }
let add_event t event = { t with events = event :: t.events }

let add_stream_entry t entry =
  { t with stream_entries = entry :: t.stream_entries }

let prepend_event_in_place t event = t.events <- event :: t.events

let prepend_stream_entry_in_place t entry =
  t.stream_entries <- entry :: t.stream_entries

let stream_kind_of_raw ~channel raw =
  match Yojson.Safe.from_string raw with
  | `Assoc fields -> (
      let find_string name =
        List.find_map fields ~f:(function
          | key, `String value when String.equal key name -> Some value
          | _ -> None)
      in
      match find_string "activity_log_kind" with
      | Some "tool_use" ->
          Stream_entry.Tool_use
            ( Option.value (find_string "name") ~default:"",
              Option.value (find_string "input") ~default:"" )
      | Some "text_chunk" ->
          Stream_entry.Text_chunk
            (Option.value (find_string "text") ~default:"")
      | Some "finished" ->
          Stream_entry.Finished
            (Option.value (find_string "reason") ~default:"")
      | Some "stream_error" ->
          Stream_entry.Stream_error
            (Option.value (find_string "message") ~default:"")
      | _ -> (
          match channel with
          | `Stdout -> Stream_entry.Text_chunk raw
          | `Stderr -> Stream_entry.Stream_error raw))
  | _ -> (
      match channel with
      | `Stdout -> Stream_entry.Text_chunk raw
      | `Stderr -> Stream_entry.Stream_error raw)
  | exception _ -> (
      match channel with
      | `Stdout -> Stream_entry.Text_chunk raw
      | `Stderr -> Stream_entry.Stream_error raw)

let recent_transitions t ~limit = List.take t.transitions limit
let recent_events t ~limit = List.take t.events limit
let recent_stream_entries t ~limit = List.take t.stream_entries limit

let trim t ~max =
  {
    transitions = List.take t.transitions max;
    events = List.take t.events max;
    stream_entries = List.take t.stream_entries max;
  }

let activity_log_sink ~log () =
  let consume = function
    | Telemetry.Event.Free_form { patch_id; message; _ } ->
        prepend_event_in_place log
          (Event.create ~timestamp:0. ?patch_id message)
    | Stream { patch_id; raw; channel; _ } ->
        let kind = stream_kind_of_raw ~channel raw in
        prepend_stream_entry_in_place log
          (Stream_entry.create ~timestamp:0. ~patch_id ~kind)
    | Poll _ | Action _ | Complete _ | Spawn_started _ | Spawn_finalized _ -> ()
  in
  {
    Telemetry.Sink.name = "activity_log";
    interested_in =
      (function
      | Telemetry.Event.Free_form _ | Telemetry.Event.Stream _ -> true
      | Poll _ | Action _ | Complete _ | Spawn_started _ | Spawn_finalized _ ->
          false);
    consume;
  }

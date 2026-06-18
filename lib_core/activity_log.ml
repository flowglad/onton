(* @archlint.module core
   @archlint.domain activity-log *)

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

(* A single row of the user-facing activity feed: either a status transition or
   a free-form event, carrying its own timestamp so the two streams can be
   interleaved chronologically. Stream entries are deliberately excluded — they
   are replay diagnostics, not feed rows (see [stream_kind_of_raw]). *)
module Merged_entry = struct
  type t = Transition of Transition_entry.t | Event of Event.t
  [@@deriving show, eq, sexp_of, compare]

  let timestamp = function
    | Transition t -> t.Transition_entry.timestamp
    | Event e -> e.Event.timestamp
end

type t = {
  transitions : Transition_entry.t list;
  events : Event.t list;
  stream_entries : Stream_entry.t list;
}
[@@deriving show, eq, sexp_of, compare]

let empty = { transitions = []; events = []; stream_entries = [] }
let add_transition t entry = { t with transitions = entry :: t.transitions }
let add_event t event = { t with events = event :: t.events }

let add_stream_entry t entry =
  { t with stream_entries = entry :: t.stream_entries }

let stream_kind_of_raw ~channel:_ raw =
  match Yojson.Safe.from_string raw with
  | exception _ -> None
  | `Assoc fields -> (
      let find_string name =
        List.find_map fields ~f:(function
          | key, `String value when String.equal key name -> Some value
          | _ -> None)
      in
      match find_string "activity_log_kind" with
      | Some "tool_use" ->
          Some
            (Stream_entry.Tool_use
               ( Option.value (find_string "name") ~default:"",
                 Option.value (find_string "input") ~default:"" ))
      | Some "text_chunk" ->
          Some
            (Stream_entry.Text_chunk
               (Option.value (find_string "text") ~default:""))
      | Some "finished" ->
          Some
            (Stream_entry.Finished
               (Option.value (find_string "reason") ~default:""))
      | Some "stream_error" ->
          Some
            (Stream_entry.Stream_error
               (Option.value (find_string "message") ~default:""))
      | _ -> None)
  | _ -> None

let recent_transitions t ~limit = List.take t.transitions limit
let recent_events t ~limit = List.take t.events limit
let recent_stream_entries t ~limit = List.take t.stream_entries limit

let merged_recent t ~limit =
  (* Merge *then* truncate. Truncating each source to [limit] before merging
     would hand the union a non-uniform density: a high-rate source (events)
     floods the recent window while a low-rate source (transitions) stretches
     far into the past, so the feed reads dense at the top and sparse at the
     bottom. Capping each source at [limit] first is still a sound upper bound —
     the [limit] newest of the union can draw at most [limit] from either
     source — so we keep only the [limit] newest of the merged, sorted result. *)
  let entries =
    List.map (recent_events t ~limit) ~f:(fun e -> Merged_entry.Event e)
    @ List.map (recent_transitions t ~limit) ~f:(fun tr ->
        Merged_entry.Transition tr)
  in
  List.take
    (List.stable_sort entries ~compare:(fun a b ->
         Float.descending (Merged_entry.timestamp a) (Merged_entry.timestamp b)))
    limit

let trim t ~max =
  {
    transitions = List.take t.transitions max;
    events = List.take t.events max;
    stream_entries = List.take t.stream_entries max;
  }

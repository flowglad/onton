open Base

module Transition_entry = struct
  type t = {
    timestamp : float;
    patch_id : Types.Patch_id.t;
    from_status : Tui.display_status;
    to_status : Tui.display_status;
    action : string;
  }
  [@@deriving show, eq, sexp_of, compare]

  let create ~timestamp ~patch_id ~from_status ~to_status ~action =
    { timestamp; patch_id; from_status; to_status; action }
end

module Event = struct
  type t = {
    timestamp : float;
    patch_id : Types.Patch_id.t option;
    message : string;
  }
  [@@deriving show, eq, sexp_of, compare]

  let create ~timestamp ?patch_id message = { timestamp; patch_id; message }
end

module Stream_event = struct
  type kind =
    | Text_chunk
    | Tool_use of string
    | Pr_detected of int
    | Process_start
    | Process_end of int
  [@@deriving show, eq, sexp_of, compare]

  type t = {
    timestamp : float;
    patch_id : Types.Patch_id.t;
    kind : kind;
    content : string;
  }
  [@@deriving show, eq, sexp_of, compare]

  let create ~timestamp ~patch_id ~kind content =
    { timestamp; patch_id; kind; content }
end

type t = {
  transitions : Transition_entry.t list;
  events : Event.t list;
  stream_events : Stream_event.t list;
}
[@@deriving show, eq, sexp_of, compare]

let empty = { transitions = []; events = []; stream_events = [] }
let add_transition t entry = { t with transitions = entry :: t.transitions }
let add_event t event = { t with events = event :: t.events }

let add_stream_event t stream_event =
  { t with stream_events = stream_event :: t.stream_events }

let recent_transitions t ~limit = List.take t.transitions limit
let recent_events t ~limit = List.take t.events limit
let recent_stream_events t ~limit = List.take t.stream_events limit

let recent_stream_events_for_patch t ~patch_id ~limit =
  List.take
    (List.filter t.stream_events ~f:(fun (se : Stream_event.t) ->
         Types.Patch_id.equal se.Stream_event.patch_id patch_id))
    limit

let trim t ~max =
  {
    transitions = List.take t.transitions max;
    events = List.take t.events max;
    stream_events = List.take t.stream_events max;
  }

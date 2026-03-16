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

module Stream_entry = struct
  type kind =
    | Tool_use of string
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
  events : Event.t list;
  stream_entries : Stream_entry.t list;
}
[@@deriving show, eq, sexp_of, compare]

let empty = { transitions = []; events = []; stream_entries = [] }
let add_transition t entry = { t with transitions = entry :: t.transitions }
let add_event t event = { t with events = event :: t.events }

let add_stream_entry t entry =
  { t with stream_entries = entry :: t.stream_entries }

let recent_transitions t ~limit = List.take t.transitions limit
let recent_events t ~limit = List.take t.events limit
let recent_stream_entries t ~limit = List.take t.stream_entries limit

let trim t ~max =
  {
    transitions = List.take t.transitions max;
    events = List.take t.events max;
    stream_entries = List.take t.stream_entries max;
  }

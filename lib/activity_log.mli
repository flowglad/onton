open Base

(** Activity log for tracking patch state transitions and system events.

    Transitions record status changes for individual patches. Events record
    general system activity (polling, errors, user actions). *)

module Transition_entry : sig
  type t = private {
    timestamp : float;
    patch_id : Types.Patch_id.t;
    from_status : Tui.display_status;
    to_status : Tui.display_status;
    action : string;
  }
  [@@deriving show, eq, sexp_of, compare]

  val create :
    timestamp:float ->
    patch_id:Types.Patch_id.t ->
    from_status:Tui.display_status ->
    to_status:Tui.display_status ->
    action:string ->
    t
end

module Event : sig
  type t = private {
    timestamp : float;
    patch_id : Types.Patch_id.t option;
    message : string;
  }
  [@@deriving show, eq, sexp_of, compare]

  val create : timestamp:float -> ?patch_id:Types.Patch_id.t -> string -> t
end

type t [@@deriving show, eq, sexp_of, compare]

val empty : t
(** An empty activity log with no transitions or events. *)

val add_transition : t -> Transition_entry.t -> t
(** Prepend a transition entry to the log. *)

val add_event : t -> Event.t -> t
(** Prepend an event to the log. *)

val recent_transitions : t -> limit:int -> Transition_entry.t list
(** Return the most recent [limit] transitions (newest first). Returns an empty
    list if [limit <= 0]. Returns all transitions if [limit] exceeds the number
    of entries. *)

val recent_events : t -> limit:int -> Event.t list
(** Return the most recent [limit] events (newest first). Returns an empty list
    if [limit <= 0]. Returns all events if [limit] exceeds the number of
    entries. *)

val trim : t -> max:int -> t
(** Truncate both transitions and events to at most [max] entries, keeping the
    most recent. Use periodically to bound memory in long-running sessions. *)

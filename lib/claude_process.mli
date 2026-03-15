open Base

(** Status of a Claude session. *)
type session_status = Idle | Busy | Failed
[@@deriving show, eq, sexp_of, compare]

(** State of a Claude process for a patch.

    Invariant: sessions are never lost — once [has_session t] is true, all
    transitions preserve it. External code can pattern-match via the private
    type but must use smart constructors for transitions. *)
type t = private
  | No_session
  | Has_session of { id : Types.Session_id.t; status : session_status }
[@@deriving show, eq, sexp_of, compare]

val no_session : t
val start : Types.Session_id.t -> t
val has_session : t -> bool
val is_busy : t -> bool
val is_failed : t -> bool
val session_id : t -> Types.Session_id.t option
val mark_busy : t -> t
val mark_idle : t -> t
val mark_failed : t -> t
val restart : Types.Session_id.t -> t -> t

val check_session_preservation : before:t -> after:t -> bool
(** Check the "sessions are never lost" invariant between two states. *)

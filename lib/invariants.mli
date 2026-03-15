type violation = { invariant : string; details : string }
[@@deriving show, eq, sexp_of]

(** Check all spec invariants, returning a list of violations (empty = ok). *)
val check_invariants : State.t -> violation list

(** Check all spec invariants, raising [Failure] if any are violated. *)
val check_invariants_exn : State.t -> unit

(** Check invariants only if [ONTON_CHECK_INVARIANTS=1] or [=true]. *)
val maybe_check_invariants_exn : State.t -> unit

(** Whether runtime invariant checking is enabled via environment variable. *)
val should_check_at_runtime : unit -> bool

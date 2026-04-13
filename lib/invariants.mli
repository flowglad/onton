type violation = { invariant : string; details : string }
[@@deriving show, eq, sexp_of]

val check_invariants : State.t -> violation list
(** Check all spec invariants, returning a list of violations (empty = ok).

    Pure function intended for use in property tests and ad-hoc inspection of
    persisted snapshots. There is no production tick that calls this — runtime
    crash-on-violation was deliberately not wired up; see the module docs. *)

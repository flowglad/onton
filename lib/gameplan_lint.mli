(** Aggregate all structural and stylistic issues in a parsed gameplan.

    Complements {!Gameplan_parser.validate}, which fails fast on the first
    structural error encountered during parsing. This module accepts an
    already-parsed gameplan and returns every issue it can find, classified by
    severity, so that gameplan authors get all the feedback in one pass. *)

module Severity : sig
  type t = Error | Warning [@@deriving show, eq, compare, sexp_of]

  val to_label : t -> string
  (** Human-readable label, e.g. ["error"], ["warning"]. *)
end

module Issue : sig
  type t = {
    severity : Severity.t;
    patch_id : Types.Patch_id.t option;
        (** [None] for gameplan-level issues. *)
    message : string;
  }
  [@@deriving show, eq, compare, sexp_of]
end

val known_classifications : string list
(** Recognised values for {!Types.Patch.classification}. Empty classifications
    produce no issue (older gameplans often omit this field). *)

val lint : Types.Gameplan.t -> Issue.t list
(** Inspect [gameplan] and return all detected issues, ordered by severity
    (errors first) then by patch id then by message. Pure: no IO, no exceptions.
*)

val has_errors : Issue.t list -> bool
(** [true] iff at least one issue has severity {!Severity.Error}. *)

val format_issues : Issue.t list -> string
(** Render issues for terminal output. Returns the empty string when the list is
    empty. The final character is a newline iff the result is non-empty. *)

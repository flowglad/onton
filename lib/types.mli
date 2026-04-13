open Base

module Patch_id : sig
  type t = private string
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  include Comparator.S with type t := t

  val of_string : string -> t
  val to_string : t -> string
end

module Pr_number : sig
  type t = private int
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  val of_int : int -> t
  val to_int : t -> int
end

module Session_id : sig
  type t = private string
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  val of_string : string -> t
  val to_string : t -> string
end

module Message_id : sig
  type t = private string
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  include Comparator.S with type t := t

  val of_string : string -> t
  val to_string : t -> string
end

module Branch : sig
  type t = private string
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  val of_string : string -> t
  val to_string : t -> string
end

module Operation_kind : sig
  type t =
    | Rebase
    | Human
    | Merge_conflict
    | Ci
    | Review_comments
    | Implementation_notes
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  val to_label : t -> string
  (** Human-readable label for log messages (e.g. ["ci"], ["review-comments"]).
  *)
end

(** Wrapper for GitHub comment [databaseId]. Synthetic IDs are always negative;
    real GitHub IDs are always positive. When restoring persisted comments, call
    {!seed_synthetic_counter} before any call to {!next_synthetic} so newly
    minted IDs stay below the existing minimum. *)
module Comment_id : sig
  type t = private int
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  include Comparator.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
  val next_synthetic : unit -> t
  val seed_synthetic_counter : t list -> unit
end

module Comment : sig
  type t = {
    id : Comment_id.t;
    thread_id : string option;
    body : string;
    path : string option;
    line : int option;
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  include Comparator.S with type t := t
end

module Patch : sig
  type t = {
    id : Patch_id.t;
    title : string;
    description : string;
    branch : Branch.t;
    dependencies : Patch_id.t list;
    spec : string; [@yojson.default ""]
    acceptance_criteria : string list; [@yojson.default []]
    files : string list; [@yojson.default []]
    classification : string; [@yojson.default ""]
    changes : string list; [@yojson.default []]
    test_stubs_introduced : string list; [@yojson.default []]
    test_stubs_implemented : string list; [@yojson.default []]
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Ci_check : sig
  type t = {
    name : string;
    conclusion : string;
    details_url : string option;
    description : string option;
    started_at : string option;
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  val failure_conclusions : string list
  (** Conclusions that represent an actionable CI failure the agent can fix.
      Excludes ["cancelled"] — a cancelled check typically means the run was
      superseded by a newer commit or manually cancelled, not a real failure. *)

  val success_conclusions : string list
  (** Conclusions that represent a terminal successful outcome. *)

  val is_failure : t -> bool
  val is_success : t -> bool
end

module Pr_url : sig
  type t = private string
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  include Comparator.S with type t := t

  val of_string : string -> t
  val to_string : t -> string
end

module Stop_reason : sig
  type t =
    | End_turn
    | Tool_use
    | Max_tokens
    | Stop_sequence
    | Pause_turn
    | Refusal
    | Model_context_window_exceeded
  [@@deriving show, eq, sexp_of, compare]

  val of_string : string -> t option

  val to_display : t -> string
  (** Human-readable label for use in user-facing strings (e.g. activity log).
      Distinct from [show], which produces OCaml variant names. *)
end

(** Events from Claude Code's NDJSON stdout (--output-format stream-json), not
    the raw Anthropic streaming API. *)
module Stream_event : sig
  type t =
    | Text_delta of string
    | Tool_use of { name : string; input : string }
    | Final_result of { text : string; stop_reason : Stop_reason.t }
    | Error of string
    | Session_init of { session_id : string }
  [@@deriving show, eq, sexp_of, compare]
end

module Gameplan : sig
  type t = {
    project_name : string;
    problem_statement : string;
    solution_summary : string;
    final_state_spec : string; [@yojson.default ""]
    patches : Patch.t list;
    current_state_analysis : string; [@yojson.default ""]
    explicit_opinions : string; [@yojson.default ""]
    acceptance_criteria : string list; [@yojson.default []]
    open_questions : string list; [@yojson.default []]
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

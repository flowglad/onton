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
    | Pr_body
    | Findings
        (** Review-service findings — deliveries from a backend in
            {!Review_backend} that mints its own finding store separate from
            GitHub review threads. The agent receives one [Findings_payload] per
            session and POSTs resolve verbs back to the originating backend
            after the session completes. *)
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  val t_of_yojson_compat : Yojson.Safe.t -> t
  (** Migration-aware deserializer that accepts removed constructors (e.g.
      [Implementation_notes] -> [Pr_body]). *)

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
    commit_sha : string option; [@yojson.default None]
    original_commit_sha : string option; [@yojson.default None]
    outdated : bool; [@yojson.default false]
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  include Comparator.S with type t := t
end

module Precedent : sig
  type t = {
    kind : string;
        (** One of "library", "algorithm", "pattern", "paper", "rfc-spec",
            "documentation", "blog-post". Not validated at parse time; the
            gameplan author chooses the most specific kind. *)
    name : string;
        (** Most-specific identifier (e.g. "Bindlib", "Tarjan 1972 strongly
            connected components"). *)
    url : string option; [@yojson.default None]
        (** Canonical link to the reference. [None] when no stable URL exists.
        *)
    why_applicable : string; [@yojson.default ""]
        (** 1-2 sentences explaining how this precedent informs THIS patch and
            the concrete shape it imposes (API call, algorithm step, invariant).
            Goes verbatim into the patch prompt. *)
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
  (** Established prior art the implementing agent should adopt for a given
      patch rather than rolling its own. Surfaced to the patch prompt so the
      agent can prefer proven libraries / algorithms / patterns. *)
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
    complexity : int option; [@yojson.default None]
        (** Conservative 1/2/3 estimate from the gameplan author. Used by
            [--model auto] to pick a stronger model for harder patches. [None]
            for legacy gameplans missing the field; orchestrators should treat
            that as the highest available tier (be conservative). *)
    precedents : Precedent.t list; [@yojson.default []]
        (** Established prior art for this patch (libraries, algorithms,
            patterns, papers). Empty list when the patch is bespoke project glue
            with no relevant precedent. *)
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
    id : int option;
        (** GitHub CheckRun [databaseId] when available, [None] for legacy
            StatusContext entries (which expose no stable numeric ID). Used as
            the dedup key for CI feedback delivery so a single failing run is
            only delivered once. *)
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
    | Turn_started
        (** Backend emitted an event indicating it accepted/started processing
            the current turn. This is stronger than session initialization:
            session IDs can be created or resumed without proving that the
            prompt was processed. *)
    | Text_delta of string
    | Tool_use of {
        name : string;
        input : string;
        status : string option;
            (** Tool-call lifecycle status from the backend stream, when
                exposed. [Some "completed"] means the tool returned a result;
                any other [Some _] (e.g. [Some "pending"], [Some "running"],
                backend-specific error states) indicates the tool was announced
                but did not finish normally. [None] for backends that do not
                surface per-tool state (all non-OpenCode backends today). *)
      }
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

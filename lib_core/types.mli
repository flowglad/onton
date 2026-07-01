(* @archlint.module interface
   @archlint.domain types *)

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
    last_reply_author : string option; [@yojson.default None]
        (** Login of the last reply's author; [None] for opener-only threads.
            When it equals the viewer login, the thread's last word is onton's
            own posted reply — position disambiguates authorship even when a
            human co-reviews from the same account, because co-reviewers open
            threads and never correspond mid-thread. A re-delivered thread in
            that state only needs its resolve retried, not a duplicate reply. *)
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

module Context_resource : sig
  type t = {
    id : string;
        (** Stable non-empty identifier referenced by patches'
            [required_context] arrays. *)
    kind : string;
        (** One of ["existing-implementation"], ["contract"], ["predecessor"],
            ["reference-doc"], ["test-or-static-check"], or
            ["external-reference"]. *)
    paths : string list; [@yojson.default []]
        (** Repo-relative paths or external references the implementing agent
            must read before editing. *)
    why : string; [@yojson.default ""]
        (** Why this resource is authoritative for consuming patches. *)
    consumed_by : Patch_id.t list; [@yojson.default []]
        (** Patches that must receive this resource in their prompt. Must agree
            with each consuming patch's [required_context]. *)
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
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
    required_context : string list; [@yojson.default []]
        (** IDs from [Gameplan.context_resources] that this patch must read
            before editing. Defaults to [[]] for legacy gameplans. *)
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
  val merge_queue_failure : unit -> t
  val is_merge_queue_failure : t -> bool
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
    | Session_init of {
        session_id : string;
        api_key_source : string option;
        model : string option;
        claude_code_version : string option;
        permission_mode : string option;
      }
  [@@deriving show, eq, sexp_of, compare]
end

module Functional_change : sig
  type t = {
    id : string;
        (** Stable identifier (e.g. ["FC-1"]) unique within the gameplan. *)
    description : string;
        (** Single observable outcome ("Merged patches are skipped"); not an
            implementation step. Goes verbatim into the owning patch's prompt.
        *)
    owned_by : Patch_id.t;
        (** The single patch responsible for delivering this change. *)
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
  (** A functional/behavioural change the gameplan introduces. The
      [functional_changes] array on [Gameplan.t] is exhaustive, and the mapping
      via [owned_by] is total and single-valued: every change has exactly one
      owning patch, no shared ownership. Surfaced to the patch agent's prompt so
      the agent knows which user-visible behaviors it must deliver and cannot
      defer them to a sibling patch. *)
end

module Trace_node : sig
  type t = {
    file : string;  (** Repo-relative path of this node's file. *)
    symbol : string option; [@yojson.default None]
        (** The function, component, or export at this node, if applicable. *)
    status : string;
        (** ["existing"] (present on disk at HEAD) or ["created"] (introduced by
            a patch in this gameplan; not expected on disk). *)
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Reachability_trace : sig
  type t = {
    observable : string;
        (** The observable behavior this trace grounds, phrased as the
            acceptance criterion does. *)
    traces_to : string option; [@yojson.default None]
        (** [functionalChange] id this observable corresponds to, or [None]. If
            set, its owner must equal [owned_by]. *)
    owned_by : Patch_id.t;
        (** The patch that delivers the observable. It must edit at least one
            node on [path]. *)
    path : Trace_node.t list; [@yojson.default []]
        (** Ordered entry-point -> leaf. First node is the entry point that
            emits the observable; last node is the leaf that does the work. *)
    test_path : Trace_node.t list option; [@yojson.default None]
        (** Optional parallel trace through the test seam (production adapters
            swapped for in-memory/test ones). *)
    runtime_reachability_note : string option; [@yojson.default None]
        (** For routable/framework-dispatched surfaces: how runtime reachability
            was confirmed (no redirect/rewrite/middleware shadows the path). *)
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
  (** The live entry->leaf path that produces one observable. Surfaced to the
      [owned_by] patch's prompt so the agent edits a symbol on the live path,
      not a plausibly-named one off it. *)
end

module Gameplan : sig
  type t = {
    project_name : string;
    repo_owner : string; [@yojson.default ""]
        (** Repository owner on the git forge (user, org, group). A gameplan
            applies to exactly one repository; cross-repo work requires multiple
            gameplans. Forge-specific validation (e.g. GitHub's handle format)
            is performed by the forge backend module — this field is only
            structurally required to be non-empty. Defaults to [""] for legacy
            gameplans that predate the field; callers consuming this for a fresh
            run must reject empties, while the resume path backfills from stored
            config. *)
    repo_name : string; [@yojson.default ""]
        (** Repository name on the git forge (paired with [repo_owner]). See
            [repo_owner] for the empty-default rationale. *)
    problem_statement : string;
    solution_summary : string;
    final_state_spec : string; [@yojson.default ""]
    patches : Patch.t list;
    functional_changes : Functional_change.t list; [@yojson.default []]
        (** Exhaustive enumeration of the functional/behavioural changes the
            gameplan introduces, each assigned to exactly one owning patch.
            Defaults to [[]] for legacy gameplans that predate the field — in
            that case no per-patch change list is surfaced. *)
    context_resources : Context_resource.t list; [@yojson.default []]
        (** Authoritative existing code, contracts, docs, tests, or external
            references that patch agents must consult before editing. Defaults
            to [[]] for legacy gameplans. *)
    reachability_traces : Reachability_trace.t list; [@yojson.default []]
        (** One entry per observable the gameplan promises, tracing the live
            path from entry point to leaf. Surfaced per-patch to the owning
            patch's prompt. Defaults to [[]] for legacy gameplans and pure
            INFRA/refactor gameplans with no runtime observable. *)
    current_state_analysis : string; [@yojson.default ""]
    explicit_opinions : string; [@yojson.default ""]
    acceptance_criteria : string list; [@yojson.default []]
    open_questions : string list; [@yojson.default []]
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  val slugify : string -> string
  (** Canonical project-name → branch-prefix slug. Shared with [Gameplan_parser]
      so runtime-added and parsed patches derive identical branch names. *)

  val branch_of_id : t -> Patch_id.t -> Branch.t
  (** Branch for a patch id, matching the parser's [{slug}/patch-{id}]
      convention. *)

  val add_patch :
    t ->
    title:string ->
    description:string ->
    dependencies:Patch_id.t list ->
    (t * Patch.t, string) Result.t
  (** Append a runtime patch carrying only user intent (title, description,
      dependencies). The id is auto-generated in an [addN] namespace that cannot
      collide with numeric ad-hoc PR ids; the branch follows {!branch_of_id};
      functional-changes/context/reachability are left empty. Returns the
      updated gameplan and the created patch, or [Error msg] when a dependency
      id does not name an existing patch. Dependencies are deduped preserving
      order. Cannot introduce a cycle (the new patch is a graph sink). *)
end

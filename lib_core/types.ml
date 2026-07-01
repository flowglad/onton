(* @archlint.module state
   @archlint.domain anchor *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Patch_id = struct
  module T = struct
    type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
  end

  include T
  include Comparator.Make (T)

  let of_string s = s
  let to_string t = t
end

module Pr_number = struct
  type t = int [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  let of_int n = n
  let to_int t = t
end

module Session_id = struct
  type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  let of_string s = s
  let to_string t = t
end

module Message_id = struct
  module T = struct
    type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
  end

  include T
  include Comparator.Make (T)

  let of_string s = s
  let to_string t = t
end

module Branch = struct
  type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  let of_string s = s
  let to_string t = t
end

module Operation_kind = struct
  type t =
    | Rebase
    | Human
    | Merge_conflict
    | Ci
    | Review_comments
    | Pr_body
    | Findings
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  (** Migration-aware deserializer: maps removed constructors to their
      replacements so persisted state from older versions can still load. *)
  let t_of_yojson_compat json =
    match json with
    | `List [ `String "Implementation_notes" ] | `String "Implementation_notes"
      ->
        Pr_body
    | _ -> t_of_yojson json

  let to_label = function
    | Rebase -> "rebase"
    | Human -> "human"
    | Merge_conflict -> "merge-conflict"
    | Ci -> "ci"
    | Review_comments -> "review-comments"
    | Pr_body -> "pr-body"
    | Findings -> "findings"
end

module Comment_id = struct
  module T = struct
    type t = int [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
  end

  include T
  include Comparator.Make (T)

  let of_int n = n
  let to_int t = t
  let counter = Atomic.make 0

  let next_synthetic () =
    (* Atomically claim the next synthetic ID. fetch_and_add returns the old
       counter value and decrements it, so counter always equals the last issued
       ID after each call. Subtract 1 from the old value to get the new value;
       this ensures the first call returns -1 (counter: 0 → -1, result: 0-1=-1)
       and 0 is never issued. seed_synthetic_counter relies on this: seeding to
       min_id causes the next call to return min_id-1, safely below all existing
       IDs. *)
    Atomic.fetch_and_add counter (-1) - 1

  let seed_synthetic_counter ids =
    let min_id =
      List.fold ids ~init:0 ~f:(fun acc id -> Int.min acc (to_int id))
    in
    let rec try_seed () =
      let current = Atomic.get counter in
      if min_id < current then
        if not (Atomic.compare_and_set counter current min_id) then try_seed ()
    in
    try_seed ()
end

module Comment = struct
  module T = struct
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
              own posted reply (co-reviewers open threads, they don't correspond
              mid-thread), so a re-delivery only needs the resolve retried — not
              a duplicate reply. *)
    }
    [@@deriving show, eq, sexp_of, compare, yojson]
  end

  include T
  include Comparator.Make (T)
end

module Precedent = struct
  type t = {
    kind : string;
    name : string;
    url : string option; [@yojson.default None]
    why_applicable : string; [@yojson.default ""]
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
  (** Established prior art (library, algorithm, pattern, paper, RFC, doc, blog
      post) that a patch should adopt. Surfaced to the implementing agent in the
      patch prompt so it can prefer proven techniques over rolling its own. *)
end

module Context_resource = struct
  type t = {
    id : string;
    kind : string;
    paths : string list; [@yojson.default []]
    why : string; [@yojson.default ""]
    consumed_by : Patch_id.t list; [@yojson.default []]
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Patch = struct
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
    precedents : Precedent.t list; [@yojson.default []]
    required_context : string list; [@yojson.default []]
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Ci_check = struct
  type t = {
    name : string;
    conclusion : string;
    details_url : string option;
    description : string option;
    started_at : string option;
    id : int option; [@yojson.default None]
        (** GitHub CheckRun [databaseId] when available, [None] for legacy
            StatusContext entries (which have no stable numeric ID). Used as the
            dedup key for CI feedback delivery so a single failing run is only
            delivered once even if [generation] bumps for other reasons. *)
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  (** Conclusions that represent an actionable CI failure the agent can fix.
      Notably excludes ["cancelled"] — a cancelled check typically means the run
      was superseded by a newer commit or manually cancelled, not that anything
      actually failed. *)
  let failure_conclusions =
    [ "failure"; "error"; "action_required"; "timed_out"; "startup_failure" ]

  (** Conclusions that represent a terminal successful outcome. *)
  let success_conclusions = [ "success"; "skipped"; "neutral" ]

  let is_failure (c : t) =
    List.mem failure_conclusions c.conclusion ~equal:String.equal

  let is_success (c : t) =
    List.mem success_conclusions c.conclusion ~equal:String.equal

  let merge_queue_failure_name = "GitHub merge queue"

  let merge_queue_failure () =
    {
      name = merge_queue_failure_name;
      conclusion = "failure";
      details_url = None;
      description =
        Some
          "GitHub reported a merge queue failure for this PR. It may have been \
           removed after queue checks failed, or marked unmergeable while \
           still in the queue.";
      started_at = None;
      id = None;
    }

  let is_merge_queue_failure (c : t) =
    String.equal c.name merge_queue_failure_name && is_failure c
end

module Pr_url = struct
  module T = struct
    type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
  end

  include T
  include Comparator.Make (T)

  let of_string s = s
  let to_string t = t
end

module Stop_reason = struct
  type t =
    | End_turn
    | Tool_use
    | Max_tokens
    | Stop_sequence
    | Pause_turn
    | Refusal
    | Model_context_window_exceeded
  [@@deriving show, eq, sexp_of, compare, yojson]

  let of_string = function
    | "end_turn" -> Some End_turn
    | "tool_use" -> Some Tool_use
    | "max_tokens" -> Some Max_tokens
    | "stop_sequence" -> Some Stop_sequence
    | "pause_turn" -> Some Pause_turn
    | "refusal" -> Some Refusal
    | "model_context_window_exceeded" -> Some Model_context_window_exceeded
    | _ -> None

  let to_display = function
    | End_turn -> "ended turn"
    | Tool_use -> "awaiting tool"
    | Max_tokens -> "max tokens"
    | Stop_sequence -> "stop sequence"
    | Pause_turn -> "paused"
    | Refusal -> "refused"
    | Model_context_window_exceeded -> "context window exceeded"
end

(* Models events from Claude Code's NDJSON stdout
   (--output-format stream-json), not the raw Anthropic streaming API. *)
module Stream_event = struct
  type t =
    | Turn_started
    | Text_delta of string
    | Tool_use of { name : string; input : string; status : string option }
    | Final_result of { text : string; stop_reason : Stop_reason.t }
    | Error of string
    | Session_init of {
        session_id : string;
        api_key_source : string option; [@yojson.default None]
        model : string option; [@yojson.default None]
        claude_code_version : string option; [@yojson.default None]
        permission_mode : string option; [@yojson.default None]
      }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Functional_change = struct
  type t = { id : string; description : string; owned_by : Patch_id.t }
  [@@deriving show, eq, sexp_of, compare, yojson]
  (** A functional/behavioural change the gameplan introduces. The
      [functional_changes] array on [Gameplan.t] is exhaustive, and the mapping
      via [owned_by] is total and single-valued: every change has exactly one
      owning patch. Surfaced to the patch agent's prompt so the agent knows
      which user-visible behaviors it must deliver. *)
end

module Trace_node = struct
  type t = {
    file : string;
    symbol : string option; [@yojson.default None]
    status : string;
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
  (** One node on a reachability trace. [status] is ["existing"] (present on
      disk at HEAD) or ["created"] (introduced by a patch in this gameplan). *)
end

module Reachability_trace = struct
  type t = {
    observable : string;
    traces_to : string option; [@yojson.default None]
    owned_by : Patch_id.t;
    path : Trace_node.t list; [@yojson.default []]
    test_path : Trace_node.t list option; [@yojson.default None]
    runtime_reachability_note : string option; [@yojson.default None]
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
  (** The live entry->leaf path that produces one observable. [path] is ordered
      from the entry point that emits the observable to the leaf that does the
      work; the [owned_by] patch must edit at least one node on it. Surfaced to
      that patch's prompt so the agent edits a symbol on the live path rather
      than a plausibly-named one off it. *)
end

module Gameplan = struct
  type t = {
    project_name : string;
    repo_owner : string; [@yojson.default ""]
    repo_name : string; [@yojson.default ""]
    problem_statement : string;
    solution_summary : string;
    final_state_spec : string; [@yojson.default ""]
    patches : Patch.t list;
    functional_changes : Functional_change.t list; [@yojson.default []]
    context_resources : Context_resource.t list; [@yojson.default []]
    reachability_traces : Reachability_trace.t list; [@yojson.default []]
        (** One entry per observable the gameplan promises, tracing the live
            path from entry point to leaf. Defaults to [[]] for legacy gameplans
            and pure INFRA/refactor gameplans with no runtime observable. *)
    current_state_analysis : string; [@yojson.default ""]
    explicit_opinions : string; [@yojson.default ""]
    acceptance_criteria : string list; [@yojson.default []]
    open_questions : string list; [@yojson.default []]
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  (* Canonical project-name → branch-prefix slug. Shared with
     [Gameplan_parser] (which aliases this) so the branch a runtime-added patch
     gets is byte-identical to the ones the parser derives at load time. *)
  let slugify name =
    String.lowercase name
    |> String.map ~f:(fun c ->
        if Char.is_alphanum c || Char.equal c '-' || Char.equal c '_' then c
        else '-')
    |> String.split ~on:'-'
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> String.concat ~sep:"-"

  (* Branch name for a patch id, matching the parser's
     [{slug}/patch-{id}] convention (gameplan_parser.ml). *)
  let branch_of_id (t : t) (id : Patch_id.t) : Branch.t =
    Branch.of_string (slugify t.project_name ^ "/patch-" ^ Patch_id.to_string id)

  (* Smallest [addN] id (N ≥ 1) not already used by a patch. Alphanumeric so it
     satisfies the parser's id invariant, and never a bare integer so it cannot
     collide with an ad-hoc PR id (which is [string_of_int pr_number]). *)
  let next_user_patch_id (t : t) : Patch_id.t =
    let used =
      List.map t.patches ~f:(fun p -> Patch_id.to_string p.Patch.id)
      |> Set.of_list (module String)
    in
    let rec pick n =
      let candidate = "add" ^ Int.to_string n in
      if Set.mem used candidate then pick (n + 1)
      else Patch_id.of_string candidate
    in
    pick 1

  (* Add a runtime patch carrying only user-supplied intent (title,
     description, dependencies). Functional-changes / context / reachability
     stay empty, so the patch agent is prompted with the gameplan context plus
     this description and nothing else. Returns the updated gameplan and the
     created patch, or a descriptive error when a dependency id is unknown.

     New patches can only point at pre-existing patches, so the dependency graph
     stays acyclic by construction. *)
  let add_patch (t : t) ~(title : string) ~(description : string)
      ~(dependencies : Patch_id.t list) : (t * Patch.t, string) Result.t =
    let existing =
      List.map t.patches ~f:(fun p -> p.Patch.id)
      |> Set.of_list (module Patch_id)
    in
    let unknown =
      List.filter dependencies ~f:(fun d -> not (Set.mem existing d))
    in
    match unknown with
    | _ :: _ ->
        Error
          (Printf.sprintf "Unknown dependency id(s): %s"
             (List.map unknown ~f:Patch_id.to_string |> String.concat ~sep:", "))
    | [] ->
        (* Dedup while preserving first-seen order. *)
        let dependencies =
          List.fold dependencies
            ~init:([], Set.empty (module Patch_id))
            ~f:(fun (acc, seen) d ->
              if Set.mem seen d then (acc, seen) else (d :: acc, Set.add seen d))
          |> fst |> List.rev
        in
        let id = next_user_patch_id t in
        let patch =
          {
            Patch.id;
            title;
            description;
            branch = branch_of_id t id;
            dependencies;
            spec = "";
            acceptance_criteria = [];
            files = [];
            classification = "";
            changes = [];
            test_stubs_introduced = [];
            test_stubs_implemented = [];
            complexity = None;
            precedents = [];
            required_context = [];
          }
        in
        Ok ({ t with patches = t.patches @ [ patch ] }, patch)
end

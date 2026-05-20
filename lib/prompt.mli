open Types

val substitute_variables : string -> (string * string) list -> string
(** [substitute_variables template vars] performs a single-pass scan of
    [template], replacing each [\{\{key\}\}] placeholder with the corresponding
    value from [vars]. Values are never re-scanned, so variable content cannot
    trigger further substitutions. Unknown placeholders are left as-is. *)

(** {1 Three-layer prompt structure}

    Every layered prompt is composed as

    {[
      render_gameplan_layer ^ render_patch_layer ^ render_turn_layer_<kind>
    ]}

    so prefix-cache hits accumulate at the layer boundaries:

    - {!render_gameplan_layer} is byte-identical across every layered prompt in
      a single gameplan run.
    - {!render_patch_layer} is byte-identical across every layered prompt for
      one patch agent during a period where [pr_number] and [base_branch] are
      stable.
    - The per-kind [render_turn_layer_*] helpers are strictly turn-dynamic.

    Project-level overrides are now per-layer: [prompts/gameplan.md],
    [prompts/patch.md], and [prompts/turn_<kind>.md] (e.g.
    [prompts/turn_ci.md]). Overriding one layer leaves the others' cache
    structure intact. *)

val render_gameplan_layer : project_name:string -> Gameplan.t -> string
(** Gameplan-stable prefix. Contains the project heading, problem statement,
    solution summary, optional final state spec / explicit opinions / current
    state analysis, and the patches list. Ends with a trailing blank line. *)

val render_patch_layer :
  project_name:string ->
  Patch.t ->
  ?pr_number:Pr_number.t ->
  ?functional_changes:Functional_change.t list ->
  ?context_resources:Context_resource.t list ->
  base_branch:string ->
  unit ->
  string
(** Patch-stable middle. Contains the patch heading, dependencies, base-branch
    note, description, the functional changes the patch owns (if any), changes,
    files, test stubs, specification (with Pantagruel guide), acceptance
    criteria, git identifiers, and PR instructions. Ends with a trailing blank
    line. [functional_changes] should contain only the entries from
    [Gameplan.t.functional_changes] whose [owned_by] equals [Patch.t.id].
    [context_resources] should contain only resources whose IDs appear in this
    patch's [required_context]. *)

val owned_functional_changes : Gameplan.t -> Patch.t -> Functional_change.t list
(** Returns the functional changes in the gameplan owned by this patch. Use this
    when constructing patch layers directly, so composed and manually layered
    prompts carry the same patch-stable content. *)

val required_context_resources :
  Gameplan.t -> Patch.t -> Context_resource.t list
(** Returns the authoritative context resources required by this patch. *)

val render_turn_layer_start : project_name:string -> string

val render_turn_layer_review :
  project_name:string ->
  ?pr_number:Pr_number.t ->
  ?current_head_sha:string ->
  Comment.t list ->
  string

val render_turn_layer_ci :
  project_name:string -> ?pr_number:Pr_number.t -> Ci_check.t list -> string

val render_turn_layer_ci_unknown :
  project_name:string -> ?pr_number:Pr_number.t -> unit -> string

val render_turn_layer_merge_conflict :
  project_name:string ->
  ?pr_number:Pr_number.t ->
  base_branch:string ->
  ?git_status:string ->
  ?git_diff:string ->
  ?conflict_info:Worktree.conflict_info ->
  unit ->
  string

(** {1 Composed prompts}

    Each public render below is the composition of the three layers. Follow-up
    renders accept the layer inputs as optional — when a caller has no
    gameplan-defined patch in scope (e.g. ad-hoc PRs), the layered prefix is
    omitted and only the turn layer is emitted. *)

val render_patch_prompt :
  project_name:string ->
  ?agents_md:string ->
  ?pr_number:Pr_number.t ->
  Patch.t ->
  Gameplan.t ->
  base_branch:string ->
  string
(** Start prompt: gameplan + patch + turn-start. The gameplan-stable prefix is
    byte-identical across patches in the same orchestrator run when using the
    built-in templates; project-level overrides are user-controlled and may not
    preserve this structure. *)

val render_pr_description :
  project_name:string -> Patch.t -> Gameplan.t -> string

(** Pure: choose between the agent-authored PR body artifact and a deterministic
    fallback (typically the gameplan-derived body). Returns [fallback] when
    [artifact] is [None] or contains only whitespace; returns the artifact
    contents otherwise. Used by the supervisor when composing the final PR body
    for the implementation-notes phase. *)

val render_spec_suffix : Types.Patch.t -> Types.Gameplan.t -> string
(** Pure: render the Gameplan Specification and Patch Specification sections.
    Returns the empty string when both specs are absent. Appended by the
    supervisor after the agent-authored PR body, so the agent cannot
    accidentally drop them. *)

val render_pr_body_prompt :
  project_name:string ->
  pr_number:Pr_number.t ->
  pr_body:string ->
  spec_suffix:string ->
  artifact_path:string ->
  string

val render_review_prompt :
  project_name:string ->
  ?agents_md:string ->
  ?pr_number:Pr_number.t ->
  ?current_head_sha:string ->
  ?patch:Patch.t ->
  ?gameplan:Gameplan.t ->
  ?base_branch:string ->
  Comment.t list ->
  string

val render_findings_prompt :
  project_name:string ->
  ?agents_md:string ->
  ?pr_number:Pr_number.t ->
  ?current_head_sha:string ->
  ?patch:Patch.t ->
  ?gameplan:Gameplan.t ->
  ?base_branch:string ->
  artifact_path:string ->
  Review_service.finding list ->
  string
(** Findings session prompt. Renders each finding with its severity, anchor
    (path:start_line-end_line), posting SHA, and body. Instructs the agent to
    address findings via code edits and, for any finding it cannot or should not
    fix, write a JSON list of [{id, reason}] objects to [artifact_path]
    ([findings_wontfix.json]). Findings not listed in the artifact are POSTed as
    [addressed] to the originating review backend after the session. *)

val render_ci_failure_prompt :
  project_name:string ->
  ?agents_md:string ->
  ?pr_number:Pr_number.t ->
  ?patch:Patch.t ->
  ?gameplan:Gameplan.t ->
  ?base_branch:string ->
  Ci_check.t list ->
  string

val render_ci_failure_unknown_prompt :
  project_name:string ->
  ?agents_md:string ->
  ?pr_number:Pr_number.t ->
  ?patch:Patch.t ->
  ?gameplan:Gameplan.t ->
  ?base_branch:string ->
  unit ->
  string

val render_merge_conflict_prompt :
  project_name:string ->
  ?agents_md:string ->
  ?pr_number:Pr_number.t ->
  ?patch:Patch.t ->
  ?gameplan:Gameplan.t ->
  base_branch:string ->
  ?git_status:string ->
  ?git_diff:string ->
  ?conflict_info:Worktree.conflict_info ->
  unit ->
  string
(** Render the merge-conflict prompt. When [~conflict_info] is provided, the
    output includes a "Recovery" section with the exact [git rebase --onto]
    command and the patch's unique commit list, so an agent that aborts the
    in-progress rebase can reconstruct it. *)

val render_human_message_prompt : project_name:string -> string list -> string

val render_base_branch_changed : old_base:string -> new_base:string -> string
(** One-time notification that the agent's base branch has changed. *)

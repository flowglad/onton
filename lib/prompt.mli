open Types

val substitute_variables : string -> (string * string) list -> string
(** [substitute_variables template vars] performs a single-pass scan of
    [template], replacing each [\{\{key\}\}] placeholder with the corresponding
    value from [vars]. Values are never re-scanned, so variable content cannot
    trigger further substitutions. Unknown placeholders are left as-is. *)

val render_patch_prompt :
  project_name:string ->
  ?pr_number:Pr_number.t ->
  Patch.t ->
  Gameplan.t ->
  base_branch:string ->
  string

val render_pr_description :
  project_name:string -> Patch.t -> Gameplan.t -> string

val resolve_pr_body_source : artifact:string option -> fallback:string -> string
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
  artifact_path:string ->
  string

val render_implementation_notes_prompt :
  project_name:string ->
  pr_number:Pr_number.t ->
  pr_description:string ->
  spec_suffix:string ->
  artifact_path:string ->
  string

val render_review_prompt :
  project_name:string -> ?pr_number:Pr_number.t -> Comment.t list -> string

val render_ci_failure_prompt :
  project_name:string -> ?pr_number:Pr_number.t -> Ci_check.t list -> string

val render_ci_failure_unknown_prompt :
  project_name:string -> ?pr_number:Pr_number.t -> unit -> string

val render_merge_conflict_prompt :
  project_name:string ->
  ?pr_number:Pr_number.t ->
  ?patch:Patch.t ->
  ?gameplan:Gameplan.t ->
  base_branch:string ->
  ?git_status:string ->
  ?git_diff:string ->
  unit ->
  string

val render_human_message_prompt : project_name:string -> string list -> string

val render_base_branch_changed : old_base:string -> new_base:string -> string
(** One-time notification that the agent's base branch has changed. *)

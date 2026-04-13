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

val render_implementation_notes_prompt :
  project_name:string -> pr_number:Pr_number.t -> pr_body:string -> string

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

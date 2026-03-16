open Types

val prompts_dir : string -> string
(** [prompts_dir project_name] returns the absolute path to the project's prompt
    override directory. *)

val substitute_variables : string -> (string * string) list -> string
(** [substitute_variables template vars] performs a single-pass scan of
    [template], replacing each [\{\{key\}\}] placeholder with the corresponding
    value from [vars]. Values are never re-scanned, so variable content cannot
    trigger further substitutions. Unknown placeholders are left as-is. *)

val load_override : project_name:string -> string -> string option
(** [load_override ~project_name name] attempts to load
    [<project_data_dir>/prompts/<name>.md]. Returns [None] if the file does not
    exist. *)

val render_with_override :
  project_name:string ->
  name:string ->
  vars:(string * string) list ->
  default:(unit -> string) ->
  string
(** [render_with_override ~project_name ~name ~vars ~default] loads the override
    template named [name] from the project's prompts directory, substitutes
    [vars], and returns the result. Falls back to [default ()] when no override
    file exists. *)

val render_patch_prompt :
  project_name:string -> Patch.t -> Gameplan.t -> base_branch:string -> string

val render_review_prompt : project_name:string -> Comment.t list -> string
val render_ci_failure_prompt : project_name:string -> Ci_check.t list -> string
val render_ci_failure_unknown_prompt : project_name:string -> string

val render_merge_conflict_prompt :
  project_name:string -> base_branch:string -> string

val render_human_message_prompt : project_name:string -> string list -> string

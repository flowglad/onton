open Types

val prompts_dir : string
(** Directory where prompt override templates are loaded from. *)

val substitute_variables : string -> (string * string) list -> string
(** [substitute_variables template vars] replaces all [{{key}}] placeholders in
    [template] with corresponding values from [vars]. Unknown placeholders are
    left as-is. *)

val load_override : string -> string option
(** [load_override name] attempts to load [.onton/prompts/<name>.md]. Returns
    [None] if the file does not exist. *)

val render_with_override :
  name:string ->
  vars:(string * string) list ->
  default:(unit -> string) ->
  string
(** [render_with_override ~name ~vars ~default] loads the override template
    named [name], substitutes [vars], and returns the result. Falls back to
    [default ()] when no override file exists. *)

val render_patch_prompt : Patch.t -> Gameplan.t -> base_branch:string -> string
val render_review_prompt : Comment.t list -> string
val render_ci_failure_prompt : Ci_check.t list -> string
val render_merge_conflict_prompt : base_branch:string -> string
val render_human_message_prompt : string list -> string

(* @archlint.module interface
   @archlint.domain adhoc-branch *)

(** Pure parsing and validation for ad-hoc change arguments. *)

type add_target =
  | Pull_request of Types.Pr_number.t
  | Remote_branch of Types.Branch.t
[@@deriving show, eq]

type operation = Add of add_target | Remove_pr of Types.Pr_number.t
[@@deriving show, eq]

val operation_supported :
  supports_pull_request_changes:bool ->
  supports_branch_changes:bool ->
  operation ->
  bool
(** Whether the forge's explicit change-identity capabilities admit an
    operation. Removals are always supported because they only unregister an
    already-known local change. *)

val validate_remote_branch : string -> (Types.Branch.t, string) Result.t
(** Validate a short remote branch name using git's ref-format constraints. *)

val parse_add_value : string -> (add_target, string) Result.t
(** Parse text entered after the TUI's add prompt. Positive integers denote PR
    numbers; other values denote remote branches. [branch:<name>] forces branch
    interpretation, including for numeric branch names. *)

val looks_like_operation : string -> bool
(** Whether a CLI token is an ad-hoc operation: [+<target>] or [-<number>]. *)

val parse_operation : string -> (operation, string) Result.t
(** Parse a CLI ad-hoc operation. *)

val branch_patch_id : change_id:Types.Pr_number.t -> Types.Patch_id.t
(** Stable, filesystem-safe patch id for a branch-backed change handle. *)

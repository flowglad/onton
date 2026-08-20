(* @archlint.module interface
   @archlint.domain sourcehut-target *)

(** Pure SourceHut repository target logic. *)

val validate_target : owner:string -> repo:string -> (unit, string) Result.t
val infer_owner_repo_from_url : string -> (string * string) option

val clone_url :
  scheme:Github_target.url_scheme -> owner:string -> repo:string -> string
(** SourceHut clone URL for the requested HTTPS or SSH transport. *)

val scheme_of_url : string -> Github_target.url_scheme option
(** Recognize SourceHut HTTPS and SSH remote URLs. *)

val branch_url : owner:string -> repo:string -> Types.Branch.t -> string
(** Browser URL for the complete branch identity. *)

type registry

val empty_registry : registry

val register_change :
  registry ->
  preferred_id:Types.Pr_number.t option ->
  branch:Types.Branch.t ->
  base:Types.Branch.t ->
  (registry * Types.Pr_number.t, string) Result.t
(** Register [branch] without aliasing another branch. A free [preferred_id] is
    retained for snapshot compatibility; a colliding persisted id is rejected so
    callers cannot continue with a stale alias. New-id collisions are resolved
    by probing for a free local id. Re-registering the same branch preserves its
    id and updates its base. *)

val restore_changes :
  (Types.Pr_number.t option * Types.Branch.t * Types.Branch.t) list ->
  (registry, string) Result.t
(** Restore a complete registry from persisted changes. Duplicate branches and
    colliding persisted ids are rejected. Persisted ids are reserved before ids
    are generated for changes without one, so input order cannot manufacture a
    collision with a later snapshot identity. *)

val find_change :
  registry -> Types.Pr_number.t -> (Types.Branch.t * Types.Branch.t) option
(** Resolve a local change id to exactly the registered head/base pair. *)

val find_branch :
  registry -> Types.Branch.t -> (Types.Pr_number.t * Types.Branch.t) option
(** Resolve a full branch identity to its local id and base. *)

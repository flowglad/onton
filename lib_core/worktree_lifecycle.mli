(* @archlint.module interface
   @archlint.domain worktree-lifecycle *)

open Base

type backend = Git | Simgit [@@deriving show, eq, sexp_of, compare]

type config = private { backend : backend; executable : string option }
[@@deriving show, eq, sexp_of, compare]

(** [executable = None] selects automatic simgit discovery. Explicit selections
    are preserved through configuration persistence. *)

val git : config

val configure :
  backend:string -> executable:string option -> (config, string) Result.t

val to_json : config -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (config, string) Result.t

val parse_optional : Yojson.Safe.t option -> (config option, string) Result.t
(** Repository configuration boundary: explicit executables must be absolute.
    Omitted simgit executables use automatic discovery. CLI and persisted
    configuration use [configure] and [of_json] instead. *)

val backend_name : backend -> string

val resolve :
  backend:string option ->
  executable:string option ->
  stored_backend:string option ->
  stored_executable:string option ->
  repo:config option ->
  (config, string) Result.t
(** CLI overrides the persisted project setting, then the repository default.
    Executable settings only carry across matching backend kinds. *)

type phase = Preparing | Cleanup_pending | Ready [@@deriving eq]

val ownership_json :
  path:string -> branch:string -> phase:phase -> config -> Yojson.Safe.t

val parse_ownership :
  path:string ->
  branch:string ->
  Yojson.Safe.t ->
  (config * phase, string) Result.t
(** [Preparing] does not establish that provisioning has stopped.
    [Cleanup_pending] records that the child has been reaped and ref rollback
    completed, so ordinary simgit cleanup may be retried. Only [Ready] is
    adoptable. *)

type registration = {
  path : string;
  branch : string option;
  mode : string option;
}

val parse_simgit_list : string -> (registration list, string) Result.t
val parse_git_list : string -> (registration list, string) Result.t

val repair_error : ?code:int -> path:string -> string -> (unit, string) Result.t
(** A successful process alone does not prove that a particular overlay was
    repaired. *)

val doctor_identity : string -> (unit, string) Result.t
(** Validate the public doctor identity before invoking lifecycle commands. *)

val registration_backend : registration -> backend
(** A non-null mode belongs to simgit, including its git-checkout fallback.
    Unknown future modes remain owned by simgit. *)

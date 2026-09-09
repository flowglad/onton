(* @archlint.module interface
   @archlint.domain worktree-lifecycle *)

open Base

type backend = Git | Simgit [@@deriving show, eq, sexp_of, compare]

type config = private { backend : backend; executable : string }
[@@deriving show, eq, sexp_of, compare]

val git : config

val configure :
  backend:string -> executable:string option -> (config, string) Result.t

val to_json : config -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (config, string) Result.t
val parse_optional : Yojson.Safe.t option -> (config option, string) Result.t
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

type phase = Preparing | Ready [@@deriving eq]

val ownership_json :
  path:string -> branch:string -> phase:phase -> config -> Yojson.Safe.t

val parse_ownership :
  path:string ->
  branch:string ->
  Yojson.Safe.t ->
  (config * phase, string) Result.t
(** Creation intent and successful publication are distinct durable states. A
    preparing checkout is not adoptable after a failed or interrupted command.
*)

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

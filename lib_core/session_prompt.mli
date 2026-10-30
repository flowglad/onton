(* @archlint.module interface
   @archlint.domain session-driver *)

type response_transport = File_artifacts | Pi_tools

val response_transport : backend_name:string -> response_transport
(** Select the supervisor response contract for the actual backend. *)

val render_opt_out :
  response_transport:response_transport -> artifact_path:string -> string
(** Preserve the pre-commit offer while selecting its response mechanism. *)

type t
(** Context is delivered only when the backend starts a fresh session. *)

val create : context:string -> turn:string -> t

val render : resume_session:string option -> t -> string
(** Use the actual backend resume target, after fallback has been decided. *)

type recovery_context

type patch_revision =
  | Absent
  | Revision of string
  | Unavailable
      (** [Absent] is a successful remote lookup with no branch. [Unavailable]
          means the lookup failed and must never be presented as branch absence.
      *)

val create_recovery :
  requirements:string ->
  operation:string ->
  guidance:string list ->
  patch_revision:patch_revision ->
  base_revision:string ->
  recovery_context

val render_recovery : recovery_context -> string
(** Full fresh-conversation context. Text is data, never shell syntax. *)

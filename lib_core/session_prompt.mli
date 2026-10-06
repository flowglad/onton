(* @archlint.module interface
   @archlint.domain session-driver *)

type t
(** Context is delivered only when the backend starts a fresh session. *)

val create : context:string -> turn:string -> t

val render : resume_session:string option -> t -> string
(** Use the actual backend resume target, after fallback has been decided. *)

(* @archlint.module interface
   @archlint.domain project-lifecycle *)

val slug : string -> string
(** Legacy storage normalization, preserved for existing project directories. *)

val resolve :
  requested:string -> stored:string option -> (string, string) result
(** An alias selects storage, never a new recovery namespace. Preserve the exact
    stored name when it belongs to the requested directory; reject empty storage
    identities and mismatched stored names. New projects retain the requested
    name. *)

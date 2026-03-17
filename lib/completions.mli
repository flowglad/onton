type t = {
  display : string;
  full : string;
}

val complete : buffer:string -> patch_ids:string list -> t list
(** [complete ~buffer ~patch_ids] returns completions for the current
    text input buffer. Handles:
    - "N>" prefix: completes patch ID N from patch_ids
    - "+" prefix: no completions (PR numbers not predictable)
    - "w " prefix: no completions (paths not enumerable here)
    - bare prefix: offers known command prefixes *)

val accept_first : buffer:string -> completions:t list -> string
(** Replace buffer with the first completion's full text, or return
    buffer unchanged if completions is empty. *)

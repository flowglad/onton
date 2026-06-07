(* @archlint.module interface
   @archlint.domain anchor-history *)

(** A bounded log of {!Anchor.t} values recorded over a patch agent's lifetime,
    used by the rebase decision to fall back from a force-pushed-or-deleted
    newest anchor to an older one that is still reachable from the upstream.

    Invariants enforced by the smart constructors:
    - newest-first ordering;
    - no two entries share the same [(base, sha)] pair (push deduplicates);
    - length never exceeds {!cap} entries (older entries are dropped). *)

type t = private Anchor.t list
[@@deriving show, eq, ord, sexp_of, compare, hash]

val cap : int
(** Maximum number of anchors retained. Anchors older than the cap are dropped
    silently on push. *)

val empty : t

val push : t -> Anchor.t -> t
(** Add [a] as the newest entry. If an entry with the same [(base, sha)] already
    exists, it is removed before [a] is prepended (so [a] becomes the new
    "newest"). The list is truncated to {!cap} entries. *)

val newest : t -> Anchor.t option
val to_list : t -> Anchor.t list
val length : t -> int
val yojson_of_t : t -> Yojson.Safe.t

val t_of_yojson : Yojson.Safe.t -> t
(** Deserialize. Raises [Of_yojson_error] on structural failure or when any
    contained anchor fails validation. Truncates to {!cap} on load if a
    persisted snapshot ever exceeded it (forward-compatible). *)

val of_yojson_opt : Yojson.Safe.t -> t option

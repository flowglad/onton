(** A recorded fact: the patch agent's branch was, at some past moment, anchored
    against [base] at the resolved tip SHA [sha]. The orchestrator uses anchors
    to compute the safe [<upstream>] argument for
    [git rebase --onto <target> <upstream> HEAD] — replaying only the commits
    the patch contributed since the recorded anchor, never the dep's pre-squash
    commits.

    Anchors are value objects: either fully valid or absent. The smart
    constructor {!make} rejects any SHA that is not exactly 40 lowercase hex
    digits, so [Some sha] always carries a usable SHA. *)

type t = private {
  base : Types.Branch.t;
  sha : string;  (** 40-char lowercase hex, enforced by {!make}. *)
  observed_at_remote : bool;
      (** [true] iff [sha] came from resolving [origin/<base>] at recording
          time; [false] when [sha] came from a local ref or a synthetic
          observation. The decision logic prefers remote-observed anchors when
          choosing between candidates. *)
}
[@@deriving show, eq, ord, sexp_of, compare, hash]

val make :
  base:Types.Branch.t -> sha:string -> observed_at_remote:bool -> t option
(** Construct an anchor. Returns [None] if [sha] is not exactly 40 lowercase hex
    digits. Whitespace is stripped before validation. Pure and total. *)

val base : t -> Types.Branch.t
val sha : t -> string
val is_remote : t -> bool

val yojson_of_t : t -> Yojson.Safe.t
(** Serialize. Always succeeds. *)

val t_of_yojson : Yojson.Safe.t -> t
(** Deserialize. Raises [Of_yojson_error] when the JSON is well-typed but the
    SHA fails the {!make} validation, or when the structure is wrong. *)

val of_yojson_opt : Yojson.Safe.t -> t option
(** Non-raising variant; returns [None] on any parse or validation failure. *)

(* @archlint.module interface
   @archlint.domain branch-reconcile *)

type sequencer =
  | Rebase of {
      target : string;
      original : string;
      step : string;
      head_ref : string;
      merge_heads : string list; [@yojson.default []]
    }
  | Merge of { target : string }
  | Cherry_pick of { target : string }
  | None_active
[@@deriving eq, compare, sexp_of, yojson]

type t = private {
  valid : bool;
  branch : string option;
  head : string;
  staged : string list;
  unstaged : string list;
  untracked : string list;
  conflicts : string list;
  sequencer : sequencer;
}
[@@deriving eq, compare, sexp_of]

val of_porcelain :
  branch:string option -> head:string -> sequencer:sequencer -> string -> t
(** Parses porcelain v1 with [-z], including rename source records. The handler
    reports probe failures separately; an empty successful status is the only
    empty input that establishes a clean checkout. *)

val clean : t -> bool
val repair_ready : t -> bool
val sequencer_key : sequencer -> string
val progress_key : t -> string

val completed_integration :
  branch:string ->
  source:string ->
  target:string ->
  head:string ->
  reflog:string ->
  bool
(** Recognize a completed named-branch transition for the captured pair.
    Unterminated reflog tails and unrelated branch mutations are not receipts.
*)

type continuation = Blocked | Continue | Skip_empty_replay | Complete_merge
[@@deriving eq, compare, sexp_of]

val continuation : t -> continuation

val materialization_failure_is_unsafe : string -> bool
(** Persistent provenance mismatches must refuse checkout use. *)

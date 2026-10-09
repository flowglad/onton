(* @archlint.module interface
   @archlint.domain forge-observation *)

type request = { id : string; pr_number : Types.Pr_number.t }
[@@deriving show, eq, compare, sexp_of, yojson]

type t = { request : request; outcome : Poll_outcome.t } [@@deriving show, eq]

val validate : expected:request -> t -> (Poll_outcome.t, string) result
(** Bind provider output to its client request and returned PR identity. Open PR
    branch decisions require an identified head/base revision pair and base
    branch. Closed/merged lifecycle observations may outlive deleted refs, but
    still require matching PR identity. Transport errors retain their outcome.
    This validates scope, not freshness of provider caches; local-context and
    direct-Git checks remain separate. *)

type scope = {
  pr_number : Types.Pr_number.t option;
  branch : string;
  generation : int;
  head : string option;
  base_branch : string option;
}
[@@deriving eq, compare, sexp_of, yojson]

type ticket = private { sequence : int; request : request; scope : scope }
[@@deriving eq, compare, sexp_of, yojson]

type fact = private {
  ticket : ticket;
  head : string;
  base : string;
  base_branch : string;
  merge_state : Pr_state.merge_state;
  confirmed_head : string option;
  confirmed_base : string option; [@yojson.default None]
}
[@@deriving eq, compare, sexp_of, yojson]

type history [@@deriving eq, compare, sexp_of, yojson]

val empty_history : history
val pending_request : history -> ticket option
val latest_fact : history -> fact option
val conflict_fact : history -> fact option
val history_valid : history -> bool

val decode_history : Yojson.Safe.t -> (history, string) result
(** Checked, total checkpoint decoder. Raw generated JSON converters are for
    composition inside the owner decoder, which also calls [history_valid]. *)

val begin_request :
  history -> request:request -> scope:scope -> history * ticket option
(** Allocate a durable, increasing request ticket before network I/O. Invalid
    scope or exhausted sequence space leaves history unchanged. A new ticket
    supersedes pending work without erasing previously accepted evidence. *)

val accept :
  ?confirmed_base:string ->
  history ->
  ticket:ticket ->
  current:scope ->
  confirmed_head:string option ->
  t ->
  history * (Poll_outcome.t, string) result
(** Only the pending ticket in its unchanged local context can supply evidence.
    Duplicate, delayed, replaced-PR and wrong-branch results cannot overwrite
    it. Infrastructure errors and unknown mergeability never erase conflict
    evidence. Even a later mergeable report retains that evidence for the Git
    owner to resolve against an exact integration receipt. This records forge
    evidence; it does not itself prove ancestry or freshness of a provider's
    cache. *)

val fact_matches_scope : fact -> scope -> bool
(** Match PR, branch, head and base name. The fact also retains the exact base
    revision; callers still need revision-bound Git verification for mutation.
*)

val conflict_for_scope : history -> scope -> fact option
(** Retained conflict evidence for the current PR/branch/head/base context and
    the latest reported base revision. A different revision pair supersedes the
    old report; a contradictory report for the same pair does not resolve it. *)

val revisions_confirmed : fact -> bool
(** Both direct Git observations match the provider's exact head/base pair.
    Request identity alone and old checkpoints without base evidence do not
    establish this. The base observation belongs to the fetch remote; the head
    observation belongs to the bound publication destination. *)

val can_apply : history -> Poll_outcome.t -> bool
(** After successful ticket acceptance, open-PR updates require matching direct
    head/base proof. Terminal lifecycle observations do not require surviving
    refs. Infrastructure outcomes cannot supply branch updates. *)

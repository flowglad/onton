(* @archlint.module interface
   @archlint.domain branch-reconcile *)

type io = { git : string list -> int * string * string }

val remote_head :
  io -> branch:string -> (Branch_reconcile.Commit.t option, string) result
(** Observe the configured push destination directly without changing tracking
    refs. A missing ref is [Ok None]; a failed probe is [Error]. *)

val observe_checkout : io -> Git_observation.t
(** Raises on failed probes; failed inspection never establishes clean state. *)

val execute :
  io:io ->
  prefix:string ->
  branch:string ->
  operation:Branch_reconcile.operation ->
  Branch_reconcile.command ->
  Branch_reconcile.result
(** Execute one checkpointed command using pinned revisions. Recovery refs are
    append-only, scoped by the caller's project/branch prefix. *)

val make_io :
  process_mgr:_ Eio.Process.mgr ->
  clock:float Eio.Time.clock_ty Eio.Time.clock ->
  path:string ->
  io

val materialization :
  io:io ->
  prefix:string ->
  (Branch_reconcile.materialization option, string) result

val pin_materialization_intent :
  io:io -> prefix:string -> Branch_reconcile.Commit.t -> (unit, string) result
(** Retain the chosen new-branch base before creating the checkout, so a
    receipt-write failure can be recovered without guessing its boundary. *)

val recover_materialization :
  io:io ->
  prefix:string ->
  branch:string ->
  (Branch_reconcile.materialization option, string) result
(** Complete a missing receipt from a pinned creation intent after checking the
    ready checkout still points at that base. *)

val record_materialization :
  io:io ->
  prefix:string ->
  branch:string ->
  new_branch_from:Branch_reconcile.Commit.t option ->
  (Branch_reconcile.materialization option, string) result
(** Retain the actual first checkout revision under a private ref. Only a newly
    created branch establishes a replay boundary; adopting existing work does
    not prove ownership of the preceding commits. Existing receipts are never
    overwritten. *)

val capture_commit :
  io:io -> ref_name:string -> (Branch_reconcile.Commit.t, string) result
(** Resolve a materialization source before using it in a Git mutation. *)

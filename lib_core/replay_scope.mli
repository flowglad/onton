(* @archlint.module interface
   @archlint.domain branch-reconcile *)

type t = private {
  boundary : string;
  source : string;
  commits : string list;
  omitted_merges : string list;
}
[@@deriving eq, compare, sexp_of]
(** A recorded contribution range. Deterministic capture requires a complete
    linear chain. Repair projection follows only first parents and excludes
    merge commits, so side-parent changes cannot become patch contributions. *)

type error =
  | Missing_boundary
  | Invalid_history
  | Nonlinear_history
  | Disconnected_history
[@@deriving eq, compare, sexp_of]

val capture :
  boundary:string option -> source:string -> string -> (t, error) result
(** The boundary must come from recorded contribution provenance, not a merge
    base or commit-subject heuristic. Decode complete newest-first NUL-delimited
    [%H%x00%P] records. The returned commit list is oldest first. Empty history
    is valid only when source equals boundary. *)

val capture_first_parent :
  boundary:string option -> source:string -> string -> (t, error) result
(** Decode the complete [--first-parent] chain to a recorded boundary. Retain
    only ordinary single-parent commits as repair inputs. Merge commits and
    side-parent histories grant no content authority. *)

val error_reason : error -> string

val todo_allowed : t -> string -> bool
(** The combined completed and pending rebase todo may reference only ordered,
    nonduplicated commits in the captured range. Git instructions that introduce
    other ancestry or arbitrary commands are never authorized by this scope. *)

type request =
  | Unproven
  | Identity of string
  | Replay of { source : string; boundary : string; target : string }
  | Merge of { source : string; target : string }
[@@deriving eq, compare, sexp_of]

val yojson_of_request : request -> Yojson.Safe.t

val request_of_yojson : Yojson.Safe.t -> request
(** Malformed checkpoints decode to [Unproven], never mutation authority. *)

val valid_request : request -> bool
val revisions : request -> string list

val extend_local : request -> t -> request option
(** Extend only the source of an existing contract after a verified linear
    continuation of explicitly requested unfinished local work. A different
    boundary or a merge cannot grant additional contribution authority. *)

val rebase_continuation :
  request ->
  scope:t ->
  original:string ->
  target:string ->
  current:string option ->
  todo:string ->
  bool

val merge_continuation : request -> head:string -> target:string -> bool
(** Resuming a sequencer cannot acquire authority for new integration inputs. *)

type tree_plan = private {
  request : request;
  expected_tree : string;
  conflicts : string list;
}
[@@deriving eq, compare, sexp_of]

val prepare :
  t -> target:string -> status:int -> string -> (tree_plan, string) result
(** Decode [merge-tree --write-tree --no-messages --name-only -z] computed
    against this range's explicit boundary. The predicted tree and conflict
    paths are independent of the recovery agent's output. Exit 0 must have no
    conflicts; exit 1 must have at least one. Other exits are never evidence. *)

val prepare_composed :
  t ->
  target:string ->
  initial_tree:string ->
  predictions:(string * int * string) list ->
  (tree_plan, string) result
(** Compose one independently computed tree prediction per selected commit, in
    captured order. Conflict paths accumulate; missing, duplicate or foreign
    contribution results cannot authorize a candidate. *)

val prepare_merge :
  source:string ->
  target:string ->
  status:int ->
  string ->
  (tree_plan, string) result
(** The caller supplies the two explicitly authorized integration inputs, not an
    unsolicited remote head. Prediction uses precisely those inputs. *)

val prepare_identity :
  source:string -> tree:string -> (tree_plan, string) result
(** Publication without integration may only publish the captured commit itself.
*)

type verified = private { request : request; candidate : string; tree : string }
[@@deriving eq, compare, sexp_of]

val verify :
  tree_plan ->
  candidate:string ->
  tree:string ->
  history:string ->
  changed_paths:string ->
  (verified, string) result
(** Verify the complete candidate history above the exact captured target and
    its tree relative to the independently predicted tree. Recovery may resolve
    only captured conflict paths; every other tree entry must match exactly.
    [changed_paths] is the complete NUL-terminated [git diff --name-only -z]
    result between predicted and candidate trees, without rename detection. This
    establishes a structural input-scope guarantee, not semantic approval of an
    agent's choices inside an explicitly authorized conflict. *)

val matches :
  verified ->
  source:string ->
  boundary:string ->
  target:string ->
  candidate:string ->
  bool
(** A verified tree cannot authorize a different request or candidate. *)

val matches_request : verified -> request:request -> candidate:string -> bool

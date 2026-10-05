open Types

val render :
  gameplan:Gameplan.t ->
  patches:Patch.t list ->
  notes:(Patch_id.t * string) list ->
  string
(** Render the root patch and integrated descendants, in the supplied order.
    Rebuilding from these inputs keeps refreshes idempotent. Notes belonging to
    patches outside this list are ignored. *)

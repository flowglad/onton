(* @archlint.module interface
   @archlint.domain execution-mode *)
open Types

type contribution

val render_contribution : patch:Patch.t -> notes:string option -> contribution
(** Render one immutable patch contribution using changes as the canonical
    representation when present, otherwise falling back to its description. *)

val render_contributions : gameplan:Gameplan.t -> contribution list -> string
(** Assemble cached contributions in the supplied patch order. *)

val render :
  gameplan:Gameplan.t ->
  patches:Patch.t list ->
  notes:(Patch_id.t * string) list ->
  string
(** Render the root patch and integrated descendants, in the supplied order.
    Rebuilding from these inputs keeps refreshes idempotent. Notes belonging to
    patches outside this list are ignored. *)

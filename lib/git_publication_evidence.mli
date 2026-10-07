(* @archlint.module interface
   @archlint.domain rewrite-lineage *)

val rewrite_authority :
  git:(string list -> int * string * string) ->
  branch:string ->
  local_sha:string ->
  remote_sha:string ->
  (Rewrite_lineage.t option, string) result
(** Preserve PR #482's content-checked, revision-bound publication lineage.
    Failed probes are errors; absent reflogs are missing evidence. *)

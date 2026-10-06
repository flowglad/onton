(* @archlint.module interface
   @archlint.domain rewrite-lineage *)

type t
(** Publication evidence from the named branch's completed Git rebase history.
    Old tips alone grant no authority: every transition from an incorporated
    remote tip to the captured local tip must preserve history or be a completed
    rebase onto a target incorporating the captured remote tip and ancestral to
    its result. Discarded or missing history breaks the chain. The resulting
    evidence is bound to both immutable commits. *)

val authorizes :
  t -> branch:string -> local_sha:string -> remote_sha:string -> bool

val of_reflog :
  branch:string ->
  local_sha:string ->
  remote_sha:string ->
  reflog:string ->
  ancestor_oracle:(string -> descendant:string -> bool) ->
  t option
(** Decode oldest-first raw branch reflog records, including their before/after
    SHAs. Only newline-terminated records are accepted; an unterminated tail
    from a concurrent append is ignored. Missing or malformed complete entries,
    disconnected transitions, and unavailable ancestry fail closed. The newest
    completed rebase must incorporate the remote tip in both its target and
    pre-rebase history; older history cannot rescue a failed check. Only
    [rebase (finish): refs/heads/<branch> onto <sha>] grants rewrite authority;
    aborted/in-progress rebases and arbitrary resets do not. *)

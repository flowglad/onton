(* @archlint.module interface
   @archlint.domain rewrite-lineage *)

type t
(** Publication evidence from the named branch's completed Git rebase history.
    Old tips alone grant no authority: every transition from an incorporated
    remote tip to the captured local tip must preserve history or be a completed
    rebase whose result incorporates the captured remote tip by ancestry or
    exact preservation of its changed paths or retention of target deletions.
    Discarded or missing history breaks the chain. The resulting evidence is
    bound to both immutable commits. *)

val authorizes :
  t -> branch:string -> local_sha:string -> remote_sha:string -> bool

val changes_preserved :
  remote_changed_paths:string ->
  local_changed_paths:string ->
  target_deleted_paths:string ->
  local_deleted_paths:string ->
  bool
(** Compare complete NUL-terminated Git path lists. [remote_changed_paths] is
    the diff from the unique merge base of remote and target to remote;
    [local_changed_paths] is the diff from remote to the completed rebase
    result. [target_deleted_paths] is the deletion-only diff from that merge
    base to target; [local_deleted_paths] is the deletion-only diff from remote
    to local. Every remote-changed path must retain its exact remote tree entry,
    or be deleted by both the target and local result. A remote-only addition
    absent from target grants no deletion authority. Malformed lists fail
    closed. *)

val of_reflog :
  branch:string ->
  local_sha:string ->
  remote_sha:string ->
  reflog:string ->
  ancestor_oracle:(string -> descendant:string -> bool) ->
  content_oracle:
    (remote_sha:string -> target:string -> result_sha:string -> bool) ->
  t option
(** Decode oldest-first raw branch reflog records, including their before/after
    SHAs. Only newline-terminated records are accepted; an unterminated tail
    from a concurrent append is ignored. Missing or malformed complete entries,
    disconnected transitions, and unavailable ancestry fail closed. The newest
    completed rebase must incorporate remote in its pre-rebase history and have
    a target ancestral to its result. If the target omits remote, the content
    oracle must prove exact preservation of remote-changed paths or retention of
    deletions made by target since their merge base at the completed rebase
    result. Older history cannot rescue a failed check. Only
    [rebase (finish): refs/heads/<branch> onto <sha>] grants rewrite authority;
    aborted/in-progress rebases and arbitrary resets do not. *)

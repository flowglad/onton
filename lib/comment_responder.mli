(* @archlint.module interface
   @archlint.domain comment-responses *)

(** Post-Review-session comment resolution: read the agent's per-comment
    response files and deterministically reply to and then resolve each
    delivered comment that has one.

    Runs only after the supervisor's post-session push succeeded — a response
    claiming "fixed in <sha>" must not land on a thread before the fix is on the
    remote. Comments without a response file are left unresolved; the poller
    re-detects them and re-delivers on the next Review_comments session.

    Failures are logged but never raise: a flaky forge call must not block the
    rest of the session pipeline. A failed reply skips the resolve for that
    comment (never resolve a thread that got no response); a failed resolve
    after a successful reply is logged and left for the next cycle, where the
    agent sees the still-unresolved comment along with its posted reply. *)

val respond_after_session :
  reply:(comment_id:Types.Comment_id.t -> body:string -> (unit, string) result) ->
  resolve:(thread_id:string -> (unit, string) result) ->
  log:(string -> unit) ->
  viewer_login:string option ->
  artifact_dir:string ->
  delivered:Types.Comment.t list ->
  unit ->
  [ `Converged | `Unresolved of int ]
(** Decode every file in [artifact_dir] ([<comment_id>.md], response text only;
    see {!Project_store.comment_responses_dir}), join against [delivered], and
    for each match call [reply] then [resolve] (skipped when the comment has no
    thread id). Unrecognized filenames, synthetic/non-positive ids, blank files,
    response files for undelivered ids, and a missing directory are all logged
    and skipped.

    A delivered comment whose thread already ends with onton's own reply
    ({!Onton_core.Comment_responses.is_resolve_retry} against [viewer_login])
    needs no response file: its resolve is retried directly, never posting a
    duplicate reply. [viewer_login = None] disables that detection — every
    comment is treated as needing a reply.

    Returns [`Converged] when every distinct delivered comment ended this cycle
    resolved (reply-then-resolve, or a successful resolve retry), else
    [`Unresolved n] with the count left unresolved (no response file, failed
    reply/resolve, unaddressable). The caller maps [`Unresolved] to
    [Orchestrator.Respond_review_unresolved] so the re-delivery loop is bounded
    by [review_unresolved_cycle_count]. *)

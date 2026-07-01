(* @archlint.module interface
   @archlint.domain comment-responses *)

(** Pure decision logic for the review-comment response artifacts.

    During a Review_comments session the agent is not asked to reply to or
    resolve review threads itself. It writes one response file per comment under
    the [comment_responses/] artifact directory (see
    {!Project_store.comment_responses_dir} in the effectful layer): the file is
    named [<comment_id>.md] — the id shown as [comment_id=…] in the review
    prompt — and contains just the response text. After the supervisor pushes
    the session's commits, it joins the response files against the comments that
    were delivered to the session and, for each match, replies to the comment
    thread and then resolves it.

    This module owns the total filename/content decoding and the join; the
    directory listing and forge side effects live in [lib/comment_responder.ml].
*)

type entry = { comment_id : int; response : string }
[@@deriving show, eq, sexp_of, compare]

type action = {
  comment_id : Types.Comment_id.t;
  thread_id : string option;
  response : string;
}
[@@deriving show, eq, sexp_of, compare]

type outcome = {
  actions : action list;
      (** One per delivered comment with a response, in delivered order. *)
  unanswered : Types.Comment_id.t list;
      (** Delivered comments the agent wrote no response for; they stay
          unresolved and re-deliver on the next poll. *)
}
[@@deriving show, eq, sexp_of, compare]

val is_resolve_retry : viewer_login:string option -> Types.Comment.t -> bool
(** True when the thread's last word is a reply authored by [viewer_login] —
    onton's own posted response (position disambiguates authorship even when a
    human co-reviews from the token's account: co-reviewers open threads, they
    never correspond mid-thread). Such a comment needs its resolve retried, not
    another reply, and needs no fresh response file. [viewer_login = None]
    (viewer fetch failed) is always [false] — fail open to a possible duplicate
    reply rather than closed to silence. *)

val entry_of_file : filename:string -> contents:string -> entry option
(** Decode one response file. The basename's stem (extension stripped) must be
    an integer — that is the comment id; [contents] is stripped and must be
    nonblank. Returns [None] for unrecognized filenames or blank files. Never
    raises. *)

val plan : delivered:Types.Comment.t list -> entries:entry list -> outcome
(** Join decoded entries against the comments delivered to the session. When
    several entries share a [comment_id], the last one wins — pass entries in
    sorted-filename order to make that pick deterministic. Duplicate delivered
    ids yield a single action (first occurrence's thread_id). *)

val unmatched_entries :
  delivered:Types.Comment.t list -> entries:entry list -> entry list
(** Entries whose [comment_id] matches no delivered comment — stale files from
    an interrupted cleanup or agent typos; the caller logs them. *)

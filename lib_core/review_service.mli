(** Pure parsers for the review-service HTTP API.

    The review-service is a separate review backend (alongside GitHub review
    comments) that owns its own findings store, severity model, and resolution
    semantics. This module decodes the JSON wire format; the effectful HTTP
    client lives in [Review_service_client]. *)

(** {2 Wire types} *)

type severity = Must_fix | Should_fix | Note
[@@deriving show, eq, sexp_of, compare]

(** Severity classification on a finding. Decoded case-sensitively from the spec
    strings ["must-fix"], ["should-fix"], ["note"]. *)

type outcome_kind =
  | Outstanding  (** Posted (or persisted-only) and not yet acted on. *)
  | Discussed  (** A non-bot human replied on the GitHub thread. *)
  | Addressed
      (** Either: code edits removed the anchored region (engine path), or: a
          caller marked it addressed via the resolve API. Distinguish via
          [outcome.actor]/[outcome.reason] — those are present only on the API
          path. *)
  | Ignored  (** PR was closed without anyone touching the finding. *)
  | Wontfix
      (** Caller actively dismissed via the resolve API. Distinct from [Ignored]
          (PR-close), distinct from [Addressed] (caller fixed it). *)
[@@deriving show, eq, sexp_of, compare]

type last_reply = { author : string; at : string; body : string }
[@@deriving show, eq, sexp_of, compare]

type outcome = {
  kind : outcome_kind;
  detected_at : string option;
  actor : string option;
  reason : string option;
  last_reply : last_reply option;
}
[@@deriving show, eq, sexp_of, compare]

type finding = {
  id : string;
  github_comment_id : int option;
  posting_sha : string;
  path : string;
  start_line : int;
  end_line : int;
  severity : severity;
  body : string;
  created_at : string;
  outcome : outcome;
}
[@@deriving show, eq, sexp_of, compare]
(** A single finding from the review-service.

    [id] is a UUID, stable across re-fetches. [github_comment_id] is [None] when
    the finding was generated but never posted (e.g. during a GitHub outage);
    those are still authoritative and will not be retroactively backfilled.

    [posting_sha] is the head SHA the line numbers are anchored to, distinct
    from the current PR head. The agent should use this when interpreting line
    numbers. *)

type finding_parse_error = { index : int; error : string; json : string }
[@@deriving show, eq, sexp_of, compare]
(** Diagnostic for an individual finding entry that was dropped while parsing.
    [index] is zero-based within the response's [findings] array, [error] is the
    field-level parser error, and [json] is the compact offending entry. *)

type findings_response = {
  repo_id : string;
  pull_number : int;
  count : int;
  findings : finding list;
  dropped_findings : finding_parse_error list;
}
[@@deriving show, eq, sexp_of, compare]

(** Resolve verb. The spec accepts ["addressed"] (the agent fixed the issue) and
    ["wontfix"] (the agent or operator decided not to act).

    Constructors are prefixed [Resolve_*] to avoid collision with
    {!outcome_kind}, which has its own [Addressed] arm. *)
type resolve_kind = Resolve_addressed | Resolve_wontfix
[@@deriving show, eq, sexp_of, compare]

(** {2 Parsers}

    Every parser is total over arbitrary JSON: malformed input returns
    [Error _], never raises. *)

val severity_of_string : string -> severity option
val severity_to_string : severity -> string
val outcome_kind_of_string : string -> outcome_kind option
val outcome_kind_to_string : outcome_kind -> string
val resolve_kind_of_string : string -> resolve_kind option
val resolve_kind_to_string : resolve_kind -> string

val parse_finding : Yojson.Safe.t -> (finding, string) Result.t
(** Parse a single [Finding] object. Skips unknown fields. *)

val parse_findings_response :
  Yojson.Safe.t -> (findings_response, string) Result.t
(** Parse the body of [GET /prs/:owner/:repo/:n/findings]. Drops unparseable
    individual entries into [dropped_findings] so callers can log schema drift
    without losing the entries that did parse. *)

val parse_findings_response_string :
  string -> (findings_response, string) Result.t
(** Convenience: [parse_findings_response_string body] runs JSON parsing first.
*)

type resolve_response = { id : string; outcome : outcome }
[@@deriving show, eq, sexp_of, compare]
(** Parsed body of [POST /prs/:owner/:repo/:n/findings/:id/resolve]. The actor
    is responsible for trusting [outcome] over the verb they sent — the server
    echoes the actual current state for already-absorbed findings. *)

val parse_resolve_response :
  Yojson.Safe.t -> (resolve_response, string) Result.t

val parse_resolve_response_string :
  string -> (resolve_response, string) Result.t

(** {2 Resolve request} *)

type resolve_request = {
  kind : resolve_kind;
  actor : string option;
  reason : string option;
}
[@@deriving show, eq, sexp_of, compare]

val resolve_request_to_yojson : resolve_request -> Yojson.Safe.t
(** Encode a resolve request body. Omits [actor]/[reason] when [None]. *)

(** {2 Error response} *)

val parse_error_message : string -> string option
(** Try to extract the [error] field from a JSON error body like
    [{"error":"unauthorized"}]. Returns [None] if the body isn't JSON or has no
    [error] field — useful for surfacing the server's reason in logs. *)

(** {2 Wontfix artifact} *)

type wontfix_entry = { id : string; reason : string }
[@@deriving show, eq, sexp_of, compare]

val parse_wontfix_artifact :
  string -> (wontfix_entry list, string) Stdlib.Result.t
(** Decode the agent-authored [findings_wontfix.json] artifact: a JSON array of
    [{id, reason}] objects. Returns [Ok []] on the empty string, an empty array,
    or a missing file — callers normalize those cases the same way: no findings
    declared wontfix.

    Total over arbitrary input — invalid JSON or schema returns [Error _] rather
    than raising. Entries with missing/empty [id] or [reason] are skipped (not
    an error) so a single malformed row doesn't poison the whole list. *)

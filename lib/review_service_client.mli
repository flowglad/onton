(** Effectful HTTP client for the review-service backend.

    Pairs with {!Onton_core.Review_service} (pure parsers) and {!Jwt} (token
    minter). Each call mints a fresh JWT, hits the configured backend over
    HTTPS, and returns either the parsed response or a structured error. *)

type error =
  | Auth_error of Jwt.error
      (** JWT minting failed (key file missing, bad PEM, sign error). *)
  | Transport_error of { meth : string; url : string; msg : string }
      (** Network/TLS layer failure. *)
  | Http_error of {
      meth : string;
      url : string;
      status : int;
      body : string;
      server_error : string option;
    }
      (** Non-2xx response. [server_error] is the [error] field decoded from the
          body when the server returned the spec's error envelope. *)
  | Parse_error of string
      (** 2xx response but the body did not decode as the expected shape. *)

val show_error : error -> string

val list_findings :
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  backend:Onton_core.Review_backend.t ->
  owner:string ->
  repo:string ->
  pr_number:int ->
  unit ->
  (Onton_core.Review_service.findings_response, error) Stdlib.Result.t
(** [list_findings ~net ~clock ~backend ~owner ~repo ~pr_number ()] performs
    [GET /prs/:owner/:repo/:pr_number/findings?status=unresolved] against the
    configured backend. Status filter is fixed to [unresolved] — the poller only
    cares about findings the agent has not yet acted on. *)

val mark_resolved :
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  backend:Onton_core.Review_backend.t ->
  owner:string ->
  repo:string ->
  pr_number:int ->
  finding_id:string ->
  kind:Onton_core.Review_service.resolve_kind ->
  ?actor:string ->
  ?reason:string ->
  unit ->
  (Onton_core.Review_service.resolve_response, error) Stdlib.Result.t
(** [mark_resolved … ~kind …] performs
    [POST /prs/:owner/:repo/:pr_number/findings/:finding_id/resolve] with a body
    of [{kind, actor?, reason?}]. The server is idempotent — repeated calls on
    an already-resolved finding return the current outcome rather than failing.
    The caller should trust [resolve_response.outcome] over the requested
    [kind]. *)

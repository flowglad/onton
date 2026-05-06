(** Configured review-source the poller queries in addition to the forge.

    Each entry corresponds to one server Onton polls for findings. Only the
    [review-service] kind is implemented today, but the variant is open-ended so
    other backends (e.g. SonarQube, custom internal review tools) can slot in
    without changing the consumer-side wiring. *)

type review_service_auth = {
  app_id : string;
      (** GitHub App ID. Embedded as the [iss] claim so the server can pick the
          right verification key when it eventually supports multiple tenants.
      *)
  private_key_path : string;
}
(** Authentication for the [review-service] backend.

    Onton mints short-lived JWTs (RS256) signed with the configured GitHub App
    private key, which the review-service verifies with the corresponding public
    key. The [private_key_path] is read at sign time — keys are not held in
    memory across polls. *)

type kind =
  | Review_service of {
      base_url : string;
          (** Origin without trailing slash, e.g.
              ["https://review.example.com"]. *)
      auth : review_service_auth;
    }

type t = {
  name : string;
      (** Operator-supplied identifier, used in logs and side-tables to
          attribute findings back to their source. Must be unique within a
          config. *)
  kind : kind;
}

val parse : known_kinds:string list -> Yojson.Safe.t -> (t, string) Result.t
(** Parse a single backend entry. Validates that the [kind] field is in
    [known_kinds]. *)

val parse_array :
  known_kinds:string list -> Yojson.Safe.t -> (t list, string) Result.t
(** Parse an array of backend entries from JSON. [`Null]/empty array returns
    [Ok []]. Validates that [name]s are unique. *)

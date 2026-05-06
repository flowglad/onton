(** Minimal RS256 JWT signer for the review-service backend.

    Mints short-lived tokens whose claims match the review-service spec:
    - [iss]: the GitHub App ID (caller-supplied)
    - [iat]: issue time (≤ 120s old per server policy)
    - [exp]: expiry (≤ 300s after [iat] per server policy)

    No verification path here — Onton is purely a token minter. The server holds
    the verification key and rejects tampered tokens with 401. *)

type error =
  | Key_read_error of string  (** Failed to read the PEM file from disk. *)
  | Key_decode_error of string
      (** PEM contents did not decode as a private key. *)
  | Key_not_rsa of string
      (** PEM decoded but the key was not RSA — the spec requires RS256. *)
  | Sign_error of string  (** [Mirage_crypto_pk] rejected the sign call. *)

val show_error : error -> string

val mint :
  now:float ->
  app_id:string ->
  private_key_path:string ->
  ttl_seconds:int ->
  (string, error) Stdlib.Result.t
(** [mint ~now ~app_id ~private_key_path ~ttl_seconds] builds and signs a JWT
    suitable for the review-service [Authorization: Bearer …] header.

    [ttl_seconds] is clamped to [[60, 300]] (60s minimum to absorb clock skew at
    the server, 300s maximum per the spec's server-side window). The PEM is read
    from disk on every call — token mints are infrequent and keeping the key
    off-heap reduces blast radius.

    Seeds [Mirage_crypto_rng] via {!Mirage_crypto_rng_unix.use_default} before
    signing (RSA PKCS1 needs randomness for blinding). The seed is idempotent —
    repeated calls are a no-op. *)

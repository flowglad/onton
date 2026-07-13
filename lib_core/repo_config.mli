(* @archlint.module interface
   @archlint.domain repo-config *)

(** Per-repo configuration loaded from
    [~/.config/onton/<owner>/<repo>/config.json] (the same directory layout the
    [User_config] hook lives in — see [User_config.config_dir]).

    Carries three sections:
    - [default] — a per-repo default [(backend, model)] pair that mirrors the
      [--backend] / [--model] CLI flags. Either field may be omitted. Slots into
      the resolution chain below [Project_store] (stored values from previous
      runs) but above the hard-coded built-in default.
    - [routing] — a per-patch override that binds each complexity tier (1/2/3)
      to a [(backend, model)] tuple. Fires when the effective model (CLI or
      [default.model]) is the literal ["auto"] (case-insensitive). Otherwise the
      effective pair drives the whole run.
    - [reviewBackends] — additional review sources to poll alongside the forge.

    Unknown top-level keys are ignored so the file can grow with new sections
    (e.g. timeouts, hook overrides) without breaking older binaries. *)

type route = {
  backend : string;
      (** Backend name. Must be in the caller's [known_backends]. *)
  model : string option;
      (** Model passed to that backend. [None] leaves [--model] off and lets the
          backend's own default apply. *)
}

type t = {
  default_backend : string option;
      (** Top-level [default.backend] from the config file. [Some name] (where
          [name] is in the caller's [known_backends]) acts as the per-repo
          default backend when neither [--backend] nor a previously stored
          backend is set. [None] means "not configured — fall through". *)
  default_model : string option;
      (** Top-level [default.model] from the config file. [Some "auto"] (case-
          insensitive) activates [routing] in the same way as [--model auto].
          [Some other] pins a specific model. [None] means "not configured". *)
  review_team : string option;
      (** Top-level [review_team] slug from the config file. [None] keeps the
          review-request feature disabled for this repo. *)
  complexity_routes : (int * route) list;
      (** Sparse map indexed by complexity (1/2/3). Stored as an alist because
          the set is tiny and the lookup is per-patch — fast enough. Missing
          keys mean "no override", and the caller falls back to the effective
          backend / built-in [auto_model] ladder. *)
  review_backends : Review_backend.t list;
      (** Additional review sources to poll alongside the forge. Empty (the
          default) means GitHub review comments are the only source. *)
}

val empty : t
(** Returned when [config.json] is absent — the no-op config. *)

val load :
  config_dir:string ->
  known_backends:string list ->
  ?known_review_kinds:string list ->
  unit ->
  (t, string) Stdlib.Result.t
(** Read and validate [<config_dir>/config.json]. [config_dir] should be the
    output of [User_config.config_dir ~github_owner ~github_repo] so the routing
    config lives alongside the [on_worktree_create] hook for the same repo.
    Returns:
    - [Ok empty] if the file does not exist (a non-error — the config is
      optional);
    - [Ok t] if the file parses cleanly and every backend name is in
      [known_backends];
    - [Error msg] on JSON parse failure, schema violations, unknown backend
      names, or out-of-range complexity keys.

    The schema is:
    {[
      {
        "default": {
          "backend": "codex",
          "model":   "auto"
        },
        "routing": {
          "1": { "backend": "claude", "model": "haiku" },
          "2": { "backend": "codex",  "model": "gpt-5.6-terra" },
          "3": { "backend": "claude", "model": "opus" }
        }
      }
    ]}
    Inside [routing.<n>], [backend] is required and [model] is optional. Inside
    [default], both [backend] and [model] are optional. Top-level extra keys are
    ignored so the file can grow without breaking older binaries. *)

val route_for_complexity : t -> complexity:int option -> route option
(** Look up the override for a given patch complexity. [None] when the patch has
    no complexity, or no rule matches — the caller should fall back to its
    default backend. *)

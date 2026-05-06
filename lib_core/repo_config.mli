(** Per-repo configuration loaded from
    [~/.config/onton/<owner>/<repo>/config.json] (the same directory layout the
    [User_config] hook lives in — see [User_config.config_dir]).

    Currently only carries the [routing] map — a per-patch override that binds
    each complexity tier (1/2/3) to a [(backend, model)] tuple. The map only
    takes effect when [onton --model auto] is in use; explicit [--model <name>]
    still wins for the whole run.

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
  complexity_routes : (int * route) list;
      (** Sparse map indexed by complexity (1/2/3). Stored as an alist because
          the set is tiny and the lookup is per-patch — fast enough. Missing
          keys mean "no override", and the caller falls back to its default
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
        "routing": {
          "1": { "backend": "claude", "model": "haiku" },
          "2": { "backend": "codex",  "model": "gpt-5.4" },
          "3": { "backend": "claude", "model": "opus" }
        }
      }
    ]}
    [model] is optional. Top-level extra keys are ignored so the file can grow
    without breaking older binaries. *)

val route_for_complexity : t -> complexity:int option -> route option
(** Look up the override for a given patch complexity. [None] when the patch has
    no complexity, or no rule matches — the caller should fall back to its
    default backend. *)

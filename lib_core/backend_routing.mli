(** Pure decision: which [(backend, model)] tuple should drive a given patch's
    session?

    Combines four inputs into one tuple, with no IO, no Eio capabilities, and no
    global state — so the decision is fully testable as a property function. The
    effectful counterpart that turns this decision into an [Llm_backend.t] is
    {!Backend_registry}. *)

type decision = {
  backend : string;
  model : string option;
      (** [Some "auto"] (case-insensitive) signals the per-backend hardcoded
          complexity → model ladder; [Some other] is an explicit model name;
          [None] drops [--model] from the CLI invocation entirely. *)
}

val decide :
  repo_config:Repo_config.t ->
  default_backend:string ->
  effective_model:string option ->
  complexity:int option ->
  decision
(** Precedence rules. [default_backend] and [effective_model] are the values
    that come out of {!resolve_pair} — i.e. the post-merge effective backend and
    model, not the raw CLI flags.

    + If [effective_model] is not the literal ["auto"] (case-insensitive), the
      effective pair wins for the whole run: result is
      [{ backend = default_backend; model = effective_model }]. The routing map
      is ignored. This preserves the "explicit override" escape hatch.
    + If [effective_model] is ["auto"] (case-insensitive) AND [Repo_config] has
      a route for this complexity: the route wins —
      [{ backend = route.backend; model = route.model }]. This is the only case
      where the routing map takes effect.
    + Otherwise ([effective_model] is ["auto"] but no route applies — typically
      because [complexity = None], or the route map has no entry for this
      complexity): result is
      [{ backend = default_backend; model = effective_model }]. The downstream
      backend will see [Some "auto"] and apply its own hardcoded ladder via
      [Llm_backend.resolve_auto_model].

    The function never raises and is total over all inputs. *)

val resolve_pair :
  cli_backend:string ->
  cli_model:string ->
  stored_backend:string ->
  stored_model:string ->
  repo_config:Repo_config.t ->
  built_in_backend:string ->
  string * string
(** Merge the four sources of backend/model — CLI flag, [Project_store] (stored
    value from a previous run), [Repo_config.default_*], and the hard-coded
    built-in default — into the effective [(backend, model)] string pair.

    Each field is resolved independently. Precedence within a field is
    [CLI > stored > config > built-in]; the first non-empty (after
    [String.strip]) value wins. [model] may come back as [""], meaning "no model
    was configured anywhere" — the caller then omits [--model] from the
    underlying CLI call so the provider's own default applies. [backend] always
    falls back to [built_in_backend] and so is never empty.

    Pure: no IO, no global state. *)

val is_auto_model : string option -> bool
(** [is_auto_model m] returns [true] iff [m] is [Some s] where [s]
    case-insensitively equals ["auto"]. Exposed for tests and call sites that
    need to gate other behavior on the same predicate. *)

val resolve_auto :
  decision ->
  auto_model:(complexity:int option -> string option) ->
  complexity:int option ->
  decision
(** Inline the [Some "auto"] sentinel: when [decision.model] is auto, replace it
    with [auto_model ~complexity]; otherwise return [decision] unchanged. Used
    by display sites (the TUI) that want to show the concrete model name a patch
    will run under, rather than the sentinel. *)

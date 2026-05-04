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
  cli_model:string option ->
  complexity:int option ->
  decision
(** Precedence rules:

    + If [cli_model] is not the literal ["auto"] (case-insensitive), the CLI
      wins for the whole run: result is
      [{ backend = default_backend; model = cli_model }]. The repo config is
      ignored. This preserves the "explicit override" escape hatch.
    + If [cli_model] is ["auto"] (case-insensitive) AND [Repo_config] has a
      route for this complexity: the route wins —
      [{ backend = route.backend; model = route.model }]. This is the only case
      where the routing map takes effect.
    + Otherwise ([cli_model] is ["auto"] but no route applies — typically
      because [complexity = None], or the route map has no entry for this
      complexity): result is [{ backend = default_backend; model = cli_model }].
      The downstream backend will see [Some "auto"] and apply its own hardcoded
      ladder via [Llm_backend.resolve_auto_model].

    The function never raises and is total over all inputs. *)

val is_auto_model : string option -> bool
(** [is_auto_model m] returns [true] iff [m] is [Some s] where [s]
    case-insensitively equals ["auto"]. Exposed for tests and call sites that
    need to gate other behavior on the same predicate. *)

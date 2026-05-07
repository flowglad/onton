(** Lazy cache of backend instances keyed by [(backend_name, model)].

    A run can route different patches to different backends (see
    [Repo_config.modelRouting]), so we may need several distinct [Llm_backend.t]
    values during a single session — one per unique [(backend, model)] tuple
    referenced by either the CLI flags or the routing map. Constructing them
    eagerly would be wasteful when the routing only uses one or two; the
    registry builds each on first lookup and caches it for the rest of the run.
*)

type t

type kind =
  | Ephemeral of Llm_backend.t
  | Long_lived of Llm_backend_long_lived.t

val create :
  process_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  setsid_exec:string option ->
  t
(** Build an empty registry that knows how to construct any of the supported
    backends. The Eio capabilities and per-session [timeout] are baked into the
    closure so callers don't re-thread them on every [get]. *)

val get : t -> backend:string -> model:string option -> kind
(** Return the cached backend for [(backend, model)], constructing it on first
    request. Raises [Invalid_argument] for unrecognised backend names — callers
    are expected to validate against the known list before asking. *)

val auto_model : backend:string -> complexity:int option -> string option
(** Look up the named backend's hardcoded complexity → model ladder — the model
    name it would resolve [--model auto] to for this complexity. Pure; no [t]
    required, no IO. Raises [Invalid_argument] on unknown backend names,
    matching {!get}. *)

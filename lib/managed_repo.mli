val ensure_managed_repo :
  ?clone_scheme:Onton_core.Github_target.url_scheme option ->
  project_name:string ->
  token:string ->
  owner:string ->
  repo:string ->
  unit ->
  (string * Onton_core.Github_target.url_scheme, string) result
(** Ensure the onton-managed checkout for [project_name] exists and is current.
    Returns the checkout root together with the {b resolved} transport scheme on
    success. The caller should persist the resolved scheme back to [config.json]
    (see {!Project_store.save_config}'s [~url_scheme]) so subsequent runs are
    stable.

    [?clone_scheme] is the explicit override (e.g. a [--clone-scheme] CLI flag
    or a previously-persisted [url_scheme] from [config.json]). If [None],
    sibling user clones are scanned ([$PWD/..], [~/code-src/], [~/src/],
    [~/code/], [~/dev/], [~/projects/]) and SSH is chosen iff any match uses
    SSH; otherwise the default is HTTPS. When the managed clone already exists
    on disk, its existing [origin] URL is authoritative — we don't flip
    transports mid-life. *)

val url_scheme_of_string : string -> Onton_core.Github_target.url_scheme option
(** Parse the persisted ["https"] / ["ssh"] form. Returns [None] for anything
    else, including the empty string (caller treats [None] as "auto-detect"). *)

val string_of_url_scheme : Onton_core.Github_target.url_scheme -> string
(** Serialize a scheme as ["https"] or ["ssh"] for storage in [config.json]. *)

val discover_sibling_clones :
  owner:string -> repo:string -> (string * string list) list
(** Effectful: scan conventional sibling-clone directories for user clones
    matching [owner/repo]. Returns [(path, remote_urls)] pairs. Used by the
    auto-detect path in {!ensure_managed_repo}; exposed for tests and for
    callers that want to display the detected siblings to the user. *)

val infer_github_token : unit -> string
(** Resolve a GitHub token from [GITHUB_TOKEN] or [gh auth token]. *)

(** Post-Findings-session resolution: read the agent's wontfix artifact and POST
    [resolve] verbs back to the originating review backends.

    Default verb is [addressed] — every delivered finding that is not in the
    wontfix artifact is reported as fixed. Findings listed in the artifact are
    reported as [wontfix] with the supplied reason.

    Failures are logged but never raise: a flaky review service must not block
    the rest of the session pipeline. *)

val resolve_after_session :
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  log:(string -> unit) ->
  findings_registry:Findings_registry.t ->
  artifact_path:string ->
  delivered:Onton_core.Review_service.finding list ->
  ?actor:string ->
  unit ->
  unit
(** Iterate [delivered], dispatching one resolve POST per finding that has a
    matching entry in [findings_registry]. Findings without a registry entry are
    logged and skipped — that means the poller never recorded them, so we don't
    know which backend to talk to.

    [artifact_path] is the absolute path of [findings_wontfix.json]. If the file
    does not exist, every delivered finding is treated as [addressed]. If the
    file exists but cannot be read or parsed, resolution is skipped so findings
    are not silently marked with the wrong outcome. *)

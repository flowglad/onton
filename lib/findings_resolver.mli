(* @archlint.module interface
   @archlint.domain review-service *)

(** Post-Findings-session resolution: read the agent's per-finding wontfix files
    and POST [resolve] verbs back to the originating review backends.

    Default verb is [addressed] — every delivered finding without a wontfix file
    is reported as fixed. A finding whose wontfix file exists with a nonblank
    body is reported as [wontfix] with that body as the reason. A finding whose
    file exists but is blank or unreadable fails closed: neither verb can be
    reported truthfully, so its resolve is skipped and logged for operator
    action.

    Failures are logged but never raise: a flaky review service must not block
    the rest of the session pipeline. *)

val resolve_after_session :
  review_clients:Review_service_client.client list ->
  log:(string -> unit) ->
  findings_registry:Findings_registry.t ->
  artifact_dir:string ->
  delivered:Onton_core.Review_service.finding list ->
  ?actor:string ->
  unit ->
  unit
(** Iterate [delivered], dispatching one resolve POST per finding that has a
    matching entry in [findings_registry]. Findings without a registry entry are
    logged and skipped — that means the poller never recorded them, so we don't
    know which backend to talk to.

    [artifact_dir] is the absolute [findings_wontfix/] directory
    ({!Project_store.findings_wontfix_dir}); each file inside is named by
    [Onton_core.Review_service.wontfix_filename_of_id] over a delivered
    finding's composite id and holds the wontfix reason. Files matching no
    delivered finding are logged and ignored. A missing directory means no
    wontfix entries — every delivered finding is [addressed]. A directory that
    exists but cannot be listed skips resolution for the whole batch so findings
    are not silently marked with the wrong outcome; delivered finding ids are
    logged for operator action and forgotten from the runtime registry. *)

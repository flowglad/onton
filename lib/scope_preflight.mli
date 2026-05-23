open Onton_core

val run :
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  scheme:Github_target.url_scheme ->
  token:string ->
  owner:string ->
  repo:string ->
  gameplan:Types.Gameplan.t ->
  unit
(** Run the OAuth-scope pre-flight check at startup. Compares the scopes the
    gameplan will need (derived from each patch's [files] paths) against the
    scopes the configured token has (read from the [X-OAuth-Scopes] response
    header). Prints a clear stderr warning with a [gh auth refresh] hint when a
    scope is missing.

    Only runs when [scheme = Https] — SSH doesn't consult OAuth scopes, so the
    warning would be a false positive. Best-effort: any API / transport error is
    logged but does not abort startup; the corresponding push-time
    classification ([Push_reject_classify.Workflow_scope_missing]) still catches
    the failure if the pre-flight never ran. *)

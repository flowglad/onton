open Base
open Onton_core

(** Startup OAuth-scope pre-flight check.

    Compares the scopes the gameplan {e will need} (derived from the union of
    [Patch.files] paths) against the scopes the configured token actually has
    (read from the [X-OAuth-Scopes] response header on an authenticated request
    to [/user]). When a required scope is missing, prints a clear stderr warning
    together with the suggested [gh auth refresh] fix.

    Only runs on HTTPS clones — SSH bypasses OAuth-scope restrictions entirely,
    so the warning would be a false positive. Best-effort: any transport / API
    error is logged at stderr but does not abort startup. *)

let gh_refresh_hint scope =
  Printf.sprintf "gh auth refresh -h github.com -s %s"
    (Oauth_scopes.show_scope_short scope)

let paths_of_gameplan (gp : Types.Gameplan.t) =
  List.concat_map gp.Types.Gameplan.patches ~f:(fun (p : Types.Patch.t) ->
      (* [Patch.files] entries are formatted [path (action): desc] by the
         parser. Recover the path prefix up to the first space — that is the
         caller-supplied path, which is what determines workflow-scope
         requirement. *)
      List.map p.Types.Patch.files ~f:(fun entry ->
          match String.lsplit2 entry ~on:' ' with
          | Some (path, _) -> path
          | None -> entry))

(** Run the preflight. [scheme] is the resolved managed-clone transport from
    [Managed_repo.ensure_managed_repo]; [token]/[owner]/[repo] are the same
    coordinates passed to [Github.create]. The check is silent on success; on
    failure it prints one or more stderr lines and returns. *)
let run ~net ~clock ~scheme ~token ~owner ~repo ~gameplan =
  match scheme with
  | Github_target.Ssh ->
      (* SSH transport doesn't consult OAuth scopes — the preflight would
         either no-op or warn incorrectly. *)
      ()
  | Github_target.Https -> (
      let required =
        Oauth_scopes.required_for_files ~paths:(paths_of_gameplan gameplan)
      in
      if List.is_empty required then ()
      else
        match Github.fetch_oauth_scopes_for ~net ~clock ~token ~owner ~repo with
        | Error err ->
            Stdlib.Printf.eprintf
              "onton: warning: could not pre-flight OAuth scopes (%s); push \
               failures will surface at first attempt instead\n\
               %!"
              (Github.show_error err)
        | Ok present -> (
            match Oauth_scopes.missing ~required ~present with
            | [] -> ()
            | missing ->
                Stdlib.Printf.eprintf
                  "onton: warning: configured token is missing OAuth scope(s): \
                   %s\n\
                  \  patches that modify the relevant files will be rejected \
                   by GitHub at push time.\n\
                   %!"
                  (String.concat ~sep:", "
                     (List.map missing ~f:Oauth_scopes.show_scope_short));
                List.iter missing ~f:(fun scope ->
                    Stdlib.Printf.eprintf "  fix: %s\n%!"
                      (gh_refresh_hint scope))))

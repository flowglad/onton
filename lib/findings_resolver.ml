let read_artifact path =
  if not (Stdlib.Sys.file_exists path) then Ok ""
  else
    try
      let ic = Stdlib.In_channel.open_text path in
      Ok
        (Fun.protect
           ~finally:(fun () -> Stdlib.In_channel.close ic)
           (fun () -> Stdlib.In_channel.input_all ic))
    with Sys_error msg -> Error msg

let resolve_after_session ~net ~clock ~log ~findings_registry ~artifact_path
    ~delivered ?actor () =
  (* Parse the wontfix artifact. Missing means "no wontfix entries"; read or
     parse failures fail closed so we do not accidentally mark findings as
     addressed when the agent's artifact cannot be trusted. *)
  let wontfix_index : (string, string) Hashtbl.t = Hashtbl.create 8 in
  let artifact_ok =
    match read_artifact artifact_path with
    | Error msg ->
        log
          (Printf.sprintf
             "wontfix artifact at %s could not be read (%s) — skipping \
              findings resolution"
             artifact_path msg);
        false
    | Ok body -> (
        match Onton_core.Review_service.parse_wontfix_artifact body with
        | Ok entries ->
            List.iter
              (fun (e : Onton_core.Review_service.wontfix_entry) ->
                Hashtbl.replace wontfix_index e.id e.reason)
              entries;
            true
        | Error msg ->
            log
              (Printf.sprintf
                 "wontfix artifact at %s could not be parsed (%s) — skipping \
                  findings resolution"
                 artifact_path msg);
            false)
  in
  if not artifact_ok then (
    let ids =
      List.map (fun (f : Onton_core.Review_service.finding) -> f.id) delivered
    in
    (match ids with
    | [] -> ()
    | _ ->
        log
          (Printf.sprintf
             "Skipped findings resolution for delivered findings; operator \
              action required for: %s"
             (String.concat ", " ids)));
    List.iter
      (fun (f : Onton_core.Review_service.finding) ->
        Findings_registry.forget findings_registry ~key:f.id)
      delivered)
  else
    List.iter
      (fun (f : Onton_core.Review_service.finding) ->
        match Findings_registry.find findings_registry ~key:f.id with
        | None ->
            log
              (Printf.sprintf
                 "Skipping resolve for finding %s — no backend registered (was \
                  it not seen by the poller this session?)"
                 f.id)
        | Some entry -> (
            let kind, reason =
              match Hashtbl.find_opt wontfix_index f.id with
              | Some r -> (Onton_core.Review_service.Resolve_wontfix, Some r)
              | None -> (Onton_core.Review_service.Resolve_addressed, None)
            in
            match
              Review_service_client.mark_resolved ~net ~clock
                ~backend:entry.Findings_registry.backend
                ~owner:entry.Findings_registry.owner
                ~repo:entry.Findings_registry.repo
                ~pr_number:entry.Findings_registry.pr_number
                ~finding_id:entry.Findings_registry.finding_id ~kind ?actor
                ?reason ()
            with
            | Ok response ->
                log
                  (Printf.sprintf "Resolved finding %s as %s (server: %s)" f.id
                     (Onton_core.Review_service.resolve_kind_to_string kind)
                     (Onton_core.Review_service.outcome_kind_to_string
                        response.Onton_core.Review_service.outcome.kind));
                Findings_registry.forget findings_registry ~key:f.id
            | Error err ->
                log
                  (Printf.sprintf "Failed to resolve finding %s — %s" f.id
                     (Review_service_client.show_error err))))
      delivered

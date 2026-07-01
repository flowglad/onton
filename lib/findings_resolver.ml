(* @archlint.module shell
   @archlint.domain review-service *)

let read_file path =
  try
    let ic = Stdlib.In_channel.open_text path in
    Ok
      (Fun.protect
         ~finally:(fun () -> Stdlib.In_channel.close ic)
         (fun () -> Stdlib.In_channel.input_all ic))
  with Sys_error msg -> Error msg

let find_review_client review_clients ~backend_name =
  List.find_opt
    (fun (module R : Review_service_client.S
           with type error = Review_service_client.error) ->
      String.equal R.name backend_name)
    review_clients

(* Per-finding verdict derived from the wontfix directory. [Wontfix] carries
   the file body; [Untrusted] means a file exists for the finding but its
   content cannot be trusted (unreadable or blank) — fail closed for that
   finding rather than guess between addressed and wontfix. *)
type wontfix_verdict = Wontfix of string | Untrusted of string

let resolve_after_session ~review_clients ~log ~findings_registry ~artifact_dir
    ~delivered ?actor () =
  (* Index the wontfix directory by expected filename. A missing directory
     means "no wontfix entries" (every delivered finding is addressed); a
     directory that exists but cannot be listed fails closed for the whole
     batch, mirroring the unreadable-artifact behavior this flow has always
     had. *)
  let filenames =
    if not (Stdlib.Sys.file_exists artifact_dir) then Some []
    else
      match Stdlib.Sys.readdir artifact_dir with
      | exception Sys_error msg ->
          log
            (Printf.sprintf
               "wontfix directory at %s could not be listed (%s) — skipping \
                findings resolution"
               artifact_dir msg);
          None
      | names -> Some (List.sort String.compare (Array.to_list names))
  in
  match filenames with
  | None ->
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
        delivered
  | Some filenames ->
      let expected : (string, string) Hashtbl.t = Hashtbl.create 8 in
      List.iter
        (fun (f : Onton_core.Review_service.finding) ->
          let filename =
            Onton_core.Review_service.wontfix_filename_of_id f.id
          in
          (* Distinct composite ids can in principle slug to the same
             filename; attribution would be a guess, so drop both from the
             filename index — they fail closed below only if a file with that
             name actually exists. *)
          match Hashtbl.find_opt expected filename with
          | Some prior when not (String.equal prior f.id) ->
              log
                (Printf.sprintf
                   "wontfix filename collision between findings %s and %s — a \
                    %s file will not be attributed"
                   prior f.id filename);
              Hashtbl.remove expected filename
          | Some _ | None -> Hashtbl.replace expected filename f.id)
        delivered;
      let verdicts : (string, wontfix_verdict) Hashtbl.t = Hashtbl.create 8 in
      List.iter
        (fun filename ->
          let path = Stdlib.Filename.concat artifact_dir filename in
          match Hashtbl.find_opt expected filename with
          | None ->
              log
                (Printf.sprintf
                   "wontfix: ignoring %s — no delivered finding uses that \
                    filename"
                   path)
          | Some finding_id -> (
              match read_file path with
              | Error msg ->
                  Hashtbl.replace verdicts finding_id
                    (Untrusted (Printf.sprintf "unreadable (%s)" msg))
              | Ok contents ->
                  let reason = String.trim contents in
                  if String.length reason = 0 then
                    Hashtbl.replace verdicts finding_id (Untrusted "blank file")
                  else Hashtbl.replace verdicts finding_id (Wontfix reason)))
        filenames;
      List.iter
        (fun (f : Onton_core.Review_service.finding) ->
          match Hashtbl.find_opt verdicts f.id with
          | Some (Untrusted why) ->
              (* Neither addressed nor wontfix can be reported truthfully —
                 skip the resolve, keep the operator informed, and forget the
                 registry entry like every other terminal path. *)
              log
                (Printf.sprintf
                   "Skipping resolve for finding %s — wontfix file is %s; \
                    operator action required"
                   f.id why);
              Findings_registry.forget findings_registry ~key:f.id
          | (Some (Wontfix _) | None) as verdict -> (
              match Findings_registry.find findings_registry ~key:f.id with
              | None ->
                  log
                    (Printf.sprintf
                       "Skipping resolve for finding %s — no backend \
                        registered (was it not seen by the poller this \
                        session?)"
                       f.id)
              | Some entry -> (
                  let kind, reason =
                    match verdict with
                    | Some (Wontfix r) ->
                        (Onton_core.Review_service.Resolve_wontfix, Some r)
                    | Some (Untrusted _) | None ->
                        (Onton_core.Review_service.Resolve_addressed, None)
                  in
                  let backend_name = entry.Findings_registry.backend_name in
                  match find_review_client review_clients ~backend_name with
                  | None ->
                      log
                        (Printf.sprintf
                           "Skipping resolve for finding %s — review backend \
                            %s is no longer configured"
                           f.id backend_name);
                      Findings_registry.forget findings_registry ~key:f.id
                  | Some
                      (module R : Review_service_client.S
                        with type error = Review_service_client.error) -> (
                      match
                        R.mark_resolved ~owner:entry.Findings_registry.owner
                          ~repo:entry.Findings_registry.repo
                          ~pr_number:entry.Findings_registry.pr_number
                          ~finding_id:entry.Findings_registry.finding_id ~kind
                          ?actor ?reason ()
                      with
                      | Ok response ->
                          log
                            (Printf.sprintf
                               "Resolved finding %s as %s (server: %s)" f.id
                               (Onton_core.Review_service.resolve_kind_to_string
                                  kind)
                               (Onton_core.Review_service.outcome_kind_to_string
                                  response.Onton_core.Review_service.outcome
                                    .kind));
                          Findings_registry.forget findings_registry ~key:f.id
                      | Error err ->
                          log
                            (Printf.sprintf "Failed to resolve finding %s — %s"
                               f.id (R.show_error err));
                          Findings_registry.forget findings_registry ~key:f.id))
              ))
        delivered

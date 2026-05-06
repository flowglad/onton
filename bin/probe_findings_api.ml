(** Live-host probe for the review-service findings API.

    Drives [Review_service_client] (and therefore [Jwt]) against a real server.
    Mirrors [scripts/probe-findings-api.ts] in the review-service repo so the
    OCaml client can be diffed against the TypeScript reference.

    Environment:
    - [GITHUB_PRIVATE_KEY] — PEM contents (preferred); OR
    - [GITHUB_PRIVATE_KEY_PATH] — path to a PEM file (fallback);
    - [REVIEW_SERVICE_URL] — default [https://review.flowglad.com];
    - [ONTON_REVIEW_BACKEND_NAME] — default ["probe"].

    Usage:
    {v
      onton-probe-findings-api <owner> <repo> <n>
      onton-probe-findings-api <owner> <repo> <n> --all
      onton-probe-findings-api <owner> <repo> <n> resolve <id> <kind> [reason]
    v} *)

let usage () =
  prerr_endline
    "usage:\n\
    \  onton-probe-findings-api <owner> <repo> <pull_number> [--all]\n\
    \  onton-probe-findings-api <owner> <repo> <pull_number> resolve <id> \
     <kind> [reason words...]\n\n\
     env:\n\
    \  GITHUB_PRIVATE_KEY        PEM contents (preferred)\n\
    \  GITHUB_PRIVATE_KEY_PATH   path to PEM file (fallback)\n\
    \  REVIEW_SERVICE_URL        default https://review.flowglad.com\n";
  exit 2

let getenv_opt key =
  match Sys.getenv_opt key with
  | Some s when String.length (String.trim s) > 0 -> Some s
  | _ -> None

let resolve_pem_path () =
  (* If the user supplied a path, use it directly. Otherwise materialise the
     in-env PEM contents to a tempfile so [Jwt.mint] (which reads from disk)
     can sign with it. *)
  match getenv_opt "GITHUB_PRIVATE_KEY_PATH" with
  | Some p -> p
  | None -> (
      match getenv_opt "GITHUB_PRIVATE_KEY" with
      | None ->
          prerr_endline
            "error: set either GITHUB_PRIVATE_KEY (PEM contents) or \
             GITHUB_PRIVATE_KEY_PATH (path to PEM file)";
          exit 2
      | Some pem ->
          let tmp = Filename.temp_file "onton-probe-key" ".pem" in
          (try Unix.chmod tmp 0o600 with Unix.Unix_error _ -> ());
          let oc = open_out tmp in
          Fun.protect
            ~finally:(fun () -> close_out_noerr oc)
            (fun () -> output_string oc pem);
          (* Best-effort cleanup: register at_exit so the tempfile dies with
             the probe. We rely on it for the duration of the probe and
             tolerate the race window where a crash might leak it. *)
          at_exit (fun () -> try Unix.unlink tmp with Unix.Unix_error _ -> ());
          tmp)

let backend_of_env () : Onton_core.Review_backend.t =
  let base_url =
    Option.value
      (getenv_opt "REVIEW_SERVICE_URL")
      ~default:"https://review.flowglad.com"
  in
  let private_key_path = resolve_pem_path () in
  let app_id = Option.value (getenv_opt "ONTON_PROBE_ISS") ~default:"cli" in
  let name =
    Option.value (getenv_opt "ONTON_REVIEW_BACKEND_NAME") ~default:"probe"
  in
  {
    Onton_core.Review_backend.name;
    kind =
      Onton_core.Review_backend.Review_service
        {
          base_url =
            (* Strip trailing slashes so we don't double them when joining. *)
            (let rec strip s =
               let n = String.length s in
               if n > 0 && s.[n - 1] = '/' then strip (String.sub s 0 (n - 1))
               else s
             in
             strip base_url);
          auth = { app_id; private_key_path };
        };
  }

let print_outcome (o : Onton_core.Review_service.outcome) =
  let so opt = match opt with Some s -> s | None -> "" in
  Printf.printf "  outcome.kind  : %s\n"
    (Onton_core.Review_service.outcome_kind_to_string o.kind);
  if Option.is_some o.detected_at then
    Printf.printf "  outcome.detect: %s\n" (so o.detected_at);
  if Option.is_some o.actor then
    Printf.printf "  outcome.actor : %s\n" (so o.actor);
  if Option.is_some o.reason then
    Printf.printf "  outcome.reason: %s\n" (so o.reason)

let print_finding (f : Onton_core.Review_service.finding) =
  Printf.printf
    "- id: %s\n  severity: %s\n  anchor: %s:%d-%d (sha=%s)\n  github_id: %s\n"
    f.id
    (Onton_core.Review_service.severity_to_string f.severity)
    f.path f.start_line f.end_line f.posting_sha
    (match f.github_comment_id with
    | Some id -> string_of_int id
    | None -> "null (outage finding)");
  print_outcome f.outcome

let do_list ~net ~clock ~backend ~owner ~repo ~pr_number ~_all () =
  ignore _all;
  (* The client hardcodes status=unresolved; --all needs the [status=all]
     branch which we don't expose yet — the probe accepts the flag but for
     now logs a warning when it can't honor it. Wire-side support is a
     small addition if needed. *)
  if _all then
    prerr_endline
      "warning: --all is accepted but the OCaml client currently always sends \
       ?status=unresolved (matches the spec default). Use the TS probe for \
       --all coverage.";
  match
    Onton.Review_service_client.list_findings ~net ~clock ~backend ~owner ~repo
      ~pr_number ()
  with
  | Error err ->
      prerr_endline (Onton.Review_service_client.show_error err);
      exit 1
  | Ok response ->
      Printf.printf "GET findings → %d findings on %s#%d\n\n"
        (List.length response.findings)
        response.repo_id response.pull_number;
      List.iter
        (fun f ->
          print_finding f;
          print_newline ())
        response.findings

let parse_kind = function
  | "addressed" -> Some Onton_core.Review_service.Resolve_addressed
  | "wontfix" -> Some Onton_core.Review_service.Resolve_wontfix
  | _ -> None

let do_resolve ~net ~clock ~backend ~owner ~repo ~pr_number ~finding_id ~kind
    ~reason () =
  match
    Onton.Review_service_client.mark_resolved ~net ~clock ~backend ~owner ~repo
      ~pr_number ~finding_id ~kind ~actor:"onton:probe-findings-api" ?reason ()
  with
  | Error err ->
      prerr_endline (Onton.Review_service_client.show_error err);
      exit 1
  | Ok response ->
      Printf.printf "POST resolve → id=%s\n" response.id;
      print_outcome response.outcome

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [] | [ "-h" ] | [ "--help" ] -> usage ()
  | owner :: repo :: n_raw :: rest -> (
      let pr_number =
        match int_of_string_opt n_raw with
        | Some n when n > 0 -> n
        | _ ->
            prerr_endline "invalid pull number";
            exit 2
      in
      let backend = backend_of_env () in
      Eio_main.run @@ fun env ->
      let net = Eio.Stdenv.net env in
      let clock = Eio.Stdenv.clock env in
      match rest with
      | [] ->
          do_list ~net ~clock ~backend ~owner ~repo ~pr_number ~_all:false ()
      | [ "--all" ] ->
          do_list ~net ~clock ~backend ~owner ~repo ~pr_number ~_all:true ()
      | "resolve" :: finding_id :: kind_str :: reason_parts -> (
          match parse_kind kind_str with
          | None ->
              prerr_endline "kind must be 'addressed' or 'wontfix'";
              exit 2
          | Some kind ->
              let reason =
                match reason_parts with
                | [] -> None
                | xs -> Some (String.concat " " xs)
              in
              do_resolve ~net ~clock ~backend ~owner ~repo ~pr_number
                ~finding_id ~kind ~reason ())
      | _ -> usage ())
  | _ -> usage ()

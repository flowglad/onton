module Unix_ = Unix
open Base

let backend_url =
  match Stdlib.Sys.getenv_opt "ONTON_DEBUG_URL" with
  | Some u -> u
  | None -> "https://debug.onton.dev/api/upload"

let max_response_size = 1_000_000
let printf fmt = Stdlib.Printf.printf fmt
let eprintf fmt = Stdlib.Printf.eprintf fmt

(** Read a file to string, returning [None] if it doesn't exist. *)
let read_file_opt path =
  if Stdlib.Sys.file_exists path then (
    let ic = Stdlib.open_in path in
    let n = Stdlib.in_channel_length ic in
    let s = Bytes.create n in
    Stdlib.really_input ic s 0 n;
    Stdlib.close_in ic;
    Some (Bytes.to_string s))
  else None

(** Replace known GitHub token patterns with [<REDACTED>]. *)
let scrub_token_patterns s =
  let re =
    Re.Pcre.re
      {|(ghp_[A-Za-z0-9_]+|gho_[A-Za-z0-9_]+|github_pat_[A-Za-z0-9_]+)|}
    |> Re.compile
  in
  Re.replace re ~f:(fun _ -> "<REDACTED>") s

(** Scrub config JSON: replace the github_token field value. *)
let scrub_config_json raw =
  match Yojson.Safe.from_string raw with
  | json ->
      let scrubbed =
        match json with
        | `Assoc fields ->
            `Assoc
              (List.map fields ~f:(fun (k, v) ->
                   if String.equal k "github_token" then
                     (k, `String "<REDACTED>")
                   else (k, v)))
        | other -> other
      in
      Yojson.Safe.to_string ~std:true scrubbed |> scrub_token_patterns
  | exception Yojson.Json_error _ -> scrub_token_patterns raw

let collect_files ~project_name =
  let pairs =
    [
      ("config.json", Project_store.config_path project_name);
      ("snapshot.json", Project_store.snapshot_path project_name);
      ("events.jsonl", Project_store.event_log_path project_name);
      ("gameplan", Project_store.stored_gameplan_path project_name);
    ]
  in
  List.filter_map pairs ~f:(fun (name, path) ->
      match read_file_opt path with
      | None -> None
      | Some contents ->
          let scrubbed =
            if String.equal name "config.json" then scrub_config_json contents
            else scrub_token_patterns contents
          in
          Some (name, scrubbed))

let build_bundle ~project_name ~version files =
  let file_assoc =
    List.map files ~f:(fun (name, contents) -> (name, `String contents))
  in
  `Assoc
    [
      ("version", `Int 1);
      ("project_name", `String project_name);
      ("onton_version", `String version);
      ( "timestamp",
        `String
          (let t = Unix_.gettimeofday () in
           let tm = Unix_.gmtime t in
           Stdlib.Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
             (1900 + tm.Unix_.tm_year) (1 + tm.Unix_.tm_mon) tm.Unix_.tm_mday
             tm.Unix_.tm_hour tm.Unix_.tm_min tm.Unix_.tm_sec) );
      ("files", `Assoc file_assoc);
    ]
  |> Yojson.Safe.to_string ~std:true

(** Set up TLS for HTTPS — same pattern as [Github]. *)
let https_config () =
  match Ca_certs.authenticator () with
  | Error (`Msg msg) -> Error ("TLS CA setup failed: " ^ msg)
  | Ok authenticator -> (
      match Tls.Config.client ~authenticator () with
      | Ok cfg -> Ok cfg
      | Error (`Msg msg) -> Error ("TLS config failed: " ^ msg))

let https_fun tls_config uri flow =
  let host =
    Uri.host uri
    |> Option.map ~f:(fun h -> Domain_name.(of_string_exn h |> host_exn))
  in
  (Tls_eio.client_of_flow tls_config ?host flow :> _ Eio.Flow.two_way)

let http_request ~net ~meth ~uri ~headers ~body =
  let parsed_uri = Uri.of_string uri in
  let is_https =
    match Uri.scheme parsed_uri with Some "http" -> false | _ -> true
  in
  let client_result =
    if is_https then (
      Mirage_crypto_rng_unix.use_default ();
      match https_config () with
      | Error msg -> Error msg
      | Ok tls_config ->
          Ok (Cohttp_eio.Client.make ~https:(Some (https_fun tls_config)) net))
    else Ok (Cohttp_eio.Client.make ~https:None net)
  in
  match client_result with
  | Error msg -> Error msg
  | Ok client ->
      let headers = Http.Header.of_list headers in
      Eio.Switch.run @@ fun sw ->
      let resp, resp_body =
        let body = Cohttp_eio.Body.of_string body in
        match meth with
        | `POST -> Cohttp_eio.Client.post client ~sw ~headers ~body parsed_uri
        | `PUT -> Cohttp_eio.Client.put client ~sw ~headers ~body parsed_uri
        | _ -> failwith "unsupported method"
      in
      let status = Http.Response.status resp |> Http.Status.to_int in
      let resp_str =
        Eio.Buf_read.(of_flow ~max_size:max_response_size resp_body |> take_all)
      in
      if status >= 200 && status < 300 then Ok resp_str
      else Error (Stdlib.Printf.sprintf "HTTP %d: %s" status resp_str)

let run ~net ~project_name ~version =
  printf "Collecting debug state for project %S...\n%!" project_name;
  let files = collect_files ~project_name in
  if List.is_empty files then (
    eprintf "Error: no state files found for project %S.\n%!" project_name;
    Stdlib.exit 1);
  let bundle = build_bundle ~project_name ~version files in
  let bundle_size = String.length bundle in
  printf "Bundle size: %s\n%!"
    (if bundle_size > 1_000_000 then
       Stdlib.Printf.sprintf "%.1f MB" (Float.of_int bundle_size /. 1_000_000.0)
     else Stdlib.Printf.sprintf "%d KB" (bundle_size / 1000));
  printf "Requesting upload URL...\n%!";
  let init_body =
    `Assoc
      [
        ("project_name", `String project_name);
        ("onton_version", `String version);
        ("content_length", `Int bundle_size);
      ]
    |> Yojson.Safe.to_string ~std:true
  in
  let upload_secret =
    match Stdlib.Sys.getenv_opt "ONTON_UPLOAD_SECRET" with
    | Some s -> s
    | None ->
        eprintf
          "Error: ONTON_UPLOAD_SECRET is not set. Set it to the shared upload \
           secret before running this command.\n\
           %!";
        Stdlib.exit 1
  in
  let headers =
    [ ("Content-Type", "application/json"); ("x-upload-secret", upload_secret) ]
  in
  match
    http_request ~net ~meth:`POST ~uri:backend_url ~headers ~body:init_body
  with
  | Error msg ->
      eprintf "Error requesting upload URL: %s\n%!" msg;
      Stdlib.exit 1
  | Ok resp_body -> (
      match Yojson.Safe.from_string resp_body with
      | `Assoc fields -> (
          let upload_url =
            List.Assoc.find fields ~equal:String.equal "upload_url"
          in
          let case_id = List.Assoc.find fields ~equal:String.equal "case_id" in
          match (upload_url, case_id) with
          | Some (`String url), Some (`String id) -> (
              printf "Uploading...\n%!";
              let upload_headers = [ ("Content-Type", "application/json") ] in
              match
                http_request ~net ~meth:`PUT ~uri:url ~headers:upload_headers
                  ~body:bundle
              with
              | Error msg ->
                  eprintf "Error uploading: %s\n%!" msg;
                  Stdlib.exit 1
              | Ok _ ->
                  printf "Upload complete.\n\n%!";
                  printf "Case ID: %s\n%!" id;
                  printf "Share this ID when reporting an issue.\n%!")
          | _ ->
              eprintf "Error: unexpected response format from upload API.\n%!";
              Stdlib.exit 1)
      | _ | (exception Yojson.Json_error _) ->
          eprintf "Error: could not parse upload API response.\n%!";
          Stdlib.exit 1)

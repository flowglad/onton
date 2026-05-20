module Unix_ = Unix
open Base

type manifest = {
  schema_version : int;
  onton_version : string;
  os : string;
  arch : string;
  timestamp : string;
  project_name : string;
  latest_subkinds_by_patch : (string * Failure_subkind.t) list;
  artifact_paths : string list;
}

type meta_summary = {
  patch_id : string;
  subkind : Failure_subkind.t;
  ended_at : string option;
  mtime : float;
}

type collected = {
  timestamp : string;
  files : (string * string) list;
  artifact_paths : string list;
  meta_summaries : meta_summary list;
}

let backend_url =
  match Stdlib.Sys.getenv_opt "ONTON_DEBUG_URL" with
  | Some u -> u
  | None -> "https://write-gameplan.dev/api/upload"

let max_response_size = 1_000_000
let printf fmt = Stdlib.Printf.printf fmt
let eprintf fmt = Stdlib.Printf.eprintf fmt

let iso8601_now () =
  let t = Unix_.gettimeofday () in
  let tm = Unix_.gmtime t in
  Stdlib.Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (1900 + tm.Unix_.tm_year) (1 + tm.Unix_.tm_mon) tm.Unix_.tm_mday
    tm.Unix_.tm_hour tm.Unix_.tm_min tm.Unix_.tm_sec

(** Read a file to string, returning [None] if it doesn't exist. *)
let read_file_opt path =
  if Stdlib.Sys.file_exists path then
    let ic = Stdlib.open_in path in
    Stdlib.Fun.protect
      ~finally:(fun () -> Stdlib.close_in_noerr ic)
      (fun () ->
        let n = Stdlib.in_channel_length ic in
        let s = Bytes.create n in
        Stdlib.really_input ic s 0 n;
        Some (Bytes.to_string s))
  else None

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
      Yojson.Safe.to_string ~std:true scrubbed
      |> Token_scrub.scrub_token_patterns
  | exception Yojson.Json_error _ -> Token_scrub.scrub_token_patterns raw

let scrub_env_contents raw =
  raw |> String.split_lines
  |> List.map ~f:Token_scrub.redact_env_entry
  |> String.concat ~sep:"\n"

let scrub_file_contents ~name raw =
  let first_pass =
    if String.equal name "config.json" then scrub_config_json raw
    else if String.equal (Stdlib.Filename.basename name) "env.txt" then
      scrub_env_contents raw
    else raw
  in
  Token_scrub.scrub_token_patterns first_pass

let rec walk_files ~root ~relative =
  if Stdlib.Sys.file_exists root then
    if Stdlib.Sys.is_directory root then
      Stdlib.Sys.readdir root |> Array.to_list
      |> List.sort ~compare:String.compare
      |> List.concat_map ~f:(fun entry ->
          let path = Stdlib.Filename.concat root entry in
          let next_relative =
            if String.is_empty relative then entry
            else Stdlib.Filename.concat relative entry
          in
          walk_files ~root:path ~relative:next_relative)
    else [ (relative, root) ]
  else []

let json_assoc_find name = function
  | `Assoc fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None

let extract_ended_at json =
  json_assoc_find "ended_at" json
  |> Option.bind ~f:(function `String ts -> Some ts | _ -> None)

let extract_patch_id json =
  json_assoc_find "patch_id" json
  |> Option.bind ~f:(function `String patch_id -> Some patch_id | _ -> None)

let extract_subkind json =
  match json_assoc_find "subkind" json with
  | Some value -> (
      match Failure_subkind.t_of_yojson value with
      | subkind -> Some subkind
      | exception _ -> None)
  | None -> None

let meta_summary_of_file ~path ~raw =
  match Yojson.Safe.from_string raw with
  | json -> (
      match (extract_patch_id json, extract_subkind json) with
      | Some patch_id, Some subkind ->
          let mtime =
            match (Unix_.stat path).st_mtime with
            | v -> v
            | exception Unix_.Unix_error _ -> 0.0
          in
          Some { patch_id; subkind; ended_at = extract_ended_at json; mtime }
      | _ -> None)
  | exception Yojson.Json_error _ -> None

let collect_base_files ~project_name =
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
      | Some contents -> Some (name, scrub_file_contents ~name contents))

let collect_session_files ~project_name =
  let session_root = Project_store.sessions_dir project_name in
  let files = walk_files ~root:session_root ~relative:"" in
  let folder name = Stdlib.Filename.concat "sessions" name in
  let files, artifact_paths, meta_summaries =
    List.fold files ~init:([], [], [])
      ~f:(fun (acc_files, acc_paths, acc_meta_summaries) (relative, path) ->
        match read_file_opt path with
        | None -> (acc_files, acc_paths, acc_meta_summaries)
        | Some raw ->
            let name = folder relative in
            let meta_summaries =
              if String.is_suffix relative ~suffix:"meta.json" then
                match meta_summary_of_file ~path ~raw with
                | Some summary -> summary :: acc_meta_summaries
                | None -> acc_meta_summaries
              else acc_meta_summaries
            in
            ( (name, scrub_file_contents ~name raw) :: acc_files,
              name :: acc_paths,
              meta_summaries ))
  in
  {
    timestamp = "";
    files = List.rev files;
    artifact_paths = List.rev artifact_paths;
    meta_summaries = List.rev meta_summaries;
  }

let is_more_recent left right =
  let left_ended_at = Option.value left.ended_at ~default:"" in
  let right_ended_at = Option.value right.ended_at ~default:"" in
  match String.compare left_ended_at right_ended_at with
  | 0 -> Float.compare left.mtime right.mtime > 0
  | cmp -> cmp > 0

let latest_subkinds_by_patch meta_summaries =
  List.fold meta_summaries
    ~init:(Map.empty (module String))
    ~f:(fun acc summary ->
      Map.update acc summary.patch_id ~f:(function
        | None -> summary
        | Some existing ->
            if is_more_recent summary existing then summary else existing))
  |> Map.to_alist
  |> List.map ~f:(fun (patch_id, summary) -> (patch_id, summary.subkind))

let detect_arch () = Int.to_string Stdlib.Sys.word_size ^ "-bit"

let manifest_to_yojson manifest =
  `Assoc
    [
      ("schema_version", `Int manifest.schema_version);
      ("onton_version", `String manifest.onton_version);
      ("os", `String manifest.os);
      ("arch", `String manifest.arch);
      ("timestamp", `String manifest.timestamp);
      ("project_name", `String manifest.project_name);
      ( "latest_subkinds_by_patch",
        `Assoc
          (List.map manifest.latest_subkinds_by_patch
             ~f:(fun (patch_id, subkind) ->
               (patch_id, Failure_subkind.yojson_of_t subkind))) );
      ( "artifact_paths",
        `List (List.map manifest.artifact_paths ~f:(fun path -> `String path))
      );
    ]

let build_manifest ~project_name ~version ~timestamp ~artifact_paths
    ~meta_summaries =
  {
    schema_version = 1;
    onton_version = version;
    os = Stdlib.Sys.os_type;
    arch = detect_arch ();
    timestamp;
    project_name;
    latest_subkinds_by_patch = latest_subkinds_by_patch meta_summaries;
    artifact_paths;
  }

let collect_files ~project_name ~version =
  let base_files = collect_base_files ~project_name in
  let session_files = collect_session_files ~project_name in
  let timestamp = iso8601_now () in
  let manifest =
    build_manifest ~project_name ~version ~timestamp
      ~artifact_paths:session_files.artifact_paths
      ~meta_summaries:session_files.meta_summaries
  in
  let manifest_json =
    manifest_to_yojson manifest
    |> Yojson.Safe.to_string ~std:true
    |> Token_scrub.scrub_token_patterns
  in
  {
    timestamp;
    files =
      ("manifest.json", manifest_json) :: (base_files @ session_files.files);
    artifact_paths = session_files.artifact_paths;
    meta_summaries = session_files.meta_summaries;
  }

let build_bundle_from_collected ~project_name ~version ~collected =
  let file_assoc =
    List.map collected.files ~f:(fun (name, contents) ->
        (name, `String contents))
  in
  `Assoc
    [
      ("version", `Int 1);
      ("project_name", `String project_name);
      ("onton_version", `String version);
      ("timestamp", `String collected.timestamp);
      ("files", `Assoc file_assoc);
    ]
  |> Yojson.Safe.to_string ~std:true

let build_bundle ~project_name ~version =
  let collected = collect_files ~project_name ~version in
  build_bundle_from_collected ~project_name ~version ~collected

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
  | Ok client -> (
      let headers = Http.Header.of_list headers in
      try
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
          Eio.Buf_read.(
            of_flow ~max_size:max_response_size resp_body |> take_all)
        in
        if status >= 200 && status < 300 then Ok resp_str
        else Error (Stdlib.Printf.sprintf "HTTP %d: %s" status resp_str)
      with
      | Failure msg -> Error (Stdlib.Printf.sprintf "request failed: %s" msg)
      | exn ->
          Error
            (Stdlib.Printf.sprintf "request failed: %s"
               (Stdlib.Printexc.to_string exn)))

let run ~net ~project_name ~version =
  printf "Collecting debug state for project %S...\n%!" project_name;
  let collected = collect_files ~project_name ~version in
  let files_without_manifest =
    List.filter collected.files ~f:(fun (name, _) ->
        not (String.equal name "manifest.json"))
  in
  if List.is_empty files_without_manifest then (
    eprintf "Error: no state files found for project %S.\n%!" project_name;
    Stdlib.exit 1);
  let bundle = build_bundle_from_collected ~project_name ~version ~collected in
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
  let headers = [ ("Content-Type", "application/json") ] in
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

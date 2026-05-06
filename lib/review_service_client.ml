type error =
  | Auth_error of Jwt.error
  | Transport_error of { meth : string; url : string; msg : string }
  | Http_error of {
      meth : string;
      url : string;
      status : int;
      body : string;
      server_error : string option;
    }
  | Parse_error of string

let show_error = function
  | Auth_error e -> Jwt.show_error e
  | Transport_error { meth; url; msg } ->
      Printf.sprintf "review-service %s %s — transport error: %s" meth url msg
  | Http_error { meth; url; status; body; server_error } ->
      let suffix =
        match server_error with
        | Some s -> Printf.sprintf " — server says: %s" s
        | None when String.length body > 0 -> Printf.sprintf " — body: %s" body
        | None -> ""
      in
      Printf.sprintf "review-service %s %s — HTTP %d%s" meth url status suffix
  | Parse_error msg -> Printf.sprintf "review-service: parse error: %s" msg

let max_response_size = 1_000_000

let https_config () =
  match Ca_certs.authenticator () with
  | Error (`Msg msg) -> Error ("TLS CA setup failed: " ^ msg)
  | Ok authenticator -> (
      match Tls.Config.client ~authenticator () with
      | Ok cfg -> Ok cfg
      | Error (`Msg msg) -> Error ("TLS config failed: " ^ msg))

let cached_https_config = lazy (https_config ())

let https_fun tls_config uri flow =
  let host =
    match Uri.host uri with
    | None -> None
    | Some h -> (
        try Some Domain_name.(of_string_exn h |> host_exn) with _ -> None)
  in
  (Tls_eio.client_of_flow tls_config ?host flow :> _ Eio.Flow.two_way)

let backend_base_url (b : Onton_core.Review_backend.t) =
  match b.kind with
  | Onton_core.Review_backend.Review_service { base_url; _ } -> base_url

let backend_auth (b : Onton_core.Review_backend.t) =
  match b.kind with
  | Onton_core.Review_backend.Review_service { auth; _ } -> auth

let mint_token ~clock backend : (string, error) Result.t =
  let auth = backend_auth backend in
  let now = Eio.Time.now clock in
  match
    Jwt.mint ~now ~app_id:auth.app_id ~private_key_path:auth.private_key_path
      ~ttl_seconds:120
  with
  | Ok t -> Ok t
  | Error e -> Error (Auth_error e)

let meth_to_string = function `GET -> "GET" | `POST -> "POST"

let request ~net ~clock ~backend ~meth ~path ?body () : (string, error) Result.t
    =
  let meth_s = meth_to_string meth in
  let url = backend_base_url backend ^ path in
  match mint_token ~clock backend with
  | Error e -> Error e
  | Ok token -> (
      try
        match
          Result.map_error
            (fun msg -> Transport_error { meth = meth_s; url; msg })
            (Lazy.force cached_https_config)
        with
        | Error _ as e -> e
        | Ok tls_config -> (
            match
              Eio.Time.with_timeout clock 30.0 (fun () ->
                  Ok
                    (let client =
                       Cohttp_eio.Client.make
                         ~https:(Some (https_fun tls_config))
                         net
                     in
                     let uri = Uri.of_string url in
                     let headers =
                       Http.Header.of_list
                         [
                           ("Authorization", "Bearer " ^ token);
                           ("Content-Type", "application/json");
                           ("Accept", "application/json");
                           ("User-Agent", "onton/0.1.0");
                         ]
                     in
                     Eio.Switch.run @@ fun sw ->
                     let resp, resp_body =
                       match meth with
                       | `GET -> Cohttp_eio.Client.get client ~sw ~headers uri
                       | `POST ->
                           let body =
                             Cohttp_eio.Body.of_string
                               (Option.value body ~default:"{}")
                           in
                           Cohttp_eio.Client.post client ~sw ~headers ~body uri
                     in
                     let status =
                       Http.Response.status resp |> Http.Status.to_int
                     in
                     let resp_str =
                       Eio.Buf_read.(
                         of_flow ~max_size:max_response_size resp_body
                         |> take_all)
                     in
                     if status >= 200 && status < 300 then Ok resp_str
                     else
                       let server_error =
                         Onton_core.Review_service.parse_error_message resp_str
                       in
                       Error
                         (Http_error
                            {
                              meth = meth_s;
                              url;
                              status;
                              body = resp_str;
                              server_error;
                            })))
            with
            | Ok result -> result
            | Error `Timeout ->
                Error
                  (Transport_error
                     { meth = meth_s; url; msg = "request timed out" }))
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
          Error
            (Transport_error
               { meth = meth_s; url; msg = Printexc.to_string exn }))

let url_encode s = Uri.pct_encode ~component:`Path s

let findings_path ~owner ~repo ~pr_number =
  Printf.sprintf "/prs/%s/%s/%d/findings?status=unresolved" (url_encode owner)
    (url_encode repo) pr_number

let resolve_path ~owner ~repo ~pr_number ~finding_id =
  Printf.sprintf "/prs/%s/%s/%d/findings/%s/resolve" (url_encode owner)
    (url_encode repo) pr_number (url_encode finding_id)

let list_findings ~net ~clock ~backend ~owner ~repo ~pr_number () =
  match
    request ~net ~clock ~backend ~meth:`GET
      ~path:(findings_path ~owner ~repo ~pr_number)
      ()
  with
  | Error _ as e -> e
  | Ok body -> (
      match Onton_core.Review_service.parse_findings_response_string body with
      | Ok r -> Ok r
      | Error msg -> Error (Parse_error msg))

let mark_resolved ~net ~clock ~backend ~owner ~repo ~pr_number ~finding_id ~kind
    ?actor ?reason () =
  let req : Onton_core.Review_service.resolve_request =
    { kind; actor; reason }
  in
  let body =
    Yojson.Safe.to_string
      (Onton_core.Review_service.resolve_request_to_yojson req)
  in
  match
    request ~net ~clock ~backend ~meth:`POST
      ~path:(resolve_path ~owner ~repo ~pr_number ~finding_id)
      ~body ()
  with
  | Error _ as e -> e
  | Ok body -> (
      match Onton_core.Review_service.parse_resolve_response_string body with
      | Ok r -> Ok r
      | Error msg -> Error (Parse_error msg))

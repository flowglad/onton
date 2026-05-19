open Base

type t =
  | Ok
  | Auth_unavailable
  | Api_error of { status : int option }
  | Network_error
  | Timed_out
  | No_session_to_resume
  | Empty_response
  | Process_error
  | Other of string
[@@deriving show, eq, sexp_of]

type init_info = {
  api_key_source : string option;
  model : string option;
  claude_code_version : string option;
}
[@@deriving show, eq]

let t_of_yojson json =
  let open Yojson.Safe.Util in
  match json with
  | `String "Ok" -> (Ok : t)
  | `String "Auth_unavailable" -> Auth_unavailable
  | `String "Network_error" -> Network_error
  | `String "Timed_out" -> Timed_out
  | `String "No_session_to_resume" -> No_session_to_resume
  | `String "Empty_response" -> Empty_response
  | `String "Process_error" -> Process_error
  | `Assoc [ ("Api_error", payload) ] ->
      let status = payload |> member "status" |> to_int_option in
      Api_error { status }
  | `Assoc [ ("Other", `String detail) ] -> Other detail
  | _ -> Other "invalid_failure_subkind"

let yojson_of_t = function
  | (Ok : t) -> `String "Ok"
  | Auth_unavailable -> `String "Auth_unavailable"
  | Api_error { status } ->
      let fields =
        match status with
        | None -> []
        | Some status -> [ ("status", `Int status) ]
      in
      `Assoc [ ("Api_error", `Assoc fields) ]
  | Network_error -> `String "Network_error"
  | Timed_out -> `String "Timed_out"
  | No_session_to_resume -> `String "No_session_to_resume"
  | Empty_response -> `String "Empty_response"
  | Process_error -> `String "Process_error"
  | Other detail -> `Assoc [ ("Other", `String detail) ]

let init_info_of_yojson json =
  let open Yojson.Safe.Util in
  let read_opt_string field =
    match member field json with
    | `Null -> None
    | value -> to_string_option value
  in
  {
    api_key_source = read_opt_string "api_key_source";
    model = read_opt_string "model";
    claude_code_version = read_opt_string "claude_code_version";
  }

let yojson_of_init_info init =
  let opt_field name = function
    | None -> []
    | Some value -> [ (name, `String value) ]
  in
  `Assoc
    (opt_field "api_key_source" init.api_key_source
    @ opt_field "model" init.model
    @ opt_field "claude_code_version" init.claude_code_version)

let contains haystack needle = String.is_substring haystack ~substring:needle

let mentions_not_logged_in text_tail =
  contains text_tail "Not logged in" || contains text_tail "Please run /login"

let api_key_source_none init =
  Option.value_map init.api_key_source ~default:false ~f:(String.equal "none")

let api_error_re = Re.Perl.compile_pat "API Error: ([0-9]{3})"

let api_error_status stderr_tail =
  match Re.exec_opt api_error_re stderr_tail with
  | None -> None
  | Some groups ->
      Re.Group.get_opt groups 1 |> Option.bind ~f:Stdlib.int_of_string_opt

let mentions_network_failure stderr_tail =
  List.exists [ "getaddrinfo"; "EAI_AGAIN"; "ECONNREFUSED"; "TLS handshake" ]
    ~f:(fun needle -> contains stderr_tail needle)

let classify_session_failed ~init ~text_tail ~stderr_tail ~detail =
  if mentions_not_logged_in text_tail then Auth_unavailable
  else if
    api_key_source_none init && String.is_empty text_tail
    && String.is_empty stderr_tail
  then Auth_unavailable
  else
    match api_error_status stderr_tail with
    | Some status -> Api_error { status = Some status }
    | None when mentions_network_failure stderr_tail -> Network_error
    | None when String.is_empty text_tail && String.is_empty stderr_tail ->
        if String.is_empty detail || String.equal detail "(no error details)"
        then Empty_response
        else Other detail
    | None -> Other detail

let classify ~classification ~init ~text_tail ~stderr_tail =
  match classification with
  | Run_classification.Process_error _ -> Process_error
  | No_session_to_resume -> No_session_to_resume
  | Timed_out -> Timed_out
  | Success _ -> Ok
  | Session_failed { detail; _ } ->
      classify_session_failed ~init ~text_tail ~stderr_tail ~detail

let to_string = function
  | (Ok : t) -> "ok"
  | Auth_unavailable -> "auth_unavailable"
  | Api_error { status = Some status } -> "api_error_" ^ Int.to_string status
  | Api_error { status = None } -> "api_error"
  | Network_error -> "network_error"
  | Timed_out -> "timed_out"
  | No_session_to_resume -> "no_session_to_resume"
  | Empty_response -> "empty_response"
  | Process_error -> "process_error"
  | Other _ -> "other"

let default_init =
  { api_key_source = None; model = None; claude_code_version = None }

let%test "Auth_unavailable from Not logged in text" =
  equal
    (classify
       ~classification:
         (Run_classification.Session_failed { exit_code = 1; detail = "detail" })
       ~init:default_init ~text_tail:"Not logged in to Claude Code"
       ~stderr_tail:"")
    Auth_unavailable

let%test "Auth_unavailable from api_key_source none + no Final_result" =
  equal
    (classify
       ~classification:
         (Run_classification.Session_failed { exit_code = 1; detail = "detail" })
       ~init:{ default_init with api_key_source = Some "none" }
       ~text_tail:"" ~stderr_tail:"")
    Auth_unavailable

let%test "api_key_source none does not mask API errors" =
  equal
    (classify
       ~classification:
         (Run_classification.Session_failed { exit_code = 1; detail = "detail" })
       ~init:{ default_init with api_key_source = Some "none" }
       ~text_tail:"" ~stderr_tail:"API Error: 401 Unauthorized")
    (Api_error { status = Some 401 })

let%test "Api_error 401 from stderr" =
  equal
    (classify
       ~classification:
         (Run_classification.Session_failed { exit_code = 1; detail = "detail" })
       ~init:default_init ~text_tail:""
       ~stderr_tail:"API Error: 401 Unauthorized")
    (Api_error { status = Some 401 })

let%test "Network_error from getaddrinfo stderr" =
  equal
    (classify
       ~classification:
         (Run_classification.Session_failed { exit_code = 1; detail = "detail" })
       ~init:default_init ~text_tail:""
       ~stderr_tail:"socket error: getaddrinfo EAI_AGAIN")
    Network_error

let%test "Empty_response when text and stderr are empty" =
  equal
    (classify
       ~classification:
         (Run_classification.Session_failed
            { exit_code = 1; detail = "(no error details)" })
       ~init:default_init ~text_tail:"" ~stderr_tail:"")
    Empty_response

let%test "non-empty detail with empty tails falls through to Other" =
  equal
    (classify
       ~classification:
         (Run_classification.Session_failed
            { exit_code = 1; detail = "exit 1 with explanation" })
       ~init:default_init ~text_tail:"" ~stderr_tail:"")
    (Other "exit 1 with explanation")

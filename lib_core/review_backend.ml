open Base

type review_service_auth = { app_id : string; private_key_path : string }

type kind =
  | Review_service of { base_url : string; auth : review_service_auth }

type t = { name : string; kind : kind }

let known_kind_default = [ "review-service" ]

let string_field field json : (string, string) Result.t =
  let open Yojson.Safe.Util in
  match json |> member field with
  | `String s when not (String.is_empty (String.strip s)) -> Ok (String.strip s)
  | `String _ -> Error (Printf.sprintf "%S must be a non-empty string" field)
  | `Null -> Error (Printf.sprintf "missing required field %S" field)
  | _ -> Error (Printf.sprintf "%S must be a string" field)

let strip_trailing_slashes s =
  let len = String.length s in
  let rec last i =
    if i <= 0 then i
    else if Char.equal (String.get s (i - 1)) '/' then last (i - 1)
    else i
  in
  let l = last len in
  if l = len then s else String.sub s ~pos:0 ~len:l

let has_authority base_url =
  match String.substr_index base_url ~pattern:"://" with
  | None -> false
  | Some scheme_sep ->
      let len = String.length base_url in
      let start = scheme_sep + 3 in
      let rec authority_end i =
        if i >= len then i
        else
          match String.get base_url i with
          | '/' | '?' | '#' -> i
          | _ -> authority_end (i + 1)
      in
      let stop = authority_end start in
      stop > start
      && String.exists
           (String.sub base_url ~pos:start ~len:(stop - start))
           ~f:(fun c -> not (Char.equal c ':'))

let parse_review_service_kind json : (kind, string) Result.t =
  let open Yojson.Safe.Util in
  let ( let* ) = Result.( >>= ) in
  let* base_url = string_field "baseUrl" json in
  let base_url = strip_trailing_slashes base_url in
  let* () =
    if String.is_empty base_url then
      Error "review-service \"baseUrl\" must not be empty"
    else if not (has_authority base_url) then
      Error "review-service \"baseUrl\" must include a host/authority"
    else Ok ()
  in
  let auth_json = json |> member "auth" in
  let* () =
    match auth_json with
    | `Assoc _ -> Ok ()
    | `Null -> Error "review-service backend missing required field \"auth\""
    | _ -> Error "review-service \"auth\" must be an object"
  in
  let* app_id = string_field "appId" auth_json in
  let* private_key_path = string_field "privateKeyPath" auth_json in
  Ok (Review_service { base_url; auth = { app_id; private_key_path } })

let parse ~known_kinds json : (t, string) Result.t =
  let ( let* ) = Result.( >>= ) in
  match json with
  | `Assoc _ ->
      let* name = string_field "name" json in
      let* kind_str = string_field "kind" json in
      let* () =
        if List.mem known_kinds kind_str ~equal:String.equal then Ok ()
        else
          Error
            (Printf.sprintf
               "reviewBackends[*].kind = %S is not a known kind (expected one \
                of: %s)"
               kind_str
               (String.concat ~sep:", " known_kinds))
      in
      let* kind =
        match kind_str with
        | "review-service" -> parse_review_service_kind json
        | other ->
            (* Defensive — [known_kinds] is the authoritative gate, but if a
               new kind is added to the gate without a parser, fail loud
               instead of silently constructing garbage. *)
            Error
              (Printf.sprintf "reviewBackends[*].kind = %S has no parser" other)
      in
      Ok { name; kind }
  | _ -> Error "reviewBackends[*] must be an object"

let parse_array ~known_kinds json : (t list, string) Result.t =
  match json with
  | `Null -> Ok []
  | `List entries ->
      let rec loop acc seen = function
        | [] -> Ok (List.rev acc)
        | entry :: rest -> (
            match parse ~known_kinds entry with
            | Error _ as e -> e
            | Ok t ->
                if Set.mem seen t.name then
                  Error
                    (Printf.sprintf "reviewBackends contains duplicate name %S"
                       t.name)
                else loop (t :: acc) (Set.add seen t.name) rest)
      in
      loop [] (Set.empty (module String)) entries
  | _ -> Error "reviewBackends must be an array"

let%test "parse_array: null -> empty" =
  match parse_array ~known_kinds:known_kind_default `Null with
  | Ok [] -> true
  | Ok _ -> false
  | Error _ -> false

let%test "parse_array: full review-service entry" =
  let raw =
    {|[{"name":"primary","kind":"review-service","baseUrl":"https://review.example.com/","auth":{"appId":"123","privateKeyPath":"/etc/onton/key.pem"}}]|}
  in
  match
    parse_array ~known_kinds:known_kind_default (Yojson.Safe.from_string raw)
  with
  | Ok [ t ] -> (
      match t.kind with
      | Review_service { base_url; auth } ->
          String.equal t.name "primary"
          && String.equal base_url "https://review.example.com"
          && String.equal auth.app_id "123"
          && String.equal auth.private_key_path "/etc/onton/key.pem")
  | Ok _ -> false
  | Error _ -> false

let%test "parse_array: trailing slash stripped" =
  let raw =
    {|[{"name":"a","kind":"review-service","baseUrl":"https://x///","auth":{"appId":"1","privateKeyPath":"/k"}}]|}
  in
  match
    parse_array ~known_kinds:known_kind_default (Yojson.Safe.from_string raw)
  with
  | Ok [ t ] -> (
      match t.kind with
      | Review_service { base_url; _ } -> String.equal base_url "https://x")
  | Ok _ -> false
  | Error _ -> false

let%test "parse_array: rejects all-slash baseUrl" =
  let raw =
    {|[{"name":"a","kind":"review-service","baseUrl":"///","auth":{"appId":"1","privateKeyPath":"/k"}}]|}
  in
  match
    parse_array ~known_kinds:known_kind_default (Yojson.Safe.from_string raw)
  with
  | Error msg -> String.is_substring msg ~substring:"baseUrl"
  | Ok _ -> false

let%test "parse_array: rejects scheme-only baseUrl" =
  let raw =
    {|[{"name":"a","kind":"review-service","baseUrl":"https://","auth":{"appId":"1","privateKeyPath":"/k"}}]|}
  in
  match
    parse_array ~known_kinds:known_kind_default (Yojson.Safe.from_string raw)
  with
  | Error msg -> String.is_substring msg ~substring:"host/authority"
  | Ok _ -> false

let%test "parse_array: rejects unknown kind" =
  let raw =
    {|[{"name":"x","kind":"sonarqube","baseUrl":"https://y","auth":{"appId":"1","privateKeyPath":"/k"}}]|}
  in
  match
    parse_array ~known_kinds:known_kind_default (Yojson.Safe.from_string raw)
  with
  | Error msg -> String.is_substring msg ~substring:"sonarqube"
  | Ok _ -> false

let%test "parse_array: rejects duplicate names" =
  let raw =
    {|[
       {"name":"a","kind":"review-service","baseUrl":"https://x","auth":{"appId":"1","privateKeyPath":"/k"}},
       {"name":"a","kind":"review-service","baseUrl":"https://y","auth":{"appId":"2","privateKeyPath":"/l"}}
     ]|}
  in
  match
    parse_array ~known_kinds:known_kind_default (Yojson.Safe.from_string raw)
  with
  | Error msg -> String.is_substring msg ~substring:"duplicate"
  | Ok _ -> false

let%test "parse_array: missing auth -> Error" =
  let raw = {|[{"name":"a","kind":"review-service","baseUrl":"https://x"}]|} in
  match
    parse_array ~known_kinds:known_kind_default (Yojson.Safe.from_string raw)
  with
  | Error msg -> String.is_substring msg ~substring:"auth"
  | Ok _ -> false

let%test "parse_array: missing privateKeyPath -> Error" =
  let raw =
    {|[{"name":"a","kind":"review-service","baseUrl":"https://x","auth":{"appId":"1"}}]|}
  in
  match
    parse_array ~known_kinds:known_kind_default (Yojson.Safe.from_string raw)
  with
  | Error msg -> String.is_substring msg ~substring:"privateKeyPath"
  | Ok _ -> false

(* @archlint.module shell
   @archlint.domain json *)

open Base

type route = { backend : string; model : string option }

type t = {
  default_backend : string option;
  default_model : string option;
  review_team : string option;
  complexity_routes : (int * route) list;
  review_backends : Review_backend.t list;
}

let empty =
  {
    default_backend = None;
    default_model = None;
    review_team = None;
    complexity_routes = [];
    review_backends = [];
  }

let config_path ~config_dir = Stdlib.Filename.concat config_dir "config.json"

let parse_route ~known_backends ~complexity (json : Yojson.Safe.t) :
    (route, string) Result.t =
  match json with
  | `Assoc _ ->
      let backend_result =
        match Json.field "backend" json with
        | None -> Ok None
        | Some (`String s) when String.is_empty (String.strip s) -> Ok None
        | Some (`String s) -> Ok (Some (String.strip s))
        | Some _ ->
            Error
              (Printf.sprintf "routing.%d.backend must be a string" complexity)
      in
      let model_result =
        match Json.field "model" json with
        | None -> Ok None
        | Some (`String s) when String.is_empty (String.strip s) -> Ok None
        | Some (`String s) -> Ok (Some (String.strip s))
        | Some _ ->
            Error
              (Printf.sprintf "routing.%d.model must be a string when present"
                 complexity)
      in
      Result.bind backend_result ~f:(fun backend ->
          Result.bind model_result ~f:(fun model ->
              match backend with
              | None ->
                  Error
                    (Printf.sprintf
                       "routing.%d: missing required string field \"backend\""
                       complexity)
              | Some name
                when not (List.mem known_backends name ~equal:String.equal) ->
                  Error
                    (Printf.sprintf
                       "routing.%d.backend = %S is not a known backend \
                        (expected one of: %s)"
                       complexity name
                       (String.concat ~sep:", " known_backends))
              | Some name -> Ok { backend = name; model }))
  | _ ->
      Error
        (Printf.sprintf "routing.%d must be an object {backend, model}"
           complexity)

let parse_routing ~known_backends (json : Yojson.Safe.t) :
    ((int * route) list, string) Result.t =
  match json with
  | `Null -> Ok []
  | `Assoc fields ->
      List.fold_result fields ~init:[] ~f:(fun acc (key, value) ->
          match Stdlib.int_of_string_opt key with
          | None ->
              Error
                (Printf.sprintf
                   "routing key %S is not an integer (expected 1, 2, or 3)" key)
          | Some k when k < 1 || k > 3 ->
              Error
                (Printf.sprintf
                   "routing.%d is out of range (complexity must be 1, 2, or 3)"
                   k)
          | Some k ->
              if List.Assoc.mem acc k ~equal:Int.equal then
                Error
                  (Printf.sprintf
                     "routing.%d is duplicated; each complexity may be \
                      configured at most once"
                     k)
              else
                Result.map (parse_route ~known_backends ~complexity:k value)
                  ~f:(fun route -> (k, route) :: acc))
      |> Result.map ~f:List.rev
  | _ ->
      Error
        ("routing must be an object mapping complexity -> {backend, model};"
       ^ " got " ^ Yojson.Safe.to_string json)

let default_known_review_kinds = [ "review-service" ]

let parse_default ~known_backends (json : Yojson.Safe.t) :
    (string option * string option, string) Result.t =
  match json with
  | `Null -> Ok (None, None)
  | `Assoc _ ->
      let backend_result =
        match Json.field "backend" json with
        | None -> Ok None
        | Some (`String s) when String.is_empty (String.strip s) -> Ok None
        | Some (`String s) ->
            let name = String.strip s in
            if List.mem known_backends name ~equal:String.equal then
              Ok (Some name)
            else
              Error
                (Printf.sprintf
                   "default.backend = %S is not a known backend (expected one \
                    of: %s)"
                   name
                   (String.concat ~sep:", " known_backends))
        | Some _ -> Error "default.backend must be a string"
      in
      let model_result =
        match Json.field "model" json with
        | None -> Ok None
        | Some (`String s) when String.is_empty (String.strip s) -> Ok None
        | Some (`String s) -> Ok (Some (String.strip s))
        | Some _ -> Error "default.model must be a string when present"
      in
      Result.bind backend_result ~f:(fun b ->
          Result.map model_result ~f:(fun m -> (b, m)))
  | _ -> Error "default must be an object {backend, model}"

let parse_string ~known_backends
    ?(known_review_kinds = default_known_review_kinds) (raw : string) :
    (t, string) Result.t =
  match Yojson.Safe.from_string raw with
  | exception Yojson.Json_error msg ->
      Error (Printf.sprintf "config.json: %s" msg)
  | json -> (
      match json with
      | `Assoc _ ->
          let default_json =
            Option.value (Json.field "default" json) ~default:`Null
          in
          let routing =
            Option.value (Json.field "routing" json) ~default:`Null
          in
          let review_backends_json =
            Option.value (Json.field "reviewBackends" json) ~default:`Null
          in
          let review_team_result =
            match Json.field "review_team" json with
            | None -> Ok None
            | Some (`String s) when String.is_empty (String.strip s) -> Ok None
            | Some (`String s) -> Ok (Some (String.strip s))
            | Some _ -> Error "review_team must be a string"
          in
          Result.bind (parse_default ~known_backends default_json)
            ~f:(fun (default_backend, default_model) ->
              Result.bind review_team_result ~f:(fun review_team ->
                  Result.bind (parse_routing ~known_backends routing)
                    ~f:(fun routes ->
                      Result.map
                        (Review_backend.parse_array
                           ~known_kinds:known_review_kinds review_backends_json)
                        ~f:(fun review_backends ->
                          {
                            default_backend;
                            default_model;
                            review_team;
                            complexity_routes = routes;
                            review_backends;
                          }))))
      | _ -> Error "config.json: top-level value must be an object")

let load ~config_dir ~known_backends ?known_review_kinds () =
  let path = config_path ~config_dir in
  if not (Stdlib.Sys.file_exists path) then Ok empty
  else
    try
      let ic = Stdlib.In_channel.open_text path in
      let raw =
        Exn.protect
          ~finally:(fun () -> Stdlib.In_channel.close ic)
          ~f:(fun () -> Stdlib.In_channel.input_all ic)
      in
      parse_string ~known_backends ?known_review_kinds raw
    with Sys_error msg ->
      Error (Printf.sprintf "config.json: cannot read: %s" msg)

let route_for_complexity (t : t) ~complexity =
  match complexity with
  | None -> None
  | Some k -> List.Assoc.find t.complexity_routes k ~equal:Int.equal

let%test "empty config has no routes" =
  Option.is_none (route_for_complexity empty ~complexity:(Some 1))

let%test "parse_string: missing routing -> empty" =
  match parse_string ~known_backends:[ "claude" ] "{}" with
  | Ok t -> List.is_empty t.complexity_routes
  | Error _ -> false

let%test "parse_string: unknown top-level keys are ignored (forward compat)" =
  let raw = {|{"futureKey":{"foo":"bar"}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Ok t -> List.is_empty t.complexity_routes
  | Error _ -> false

let%test "parse_string: full routing parses" =
  let raw =
    {|{"routing":{"1":{"backend":"claude","model":"haiku"},"3":{"backend":"codex","model":"gpt-5.5"}}}|}
  in
  match parse_string ~known_backends:[ "claude"; "codex"; "gemini" ] raw with
  | Ok t -> (
      let r1 = route_for_complexity t ~complexity:(Some 1) in
      let r3 = route_for_complexity t ~complexity:(Some 3) in
      let r2 = route_for_complexity t ~complexity:(Some 2) in
      Option.is_none r2
      && (match r1 with
        | Some { backend = "claude"; model = Some "haiku" } -> true
        | _ -> false)
      &&
      match r3 with
      | Some { backend = "codex"; model = Some "gpt-5.5" } -> true
      | _ -> false)
  | Error _ -> false

let%test "parse_string: model omitted -> None" =
  let raw = {|{"routing":{"2":{"backend":"claude"}}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Ok t -> (
      match route_for_complexity t ~complexity:(Some 2) with
      | Some { backend = "claude"; model = None } -> true
      | _ -> false)
  | Error _ -> false

let%test "parse_string: unknown backend rejected" =
  let raw = {|{"routing":{"1":{"backend":"made-up"}}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Error msg -> String.is_substring msg ~substring:"made-up"
  | Ok _ -> false

let%test "parse_string: out-of-range complexity rejected" =
  let raw = {|{"routing":{"4":{"backend":"claude"}}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Error msg -> String.is_substring msg ~substring:"out of range"
  | Ok _ -> false

let%test "parse_string: non-int key rejected" =
  let raw = {|{"routing":{"high":{"backend":"claude"}}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Error msg -> String.is_substring msg ~substring:"high"
  | Ok _ -> false

let%test "parse_string: missing backend rejected" =
  let raw = {|{"routing":{"1":{"model":"haiku"}}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Error msg -> String.is_substring msg ~substring:"backend"
  | Ok _ -> false

let%test "parse_string: malformed json -> Error" =
  match parse_string ~known_backends:[ "claude" ] "{not json}" with
  | Error _ -> true
  | Ok _ -> false

let%test "route_for_complexity: None complexity -> None route" =
  let raw = {|{"routing":{"1":{"backend":"claude"}}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Ok t -> Option.is_none (route_for_complexity t ~complexity:None)
  | Error _ -> false

let%test
    "parse_string: non-string backend rejected with type error (not 'missing')"
    =
  let raw = {|{"routing":{"1":{"backend":123}}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Error msg -> String.is_substring msg ~substring:"backend must be a string"
  | Ok _ -> false

let%test "parse_string: non-string model rejected" =
  let raw = {|{"routing":{"1":{"backend":"claude","model":123}}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Error msg -> String.is_substring msg ~substring:"model must be a string"
  | Ok _ -> false

let%test "parse_string: model whitespace is trimmed" =
  let raw = {|{"routing":{"1":{"backend":"claude","model":"  haiku\t"}}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Ok t -> (
      match route_for_complexity t ~complexity:(Some 1) with
      | Some { backend = "claude"; model = Some "haiku" } -> true
      | _ -> false)
  | Error _ -> false

let%test "parse_string: duplicate complexity keys rejected" =
  let raw =
    {|{"routing":{"1":{"backend":"claude"},"1":{"backend":"codex"}}}|}
  in
  match parse_string ~known_backends:[ "claude"; "codex" ] raw with
  | Error msg -> String.is_substring msg ~substring:"duplicated"
  | Ok _ -> false

let%test "parse_string: reviewBackends parse alongside routing" =
  let raw =
    {|{
       "routing":{"1":{"backend":"claude"}},
       "reviewBackends":[
         {"name":"primary","kind":"review-service","baseUrl":"https://r.example.com","auth":{"appId":"1","privateKeyPath":"/k"}}
       ]
     }|}
  in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Ok t ->
      List.length t.complexity_routes = 1
      && List.length t.review_backends = 1
      && String.equal (List.hd_exn t.review_backends).name "primary"
  | Error _ -> false

let%test "parse_string: reviewBackends absent -> empty list" =
  let raw = {|{"routing":{"1":{"backend":"claude"}}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Ok t -> List.is_empty t.review_backends
  | Error _ -> false

let%test "parse_string: review_team absent -> None" =
  let raw = {|{"routing":{"1":{"backend":"claude"}}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Ok t -> Option.is_none t.review_team
  | Error _ -> false

let%test "parse_string: review_team present -> parsed" =
  let raw =
    {|{"review_team":"platform","routing":{"1":{"backend":"claude"}}}|}
  in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Ok t -> Option.equal String.equal t.review_team (Some "platform")
  | Error _ -> false

let%test "parse_string: reviewBackends-only config" =
  let raw =
    {|{"reviewBackends":[{"name":"a","kind":"review-service","baseUrl":"https://x","auth":{"appId":"1","privateKeyPath":"/k"}}]}|}
  in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Ok t ->
      List.is_empty t.complexity_routes && List.length t.review_backends = 1
  | Error _ -> false

let%test "parse_string: invalid reviewBackends propagates as Error" =
  let raw = {|{"reviewBackends":[{"name":"a","kind":"unknown"}]}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Error msg -> String.is_substring msg ~substring:"unknown"
  | Ok _ -> false

let%test "parse_string: default absent -> both None" =
  match parse_string ~known_backends:[ "claude" ] "{}" with
  | Ok t -> Option.is_none t.default_backend && Option.is_none t.default_model
  | Error _ -> false

let%test "parse_string: default with backend only" =
  let raw = {|{"default":{"backend":"codex"}}|} in
  match parse_string ~known_backends:[ "claude"; "codex" ] raw with
  | Ok t ->
      Option.equal String.equal t.default_backend (Some "codex")
      && Option.is_none t.default_model
  | Error _ -> false

let%test "parse_string: default with model only" =
  let raw = {|{"default":{"model":"sonnet"}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Ok t ->
      Option.is_none t.default_backend
      && Option.equal String.equal t.default_model (Some "sonnet")
  | Error _ -> false

let%test "parse_string: default with both fields" =
  let raw = {|{"default":{"backend":"codex","model":"gpt-5.5"}}|} in
  match parse_string ~known_backends:[ "claude"; "codex" ] raw with
  | Ok t ->
      Option.equal String.equal t.default_backend (Some "codex")
      && Option.equal String.equal t.default_model (Some "gpt-5.5")
  | Error _ -> false

let%test "parse_string: default.backend unknown rejected" =
  let raw = {|{"default":{"backend":"made-up"}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Error msg ->
      String.is_substring msg ~substring:"made-up"
      && String.is_substring msg ~substring:"default.backend"
  | Ok _ -> false

let%test "parse_string: default.model = 'auto' preserved verbatim" =
  let raw = {|{"default":{"backend":"claude","model":"auto"}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Ok t -> Option.equal String.equal t.default_model (Some "auto")
  | Error _ -> false

let%test "parse_string: default.backend empty string treated as None" =
  let raw = {|{"default":{"backend":"  ","model":"sonnet"}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Ok t ->
      Option.is_none t.default_backend
      && Option.equal String.equal t.default_model (Some "sonnet")
  | Error _ -> false

let%test "parse_string: default coexists with routing" =
  let raw =
    {|{
       "default":{"backend":"codex","model":"auto"},
       "routing":{"1":{"backend":"claude","model":"haiku"}}
     }|}
  in
  match parse_string ~known_backends:[ "claude"; "codex" ] raw with
  | Ok t -> (
      Option.equal String.equal t.default_backend (Some "codex")
      && Option.equal String.equal t.default_model (Some "auto")
      &&
      match t.complexity_routes with
      | [ (1, { backend = "claude"; model = Some "haiku" }) ] -> true
      | _ -> false)
  | Error _ -> false

let%test "parse_string: non-string default.backend rejected" =
  let raw = {|{"default":{"backend":123}}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Error msg ->
      String.is_substring msg ~substring:"default.backend must be a string"
  | Ok _ -> false

let%test "parse_string: default not an object rejected" =
  let raw = {|{"default":"claude"}|} in
  match parse_string ~known_backends:[ "claude" ] raw with
  | Error msg -> String.is_substring msg ~substring:"default must be an object"
  | Ok _ -> false

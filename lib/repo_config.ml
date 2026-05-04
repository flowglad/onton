open Base

type route = { backend : string; model : string option }
type t = { complexity_routes : (int * route) list }

let empty = { complexity_routes = [] }
let config_path ~config_dir = Stdlib.Filename.concat config_dir "config.json"

let parse_route ~known_backends ~complexity (json : Yojson.Safe.t) :
    (route, string) Result.t =
  let open Yojson.Safe.Util in
  match json with
  | `Assoc _ -> (
      let backend =
        try Some (member "backend" json |> to_string) with _ -> None
      in
      let model =
        match member "model" json with
        | `Null -> None
        | `String s when String.is_empty (String.strip s) -> None
        | `String s -> Some s
        | _ -> None
      in
      match backend with
      | None ->
          Error
            (Printf.sprintf
               "routing.%d: missing required string field \"backend\""
               complexity)
      | Some name when not (List.mem known_backends name ~equal:String.equal) ->
          Error
            (Printf.sprintf
               "routing.%d.backend = %S is not a known backend (expected one \
                of: %s)"
               complexity name
               (String.concat ~sep:", " known_backends))
      | Some name -> Ok { backend = name; model })
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
              Result.map (parse_route ~known_backends ~complexity:k value)
                ~f:(fun route -> (k, route) :: acc))
      |> Result.map ~f:List.rev
  | _ ->
      Error
        ("routing must be an object mapping complexity -> {backend, model};"
       ^ " got " ^ Yojson.Safe.to_string json)

let parse_string ~known_backends (raw : string) : (t, string) Result.t =
  match
    try Ok (Yojson.Safe.from_string raw)
    with Yojson.Json_error msg -> Error (Printf.sprintf "config.json: %s" msg)
  with
  | Error _ as e -> e
  | Ok json -> (
      let open Yojson.Safe.Util in
      match json with
      | `Assoc _ ->
          let routing = member "routing" json in
          Result.map (parse_routing ~known_backends routing) ~f:(fun routes ->
              { complexity_routes = routes })
      | _ -> Error "config.json: top-level value must be an object")

let load ~config_dir ~known_backends =
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
      parse_string ~known_backends raw
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

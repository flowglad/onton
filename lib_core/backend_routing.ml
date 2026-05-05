open Base

type decision = { backend : string; model : string option }

let is_auto_model = function
  | Some s -> String.equal (String.lowercase s) "auto"
  | None -> false

let decide ~(repo_config : Repo_config.t) ~default_backend ~cli_model
    ~complexity : decision =
  if not (is_auto_model cli_model) then
    { backend = default_backend; model = cli_model }
  else
    match Repo_config.route_for_complexity repo_config ~complexity with
    | Some { Repo_config.backend; model } -> { backend; model }
    | None ->
        (* Canonicalise the auto sentinel to lowercase so e.g. [--model AUTO]
           and [--model auto] hit the same [Backend_registry] cache key
           ([(backend, model)]). [resolve_auto_model] downstream lowercases
           anyway, so observable behaviour is unchanged. *)
        { backend = default_backend; model = Some "auto" }

let resolve_auto (dec : decision) ~auto_model ~complexity : decision =
  if is_auto_model dec.model then { dec with model = auto_model ~complexity }
  else dec

let%test "resolve_auto: replaces 'auto' with auto_model result" =
  let auto_model ~complexity:_ = Some "resolved" in
  let dec = { backend = "claude"; model = Some "auto" } in
  let out = resolve_auto dec ~auto_model ~complexity:(Some 1) in
  Option.equal String.equal out.model (Some "resolved")
  && String.equal out.backend "claude"

let%test "resolve_auto: case-insensitive on AUTO" =
  let auto_model ~complexity:_ = Some "resolved" in
  let dec = { backend = "codex"; model = Some "AUTO" } in
  let out = resolve_auto dec ~auto_model ~complexity:None in
  Option.equal String.equal out.model (Some "resolved")

let%test "resolve_auto: leaves explicit model unchanged" =
  let auto_model ~complexity:_ = Some "resolved" in
  let dec = { backend = "claude"; model = Some "sonnet" } in
  let out = resolve_auto dec ~auto_model ~complexity:(Some 2) in
  Option.equal String.equal out.model (Some "sonnet")

let%test "resolve_auto: leaves None unchanged" =
  let auto_model ~complexity:_ = Some "resolved" in
  let dec = { backend = "claude"; model = None } in
  let out = resolve_auto dec ~auto_model ~complexity:(Some 2) in
  Option.is_none out.model

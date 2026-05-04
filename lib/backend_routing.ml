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

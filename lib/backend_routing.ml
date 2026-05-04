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
    | None -> { backend = default_backend; model = cli_model }

(* @archlint.module core
   @archlint.domain backend-routing *)

open Base

type decision = {
  backend : string;
  model : string option;
  effort : string option;
}

let is_auto_model = function
  | Some s -> String.equal (String.lowercase s) "auto"
  | None -> false

let effort_is_supported = Repo_config.effort_is_supported

let first_nonempty (xs : string list) ~default =
  match List.find xs ~f:(fun s -> not (String.is_empty (String.strip s))) with
  | Some s -> String.strip s
  | None -> default

let resolve_pair ~cli_backend ~cli_model ~stored_backend ~stored_model
    ~(repo_config : Repo_config.t) ~built_in_backend =
  let cfg_backend =
    Option.value repo_config.Repo_config.default_backend ~default:""
  in
  let cfg_model =
    Option.value repo_config.Repo_config.default_model ~default:""
  in
  let backend =
    first_nonempty
      [ cli_backend; stored_backend; cfg_backend ]
      ~default:built_in_backend
  in
  let model =
    first_nonempty [ cli_model; stored_model; cfg_model ] ~default:""
  in
  (backend, model)

let decide ~(repo_config : Repo_config.t) ~default_backend ~effective_model
    ~complexity : decision =
  if not (is_auto_model effective_model) then
    {
      backend = default_backend;
      model = effective_model;
      effort = repo_config.default_effort;
    }
  else
    match Repo_config.route_for_complexity repo_config ~complexity with
    | Some { Repo_config.backend; model; effort } ->
        let effort =
          match effort with
          | Repo_config.Inherit -> repo_config.default_effort
          | Repo_config.Provider_default -> None
          | Repo_config.Level level -> Some level
        in
        { backend; model; effort }
    | None ->
        (* Canonicalise the auto sentinel to lowercase so e.g. [--model AUTO]
           and [--model auto] hit the same [Backend_registry] cache key
           ([(backend, model, effort)]). [resolve_auto_model] downstream
           lowercases anyway, so observable behaviour is unchanged. *)
        {
          backend = default_backend;
          model = Some "auto";
          effort = repo_config.default_effort;
        }

type unsupported_effort = {
  unsupported_backend : string;
  unsupported_level : string;
  unsupported_complexity : int option;
}

let unsupported_efforts ~(repo_config : Repo_config.t) ~default_backend
    ~effective_model =
  let complexities =
    if is_auto_model effective_model then
      None
      :: List.map repo_config.complexity_routes ~f:(fun (complexity, _) ->
          Some complexity)
      |> List.dedup_and_sort ~compare:(Option.compare Int.compare)
    else [ None ]
  in
  List.filter_map complexities ~f:(fun complexity ->
      let ({ backend; effort; _ } : decision) =
        decide ~repo_config ~default_backend ~effective_model ~complexity
      in
      match effort with
      | None -> None
      | Some effort when effort_is_supported ~backend effort -> None
      | Some effort ->
          Some
            {
              unsupported_backend = backend;
              unsupported_level = effort;
              unsupported_complexity = complexity;
            })

let resolve_auto (dec : decision) ~auto_model ~complexity : decision =
  if is_auto_model dec.model then { dec with model = auto_model ~complexity }
  else dec

let%test "resolve_auto: replaces 'auto' with auto_model result" =
  let auto_model ~complexity:_ = Some "resolved" in
  let dec = { backend = "claude"; model = Some "auto"; effort = None } in
  let out = resolve_auto dec ~auto_model ~complexity:(Some 1) in
  Option.equal String.equal out.model (Some "resolved")
  && String.equal out.backend "claude"

let%test "resolve_auto: case-insensitive on AUTO" =
  let auto_model ~complexity:_ = Some "resolved" in
  let dec = { backend = "codex"; model = Some "AUTO"; effort = None } in
  let out = resolve_auto dec ~auto_model ~complexity:None in
  Option.equal String.equal out.model (Some "resolved")

let%test "resolve_auto: leaves explicit model unchanged" =
  let auto_model ~complexity:_ = Some "resolved" in
  let dec = { backend = "claude"; model = Some "sonnet"; effort = None } in
  let out = resolve_auto dec ~auto_model ~complexity:(Some 2) in
  Option.equal String.equal out.model (Some "sonnet")

let%test "resolve_auto: leaves None unchanged" =
  let auto_model ~complexity:_ = Some "resolved" in
  let dec = { backend = "claude"; model = None; effort = None } in
  let out = resolve_auto dec ~auto_model ~complexity:(Some 2) in
  Option.is_none out.model

let%test "decide: route effort inherits, clears, or overrides default" =
  let route effort = { Repo_config.backend = "codex"; model = None; effort } in
  let decide effort =
    decide
      ~repo_config:
        {
          Repo_config.empty with
          default_effort = Some "high";
          complexity_routes = [ (1, route effort) ];
        }
      ~default_backend:"codex" ~effective_model:(Some "auto")
      ~complexity:(Some 1)
    |> fun d -> d.effort
  in
  Option.equal String.equal (decide Repo_config.Inherit) (Some "high")
  && Option.is_none (decide Repo_config.Provider_default)
  && Option.equal String.equal
       (decide (Repo_config.Level "xhigh"))
       (Some "xhigh")

let%test "effort_is_supported is provider-specific" =
  effort_is_supported ~backend:"codex" "minimal"
  && (not (effort_is_supported ~backend:"claude" "minimal"))
  && effort_is_supported ~backend:"claude" "max"
  && (not (effort_is_supported ~backend:"codex" "max"))
  && not (effort_is_supported ~backend:"gemini" "high")

let%test "unsupported_efforts checks configured route keys" =
  let repo_config =
    {
      Repo_config.empty with
      complexity_routes =
        [
          (4, { backend = "codex"; model = None; effort = Level "max" });
          (9, { backend = "claude"; model = None; effort = Level "max" });
        ];
    }
  in
  match
    unsupported_efforts ~repo_config ~default_backend:"codex"
      ~effective_model:(Some "auto")
  with
  | [
   {
     unsupported_backend = "codex";
     unsupported_level = "max";
     unsupported_complexity = Some 4;
   };
  ] ->
      true
  | _ -> false

(* [resolve_pair] tests. *)

let empty_config = Repo_config.empty

let config_with ?(default_backend = None) ?(default_model = None) () =
  { Repo_config.empty with default_backend; default_model }

let%test "resolve_pair: CLI wins over everything" =
  let b, m =
    resolve_pair ~cli_backend:"codex" ~cli_model:"gpt-5"
      ~stored_backend:"claude" ~stored_model:"sonnet"
      ~repo_config:
        (config_with ~default_backend:(Some "gemini")
           ~default_model:(Some "gemini-2.5-pro") ())
      ~built_in_backend:"claude"
  in
  String.equal b "codex" && String.equal m "gpt-5"

let%test "resolve_pair: stored wins over config when CLI empty" =
  let b, m =
    resolve_pair ~cli_backend:"" ~cli_model:"" ~stored_backend:"claude"
      ~stored_model:"sonnet"
      ~repo_config:
        (config_with ~default_backend:(Some "codex")
           ~default_model:(Some "gpt-5") ())
      ~built_in_backend:"claude"
  in
  String.equal b "claude" && String.equal m "sonnet"

let%test "resolve_pair: config wins when CLI and stored both empty" =
  let b, m =
    resolve_pair ~cli_backend:"" ~cli_model:"" ~stored_backend:""
      ~stored_model:""
      ~repo_config:
        (config_with ~default_backend:(Some "codex")
           ~default_model:(Some "auto") ())
      ~built_in_backend:"claude"
  in
  String.equal b "codex" && String.equal m "auto"

let%test "resolve_pair: built-in backend wins as last resort" =
  let b, m =
    resolve_pair ~cli_backend:"" ~cli_model:"" ~stored_backend:""
      ~stored_model:"" ~repo_config:empty_config ~built_in_backend:"claude"
  in
  String.equal b "claude" && String.equal m ""

let%test "resolve_pair: per-field independence (CLI backend, config model)" =
  let b, m =
    resolve_pair ~cli_backend:"codex" ~cli_model:"" ~stored_backend:""
      ~stored_model:""
      ~repo_config:(config_with ~default_model:(Some "auto") ())
      ~built_in_backend:"claude"
  in
  String.equal b "codex" && String.equal m "auto"

let%test "resolve_pair: per-field independence (CLI model, stored backend)" =
  let b, m =
    resolve_pair ~cli_backend:"" ~cli_model:"sonnet" ~stored_backend:"claude"
      ~stored_model:"" ~repo_config:empty_config ~built_in_backend:"codex"
  in
  String.equal b "claude" && String.equal m "sonnet"

let%test "resolve_pair: whitespace-only treated as empty" =
  let b, m =
    resolve_pair ~cli_backend:"   " ~cli_model:"\t" ~stored_backend:"\n "
      ~stored_model:""
      ~repo_config:
        (config_with ~default_backend:(Some "gemini")
           ~default_model:(Some "gemini-2.5-pro") ())
      ~built_in_backend:"claude"
  in
  String.equal b "gemini" && String.equal m "gemini-2.5-pro"

let%test "resolve_pair: strips surrounding whitespace from chosen value" =
  let b, m =
    resolve_pair ~cli_backend:"  codex  " ~cli_model:"  gpt-5  "
      ~stored_backend:"" ~stored_model:"" ~repo_config:empty_config
      ~built_in_backend:"claude"
  in
  String.equal b "codex" && String.equal m "gpt-5"

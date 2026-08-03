(* @archlint.module shell
   @archlint.domain priority *)

open Base

let command_for_backend = function
  | "claude" -> Some "claude"
  | "codex" -> Some "codex"
  | "opencode" -> Some "opencode"
  | "pi" -> Some "pi"
  | "gemini" -> Some "gemini"
  | "patch-agent" -> Some "patch-agent"
  | _ -> None

let path_dirs getenv_opt =
  match getenv_opt "PATH" with
  | None | Some "" -> []
  | Some path ->
      String.split path ~on:':'
      |> List.map ~f:(fun dir -> if String.is_empty dir then "." else dir)

let is_executable_file path =
  try
    match (Unix.stat path).Unix.st_kind with
    | Unix.S_REG ->
        Unix.access path [ Unix.X_OK ];
        true
    | Unix.S_DIR | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
    | Unix.S_SOCK ->
        false
  with Unix.Unix_error _ -> false

let find_executable ?(getenv_opt = Stdlib.Sys.getenv_opt)
    ?(is_executable = is_executable_file) command =
  if String.contains command '/' then
    if is_executable command then Some command else None
  else
    path_dirs getenv_opt
    |> List.find_map ~f:(fun dir ->
        let path = Stdlib.Filename.concat dir command in
        if is_executable path then Some path else None)

let check_backend ?getenv_opt ?is_executable backend =
  match command_for_backend backend with
  | None ->
      Error
        (Printf.sprintf "unknown backend %S; cannot run backend preflight"
           backend)
  | Some command -> (
      match find_executable ?getenv_opt ?is_executable command with
      | Some _ -> Ok ()
      | None ->
          Error
            (Printf.sprintf
               "backend %S requires executable %S on PATH, but it was not \
                found or is not executable. Install that backend CLI, fix \
                PATH, or choose another backend with --backend."
               backend command))

let selected_backends ~default_backend ~effective_model
    ~(repo_config : Repo_config.t) =
  let add acc backend =
    if List.mem acc backend ~equal:String.equal then acc else backend :: acc
  in
  let acc = add [] default_backend in
  let uses_routes =
    match effective_model with
    | Some model -> Backend_routing.is_auto_model (Some model)
    | None -> false
  in
  let acc =
    if uses_routes then
      List.fold repo_config.complexity_routes ~init:acc
        ~f:(fun acc (_, route) -> add acc route.Repo_config.backend)
    else acc
  in
  List.rev acc

let validate ?getenv_opt ?is_executable ~default_backend ~effective_model
    ~repo_config () =
  let backends =
    selected_backends ~default_backend ~effective_model ~repo_config
  in
  let executable_errors =
    List.filter_map backends ~f:(fun backend ->
        match check_backend ?getenv_opt ?is_executable backend with
        | Ok () -> None
        | Error msg -> Some msg)
  in
  let effort_errors =
    Backend_routing.unsupported_efforts ~repo_config ~default_backend
      ~effective_model
    |> List.map
         ~f:(fun
             ({ unsupported_backend; unsupported_level; unsupported_complexity } :
               Backend_routing.unsupported_effort)
           ->
           Printf.sprintf
             "backend %S does not support reasoning effort %S for complexity %s"
             unsupported_backend unsupported_level
             (Option.value_map unsupported_complexity ~default:"default"
                ~f:Int.to_string))
  in
  let errors =
    executable_errors @ effort_errors
    |> List.dedup_and_sort ~compare:String.compare
  in
  match errors with [] -> Ok () | _ :: _ -> Error errors

let%test "selected_backends ignores routes unless effective model is auto" =
  let repo_config =
    {
      Repo_config.empty with
      complexity_routes =
        [ (1, { backend = "codex"; model = None; effort = Inherit }) ];
    }
  in
  List.equal String.equal
    (selected_backends ~default_backend:"claude" ~effective_model:None
       ~repo_config)
    [ "claude" ]

let%test "selected_backends includes routes for auto effective model" =
  let repo_config =
    {
      Repo_config.empty with
      complexity_routes =
        [
          (1, { backend = "codex"; model = None; effort = Inherit });
          (2, { backend = "claude"; model = Some "sonnet"; effort = Inherit });
        ];
    }
  in
  List.equal String.equal
    (selected_backends ~default_backend:"claude" ~effective_model:(Some "auto")
       ~repo_config)
    [ "claude"; "codex" ]

let%test "validate reports missing backend executable" =
  let repo_config = Repo_config.empty in
  match
    validate
      ~getenv_opt:(fun _ -> Some "/bin")
      ~is_executable:(fun _ -> false)
      ~default_backend:"codex" ~effective_model:None ~repo_config ()
  with
  | Error [ msg ] ->
      String.is_substring msg ~substring:"codex"
      && String.is_substring msg ~substring:"not found or is not executable"
  | Ok () | Error _ -> false

let%test "validate rejects provider-incompatible reasoning effort" =
  let repo_config = { Repo_config.empty with default_effort = Some "max" } in
  match
    validate
      ~getenv_opt:(fun _ -> Some "/bin")
      ~is_executable:(fun _ -> true)
      ~default_backend:"codex" ~effective_model:None ~repo_config ()
  with
  | Error [ msg ] ->
      String.is_substring msg ~substring:"codex"
      && String.is_substring msg ~substring:"max"
  | Ok () | Error _ -> false

let%test "validate accepts supported route reasoning efforts" =
  let repo_config =
    {
      Repo_config.empty with
      complexity_routes =
        [
          (1, { backend = "codex"; model = None; effort = Level "minimal" });
          (2, { backend = "claude"; model = None; effort = Level "max" });
        ];
    }
  in
  Result.is_ok
    (validate
       ~getenv_opt:(fun _ -> Some "/bin")
       ~is_executable:(fun _ -> true)
       ~default_backend:"codex" ~effective_model:(Some "auto") ~repo_config ())

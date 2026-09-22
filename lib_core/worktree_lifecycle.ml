(* @archlint.module core
   @archlint.domain worktree-lifecycle *)

open Base

type backend = Git | Simgit [@@deriving show, eq, sexp_of, compare]

type config = { backend : backend; executable : string option }
[@@deriving show, eq, sexp_of, compare]

let git = { backend = Git; executable = Some "git" }
let backend_name = function Git -> "git" | Simgit -> "simgit"

let configure ~backend ~executable =
  let kind =
    match backend with
    | "git" -> Ok Git
    | "simgit" -> Ok Simgit
    | _ -> Error "worktree.backend must be git or simgit"
  in
  Result.bind kind ~f:(fun backend ->
      match (backend, executable) with
      | Git, (None | Some "git") -> Ok git
      | Git, Some _ -> Error "worktree.executable is only supported for simgit"
      | Simgit, Some name
        when String.is_empty (String.strip name) || String.contains name '\000'
        ->
          Error "worktree.executable must be a nonempty executable name or path"
      | Simgit, _ -> Ok { backend; executable })

let to_json t =
  `Assoc
    [
      ("backend", `String (backend_name t.backend));
      ( "executable",
        Option.value_map t.executable ~default:`Null ~f:(fun x -> `String x) );
    ]

let of_json = function
  | `Assoc fields -> (
      let field k = List.Assoc.find fields k ~equal:String.equal in
      match (field "backend", field "executable") with
      | Some (`String backend), (None | Some `Null) ->
          configure ~backend ~executable:None
      | Some (`String backend), Some (`String executable) ->
          configure ~backend ~executable:(Some executable)
      | _ ->
          Error
            "worktree must contain backend (git or simgit) and an optional \
             executable string")
  | _ -> Error "worktree must be an object"

let parse_optional = function
  | None -> Ok None
  | Some json ->
      Result.bind (of_json json) ~f:(fun config ->
          match Json.field "executable" json with
          | Some (`String executable)
            when Stdlib.Filename.is_relative executable ->
              Error "repository worktree.executable must be an absolute path"
          | _ -> Ok (Some config))

let resolve ~backend ~executable ~stored_backend ~stored_executable ~repo =
  let default = Option.value repo ~default:git in
  let selected =
    Option.value backend
      ~default:
        (Option.value stored_backend ~default:(backend_name default.backend))
  in
  let executable =
    match executable with
    | Some _ as e -> e
    | None -> (
        match (stored_backend, stored_executable) with
        | Some b, e when String.equal b selected -> e
        | _ ->
            if String.equal selected (backend_name default.backend) then
              default.executable
            else None)
  in
  configure ~backend:selected ~executable

type registration = {
  path : string;
  branch : string option;
  mode : string option;
}

type phase = Preparing | Cleanup_pending | Ready [@@deriving eq]

let ownership_json ~path ~branch ~phase config =
  `Assoc
    [
      ("path", `String path);
      ("branch", `String branch);
      ("owner", to_json config);
      ( "state",
        `String
          (match phase with
          | Preparing -> "preparing"
          | Cleanup_pending -> "cleanup_pending"
          | Ready -> "ready") );
    ]

let parse_ownership_for_path ~path json =
  match
    ( Json.field "path" json,
      Json.field "branch" json,
      Json.field "owner" json,
      Json.field "state" json )
  with
  | Some (`String p), Some (`String b), Some owner, Some (`String state)
    when String.equal p path ->
      Result.bind (of_json owner) ~f:(fun config ->
          match state with
          | "preparing" -> Ok (b, config, Preparing)
          | "cleanup_pending" when equal_backend config.backend Simgit ->
              Ok (b, config, Cleanup_pending)
          | "ready" -> Ok (b, config, Ready)
          | _ -> Error "Unknown checkout publication state")
  | _ ->
      Error
        "Checkout ownership conflicts with the requested path or branch, or \
         lacks publication state"

let parse_ownership ~path ~branch json =
  Result.bind (parse_ownership_for_path ~path json)
    ~f:(fun (recorded, c, phase) ->
      if String.equal recorded branch then Ok (c, phase)
      else
        Error
          "Checkout ownership conflicts with the requested path or branch, or \
           lacks publication state")

let parse raw =
  try Ok (Yojson.Safe.from_string raw) with Yojson.Json_error msg -> Error msg

let string_field fields key =
  match List.Assoc.find fields key ~equal:String.equal with
  | Some (`String s) -> Some s
  | _ -> None

let parse_git_list raw =
  let finish (path, branch, acc) =
    match path with
    | None -> (None, None, acc)
    | Some path -> (None, None, { path; branch; mode = None } :: acc)
  in
  let state =
    List.fold (String.split raw ~on:'\000') ~init:(None, None, [])
      ~f:(fun (path, branch, acc) line ->
        if String.is_empty line then finish (path, branch, acc)
        else
          match
            ( String.chop_prefix line ~prefix:"worktree ",
              String.chop_prefix line ~prefix:"branch refs/heads/" )
          with
          | Some p, _ -> (Some p, branch, acc)
          | _, Some b -> (path, Some b, acc)
          | _ -> (path, branch, acc))
  in
  let _, _, entries = finish state in
  if
    List.is_empty entries
    || List.exists entries ~f:(fun e -> String.is_empty e.path)
  then Error "git worktree list: missing worktree path"
  else Ok (List.rev entries)

let parse_simgit_list raw =
  Result.bind (parse raw) ~f:(function
    | `List entries ->
        List.map entries ~f:(function
          | `Assoc fields -> (
              match
                ( string_field fields "worktree",
                  List.Assoc.find fields "mode" ~equal:String.equal )
              with
              | Some path, (Some `Null | Some (`String _))
                when not (String.is_empty path) ->
                  Ok
                    {
                      path;
                      branch =
                        Option.map (string_field fields "branch") ~f:(fun b ->
                            Option.value
                              (String.chop_prefix b ~prefix:"refs/heads/")
                              ~default:b);
                      mode = string_field fields "mode";
                    }
              | _ -> Error "simgit list: missing path or invalid mode")
          | _ -> Error "simgit list: invalid entry")
        |> Result.all
    | _ -> Error "simgit list: expected an array")

let repair_error ?(code = 0) ~path raw =
  Result.bind (parse raw) ~f:(function
    | `Assoc fields -> (
        match List.Assoc.find fields "failed" ~equal:String.equal with
        | Some (`List failures)
          when code <> 0 && (code <> 1 || List.is_empty failures) ->
            Error (Printf.sprintf "simgit repair failed (exit %d)" code)
        | Some (`List failures) ->
            List.fold_result failures ~init:() ~f:(fun () -> function
              | `Assoc entry -> (
                  match
                    (string_field entry "worktree", string_field entry "error")
                  with
                  | Some p, Some error ->
                      if String.equal path p then Error error else Ok ()
                  | _ -> Error "simgit repair: invalid failure entry")
              | _ -> Error "simgit repair: invalid failure entry")
        | _ -> Error "simgit repair: missing failed array")
    | _ -> Error "simgit repair: expected an object")

let doctor_identity raw =
  Result.bind (parse raw) ~f:(fun json ->
      match (Json.field "identity" json, Json.field "version" json) with
      | Some (`String "simgit"), Some (`String version)
        when not (String.is_empty (String.strip version)) ->
          Ok ()
      | _ -> Error "Expected simgit doctor identity and version")

let registration_backend (entry : registration) =
  match entry.mode with None -> Git | Some _ -> Simgit

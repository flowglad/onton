(* @archlint.module core
   @archlint.domain graph *)

open Base

type t = { path : string; content : string }
[@@deriving show, eq, sexp_of, compare]

type persisted = t option [@@deriving show, eq, sexp_of, compare]

let normalize_directory raw =
  let parts =
    String.split raw ~on:'/'
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  if
    List.is_empty parts
    || List.exists parts ~f:(fun s ->
        String.equal s "." || String.equal s ".."
        || String.equal (String.lowercase s) ".git")
    || String.exists raw ~f:(fun c -> Char.to_int c < 32 || Char.equal c '\\')
  then
    Error
      "gameplan.directory must be a repository-relative directory without ., \
       .., or .git components"
  else Ok (String.concat ~sep:"/" parts)

let create ~directory ~project_name ~yaml ~content =
  Result.bind (normalize_directory directory) ~f:(fun directory ->
      let project =
        String.lowercase project_name
        |> String.map ~f:(fun c ->
            if Char.is_alphanum c || Char.equal c '-' || Char.equal c '_' then c
            else '-')
      in
      if String.is_empty project then
        Error "Gameplan publication requires a non-empty project name"
      else
        Ok
          {
            path =
              (directory ^ "/" ^ project ^ "/gameplan."
              ^ if yaml then "yaml" else "json");
            content;
          })

let path t = t.path
let content t = t.content

let yojson_of_t t =
  `Assoc [ ("path", `String t.path); ("content", `String t.content) ]

let of_yojson json =
  match (Json.string_field "path" json, Json.string_field "content" json) with
  | Some path, Some content -> (
      match normalize_directory path with
      | Ok normalized when String.equal normalized path -> Ok { path; content }
      | Ok _ | Error _ -> Error "Invalid persisted gameplan publication path")
  | _ -> Error "Gameplan publication requires path and content strings"

let parse_optional = function
  | None | Some `Null -> Ok None
  | Some json -> Result.map (of_yojson json) ~f:Option.some

let yojson_of_persisted = function
  | None -> `Null
  | Some publication -> yojson_of_t publication

(* Generated record conversions need a value-returning decoder. The owning
   I/O boundary uses parse_optional first, so malformed metadata fails closed. *)
let persisted_of_yojson json =
  match parse_optional (Some json) with Ok value -> value | Error _ -> None

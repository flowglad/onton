(* @archlint.module core
   @archlint.domain project-lifecycle *)

open Base

let slug name =
  String.concat_map name ~f:(fun c ->
      if Char.is_alphanum c || Char.equal c '-' then String.of_char c
      else if Char.equal c ' ' || Char.equal c '_' then "-"
      else "")
  |> String.lowercase

let resolve ~requested ~stored =
  let requested_slug = slug requested in
  if String.is_empty requested_slug then
    Error "project name has an empty storage identity"
  else
    match stored with
    | None -> Ok requested
    | Some name ->
        if String.equal requested_slug (slug name) then Ok name
        else Error "stored project name does not match its storage directory"

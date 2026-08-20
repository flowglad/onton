(* @archlint.module core
   @archlint.domain sourcehut-target *)

open Base

let component_re = Re.Pcre.re {|^[A-Za-z0-9_][A-Za-z0-9._-]*$|} |> Re.compile

let validate_component label value =
  let value = String.strip value in
  if String.is_empty value then Error (label ^ " is empty")
  else if String.is_suffix (String.lowercase value) ~suffix:".git" then
    Error (Printf.sprintf "%s %S must not end with .git" label value)
  else if Re.execp component_re value then Ok ()
  else
    Error
      (Printf.sprintf
         "%s %S may contain letters, digits, dot, underscore, and dash, and \
          must not begin with punctuation other than underscore"
         label value)

let validate_target ~owner ~repo =
  match validate_component "owner" owner with
  | Error _ as error -> error
  | Ok () -> validate_component "repo" repo

let remote_url_re =
  Re.Pcre.re
    {|^(?:https?://|ssh://git@|git@)git\.sr\.ht(?::|/)/?~([^/]+)/([^/\s]+?)(?:\.git)?/?$|}
  |> Re.compile

let infer_owner_repo_from_url url =
  match Re.exec_opt remote_url_re (String.strip url) with
  | None -> None
  | Some groups -> Some (Re.Group.get groups 1, Re.Group.get groups 2)

let clone_url ~owner ~repo = Printf.sprintf "git@git.sr.ht:~%s/%s" owner repo

(* FNV-1a, reduced to a positive 30-bit OCaml integer so the compatibility id
   stays readable in the TUI. Zero is reserved by [Pr_number], so map it to
   one. *)
let change_id branch =
  let hash =
    String.fold (Types.Branch.to_string branch)
      ~init:(Int64.of_string "2166136261") ~f:(fun hash c ->
        Stdlib.Int64.mul
          (Stdlib.Int64.logxor hash (Int64.of_int (Char.to_int c)))
          (Int64.of_string "16777619"))
  in
  let id = Stdlib.Int64.logand hash 0x3fffffffL |> Int64.to_int_exn in
  Types.Pr_number.of_int (if id = 0 then 1 else id)

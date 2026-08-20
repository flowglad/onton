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

let clone_url ~scheme ~owner ~repo =
  match scheme with
  | Github_target.Https -> Printf.sprintf "https://git.sr.ht/~%s/%s" owner repo
  | Github_target.Ssh -> Printf.sprintf "git@git.sr.ht:~%s/%s" owner repo

let ssh_url_re = Re.Pcre.re {|^(?:ssh://)?git@git\.sr\.ht[:/]|} |> Re.compile
let https_url_re = Re.Pcre.re {|^https?://git\.sr\.ht/|} |> Re.compile

let scheme_of_url url =
  let url = String.strip url in
  if String.is_empty url then None
  else if Re.execp ssh_url_re url then Some Github_target.Ssh
  else if Re.execp https_url_re url then Some Github_target.Https
  else None

let branch_url ~owner ~repo branch =
  Printf.sprintf "https://git.sr.ht/~%s/%s/tree/%s" owner repo
    (Types.Branch.to_string branch)

(* FNV-1a, reduced to a positive 30-bit OCaml integer so the compatibility id
   stays readable in the TUI. Zero is reserved by [Pr_number], so map it to
   one. *)
let candidate_change_id branch =
  let hash =
    String.fold (Types.Branch.to_string branch)
      ~init:(Int64.of_string "2166136261") ~f:(fun hash c ->
        Stdlib.Int64.mul
          (Stdlib.Int64.logxor hash (Int64.of_int (Char.to_int c)))
          (Int64.of_string "16777619"))
  in
  let id = Stdlib.Int64.logand hash 0x3fffffffL |> Int64.to_int_exn in
  Types.Pr_number.of_int (if id = 0 then 1 else id)

type change = {
  id : Types.Pr_number.t;
  branch : Types.Branch.t;
  base : Types.Branch.t;
}

type registry = change list

let empty_registry = []

let find_change registry id =
  List.find registry ~f:(fun change -> Types.Pr_number.equal change.id id)
  |> Option.map ~f:(fun change -> (change.branch, change.base))

let find_branch registry branch =
  List.find registry ~f:(fun change -> Types.Branch.equal change.branch branch)
  |> Option.map ~f:(fun change -> (change.id, change.base))

let next_id id =
  let value = Types.Pr_number.to_int id in
  Types.Pr_number.of_int (if value = 0x3fffffff then 1 else value + 1)

let find_available_id registry start =
  let rec loop candidate remaining =
    if remaining = 0 then Error "SourceHut change identity space exhausted"
    else
      match find_change registry candidate with
      | None -> Ok candidate
      | Some _ -> loop (next_id candidate) (remaining - 1)
  in
  if List.length registry >= 0x3fffffff then
    Error "SourceHut change identity space exhausted"
  else loop start (List.length registry + 1)

let register_change registry ~preferred_id ~branch ~base =
  match find_branch registry branch with
  | Some (id, _) ->
      let registry =
        List.map registry ~f:(fun change ->
            if Types.Branch.equal change.branch branch then { change with base }
            else change)
      in
      Ok (registry, id)
  | None ->
      let preferred_id =
        Option.filter preferred_id ~f:(fun id -> Types.Pr_number.to_int id > 0)
      in
      (match preferred_id with
        | Some id -> (
            match find_change registry id with
            | Some (existing_branch, _) ->
                Error
                  (Printf.sprintf
                     "SourceHut change id collision between branches %s and %s"
                     (Types.Branch.to_string existing_branch)
                     (Types.Branch.to_string branch))
            | None -> Ok id)
        | None -> find_available_id registry (candidate_change_id branch))
      |> Result.map ~f:(fun id -> ({ id; branch; base } :: registry, id))

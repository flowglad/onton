open Base

let owner_re =
  Re.Pcre.re {|^[A-Za-z0-9][A-Za-z0-9-]{0,38}$|} |> Re.compile

let repo_re =
  Re.Pcre.re {|^[A-Za-z0-9][A-Za-z0-9._-]{0,99}$|} |> Re.compile

let remote_url_re =
  Re.Pcre.re {|github\.com[:/]([^/]+)/([^/\s]+?)(?:\.git)?/?$|} |> Re.compile

let validate_owner s =
  let s = String.strip s in
  if String.is_empty s then Error "owner is empty"
  else if Re.execp owner_re s then Ok ()
  else
    Error
      (Printf.sprintf
         "owner %S must match ^[A-Za-z0-9][A-Za-z0-9-]{0,38}$ (GitHub handle: \
          1–39 chars, alphanumeric or dash, first char not a dash)"
         s)

let validate_repo s =
  let s = String.strip s in
  if String.is_empty s then Error "repo is empty"
  else if Re.execp repo_re s then Ok ()
  else
    Error
      (Printf.sprintf
         "repo %S must match ^[A-Za-z0-9][A-Za-z0-9._-]{0,99}$ (GitHub repo: \
          1–100 chars, alphanumeric or .-_, first char not punctuation)"
         s)

let validate_target ~owner ~repo =
  match validate_owner owner with
  | Error _ as e -> e
  | Ok () -> validate_repo repo

let clone_url ~owner ~repo =
  Printf.sprintf "https://github.com/%s/%s.git" owner repo

let infer_owner_repo_from_url url =
  let url = String.strip url in
  match Re.exec_opt remote_url_re url with
  | Some g -> Some (Re.Group.get g 1, Re.Group.get g 2)
  | None -> None

(* Basic spot-check tests stay inline; the cross-cutting properties
   (totality, regex consistency, round-trip with [clone_url]) live in
   [test/test_github_target_properties.ml]. *)

let%test "validate_owner accepts flowglad" =
  Result.is_ok (validate_owner "flowglad")

let%test "validate_owner rejects leading dash" =
  Result.is_error (validate_owner "-bad")

let%test "validate_owner rejects slash" =
  Result.is_error (validate_owner "foo/bar")

let%test "validate_owner rejects empty" =
  Result.is_error (validate_owner "")

let%test "validate_repo accepts onton" =
  Result.is_ok (validate_repo "onton")

let%test "validate_repo accepts dots and dashes" =
  Result.is_ok (validate_repo "my.repo-name_v2")

let%test "validate_repo rejects space" =
  Result.is_error (validate_repo "bad name")

let%test "validate_target reports owner error first" =
  match validate_target ~owner:"" ~repo:"" with
  | Error msg -> String.is_substring msg ~substring:"owner"
  | Ok () -> false

let%test "clone_url is https github.com path" =
  String.equal
    (clone_url ~owner:"flowglad" ~repo:"onton")
    "https://github.com/flowglad/onton.git"

let%test "infer_owner_repo_from_url parses https with .git" =
  match infer_owner_repo_from_url "https://github.com/flowglad/onton.git" with
  | Some ("flowglad", "onton") -> true
  | _ -> false

let%test "infer_owner_repo_from_url parses ssh" =
  match infer_owner_repo_from_url "git@github.com:flowglad/onton.git" with
  | Some ("flowglad", "onton") -> true
  | _ -> false

let%test "infer_owner_repo_from_url returns None for non-github" =
  Option.is_none
    (infer_owner_repo_from_url "https://gitlab.com/flowglad/onton.git")

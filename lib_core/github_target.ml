open Base

let owner_re = Re.Pcre.re {|^[A-Za-z0-9][A-Za-z0-9-]{0,38}$|} |> Re.compile
let repo_re = Re.Pcre.re {|^[A-Za-z0-9][A-Za-z0-9._-]{0,99}$|} |> Re.compile

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
  else if String.is_suffix (String.lowercase s) ~suffix:".git" then
    Error (Printf.sprintf "repo %S must not end with .git" s)
  else if Re.execp repo_re s then Ok ()
  else
    Error
      (Printf.sprintf
         "repo %S must match ^[A-Za-z0-9][A-Za-z0-9._-]{0,99}$ (GitHub repo: \
          1–100 chars, alphanumeric or .-_, first char not punctuation, not \
          ending in .git)"
         s)

let validate_target ~owner ~repo =
  match validate_owner owner with
  | Error _ as e -> e
  | Ok () -> validate_repo repo

type url_scheme = Https | Ssh [@@deriving show, eq, sexp_of, compare, yojson]

let clone_url ~scheme ~owner ~repo =
  match scheme with
  | Https -> Printf.sprintf "https://github.com/%s/%s.git" owner repo
  | Ssh -> Printf.sprintf "git@github.com:%s/%s.git" owner repo

(* SSH-form GitHub URLs use the [git@github.com:owner/repo.git] shape (note
   the colon, no scheme prefix). HTTPS-form is [https://github.com/...]. The
   [remote_url_re] above accepts both shapes for [infer_owner_repo_from_url];
   here we only need to know which side we hit. *)
let ssh_url_re = Re.Pcre.re {|^(?:ssh://)?git@github\.com[:/]|} |> Re.compile
let https_url_re = Re.Pcre.re {|^https?://github\.com/|} |> Re.compile

let scheme_of_url url =
  let url = String.strip url in
  if String.is_empty url then None
  else if Re.execp ssh_url_re url then Some Ssh
  else if Re.execp https_url_re url then Some Https
  else None

let resolve_scheme ~override ~ssh_available =
  match override with
  | Some s -> s
  | None -> if ssh_available then Ssh else Https

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

let%test "validate_owner rejects empty" = Result.is_error (validate_owner "")
let%test "validate_repo accepts onton" = Result.is_ok (validate_repo "onton")

let%test "validate_repo accepts dots and dashes" =
  Result.is_ok (validate_repo "my.repo-name_v2")

let%test "validate_repo rejects dot-git suffix" =
  Result.is_error (validate_repo "my-repo.git")

let%test "validate_repo rejects space" =
  Result.is_error (validate_repo "bad name")

let%test "validate_target reports owner error first" =
  match validate_target ~owner:"" ~repo:"" with
  | Error msg -> String.is_substring msg ~substring:"owner"
  | Ok () -> false

let%test "clone_url ~scheme:Https is https github.com path" =
  String.equal
    (clone_url ~scheme:Https ~owner:"flowglad" ~repo:"onton")
    "https://github.com/flowglad/onton.git"

let%test "clone_url ~scheme:Ssh is git@github.com:owner/repo.git" =
  String.equal
    (clone_url ~scheme:Ssh ~owner:"flowglad" ~repo:"onton")
    "git@github.com:flowglad/onton.git"

let%test "scheme_of_url recognizes https" =
  match scheme_of_url "https://github.com/flowglad/onton.git" with
  | Some Https -> true
  | Some Ssh | None -> false

let%test "scheme_of_url recognizes git@ ssh" =
  match scheme_of_url "git@github.com:flowglad/onton.git" with
  | Some Ssh -> true
  | Some Https | None -> false

let%test "scheme_of_url rejects junk" =
  Option.is_none (scheme_of_url "https://gitlab.com/x/y.git")
  && Option.is_none (scheme_of_url "")
  && Option.is_none (scheme_of_url "not a url")

let%test "resolve_scheme: override Some wins regardless of probe" =
  equal_url_scheme
    (resolve_scheme ~override:(Some Ssh) ~ssh_available:false)
    Ssh
  && equal_url_scheme
       (resolve_scheme ~override:(Some Https) ~ssh_available:true)
       Https

let%test "resolve_scheme: no override, ssh_available=true -> Ssh" =
  equal_url_scheme (resolve_scheme ~override:None ~ssh_available:true) Ssh

let%test "resolve_scheme: no override, ssh_available=false -> Https" =
  equal_url_scheme (resolve_scheme ~override:None ~ssh_available:false) Https

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

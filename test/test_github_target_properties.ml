open Base
open Onton_core

(** Property + interleaving tests for {!Github_target}.

    The functions under test are total and pure: validators consume any string
    and never raise; [clone_url] is a string formatter; and
    [infer_owner_repo_from_url] is a parser. Properties pin down the rules
    (regex consistency, totality, round-trip with [clone_url], idempotence of
    [validate_target] on validated input) so future refactors don't silently
    drift away from them. *)

(* ---------- Generators ---------- *)

(* Reasonable identifier alphabet: alphanumeric plus the punctuation classes
   the GitHub rules touch (dash, dot, underscore) plus a few characters that
   should always be rejected (space, slash, colon, ampersand). *)
let gen_ident_char =
  QCheck2.Gen.oneof_list
    [
      'a';
      'b';
      'c';
      'z';
      'A';
      'Z';
      '0';
      '5';
      '9';
      '-';
      '.';
      '_';
      ' ';
      '/';
      ':';
      '&';
    ]

let gen_ident_string =
  QCheck2.Gen.(string_size ~gen:gen_ident_char (int_range 0 60))

(* Strings drawn from any printable byte range, including unicode-like
   sequences and control chars (modulo printable). Used to prove validators
   and the URL parser are total — they must never raise on adversarial
   input. *)
let gen_arbitrary_string =
  QCheck2.Gen.(string_size ~gen:printable (int_range 0 200))

(* A "valid GitHub handle" generator: 1–39 chars, first alphanumeric, body
   alphanumeric-or-dash. Used to assert that validation accepts the entire
   valid space, not just a few hard-coded examples. *)
let gen_valid_owner =
  let open QCheck2.Gen in
  let first =
    oneof_list
      [
        'a';
        'b';
        'c';
        'd';
        'e';
        'f';
        'g';
        'h';
        'i';
        'j';
        'k';
        'l';
        'm';
        'n';
        'o';
        'p';
        'q';
        'r';
        's';
        't';
        'u';
        'v';
        'w';
        'x';
        'y';
        'z';
        'A';
        'B';
        'C';
        'D';
        'E';
        'F';
        'G';
        'H';
        '0';
        '1';
        '2';
        '3';
        '4';
        '5';
        '6';
        '7';
        '8';
        '9';
      ]
  in
  let rest_char =
    oneof_list
      [ 'a'; 'b'; 'c'; 'd'; 'e'; 'z'; 'A'; 'M'; 'Z'; '0'; '1'; '5'; '9'; '-' ]
  in
  let* len_minus_one = int_range 0 38 in
  let* head = first in
  let* tail = string_size ~gen:rest_char (return len_minus_one) in
  return (String.of_char head ^ tail)

let gen_valid_repo =
  let open QCheck2.Gen in
  let first =
    oneof_list
      [
        'a';
        'b';
        'c';
        'd';
        'e';
        'f';
        'g';
        'h';
        'i';
        'j';
        'k';
        'l';
        'm';
        'n';
        'o';
        'p';
        'q';
        'r';
        's';
        't';
        'u';
        'v';
        'w';
        'x';
        'y';
        'z';
        'A';
        'B';
        'M';
        '0';
        '1';
        '5';
        '9';
      ]
  in
  let rest_char =
    oneof_list
      [ 'a'; 'b'; 'c'; 'm'; 'z'; 'Q'; 'Z'; '0'; '5'; '9'; '-'; '.'; '_' ]
  in
  let* len_minus_one = int_range 0 99 in
  let* head = first in
  let* tail = string_size ~gen:rest_char (return len_minus_one) in
  return (String.of_char head ^ tail)

(* ---------- Totality properties ---------- *)

let prop_validate_owner_total =
  QCheck2.Test.make ~name:"validate_owner: total on arbitrary input"
    gen_arbitrary_string (fun s ->
      match Github_target.validate_owner s with Ok _ | Error _ -> true)

let prop_validate_repo_total =
  QCheck2.Test.make ~name:"validate_repo: total on arbitrary input"
    gen_arbitrary_string (fun s ->
      match Github_target.validate_repo s with Ok _ | Error _ -> true)

let prop_validate_target_total =
  QCheck2.Test.make ~name:"validate_target: total on arbitrary input pairs"
    QCheck2.Gen.(pair gen_arbitrary_string gen_arbitrary_string)
    (fun (owner, repo) ->
      match Github_target.validate_target ~owner ~repo with
      | Ok _ | Error _ -> true)

let prop_infer_url_total =
  QCheck2.Test.make ~name:"infer_owner_repo_from_url: total on arbitrary input"
    gen_arbitrary_string (fun s ->
      match Github_target.infer_owner_repo_from_url s with
      | Some _ | None -> true)

let prop_clone_url_total =
  QCheck2.Test.make ~name:"clone_url: total"
    QCheck2.Gen.(pair gen_ident_string gen_ident_string)
    (fun (owner, repo) ->
      let _ = Github_target.clone_url ~owner ~repo in
      true)

(* ---------- Acceptance properties: every valid handle / repo is accepted ---------- *)

let prop_valid_owner_accepted =
  QCheck2.Test.make ~name:"validate_owner: accepts every valid handle shape"
    gen_valid_owner (fun s -> Result.is_ok (Github_target.validate_owner s))

let prop_valid_repo_accepted =
  QCheck2.Test.make ~name:"validate_repo: accepts every valid repo shape"
    gen_valid_repo (fun s -> Result.is_ok (Github_target.validate_repo s))

(* ---------- Rejection properties: structural negatives ---------- *)

let prop_empty_owner_rejected =
  QCheck2.Test.make ~name:"validate_owner: rejects whitespace-only"
    QCheck2.Gen.unit (fun () ->
      Result.is_error (Github_target.validate_owner "")
      && Result.is_error (Github_target.validate_owner "   ")
      && Result.is_error (Github_target.validate_owner "\t\n "))

let prop_empty_repo_rejected =
  QCheck2.Test.make ~name:"validate_repo: rejects whitespace-only"
    QCheck2.Gen.unit (fun () ->
      Result.is_error (Github_target.validate_repo "")
      && Result.is_error (Github_target.validate_repo "   "))

let prop_owner_with_slash_rejected =
  QCheck2.Test.make ~name:"validate_owner: rejects any handle containing '/'"
    QCheck2.Gen.(triple gen_valid_owner gen_valid_owner (int_range 1 5))
    (fun (a, b, n) ->
      let s = a ^ String.make n '/' ^ b in
      Result.is_error (Github_target.validate_owner s))

let prop_owner_with_space_rejected =
  QCheck2.Test.make
    ~name:"validate_owner: rejects any handle containing whitespace inside"
    QCheck2.Gen.(pair gen_valid_owner gen_valid_owner)
    (fun (a, b) ->
      let s = a ^ " " ^ b in
      Result.is_error (Github_target.validate_owner s))

let prop_owner_too_long_rejected =
  QCheck2.Test.make ~name:"validate_owner: rejects strings longer than 39 chars"
    QCheck2.Gen.(int_range 40 200)
    (fun n ->
      let s = String.make n 'a' in
      Result.is_error (Github_target.validate_owner s))

let prop_repo_too_long_rejected =
  QCheck2.Test.make ~name:"validate_repo: rejects strings longer than 100 chars"
    QCheck2.Gen.(int_range 101 300)
    (fun n ->
      let s = String.make n 'a' in
      Result.is_error (Github_target.validate_repo s))

let prop_repo_dot_git_suffix_rejected =
  QCheck2.Test.make ~name:"validate_repo: rejects repo names ending in .git"
    gen_valid_repo (fun repo ->
      Result.is_error (Github_target.validate_repo (repo ^ ".git")))

(* ---------- Composition properties ---------- *)

let prop_validate_target_short_circuits_on_owner =
  QCheck2.Test.make
    ~name:"validate_target: rejects invalid owner, regardless of repo"
    QCheck2.Gen.(pair gen_arbitrary_string gen_arbitrary_string)
    (fun (owner, repo) ->
      match Github_target.validate_owner owner with
      | Error _ -> Result.is_error (Github_target.validate_target ~owner ~repo)
      | Ok () -> true (* property is conditional on owner being invalid *))

let prop_validate_target_passes_iff_both_pass =
  QCheck2.Test.make
    ~name:"validate_target: Ok iff both validate_owner and validate_repo Ok"
    QCheck2.Gen.(pair gen_arbitrary_string gen_arbitrary_string)
    (fun (owner, repo) ->
      let both_ok =
        Result.is_ok (Github_target.validate_owner owner)
        && Result.is_ok (Github_target.validate_repo repo)
      in
      Bool.equal both_ok
        (Result.is_ok (Github_target.validate_target ~owner ~repo)))

(* ---------- URL round-trip ---------- *)

let prop_clone_url_roundtrips =
  QCheck2.Test.make
    ~name:
      "infer_owner_repo_from_url (clone_url ~owner ~repo) = Some (owner, repo) \
       for valid pairs"
    QCheck2.Gen.(pair gen_valid_owner gen_valid_repo)
    (fun (owner, repo) ->
      let url = Github_target.clone_url ~owner ~repo in
      match Github_target.infer_owner_repo_from_url url with
      | Some (o, r) -> String.equal o owner && String.equal r repo
      | None -> false)

let prop_clone_url_format =
  QCheck2.Test.make ~name:"clone_url starts with https://github.com/"
    QCheck2.Gen.(pair gen_ident_string gen_ident_string)
    (fun (owner, repo) ->
      String.is_prefix
        (Github_target.clone_url ~owner ~repo)
        ~prefix:"https://github.com/")

let prop_clone_url_ends_dot_git =
  QCheck2.Test.make ~name:"clone_url ends with .git"
    QCheck2.Gen.(pair gen_ident_string gen_ident_string)
    (fun (owner, repo) ->
      String.is_suffix (Github_target.clone_url ~owner ~repo) ~suffix:".git")

let prop_infer_non_github_rejected =
  QCheck2.Test.make
    ~name:"infer_owner_repo_from_url: returns None for non-github hosts"
    QCheck2.Gen.(pair gen_valid_owner gen_valid_repo)
    (fun (owner, repo) ->
      let bad_hosts =
        [
          Printf.sprintf "https://gitlab.com/%s/%s.git" owner repo;
          Printf.sprintf "https://bitbucket.org/%s/%s.git" owner repo;
          Printf.sprintf "https://example.com/%s/%s" owner repo;
        ]
      in
      List.for_all bad_hosts ~f:(fun u ->
          Option.is_none (Github_target.infer_owner_repo_from_url u)))

(* ---------- Interleaving: validate_target is idempotent on its own input ---------- *)

let prop_validate_target_idempotent =
  QCheck2.Test.make
    ~name:"validate_target ∘ trim is fixed-point for already-valid pairs"
    QCheck2.Gen.(pair gen_valid_owner gen_valid_repo)
    (fun (owner, repo) ->
      match Github_target.validate_target ~owner ~repo with
      | Ok () -> (
          let owner' = String.strip owner in
          let repo' = String.strip repo in
          match Github_target.validate_target ~owner:owner' ~repo:repo' with
          | Ok () -> true
          | Error _ -> false)
      | Error _ -> false)

(* Interleaving with surrounding whitespace: leading and trailing whitespace
   must not change the verdict for any input string. The validators document
   that they strip first; this property exercises the contract. *)
let prop_validate_owner_strips =
  QCheck2.Test.make
    ~name:"validate_owner: surrounding whitespace doesn't change the verdict"
    QCheck2.Gen.(pair gen_arbitrary_string (int_range 0 5))
    (fun (s, n) ->
      let pad = String.make n ' ' in
      Bool.equal
        (Result.is_ok (Github_target.validate_owner s))
        (Result.is_ok (Github_target.validate_owner (pad ^ s ^ pad))))

let prop_validate_repo_strips =
  QCheck2.Test.make
    ~name:"validate_repo: surrounding whitespace doesn't change the verdict"
    QCheck2.Gen.(pair gen_arbitrary_string (int_range 0 5))
    (fun (s, n) ->
      let pad = String.make n ' ' in
      Bool.equal
        (Result.is_ok (Github_target.validate_repo s))
        (Result.is_ok (Github_target.validate_repo (pad ^ s ^ pad))))

let () =
  let tests =
    [
      prop_validate_owner_total;
      prop_validate_repo_total;
      prop_validate_target_total;
      prop_infer_url_total;
      prop_clone_url_total;
      prop_valid_owner_accepted;
      prop_valid_repo_accepted;
      prop_empty_owner_rejected;
      prop_empty_repo_rejected;
      prop_owner_with_slash_rejected;
      prop_owner_with_space_rejected;
      prop_owner_too_long_rejected;
      prop_repo_too_long_rejected;
      prop_repo_dot_git_suffix_rejected;
      prop_validate_target_short_circuits_on_owner;
      prop_validate_target_passes_iff_both_pass;
      prop_clone_url_roundtrips;
      prop_clone_url_format;
      prop_clone_url_ends_dot_git;
      prop_infer_non_github_rejected;
      prop_validate_target_idempotent;
      prop_validate_owner_strips;
      prop_validate_repo_strips;
    ]
  in
  Stdlib.exit (QCheck_base_runner.run_tests ~verbose:false tests)

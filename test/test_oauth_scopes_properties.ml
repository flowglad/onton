open Base
open Onton_core

(** Property tests for {!Oauth_scopes}.

    The module decides which OAuth scopes a gameplan requires (from the files
    each patch touches) and compares against a header-derived "present" list.
    Properties cover:

    - {b Totality} of [parse_header] over arbitrary strings.
    - {b Recognizers} for the named scopes ([repo], [workflow], [read:org]).
    - {b Workflow trigger}: [required_for_files] returns [[Workflow]] iff any
      path is under [.github/workflows/].
    - {b Set difference} algebra for [missing]: idempotent, empty-when-
      superset, preserves [required] order. *)

module Gen = QCheck2.Gen
module Test = QCheck2.Test

(* ---------- Generators ---------- *)

let gen_arb_text = Gen.string_size ~gen:Gen.printable (Gen.int_range 0 200)

let gen_path =
  let prefixes =
    [
      ".github/workflows/"; "./.github/workflows/"; "src/"; "lib/"; "test/"; "";
    ]
  in
  let open Gen in
  let* prefix = oneof_list prefixes in
  let* tail = string_size ~gen:(char_range 'a' 'z') (int_range 1 30) in
  return (prefix ^ tail ^ ".ml")

let gen_known_scope =
  Gen.oneof_list
    [ Oauth_scopes.Repo; Oauth_scopes.Workflow; Oauth_scopes.Read_org ]

(* ---------- Properties ---------- *)

let prop_parse_total =
  Test.make ~count:500
    ~name:"OSC-1: parse_header is total over arbitrary strings" gen_arb_text
    (fun s ->
      try
        let _ : Oauth_scopes.scope list = Oauth_scopes.parse_header s in
        true
      with _ -> false)

let prop_parse_empty =
  Test.make ~name:"OSC-2: parse_header on empty / whitespace returns []"
    (Gen.oneof_list [ ""; " "; "   \t\n " ])
    (fun s -> List.is_empty (Oauth_scopes.parse_header s))

let prop_parse_named =
  Test.make ~name:"OSC-3: parse_header recognises repo, workflow, read:org"
    (Gen.return ()) (fun () ->
      let scopes = Oauth_scopes.parse_header "repo, workflow, read:org" in
      List.mem scopes Oauth_scopes.Repo ~equal:Oauth_scopes.equal_scope
      && List.mem scopes Oauth_scopes.Workflow ~equal:Oauth_scopes.equal_scope
      && List.mem scopes Oauth_scopes.Read_org ~equal:Oauth_scopes.equal_scope)

let prop_parse_unknown_to_other =
  Test.make ~name:"OSC-4: parse_header maps unknown tokens to Other (preserved)"
    (Gen.return ()) (fun () ->
      let scopes = Oauth_scopes.parse_header "admin:public_key, gist" in
      List.mem scopes (Oauth_scopes.Other "admin:public_key")
        ~equal:Oauth_scopes.equal_scope
      && List.mem scopes (Oauth_scopes.Other "gist")
           ~equal:Oauth_scopes.equal_scope)

let prop_required_workflow =
  Test.make ~count:300
    ~name:
      "OSC-5: required_for_files returns [Workflow] iff some path is under \
       .github/workflows/"
    (Gen.list_size (Gen.int_range 0 5) gen_path)
    (fun paths ->
      let needs_workflow =
        List.exists paths ~f:(fun p ->
            String.is_prefix p ~prefix:".github/workflows/"
            || String.is_prefix p ~prefix:"./.github/workflows/")
      in
      let result = Oauth_scopes.required_for_files ~paths in
      if needs_workflow then
        List.mem result Oauth_scopes.Workflow ~equal:Oauth_scopes.equal_scope
      else List.is_empty result)

let prop_missing_empty_when_superset =
  Test.make ~name:"OSC-6: missing is empty when present is a superset"
    (Gen.list_size (Gen.int_range 0 3) gen_known_scope)
    (fun required ->
      let present =
        Oauth_scopes.Repo :: Oauth_scopes.Workflow :: Oauth_scopes.Read_org
        :: required
      in
      List.is_empty (Oauth_scopes.missing ~required ~present))

let prop_missing_difference =
  Test.make ~name:"OSC-7: missing == required \\ present, set-wise"
    (Gen.return ()) (fun () ->
      let required =
        [ Oauth_scopes.Repo; Oauth_scopes.Workflow; Oauth_scopes.Read_org ]
      in
      let present = [ Oauth_scopes.Repo ] in
      let want = [ Oauth_scopes.Workflow; Oauth_scopes.Read_org ] in
      List.equal Oauth_scopes.equal_scope
        (Oauth_scopes.missing ~required ~present)
        want)

let prop_missing_idempotent =
  Test.make ~name:"OSC-8: missing is idempotent w.r.t. the result"
    (Gen.pair
       (Gen.list_size (Gen.int_range 0 3) gen_known_scope)
       (Gen.list_size (Gen.int_range 0 3) gen_known_scope))
    (fun (required, present) ->
      let m1 = Oauth_scopes.missing ~required ~present in
      let m2 = Oauth_scopes.missing ~required:m1 ~present:m1 in
      List.is_empty m2)

let prop_missing_preserves_required_order =
  Test.make ~name:"OSC-9: missing preserves the relative order of required"
    (Gen.return ()) (fun () ->
      let required =
        [ Oauth_scopes.Workflow; Oauth_scopes.Repo; Oauth_scopes.Read_org ]
      in
      let present = [] in
      List.equal Oauth_scopes.equal_scope
        (Oauth_scopes.missing ~required ~present)
        required)

let prop_show_scope_short_nonempty =
  Test.make ~count:200 ~name:"OSC-10: show_scope_short is non-empty"
    (Gen.oneof
       [
         Gen.map (fun () -> Oauth_scopes.Repo) (Gen.return ());
         Gen.map (fun () -> Oauth_scopes.Workflow) (Gen.return ());
         Gen.map (fun () -> Oauth_scopes.Read_org) (Gen.return ());
         Gen.map (fun s -> Oauth_scopes.Other s) (Gen.string_small_of Gen.char);
       ])
    (fun s ->
      let label = Oauth_scopes.show_scope_short s in
      match s with
      | Oauth_scopes.Other "" -> String.is_empty label
      | Oauth_scopes.Other _ | Oauth_scopes.Repo | Oauth_scopes.Workflow
      | Oauth_scopes.Read_org ->
          not (String.is_empty label))

let () =
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_parse_total;
      prop_parse_empty;
      prop_parse_named;
      prop_parse_unknown_to_other;
      prop_required_workflow;
      prop_missing_empty_when_superset;
      prop_missing_difference;
      prop_missing_idempotent;
      prop_missing_preserves_required_order;
      prop_show_scope_short_nonempty;
    ];
  Stdlib.print_endline "Oauth_scopes: all properties passed"

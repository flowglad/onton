(* @archlint.module test
   @archlint.domain sourcehut-target *)

open Onton_core

let gen_string = QCheck2.Gen.string_size (QCheck2.Gen.int_bound 200)
let component_char = QCheck2.Gen.oneof_list [ 'a'; 'b'; 'z'; '0'; '1'; '9' ]

let totality =
  QCheck2.Test.make ~name:"sourcehut target parsers are total" ~count:1000
    QCheck2.Gen.(pair gen_string gen_string)
    (fun (owner, repo) ->
      ignore (Sourcehut_target.validate_target ~owner ~repo);
      ignore (Sourcehut_target.infer_owner_repo_from_url owner);
      ignore
        (Sourcehut_target.branch_url ~owner ~repo
           (Types.Branch.of_string owner));
      true)

let remote_round_trip =
  QCheck2.Test.make ~name:"sourcehut clone URLs round-trip" ~count:500
    QCheck2.Gen.(
      triple bool
        (string_size ~gen:component_char (1 -- 20))
        (string_size ~gen:component_char (1 -- 30)))
    (fun (ssh, owner, repo) ->
      let scheme = if ssh then Github_target.Ssh else Github_target.Https in
      let url = Sourcehut_target.clone_url ~scheme ~owner ~repo in
      match
        ( Sourcehut_target.infer_owner_repo_from_url url,
          Sourcehut_target.scheme_of_url url )
      with
      | Some (actual_owner, actual_repo), Some actual_scheme ->
          String.equal actual_owner owner
          && String.equal actual_repo repo
          && Github_target.equal_url_scheme actual_scheme scheme
      | (Some _ | None), (Some _ | None) -> false)

let distinct_branches_never_alias =
  QCheck2.Test.make
    ~name:"colliding sourcehut branch hashes receive distinct ids" ~count:1
    QCheck2.Gen.unit (fun () ->
      (* These names have the same legacy 30-bit FNV candidate. *)
      let first = Types.Branch.of_string "collision/fuVefpP80ZGF" in
      let second = Types.Branch.of_string "collision/gRJIw8wGHQJs" in
      let base = Types.Branch.of_string "main" in
      match
        Sourcehut_target.register_change Sourcehut_target.empty_registry
          ~preferred_id:None ~branch:first ~base
      with
      | Error _ -> false
      | Ok (registry, first_id) -> (
          match
            Sourcehut_target.register_change registry ~preferred_id:None
              ~branch:second ~base
          with
          | Error _ -> false
          | Ok (registry, second_id) ->
              (not (Types.Pr_number.equal first_id second_id))
              && Sourcehut_target.find_change registry first_id
                 = Some (first, base)
              && Sourcehut_target.find_change registry second_id
                 = Some (second, base)
              && Sourcehut_target.find_branch registry first
                 = Some (first_id, base)))

let persisted_collision_is_rejected =
  QCheck2.Test.make
    ~name:"colliding persisted sourcehut ids are rejected without aliasing"
    ~count:500 gen_string (fun raw ->
      let first = Types.Branch.of_string ("first/" ^ raw) in
      let second = Types.Branch.of_string ("second/" ^ raw) in
      let base = Types.Branch.of_string "main" in
      let preferred_id = Some (Types.Pr_number.of_int 7) in
      match
        Sourcehut_target.register_change Sourcehut_target.empty_registry
          ~preferred_id ~branch:first ~base
      with
      | Error _ -> false
      | Ok (registry, first_id) -> (
          match
            Sourcehut_target.register_change registry ~preferred_id
              ~branch:second ~base
          with
          | Ok _ -> false
          | Error _ ->
              Sourcehut_target.find_change registry first_id = Some (first, base)
              && Sourcehut_target.find_branch registry first
                 = Some (first_id, base)))

let registry_interleavings_remain_injective =
  QCheck2.Test.make
    ~name:"sourcehut registry remains injective across registration sequences"
    ~count:500
    QCheck2.Gen.(list_size (int_bound 50) gen_string)
    (fun raws ->
      let rec apply registry assigned index = function
        | [] -> true
        | raw :: rest -> (
            let branch = Types.Branch.of_string ("branch/" ^ raw) in
            let base = Types.Branch.of_string ("base/" ^ string_of_int index) in
            match
              Sourcehut_target.register_change registry ~preferred_id:None
                ~branch ~base
            with
            | Error _ -> false
            | Ok (registry, id) ->
                let identities_hold =
                  List.for_all
                    (fun (known_branch, known_id) ->
                      Types.Branch.equal known_branch branch
                      = Types.Pr_number.equal known_id id)
                    assigned
                in
                let assigned =
                  (branch, id)
                  :: List.filter
                       (fun (known_branch, _) ->
                         not (Types.Branch.equal known_branch branch))
                       assigned
                in
                identities_hold
                && Sourcehut_target.find_change registry id = Some (branch, base)
                && apply registry assigned (index + 1) rest)
      in
      apply Sourcehut_target.empty_registry [] 0 raws)

let branch_url_uses_full_identity =
  QCheck2.Test.make ~name:"sourcehut branch URLs retain the full branch name"
    ~count:500
    QCheck2.Gen.(string_size ~gen:component_char (0 -- 200))
    (fun raw ->
      let branch = Types.Branch.of_string ("feature/" ^ raw) in
      String.equal
        (Sourcehut_target.branch_url ~owner:"alice" ~repo:"demo" branch)
        ("https://git.sr.ht/~alice/demo/tree/feature/" ^ raw))

let branch_url_encodes_reserved_characters =
  QCheck2.Test.make
    ~name:"sourcehut branch URLs encode reserved bytes and preserve slashes"
    ~count:500
    QCheck2.Gen.(
      triple
        (string_size ~gen:component_char (0 -- 40))
        (oneof_list [ '#'; '?'; '%'; '&'; '+'; ':' ])
        (string_size ~gen:component_char (0 -- 40)))
    (fun (left, reserved, right) ->
      let branch =
        Types.Branch.of_string
          ("feature/" ^ left ^ String.of_char reserved ^ "/" ^ right)
      in
      String.equal
        (Sourcehut_target.branch_url ~owner:"alice" ~repo:"demo" branch)
        (Printf.sprintf "https://git.sr.ht/~alice/demo/tree/feature/%s%%%02X/%s"
           left (Char.code reserved) right))

let restoration_rejects_duplicate_branches =
  QCheck2.Test.make
    ~name:"sourcehut snapshot restoration rejects duplicate branches" ~count:500
    gen_string (fun raw ->
      let branch = Types.Branch.of_string ("branch/" ^ raw) in
      let first_base = Types.Branch.of_string "main" in
      let second_base = Types.Branch.of_string "next" in
      match
        Sourcehut_target.restore_changes
          [
            (Some (Types.Pr_number.of_int 1), branch, first_base);
            (Some (Types.Pr_number.of_int 2), branch, second_base);
          ]
      with
      | Error _ -> true
      | Ok _ -> false)

let restoration_reserves_persisted_ids_first =
  QCheck2.Test.make
    ~name:"sourcehut restoration reserves persisted ids before generated ids"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let generated = Types.Branch.of_string "collision/fuVefpP80ZGF" in
      let persisted = Types.Branch.of_string "collision/gRJIw8wGHQJs" in
      let base = Types.Branch.of_string "main" in
      let persisted_id = Types.Pr_number.of_int 1054795941 in
      match
        Sourcehut_target.restore_changes
          [ (None, generated, base); (Some persisted_id, persisted, base) ]
      with
      | Error _ -> false
      | Ok registry -> (
          match
            ( Sourcehut_target.find_branch registry generated,
              Sourcehut_target.find_branch registry persisted )
          with
          | Some (generated_id, _), Some (actual_persisted_id, _) ->
              (not (Types.Pr_number.equal generated_id persisted_id))
              && Types.Pr_number.equal actual_persisted_id persisted_id
          | (Some _ | None), (Some _ | None) -> false))

let supported_remote_forms =
  QCheck2.Test.make ~name:"sourcehut remote URL forms parse consistently"
    ~count:1 QCheck2.Gen.unit (fun () ->
      Stdlib.List.for_all
        (fun url ->
          match Sourcehut_target.infer_owner_repo_from_url url with
          | Some (owner, repo) ->
              String.equal owner "alice" && String.equal repo "demo"
          | None -> false)
        [
          "https://git.sr.ht/~alice/demo";
          "ssh://git@git.sr.ht/~alice/demo.git";
          "git@git.sr.ht:~alice/demo.git";
        ]
      && Option.is_none
           (Sourcehut_target.infer_owner_repo_from_url
              "git@github.com:alice/demo.git"))

let () =
  QCheck_base_runner.run_tests_main
    [
      totality;
      remote_round_trip;
      distinct_branches_never_alias;
      persisted_collision_is_rejected;
      registry_interleavings_remain_injective;
      branch_url_uses_full_identity;
      branch_url_encodes_reserved_characters;
      restoration_rejects_duplicate_branches;
      restoration_reserves_persisted_ids_first;
      supported_remote_forms;
    ]

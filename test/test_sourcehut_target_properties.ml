open Onton_core

let gen_string = QCheck2.Gen.string_size (QCheck2.Gen.int_bound 200)
let component_char = QCheck2.Gen.oneof_list [ 'a'; 'b'; 'z'; '0'; '1'; '9' ]

let totality =
  QCheck2.Test.make ~name:"sourcehut target parsers are total" ~count:1000
    QCheck2.Gen.(pair gen_string gen_string)
    (fun (owner, repo) ->
      ignore (Sourcehut_target.validate_target ~owner ~repo);
      ignore (Sourcehut_target.infer_owner_repo_from_url owner);
      true)

let remote_round_trip =
  QCheck2.Test.make ~name:"sourcehut SSH clone URL round-trips" ~count:500
    QCheck2.Gen.(
      pair
        (string_size ~gen:component_char (1 -- 20))
        (string_size ~gen:component_char (1 -- 30)))
    (fun (owner, repo) ->
      match
        Sourcehut_target.infer_owner_repo_from_url
          (Sourcehut_target.clone_url ~owner ~repo)
      with
      | Some (actual_owner, actual_repo) ->
          String.equal actual_owner owner && String.equal actual_repo repo
      | None -> false)

let stable_positive_change_id =
  QCheck2.Test.make ~name:"sourcehut change ids are stable and positive"
    ~count:1000 gen_string (fun raw ->
      let branch = Types.Branch.of_string raw in
      let first = Sourcehut_target.change_id branch |> Types.Pr_number.to_int in
      let second =
        Sourcehut_target.change_id branch |> Types.Pr_number.to_int
      in
      first > 0 && first = second)

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
      stable_positive_change_id;
      supported_remote_forms;
    ]

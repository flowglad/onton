open Base
open Onton
open Onton.Types

(** QCheck2 property-based tests for [Worktree.parse_porcelain]. *)

let () =
  let open QCheck2 in
  (* Empty input -> empty list *)
  let prop_empty_input =
    Test.make ~name:"parse_porcelain: empty input -> empty list" ~count:1
      Gen.unit (fun () ->
        let result = Worktree.parse_porcelain ~repo_root:"/repo" "" in
        List.is_empty result)
  in

  (* Detached HEAD entries are skipped *)
  let prop_detached_head_skipped =
    Test.make ~name:"parse_porcelain: detached HEAD entries skipped" ~count:1
      Gen.unit (fun () ->
        let input = "worktree /tmp/wt\nHEAD abc123\ndetached\n" in
        let result = Worktree.parse_porcelain ~repo_root:"/repo" input in
        List.is_empty result)
  in

  (* Repo root entry is excluded *)
  let prop_repo_root_excluded =
    Test.make ~name:"parse_porcelain: repo root excluded" ~count:1 Gen.unit
      (fun () ->
        let input =
          "worktree /repo\n\
           branch refs/heads/main\n\n\
           worktree /wt/foo\n\
           branch refs/heads/feature\n"
        in
        let result = Worktree.parse_porcelain ~repo_root:"/repo" input in
        match result with
        | [ (path, branch) ] ->
            String.equal path "/wt/foo"
            && Branch.equal branch (Branch.of_string "feature")
        | _ -> false)
  in

  (* Multiple entries separated by blank lines parse independently *)
  let prop_multiple_entries =
    Test.make ~name:"parse_porcelain: multiple entries parse independently"
      ~count:1 Gen.unit (fun () ->
        let input =
          "worktree /wt/a\n\
           branch refs/heads/a\n\n\
           worktree /wt/b\n\
           branch refs/heads/b\n"
        in
        let result = Worktree.parse_porcelain ~repo_root:"/repo" input in
        List.length result = 2)
  in

  (* Round-trip: generated porcelain text parses correctly *)
  let prop_roundtrip =
    Test.make ~name:"parse_porcelain: generated entries parse correctly"
      ~count:500
      Gen.(
        list_size (int_range 0 5)
          (pair
             (string_size ~gen:(char_range 'a' 'z') (int_range 3 15))
             (string_size ~gen:(char_range 'a' 'z') (int_range 3 10))))
      (fun entries ->
        try
          let repo_root = "/repo" in
          let porcelain =
            List.map entries ~f:(fun (path, branch) ->
                Printf.sprintf "worktree /wt/%s\nbranch refs/heads/%s" path
                  branch)
            |> String.concat ~sep:"\n\n"
          in
          let result = Worktree.parse_porcelain ~repo_root porcelain in
          (* Each generated entry should appear in results *)
          match List.zip entries result with
          | Unequal_lengths -> false
          | Ok paired ->
              List.for_all paired
                ~f:(fun ((path, branch), (parsed_path, parsed_branch)) ->
                  String.is_suffix parsed_path ~suffix:path
                  && Branch.equal parsed_branch (Branch.of_string branch))
        with _ -> false)
  in

  (* Entries without branch line (no "branch" prefix) are skipped *)
  let prop_no_branch_skipped =
    Test.make ~name:"parse_porcelain: entries without branch are skipped"
      ~count:1 Gen.unit (fun () ->
        let input = "worktree /wt/bare\n\n" in
        let result = Worktree.parse_porcelain ~repo_root:"/repo" input in
        List.is_empty result)
  in

  (* branch_prefixes: no slashes -> empty *)
  let prop_prefixes_no_slash =
    Test.make ~name:"branch_prefixes: no slashes -> empty" ~count:200
      Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 1 20))
      (fun s ->
        (* No slashes means no prefixes *)
        if String.mem s '/' then true (* skip *)
        else List.is_empty (Worktree.branch_prefixes s))
  in

  (* branch_prefixes: result length = number of slashes - but only
     internal slashes count (the last segment is excluded) *)
  let prop_prefixes_count =
    Test.make
      ~name:"branch_prefixes: count = number of slash-separated segments - 1"
      ~count:500
      Gen.(
        list_size (int_range 2 5)
          (string_size ~gen:(char_range 'a' 'z') (int_range 1 8)))
      (fun segments ->
        let branch = String.concat ~sep:"/" segments in
        let prefixes = Worktree.branch_prefixes branch in
        List.length prefixes = List.length segments - 1)
  in

  (* branch_prefixes: each prefix is a proper prefix of the branch *)
  let prop_prefixes_are_prefixes =
    Test.make ~name:"branch_prefixes: each result is a proper prefix of input"
      ~count:500
      Gen.(
        list_size (int_range 2 5)
          (string_size ~gen:(char_range 'a' 'z') (int_range 1 8)))
      (fun segments ->
        let branch = String.concat ~sep:"/" segments in
        let prefixes = Worktree.branch_prefixes branch in
        List.for_all prefixes ~f:(fun pfx ->
            String.is_prefix branch ~prefix:(pfx ^ "/")))
  in

  (* find_ci_ref_collision: exact case match is found *)
  let prop_collision_exact =
    Test.make ~name:"find_ci_ref_collision: exact prefix match detected"
      ~count:1 Gen.unit (fun () ->
        let r =
          Worktree.find_ci_ref_collision
            ~existing_branches:[ "main"; "my-project" ] "my-project/patch-1"
        in
        Option.equal String.equal r (Some "my-project"))
  in

  (* find_ci_ref_collision: case-insensitive match is found *)
  let prop_collision_ci =
    Test.make ~name:"find_ci_ref_collision: case-insensitive match detected"
      ~count:1 Gen.unit (fun () ->
        let r =
          Worktree.find_ci_ref_collision
            ~existing_branches:[ "main"; "My-Project" ] "my-project/patch-1"
        in
        Option.equal String.equal r (Some "My-Project"))
  in

  (* find_ci_ref_collision: no collision when no prefix matches *)
  let prop_collision_none =
    Test.make
      ~name:"find_ci_ref_collision: no collision when unrelated branches"
      ~count:1 Gen.unit (fun () ->
        let r =
          Worktree.find_ci_ref_collision
            ~existing_branches:[ "main"; "feature-x"; "other/thing" ]
            "my-project/patch-1"
        in
        Option.is_none r)
  in

  (* find_ci_ref_collision: property — if a collision is found, it must
     case-insensitively equal a prefix of the branch *)
  let prop_collision_valid =
    Test.make
      ~name:"find_ci_ref_collision: collision is always a ci-equal prefix"
      ~count:500
      Gen.(
        pair
          (list_size (int_range 0 10)
             (string_size ~gen:(char_range 'a' 'z') (int_range 1 10)))
          (list_size (int_range 2 4)
             (string_size ~gen:(char_range 'a' 'z') (int_range 1 8))))
      (fun (existing_branches, segments) ->
        let branch = String.concat ~sep:"/" segments in
        let prefixes = Worktree.branch_prefixes branch in
        match Worktree.find_ci_ref_collision ~existing_branches branch with
        | None -> true
        | Some colliding ->
            let lower_colliding = String.lowercase colliding in
            List.exists prefixes ~f:(fun pfx ->
                String.equal (String.lowercase pfx) lower_colliding))
  in

  let suite =
    [
      prop_empty_input;
      prop_detached_head_skipped;
      prop_repo_root_excluded;
      prop_multiple_entries;
      prop_roundtrip;
      prop_no_branch_skipped;
      prop_prefixes_no_slash;
      prop_prefixes_count;
      prop_prefixes_are_prefixes;
      prop_collision_exact;
      prop_collision_ci;
      prop_collision_none;
      prop_collision_valid;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

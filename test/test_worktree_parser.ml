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

  let suite =
    [
      prop_empty_input;
      prop_detached_head_skipped;
      prop_repo_root_excluded;
      prop_multiple_entries;
      prop_roundtrip;
      prop_no_branch_skipped;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

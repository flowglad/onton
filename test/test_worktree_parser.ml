(* @archlint.module test
   @archlint.domain worktree-parser *)

open Base
open Onton_core
open Onton_core.Types

(** QCheck2 property-based tests for [Worktree.parse_porcelain]. *)

let () =
  let open QCheck2 in
  (* Empty input -> empty list *)
  let prop_empty_input =
    Test.make ~name:"parse_porcelain: empty input -> empty list" ~count:1
      Gen.unit (fun () ->
        let result =
          Worktree_parser.parse_porcelain ~cwd:"" ~repo_root:"/repo" ""
        in
        List.is_empty result)
  in

  (* Detached HEAD entries are skipped *)
  let prop_detached_head_skipped =
    Test.make ~name:"parse_porcelain: detached HEAD entries skipped" ~count:1
      Gen.unit (fun () ->
        let input = "worktree /tmp/wt\nHEAD abc123\ndetached\n" in
        let result =
          Worktree_parser.parse_porcelain ~cwd:"" ~repo_root:"/repo" input
        in
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
        let result =
          Worktree_parser.parse_porcelain ~cwd:"" ~repo_root:"/repo" input
        in
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
        let result =
          Worktree_parser.parse_porcelain ~cwd:"" ~repo_root:"/repo" input
        in
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
          let result =
            Worktree_parser.parse_porcelain ~cwd:"" ~repo_root porcelain
          in
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
        let result =
          Worktree_parser.parse_porcelain ~cwd:"" ~repo_root:"/repo" input
        in
        List.is_empty result)
  in

  (* branch_prefixes: no slashes -> empty *)
  let prop_prefixes_no_slash =
    Test.make ~name:"branch_prefixes: no slashes -> empty" ~count:200
      Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 1 20))
      (fun s ->
        (* No slashes means no prefixes *)
        if String.mem s '/' then true (* skip *)
        else List.is_empty (Worktree_parser.branch_prefixes s))
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
        let prefixes = Worktree_parser.branch_prefixes branch in
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
        let prefixes = Worktree_parser.branch_prefixes branch in
        List.for_all prefixes ~f:(fun pfx ->
            String.is_prefix branch ~prefix:(pfx ^ "/")))
  in

  (* find_ci_ref_collision: exact case match is found *)
  let prop_collision_exact =
    Test.make ~name:"find_ci_ref_collision: exact prefix match detected"
      ~count:1 Gen.unit (fun () ->
        let r =
          Worktree_parser.find_ci_ref_collision
            ~existing_branches:[ "main"; "my-project" ] "my-project/patch-1"
        in
        Option.equal String.equal r (Some "my-project"))
  in

  (* find_ci_ref_collision: case-insensitive match is found *)
  let prop_collision_ci =
    Test.make ~name:"find_ci_ref_collision: case-insensitive match detected"
      ~count:1 Gen.unit (fun () ->
        let r =
          Worktree_parser.find_ci_ref_collision
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
          Worktree_parser.find_ci_ref_collision
            ~existing_branches:[ "main"; "feature-x"; "other/thing" ]
            "my-project/patch-1"
        in
        Option.is_none r)
  in

  (* find_ci_ref_collision: reverse direction — existing "Foo/bar" vs new "foo" *)
  let prop_collision_reverse =
    Test.make ~name:"find_ci_ref_collision: reverse prefix collision detected"
      ~count:1 Gen.unit (fun () ->
        let r =
          Worktree_parser.find_ci_ref_collision
            ~existing_branches:[ "main"; "Foo/bar" ] "foo"
        in
        Option.equal String.equal r (Some "Foo/bar"))
  in

  (* find_ci_ref_collision: property — if a collision is found, it must
     case-insensitively equal a prefix of the branch OR the branch must be
     a case-insensitive prefix of the colliding branch *)
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
        let branch_lc = String.lowercase branch in
        let prefixes = Worktree_parser.branch_prefixes branch in
        match
          Worktree_parser.find_ci_ref_collision ~existing_branches branch
        with
        | None -> true
        | Some colliding ->
            let lower_colliding = String.lowercase colliding in
            (* Forward: existing branch equals a prefix of new branch *)
            List.exists prefixes ~f:(fun pfx ->
                String.equal (String.lowercase pfx) lower_colliding)
            (* Reverse: existing branch has new branch as a prefix *)
            || String.is_prefix lower_colliding ~prefix:(branch_lc ^ "/"))
  in

  let prop_merge_failure_total =
    Test.make ~name:"merge failure classification is total" ~count:500
      Gen.(pair (triple int string string) (pair (option string) string))
      (fun ((code, stdout, stderr), (merge_head, unmerged_paths)) ->
        match
          Worktree_parser.classify_merge_failure ~code ~stdout ~stderr
            ~merge_head ~unmerged_paths
        with
        | `Conflict sha -> not (String.is_empty sha)
        | `Error detail -> not (String.is_empty detail))
  in
  let prop_merge_conflict_evidence =
    Test.make ~name:"merge conflict requires MERGE_HEAD and unmerged index"
      ~count:500
      Gen.(pair (option string) string)
      (fun (merge_head, unmerged_paths) ->
        let expected =
          Option.value_map merge_head ~default:false ~f:(fun sha ->
              not (String.is_empty (String.strip sha)))
          && not (String.is_empty (String.strip unmerged_paths))
        in
        match
          Worktree_parser.classify_merge_failure ~code:1 ~stdout:"CONFLICT"
            ~stderr:"" ~merge_head ~unmerged_paths
        with
        | `Conflict _ -> expected
        | `Error _ -> not expected)
  in
  let prop_merge_error_diagnostics =
    Test.make ~name:"merge errors retain exit code and both output streams"
      ~count:200
      Gen.(triple int string string)
      (fun (code, stdout, stderr) ->
        match
          Worktree_parser.classify_merge_failure ~code ~stdout ~stderr
            ~merge_head:None ~unmerged_paths:""
        with
        | `Error detail ->
            String.is_substring detail ~substring:(Int.to_string code)
            && String.is_substring detail ~substring:(String.strip stdout)
            && String.is_substring detail ~substring:(String.strip stderr)
        | `Conflict _ -> false)
  in
  let prop_merge_failure_boundaries =
    Test.make ~name:"merge classification: blank evidence and hook failures"
      ~count:1 Gen.unit (fun () ->
        let classify merge_head unmerged_paths =
          Worktree_parser.classify_merge_failure ~code:1 ~stdout:"hook failed"
            ~stderr:"" ~merge_head ~unmerged_paths
        in
        let error = function `Error _ -> true | `Conflict _ -> false in
        error (classify None "file")
        && error (classify (Some " \n") "file")
        && error (classify (Some "sha") " \n")
        && Poly.equal (classify (Some " sha\n") "file") (`Conflict "sha"))
  in

  let prop_remote_sha_total =
    Test.make ~name:"ls-remote SHA decoding is total" ~count:1000
      Gen.(pair string string)
      (fun (ref_name, stdout) ->
        match Worktree_parser.parse_ls_remote_sha ~ref_name stdout with
        | None -> true
        | Some sha ->
            (String.length sha = 40 || String.length sha = 64)
            && List.equal String.equal
                 (String.split_lines stdout)
                 [ sha ^ "\t" ^ ref_name ])
  in
  let prop_remote_sha_roundtrip =
    Test.make ~name:"ls-remote exact SHA roundtrip" ~count:300
      Gen.(pair (oneof_list [ 40; 64 ]) (int_range 0 15))
      (fun (length, digit) ->
        try
          let sha = String.make length "0123456789abcdef".[digit] in
          let ref_name = "refs/heads/patch" in
          Option.equal String.equal
            (Worktree_parser.parse_ls_remote_sha ~ref_name
               (sha ^ "\t" ^ ref_name ^ "\n"))
            (Some sha)
        with _ -> false)
  in
  let prop_remote_sha_boundaries =
    Test.make ~name:"ls-remote malformed or mismatched refs fail closed"
      ~count:1 Gen.unit (fun () ->
        let ref_name = "refs/heads/patch" in
        let sha = String.make 40 'a' in
        List.for_all
          [
            "";
            sha;
            sha ^ "\trefs/heads/other";
            String.make 39 'a' ^ "\t" ^ ref_name;
            String.make 40 'z' ^ "\t" ^ ref_name;
            sha ^ "\t" ^ ref_name ^ "\n" ^ sha ^ "\t" ^ ref_name;
          ]
          ~f:(fun stdout ->
            Option.is_none
              (Worktree_parser.parse_ls_remote_sha ~ref_name stdout)))
  in
  let suite =
    [
      prop_remote_sha_total;
      prop_remote_sha_roundtrip;
      prop_remote_sha_boundaries;
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
      prop_collision_reverse;
      prop_collision_valid;
      prop_merge_failure_total;
      prop_merge_conflict_evidence;
      prop_merge_error_diagnostics;
      prop_merge_failure_boundaries;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

let () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"push command classification is total" ~count:1000
       QCheck2.Gen.(triple int string_small string_small)
       (fun (code, stdout, stderr) ->
         try
           ignore (Worktree_parser.classify_push_result ~code ~stdout ~stderr);
           true
         with _ -> false))

let () =
  let open QCheck2 in
  let tests =
    [
      Test.make
        ~name:"fetch classifiers are total over arbitrary process output"
        ~count:1000
        Gen.(pair int string)
        (fun (code, stderr) ->
          try
            ignore (Worktree_parser.classify_fetch_result ~code ~stderr);
            ignore (Worktree_parser.classify_fetch_branch_result ~code ~stderr);
            true
          with _ -> false);
      Test.make ~name:"successful fetch ignores stale error output" ~count:500
        Gen.string (fun stderr ->
          try
            Result.is_ok (Worktree_parser.classify_fetch_result ~code:0 ~stderr)
            && Worktree_parser.equal_fetch_branch_result
                 (Worktree_parser.classify_fetch_branch_result ~code:0
                    ~stderr:("couldn't find remote ref " ^ stderr))
                 Worktree_parser.Fetch_branch_ok
          with _ -> false);
      Test.make ~name:"missing remote ref differs from transport failure"
        ~count:500
        Gen.(pair (int_range 1 255) (int_range 0 100000))
        (fun (code, number) ->
          try
            let missing =
              Printf.sprintf "fatal: couldn't find remote ref patch-%d\n" number
            in
            let transport =
              Printf.sprintf "fatal: failed to connect to host-%d\n" number
            in
            Worktree_parser.equal_fetch_branch_result
              (Worktree_parser.classify_fetch_branch_result ~code
                 ~stderr:missing)
              Worktree_parser.Fetch_branch_no_remote_ref
            && (match
                  Worktree_parser.classify_fetch_branch_result ~code
                    ~stderr:transport
                with
              | Worktree_parser.Fetch_branch_error message ->
                  String.is_substring message
                    ~substring:(String.strip transport)
              | Worktree_parser.Fetch_branch_ok
              | Worktree_parser.Fetch_branch_no_remote_ref ->
                  false)
            && Result.is_error
                 (Worktree_parser.classify_fetch_result ~code ~stderr:missing)
          with _ -> false);
      Test.make
        ~name:
          "dependency subject matching retains exact project and patch scope"
        ~count:500
        Gen.(pair (int_range 1 100000) (int_range 1 100000))
        (fun (project_number, patch_number) ->
          try
            let project = Printf.sprintf "project-%d" project_number in
            let id = Int.to_string patch_number in
            let subject =
              Printf.sprintf "[%s] Patch %s: dependency" project id
            in
            let matches project_name ids =
              Worktree_parser.is_ancestor_patch_subject ~project_name
                ~ancestor_ids:(List.map ids ~f:Patch_id.of_string)
                subject
            in
            matches project [ id ]
            && (not (matches (project ^ "-other") [ id ]))
            && (not (matches project [ id ^ "0" ]))
            && (not (matches project []))
            && not (matches "" [ id ])
          with _ -> false);
    ]
  in
  List.iter tests ~f:(fun test -> Test.check_exn test)

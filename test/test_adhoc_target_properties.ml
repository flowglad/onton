(* @archlint.module test
   @archlint.domain adhoc-branch *)

open Onton_core
open Adhoc_target

let arbitrary_string = QCheck2.Gen.string_size (QCheck2.Gen.int_bound 300)

let totality =
  QCheck2.Test.make ~name:"ad-hoc target parsing is total" ~count:1000
    arbitrary_string (fun value ->
      ignore (Adhoc_target.validate_remote_branch value);
      ignore (Adhoc_target.parse_add_value value);
      ignore (Adhoc_target.looks_like_operation value);
      ignore (Adhoc_target.parse_operation value);
      true)

let numeric_priority =
  QCheck2.Test.make ~name:"positive numeric add targets remain PR numbers"
    ~count:1 QCheck2.Gen.unit (fun () ->
      match Adhoc_target.parse_operation "+123" with
      | Ok (Add (Pull_request pr)) -> Types.Pr_number.to_int pr = 123
      | Ok (Add (Remote_branch _) | Remove_pr _) | Error _ -> false)

let remote_branch_forms =
  QCheck2.Test.make
    ~name:"named and explicitly numeric remote branches parse as branches"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let as_branch = function
        | Ok (Add (Remote_branch branch)) -> Some branch
        | Ok (Add (Pull_request _)) | Ok (Remove_pr _) | Error _ -> None
      in
      match
        ( as_branch (Adhoc_target.parse_operation "+feature/login"),
          as_branch (Adhoc_target.parse_operation "+branch:123") )
      with
      | Some named, Some numeric ->
          String.equal (Types.Branch.to_string named) "feature/login"
          && String.equal (Types.Branch.to_string numeric) "123"
      | Some _, None | None, Some _ | None, None -> false)

let ref_boundaries =
  QCheck2.Test.make ~name:"remote branch validation enforces git ref boundaries"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let valid = [ "feature/login"; "release-1.2"; "123"; "head" ] in
      let invalid =
        [
          "";
          "@";
          "HEAD";
          "-flag";
          "refs/heads/main";
          ".hidden";
          "feature..login";
          "feature//login";
          "feature.lock";
          "feature@{1}";
          "feature login";
        ]
      in
      Stdlib.List.for_all
        (fun branch ->
          Result.is_ok (Adhoc_target.validate_remote_branch branch))
        valid
      && Stdlib.List.for_all
           (fun branch ->
             Result.is_error (Adhoc_target.validate_remote_branch branch))
           invalid)

let safe_patch_id =
  QCheck2.Test.make
    ~name:"branch-backed patch ids are deterministic and filesystem-safe"
    ~count:1000
    QCheck2.Gen.(1 -- 1_000_000)
    (fun change_id ->
      let change_id = Types.Pr_number.of_int change_id in
      let first = Adhoc_target.branch_patch_id ~change_id in
      let second = Adhoc_target.branch_patch_id ~change_id in
      let value = Types.Patch_id.to_string first in
      Types.Patch_id.equal first second
      && (not (String.contains value '/'))
      && not (String.contains value '\\'))

let () =
  QCheck_base_runner.run_tests_main
    [
      totality;
      numeric_priority;
      remote_branch_forms;
      ref_boundaries;
      safe_patch_id;
    ]

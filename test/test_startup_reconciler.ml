open Base
open Onton
open Onton.Types

(** QCheck2 property-based tests for [Startup_reconciler.discover_pr_from_json].
*)

let () =
  let open QCheck2 in
  (* First live (OPEN/MERGED) entry wins *)
  let prop_first_live_wins =
    Test.make ~name:"discover_pr_from_json: first OPEN/MERGED entry wins"
      ~count:1 Gen.unit (fun () ->
        let json =
          {|[
               {"number":11,"state":"OPEN","baseRefName":"main"},
               {"number":22,"state":"MERGED","baseRefName":"dev"}
             ]|}
        in
        match Startup_reconciler.discover_pr_from_json json with
        | Ok (Some (pr, base, merged)) ->
            Pr_number.equal pr (Pr_number.of_int 11)
            && Branch.equal base (Branch.of_string "main")
            && not merged
        | Ok None -> false
        | Error _ -> false)
  in

  (* Valid OPEN entry returns correct data *)
  let prop_open_entry =
    Test.make ~name:"discover_pr_from_json: OPEN entry parsed" ~count:1 Gen.unit
      (fun () ->
        let json = {|[{"number":42,"state":"OPEN","baseRefName":"main"}]|} in
        match Startup_reconciler.discover_pr_from_json json with
        | Ok (Some (pr, base, merged)) ->
            Pr_number.equal pr (Pr_number.of_int 42)
            && Branch.equal base (Branch.of_string "main")
            && not merged
        | Ok None | Error _ -> false)
  in

  (* MERGED entry returns merged=true *)
  let prop_merged_entry =
    Test.make ~name:"discover_pr_from_json: MERGED entry returns merged=true"
      ~count:1 Gen.unit (fun () ->
        let json =
          {|[{"number":7,"state":"MERGED","baseRefName":"develop"}]|}
        in
        match Startup_reconciler.discover_pr_from_json json with
        | Ok (Some (_, _, merged)) -> merged
        | Ok None | Error _ -> false)
  in

  (* CLOSED entries are skipped *)
  let prop_closed_skipped =
    Test.make ~name:"discover_pr_from_json: CLOSED entries skipped" ~count:1
      Gen.unit (fun () ->
        let json = {|[{"number":1,"state":"CLOSED","baseRefName":"main"}]|} in
        match Startup_reconciler.discover_pr_from_json json with
        | Ok None -> true
        | Ok (Some _) | Error _ -> false)
  in

  (* CLOSED then OPEN: OPEN wins *)
  let prop_closed_then_open =
    Test.make ~name:"discover_pr_from_json: CLOSED then OPEN -> OPEN wins"
      ~count:1 Gen.unit (fun () ->
        let json =
          {|[{"number":1,"state":"CLOSED","baseRefName":"main"},{"number":2,"state":"OPEN","baseRefName":"dev"}]|}
        in
        match Startup_reconciler.discover_pr_from_json json with
        | Ok (Some (pr, _, _)) -> Pr_number.equal pr (Pr_number.of_int 2)
        | Ok None | Error _ -> false)
  in

  (* Malformed JSON -> Error *)
  let prop_malformed_json =
    Test.make ~name:"discover_pr_from_json: malformed JSON -> Error" ~count:1
      Gen.unit (fun () ->
        match Startup_reconciler.discover_pr_from_json "not json" with
        | Error _ -> true
        | Ok _ -> false)
  in

  (* Empty list -> Ok None *)
  let prop_empty_list =
    Test.make ~name:"discover_pr_from_json: empty list -> Ok None" ~count:1
      Gen.unit (fun () ->
        match Startup_reconciler.discover_pr_from_json "[]" with
        | Ok None -> true
        | Ok (Some _) | Error _ -> false)
  in

  let suite =
    [
      prop_first_live_wins;
      prop_open_entry;
      prop_merged_entry;
      prop_closed_skipped;
      prop_closed_then_open;
      prop_malformed_json;
      prop_empty_list;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode

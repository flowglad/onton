(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton_core
module G = Git_observation
module Gen = QCheck2.Gen

let parse =
  G.of_porcelain ~branch:(Some "patch") ~head:"head" ~sequencer:G.None_active

let tests =
  [
    QCheck2.Test.make ~name:"materialization safety classification is exact"
      ~count:500
      Gen.(
        oneof
          [
            string;
            oneof_list
              [
                "materialization_revision_changed";
                "materialization_branch_changed";
                "contradictory_materialization_receipts";
                "materialization pin failed";
              ];
          ])
      (fun reason ->
        G.materialization_failure_is_unsafe reason
        = List.mem reason
            [
              "materialization_revision_changed";
              "materialization_branch_changed";
              "contradictory_materialization_receipts";
            ]);
    QCheck2.Test.make ~name:"checkout observation totality" ~count:1000
      Gen.string (fun s ->
        try
          ignore (parse s);
          true
        with _ -> false);
    QCheck2.Test.make ~name:"staged and unstaged are independent" ~count:500
      Gen.(oneof_list [ "name"; "a\nb"; "a b"; "\"quoted\"" ])
      (fun path ->
        let o = parse ("MM " ^ path ^ "\000") in
        o.staged = [ path ] && o.unstaged = [ path ] && not (G.clean o));
    QCheck2.Test.make ~name:"all unmerged index statuses are conflicts"
      ~count:100
      Gen.(oneof_list [ "DD"; "AU"; "UD"; "UA"; "DU"; "AA"; "UU" ])
      (fun status ->
        let o = parse (status ^ " path\000") in
        o.conflicts = [ "path" ] && not (G.clean o));
    QCheck2.Test.make ~name:"rename source records are not additional dirtiness"
      ~count:500 Gen.string (fun path ->
        let path =
          "file" ^ String.map (fun c -> if c = '\000' then 'x' else c) path
        in
        let o = parse ("R  " ^ path ^ "\000old\000?? new\000") in
        o.staged = [ path ] && o.untracked = [ "new" ]);
    QCheck2.Test.make
      ~name:"repair requires staged resolution and active sequencer" ~count:100
      Gen.bool (fun active ->
        let o =
          G.of_porcelain ~branch:None ~head:"head"
            ~sequencer:
              (if active then
                 G.Rebase
                   {
                     target = "target";
                     original = "source";
                     step = "1";
                     head_ref = "refs/heads/patch";
                     merge_heads = [];
                   }
               else G.None_active)
            "M  resolved\000"
        in
        G.repair_ready o = active);
    QCheck2.Test.make
      ~name:"empty conflict resolutions select supervisor-owned skip" ~count:100
      Gen.bool (fun staged ->
        let o =
          G.of_porcelain ~branch:None ~head:"head"
            ~sequencer:
              (G.Rebase
                 {
                   target = "target";
                   original = "source";
                   step = "1";
                   head_ref = "refs/heads/patch";
                   merge_heads = [];
                 })
            (if staged then "M  file\000" else "")
        in
        G.equal_continuation (G.continuation o)
          (if staged then G.Continue else G.Skip_empty_replay));
    QCheck2.Test.make
      ~name:"recreated merges retain topology even with an empty staged diff"
      ~count:100 Gen.bool (fun staged ->
        let observe merge_heads =
          G.of_porcelain ~branch:None ~head:"head"
            ~sequencer:
              (G.Rebase
                 {
                   target = "target";
                   original = "source";
                   step = "5";
                   head_ref = "refs/heads/patch";
                   merge_heads;
                 })
            (if staged then "M  file\000" else "")
        in
        let merging = observe [ "side" ] in
        G.equal_continuation (G.continuation merging)
          (if staged then G.Continue else G.Complete_merge)
        && G.progress_key merging <> G.progress_key (observe [])
        && G.progress_key merging
           <> G.progress_key (observe [ "different-side" ]));
    QCheck2.Test.make ~name:"malformed status cannot establish clean state"
      ~count:500 Gen.string (fun text ->
        let o = parse ("?? " ^ text) in
        not (G.clean o));
    QCheck2.Test.make
      ~name:"integration receipts require the captured revision pair" ~count:500
      Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 1 30))
      (fun branch ->
        let line =
          "source head actor 123 +0000\trebase (finish): refs/heads/" ^ branch
          ^ " onto target\n"
        in
        G.completed_integration ~branch ~source:"source" ~target:"target"
          ~head:"head" ~reflog:line
        && (not
              (G.completed_integration ~branch ~source:"other" ~target:"target"
                 ~head:"head" ~reflog:line))
        && (not
              (G.completed_integration ~branch ~source:"source" ~target:"other"
                 ~head:"head" ~reflog:line))
        && not
             (G.completed_integration ~branch ~source:"source" ~target:"target"
                ~head:"head"
                ~reflog:(String.sub line 0 (String.length line - 1))));
  ]

let () = QCheck_base_runner.run_tests_main tests

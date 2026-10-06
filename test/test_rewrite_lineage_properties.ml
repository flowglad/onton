(* @archlint.module test
   @archlint.domain rewrite-lineage *)

open Base
open Onton_core
module Gen = QCheck2.Gen
module Test = QCheck2.Test

let sha n = Printf.sprintf "%040x" n

let entry before after message =
  before ^ " " ^ after ^ " Test <test@example.com> 0 +0000\t" ^ message

let finish before after target =
  entry before after ("rebase (finish): refs/heads/patch onto " ^ target)

type operation =
  | Rebase
  | Rebase_without_remote
  | Append
  | Discard
  | Remote_write
  | Fetch
  | Publish

let show_operation = function
  | Rebase -> "rebase"
  | Rebase_without_remote -> "rebase-without-remote"
  | Append -> "append"
  | Discard -> "discard"
  | Remote_write -> "remote-write"
  | Fetch -> "fetch"
  | Publish -> "publish"

let operations =
  [
    Rebase; Rebase_without_remote; Append; Discard; Remote_write; Fetch; Publish;
  ]

(* Independent commit graph: rebases either incorporate the observed remote in
   their target or discard it. Fetches update only tracking. Publication must
   preserve the remote history at every step, including retries. *)
let trace operations =
  let graph = ref [ (sha 1, []); (sha 2, [ sha 1 ]) ] in
  let rec ancestor a b =
    String.equal a b
    || List.exists
         (Option.value
            (List.Assoc.find !graph b ~equal:String.equal)
            ~default:[])
         ~f:(ancestor a)
  in
  let next = ref 2 in
  let create parents =
    Int.incr next;
    let commit = sha !next in
    graph := (commit, parents) :: !graph;
    commit
  in
  let local = ref (sha 2) in
  let remote = ref (sha 2) in
  let tracking = ref (sha 2) in
  let reflog = ref [ entry (sha 0) !local "branch: Created from HEAD" ] in
  let update message commit =
    reflog := entry !local commit message :: !reflog;
    local := commit
  in
  let rebase parent =
    let target = create [ parent ] in
    let result = create [ target ] in
    update ("rebase (finish): refs/heads/patch onto " ^ target) result
  in
  let inspect () =
    let authority =
      Rewrite_lineage.of_reflog ~branch:"patch" ~local_sha:!local
        ~remote_sha:!tracking
        ~reflog:(String.concat ~sep:"\n" (List.rev !reflog))
        ~ancestor_oracle:(fun a ~descendant -> ancestor a descendant)
    in
    let ancestry =
      if ancestor !tracking !local then Push_plan.Local_includes_remote
      else if ancestor !local !tracking then Push_plan.Local_missing_remote
      else Push_plan.Local_diverged_from_remote
    in
    List.for_all [ false; true ] ~f:(fun preserve_history ->
        let decision =
          Push_plan.plan ~preserve_history ~expected_branch:"patch"
            ~worktree_path_exists:true ~worktree_head_branch:(Some "patch")
            ~branch_ref_sha:(Some !local) ~remote_tracking_sha:(Some !tracking)
            ~ancestry ~remote_changes_included:false
            ~rewrite_authority:authority ~commits_ahead_of_base:(Some 1)
        in
        let expected = ancestor !tracking !local in
        match decision with
        | Push_plan.Refuse _ -> not expected
        | Push_plan.Push (Push_plan.Initial_push _) -> false
        | Push_plan.Push (Push_plan.Force_push_with_lease lease) ->
            expected
            && String.equal lease.local_sha !local
            && String.equal lease.remote_sha !tracking)
  in
  List.for_all operations ~f:(fun operation ->
      (match operation with
      | Rebase -> rebase !tracking
      | Rebase_without_remote -> rebase (sha 1)
      | Append -> update "commit: work" (create [ !local ])
      | Discard -> update "reset: moving to unrelated" (create [ sha 1 ])
      | Remote_write -> remote := create [ !remote ]
      | Fetch -> tracking := !remote
      | Publish -> ());
      inspect ())

let authority () =
  Rewrite_lineage.of_reflog ~branch:"patch" ~local_sha:(sha 3)
    ~remote_sha:(sha 2)
    ~reflog:
      (String.concat ~sep:"\n"
         [
           entry (sha 1) (sha 2) "commit: original";
           finish (sha 2) (sha 3) (sha 2);
         ])
    ~ancestor_oracle:(fun a ~descendant ->
      String.equal a descendant
      || String.equal a (sha 1)
      || (String.equal a (sha 2) && String.equal descendant (sha 3)))

let properties =
  [
    Test.make ~name:"rewrite lineage is total over malformed observations"
      ~count:1000
      Gen.(triple string string string)
      (fun (local_sha, remote_sha, reflog) ->
        Option.is_none
          (Rewrite_lineage.of_reflog ~branch:"patch" ~local_sha ~remote_sha
             ~reflog ~ancestor_oracle:(fun _ ~descendant:_ -> false)));
    Test.make
      ~name:
        "rebases preserve remote incorporation across Git event interleavings"
      ~count:3000
      ~print:(fun ops ->
        String.concat ~sep:", " (List.map ops ~f:show_operation))
      Gen.(list_size (int_range 1 80) (oneof_list operations))
      trace;
    Test.make
      ~name:
        "an incorporated old tip cannot authorize a rebase that drops remote"
      ~count:500
      Gen.(pair bool bool)
      (fun (incorporates_remote, append) ->
        let target = sha 5 in
        let result = sha 3 in
        let local_sha = if append then sha 6 else result in
        let graph =
          [
            (sha 1, []);
            (sha 2, [ sha 1 ]);
            (sha 4, [ sha 2 ]);
            (target, [ (if incorporates_remote then sha 2 else sha 1) ]);
            (result, [ target ]);
            (sha 6, [ result ]);
          ]
        in
        let rec ancestor a b =
          String.equal a b
          || List.exists
               (Option.value
                  (List.Assoc.find graph b ~equal:String.equal)
                  ~default:[])
               ~f:(ancestor a)
        in
        let reflog =
          [ finish (sha 4) result target ]
          @ if append then [ entry result local_sha "commit: work" ] else []
        in
        Bool.equal
          (Option.is_some
             (Rewrite_lineage.of_reflog ~branch:"patch" ~local_sha
                ~remote_sha:(sha 2) ~reflog:(String.concat ~sep:"\n" reflog)
                ~ancestor_oracle:(fun a ~descendant -> ancestor a descendant)))
          incorporates_remote);
    Test.make ~name:"rebase authority binds both captured publication commits"
      ~count:500
      Gen.(triple bool bool bool)
      (fun (change_local, change_remote, change_branch) ->
        match authority () with
        | None -> false
        | Some authority ->
            let local_sha = if change_local then sha 4 else sha 3 in
            let remote_sha = if change_remote then sha 5 else sha 2 in
            Bool.equal
              (Rewrite_lineage.authorizes authority
                 ~branch:(if change_branch then "other" else "patch")
                 ~local_sha ~remote_sha)
              (not (change_local || change_remote || change_branch)));
    Test.make
      ~name:
        "incomplete, foreign, malformed, and discarded rebases grant no \
         authority"
      ~count:500
      Gen.(
        oneof_list
          [
            "rebase (start): checkout base";
            "rebase (abort): returning to patch";
            "rebase (finish): refs/heads/other onto " ^ sha 1;
            "rebase (finish): refs/heads/patch onto invalid";
            "reset: moving to rewritten";
            "commit (amend): original";
          ])
      (fun message ->
        Option.is_none
          (Rewrite_lineage.of_reflog ~branch:"patch" ~local_sha:(sha 3)
             ~remote_sha:(sha 2)
             ~reflog:
               (String.concat ~sep:"\n"
                  [
                    entry (sha 1) (sha 2) "commit: original";
                    entry (sha 2) (sha 3) message;
                  ])
             ~ancestor_oracle:(fun a ~descendant ->
               String.equal a descendant || String.equal a (sha 1))));
    Test.make ~name:"missing reflog or ancestry fails closed" ~count:500
      Gen.bool (fun missing_reflog ->
        Option.is_none
          (Rewrite_lineage.of_reflog ~branch:"patch" ~local_sha:(sha 3)
             ~remote_sha:(sha 2)
             ~reflog:
               (if missing_reflog then ""
                else
                  String.concat ~sep:"\n"
                    [
                      entry (sha 1) (sha 2) "commit: original";
                      finish (sha 2) (sha 3) (sha 1);
                    ])
             ~ancestor_oracle:(fun _ ~descendant:_ -> false)));
    Test.make
      ~name:
        "captured rebase evidence cannot authorize changed refs or a different \
         branch"
      ~count:500
      Gen.(triple bool bool bool)
      (fun (change_local, change_remote, change_branch) ->
        let expected_branch = if change_branch then "other" else "patch" in
        let local_sha = if change_local then sha 4 else sha 3 in
        let remote_sha = if change_remote then sha 5 else sha 2 in
        let decision =
          Push_plan.plan ~preserve_history:false ~expected_branch
            ~worktree_path_exists:true
            ~worktree_head_branch:(Some expected_branch)
            ~branch_ref_sha:(Some local_sha)
            ~remote_tracking_sha:(Some remote_sha)
            ~ancestry:Push_plan.Local_diverged_from_remote
            ~remote_changes_included:false ~rewrite_authority:(authority ())
            ~commits_ahead_of_base:(Some 1)
        in
        match decision with
        | Push_plan.Push (Push_plan.Force_push_with_lease lease) ->
            (not (change_local || change_remote || change_branch))
            && String.equal lease.local_sha local_sha
            && String.equal lease.remote_sha remote_sha
        | Push_plan.Refuse refusal ->
            Push_plan.equal_refusal refusal
              (Push_plan.Remote_not_integrated { remote_sha })
            && (change_local || change_remote || change_branch)
        | Push_plan.Push (Push_plan.Initial_push _) -> false);
    Test.make ~name:"missing transitions cannot reconnect discarded history"
      ~count:300 Gen.bool (fun malformed ->
        let gap = if malformed then [ "malformed" ] else [] in
        Option.is_none
          (Rewrite_lineage.of_reflog ~branch:"patch" ~local_sha:(sha 3)
             ~remote_sha:(sha 2)
             ~reflog:
               (String.concat ~sep:"\n"
                  ([ entry (sha 1) (sha 2) "commit: original" ]
                  @ gap
                  @ [ finish (sha 4) (sha 3) (sha 1) ]))
             ~ancestor_oracle:(fun a ~descendant ->
               String.equal a descendant || String.equal a (sha 1))));
  ]

let () =
  (* The production retry suffix: resolve once, then retry without new commits. *)
  assert (trace [ Rebase; Publish; Publish; Publish ]);
  List.iter properties ~f:(fun test -> Test.check_exn test)

(* @archlint.module test
   @archlint.domain push-plan *)

open Base
open Onton_core
module Gen = QCheck2.Gen
module Test = QCheck2.Test
module PP = Push_plan

type inputs = {
  preserve_history : bool;
  exists : bool;
  head : string option;
  local : string option;
  remote : string option;
  ancestry : PP.ancestry;
  integrated : bool;
  commits : int option;
}

let gen =
  let open Gen in
  let* preserve_history = bool in
  let* exists = bool in
  let* head = option (oneof_list [ "patch"; "other" ]) in
  let* local = option string in
  let* remote = option string in
  let* ancestry =
    oneof_list
      [
        PP.Local_includes_remote;
        PP.Local_missing_remote;
        PP.Local_diverged_from_remote;
        PP.No_remote_yet;
        PP.Unknown;
      ]
  in
  let* integrated = bool in
  let* commits = option int in
  return
    {
      preserve_history;
      exists;
      head;
      local;
      remote;
      ancestry;
      integrated;
      commits;
    }

let plan i =
  PP.plan ~preserve_history:i.preserve_history ~expected_branch:"patch"
    ~worktree_path_exists:i.exists ~worktree_head_branch:i.head
    ~branch_ref_sha:i.local ~remote_tracking_sha:i.remote ~ancestry:i.ancestry
    ~remote_changes_included:i.integrated ~commits_ahead_of_base:i.commits

let safe ~local ~remote ~integrated =
  {
    preserve_history = false;
    exists = true;
    head = Some "patch";
    local = Some local;
    remote = Some remote;
    ancestry = PP.Local_diverged_from_remote;
    integrated;
    commits = Some 1;
  }

let label_pattern = Re.compile (Re.Pcre.re {|^[a-z][a-z0-9_]*$|})

let properties =
  [
    Test.make ~name:"publication planner is total and deterministic" ~count:1000
      gen (fun i -> PP.equal_decision (plan i) (plan i));
    Test.make ~name:"publication carries exactly the validated refs" ~count:1000
      gen (fun i ->
        match plan i with
        | PP.Refuse _ -> true
        | PP.Push (PP.Initial_push { local_sha }) ->
            i.exists
            && Option.equal String.equal i.head (Some "patch")
            && Option.equal String.equal i.local (Some local_sha)
            && Option.is_none i.remote
            && not (Option.equal Int.equal i.commits (Some 0))
        | PP.Push (PP.Force_push_with_lease { local_sha; remote_sha }) ->
            i.exists
            && Option.equal String.equal i.head (Some "patch")
            && Option.equal String.equal i.local (Some local_sha)
            && Option.equal String.equal i.remote (Some remote_sha)
            && (not (Option.equal Int.equal i.commits (Some 0)))
            && (not (PP.equal_ancestry i.ancestry PP.Local_missing_remote))
            && (PP.equal_ancestry i.ancestry PP.Local_includes_remote
               || ((not i.preserve_history) && i.integrated)));
    Test.make ~name:"valid initial and fast-forward publications always proceed"
      ~count:1000
      Gen.(pair (triple string string bool) bool)
      (fun ((local, remote, initial), preserve_history) ->
        let input =
          {
            (safe ~local ~remote ~integrated:false) with
            preserve_history;
            remote = (if initial then None else Some remote);
            ancestry =
              (if initial then PP.No_remote_yet else PP.Local_includes_remote);
          }
        in
        match plan input with
        | PP.Push (PP.Initial_push publication) ->
            initial && String.equal publication.local_sha local
        | PP.Push (PP.Force_push_with_lease publication) ->
            (not initial)
            && String.equal publication.local_sha local
            && String.equal publication.remote_sha remote
        | PP.Refuse _ -> false);
    Test.make
      ~name:"incorporated rewrites can always publish to an unchanged remote"
      ~count:1000
      Gen.(pair string string)
      (fun (local, remote) ->
        match plan (safe ~local ~remote ~integrated:true) with
        | PP.Push (PP.Force_push_with_lease lease) ->
            String.equal lease.local_sha local
            && String.equal lease.remote_sha remote
        | PP.Refuse _ | PP.Push (PP.Initial_push _) -> false);
    Test.make ~name:"unincorporated divergence cannot authorize publication"
      ~count:1000
      Gen.(pair string string)
      (fun (local, remote) ->
        match plan (safe ~local ~remote ~integrated:false) with
        | PP.Refuse (PP.Remote_not_integrated _) -> true
        | PP.Push _
        | PP.Refuse
            ( PP.Worktree_missing | PP.No_commits_ahead_of_base
            | PP.Branch_ref_missing _ | PP.Branch_switched _
            | PP.Local_missing_remote_commits _
            | PP.History_would_be_rewritten _ ) ->
            false);
    Test.make ~name:"every protected update preserves captured remote ancestry"
      ~count:1000 gen (fun i ->
        match plan { i with preserve_history = true } with
        | PP.Refuse _ -> true
        | PP.Push (PP.Initial_push _) -> Option.is_none i.remote
        | PP.Push (PP.Force_push_with_lease { local_sha; remote_sha }) ->
            PP.equal_ancestry i.ancestry PP.Local_includes_remote
            && Option.equal String.equal i.local (Some local_sha)
            && Option.equal String.equal i.remote (Some remote_sha));
    Test.make ~name:"history preservation only restricts publication authority"
      ~count:1000 gen (fun i ->
        let protected = plan { i with preserve_history = true } in
        match protected with
        | PP.Refuse _ -> true
        | PP.Push _ ->
            PP.equal_decision protected
              (plan { i with preserve_history = false }));
    Test.make
      ~name:"protected divergence is permanent even with equivalent patches"
      ~count:300
      Gen.(pair string string)
      (fun (local, remote) ->
        let i =
          {
            (safe ~local ~remote ~integrated:true) with
            preserve_history = true;
          }
        in
        match plan i with
        | PP.Refuse (PP.History_would_be_rewritten refusal) ->
            String.equal refusal.local_sha local
            && String.equal refusal.remote_sha remote
            && Option.value_map
                 (PP.to_push_reject_classify_rejection
                    (PP.History_would_be_rewritten refusal))
                 ~default:false ~f:Push_reject_classify.is_permanent
        | PP.Push _
        | PP.Refuse
            ( PP.No_commits_ahead_of_base | PP.Worktree_missing
            | PP.Branch_ref_missing _ | PP.Branch_switched _
            | PP.Local_missing_remote_commits _ | PP.Remote_not_integrated _ )
          ->
            false);
    Test.make
      ~name:"lease remains fixed across fetch and remote-write interleavings"
      ~count:3000
      Gen.(triple string string (list bool))
      (fun (local, incorporated, operations) ->
        match plan (safe ~local ~remote:incorporated ~integrated:true) with
        | PP.Refuse _ | PP.Push (PP.Initial_push _) -> false
        | PP.Push (PP.Force_push_with_lease lease) ->
            (* Model an independently owned remote and a mutable tracking ref.
             A fetch may change tracking but cannot alter publication authority.
             A stable suffix must permit publication; any intervening writer
             must invalidate the old lease. *)
            let remote, tracking =
              List.foldi operations ~init:(incorporated, incorporated)
                ~f:(fun index (remote, tracking) writes ->
                  if writes then
                    (incorporated ^ Int.to_string index ^ "-writer", tracking)
                  else (remote, remote))
            in
            let _ = tracking in
            let accepted = String.equal remote lease.remote_sha in
            if List.exists operations ~f:Fn.id then not accepted
            else accepted && String.equal lease.local_sha local);
    Test.make ~name:"planner labels are bounded lowercase snake_case"
      ~count:1000 gen (fun i ->
        let label = PP.short_label (plan i) in
        (not (String.is_empty label))
        && String.length label <= 32
        && Re.execp label_pattern label);
  ]

let () = List.iter properties ~f:(fun test -> Test.check_exn test)

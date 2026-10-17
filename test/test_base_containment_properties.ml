(* @archlint.module test
   @archlint.domain base-containment *)
open Base
open Onton_core
module G = QCheck2.Gen
module T = QCheck2.Test

let pid = Types.Patch_id.of_string
let branch = Types.Branch.of_string
let main = branch "main"
let branch_of id = branch (Types.Patch_id.to_string id)

let graph depth =
  let add g name deps =
    List.fold deps
      ~init:(Graph.add_patch g (pid name))
      ~f:(fun g dep -> Graph.add_dependency g (pid name) ~dep:(pid dep))
  in
  let g = add (Graph.of_patches []) "root" [] in
  let g = add (add g "a" [ "root" ]) "b" [ "root" ] in
  let g =
    List.fold
      (List.range 1 (depth + 1))
      ~init:g
      ~f:(fun g n ->
        add g (Int.to_string n)
          [ (if n = 1 then "root" else Int.to_string (n - 1)) ])
  in
  add g "p" [ "root"; "a"; "b"; Int.to_string depth ]

let mode graph =
  match Execution_mode.infer graph with
  | Ok mode -> mode
  | Error message -> failwith message

let strings = Set.of_list (module String)

type history = { content : Set.M(String).t; ancestry : Set.M(String).t }

type state = {
  root : history;
  layers : history list;
  merged : Set.M(String).t;
  epoch : int;
}

let empty = { content = strings []; ancestry = strings [] }

let initial depth =
  {
    root = empty;
    layers = List.init depth ~f:(fun _ -> empty);
    merged = strings [];
    epoch = 0;
  }

let token epoch = "root-revision-" ^ Int.to_string epoch
let sha name = "integrated-" ^ name

let advance state name =
  let epoch = state.epoch + 1 in
  let merged = Set.add state.merged name in
  {
    state with
    epoch;
    merged;
    root =
      {
        content = merged;
        ancestry =
          Set.add (Set.add state.root.ancestry (sha name)) (token epoch);
      };
  }

let rewrite state =
  let epoch = state.epoch + 1 in
  {
    state with
    epoch;
    root = { state.root with ancestry = strings [ token epoch ] };
  }

let heal state n =
  let source =
    if n = 0 then state.root else List.nth_exn state.layers (n - 1)
  in
  {
    state with
    layers =
      List.mapi state.layers ~f:(fun i old -> if i = n then source else old);
  }

let decisions ?(known = true) graph mode state =
  let has_merged id =
    Set.mem state.merged (Types.Patch_id.to_string id)
    || Execution_mode.equal mode Execution_mode.mainline
       && String.equal (Types.Patch_id.to_string id) "root"
  in
  let merge_sha id =
    if known then Some (sha (Types.Patch_id.to_string id)) else None
  in
  let ancestor_oracle ancestor ~descendant =
    let history =
      if String.equal descendant "root" || String.equal descendant "main" then
        state.root
      else
        match Int.of_string_opt descendant with
        | Some n -> Option.value (List.nth state.layers (n - 1)) ~default:empty
        | None -> empty
    in
    Set.mem history.ancestry
      (if String.equal ancestor "root" || String.equal ancestor "main" then
         token state.epoch
       else ancestor)
  in
  let contains =
    Base_containment.contains_merged_siblings_with_mode ~mode
      ~agent_of_branch:(fun _ -> None)
      ~revision_of_branch:(fun name ->
        if Types.Branch.equal name main then Some (token state.epoch) else None)
      ~graph ~patch_id:(pid "p") ~has_merged ~merge_sha ~branch_of ~main
      ~ancestor_oracle
  in
  let target =
    Base_containment.stale_chain_rebase_target_with_mode ~mode
      ~agent_of_branch:(fun _ -> None)
      ~revision_of_branch:(fun name ->
        if Types.Branch.equal name main then Some (token state.epoch) else None)
      ~graph ~patch_id:(pid "p") ~has_merged ~merge_sha ~branch_of ~main
      ~ancestor_oracle
  in
  (contains, target)

(* Create authority through the public owner protocol, including an agent
   recovery whose output has no asserted ancestry relationship to the target. *)
let repaired_agent ~base ~target ~candidate =
  let module B = Branch_reconcile in
  let commit value =
    match B.Commit.make value with
    | Some value -> value
    | None -> failwith "invalid fixture revision"
  in
  let source = commit (String.make 40 'a') in
  let target = commit target in
  let candidate = commit candidate in
  let step agent event = fst (Patch_agent.reconcile_branch agent event) in
  let reply agent result =
    match B.pending agent.Patch_agent.branch_reconcile with
    | None -> failwith "missing owned command"
    | Some command ->
        step agent (B.Result { token = command.token; at = 100.; result })
  in
  let observation =
    B.
      {
        source;
        target;
        remote = Some source;
        boundary = Recorded source;
        topology = Equal;
        clean = true;
        sequencer = None;
        conflicts = 0;
        target_included = false;
        base_contains_source = false;
        head = source;
        completed_integration = false;
        destination = Remote_id.of_destination "fixture-origin";
      }
  in
  let agent = Patch_agent.create ~branch:(branch "1") (pid "1") in
  let agent =
    step agent (B.Request { base; policy = Rewrite; purpose = Reconcile_base })
  in
  let agent = reply agent (B.Observed observation) in
  let agent = reply agent B.Pinned in
  let agent = step agent B.Recover in
  let observation = { observation with source = candidate; head = candidate } in
  let agent = reply agent (B.Inspected observation) in
  let command =
    match B.pending agent.Patch_agent.branch_reconcile with
    | Some command -> command
    | None -> failwith "missing recovery command"
  in
  let agent, effects =
    Patch_agent.reconcile_branch agent
      (B.Result
         {
           token = command.token;
           at = 100.;
           result = B.Recovery_observed observation;
         })
  in
  let token =
    match effects with
    | [ B.Repair token ] -> token
    | []
    | (B.Execute _ | B.Start_repair _ | B.Completed _) :: _
    | B.Repair _ :: _ :: _ ->
        failwith "missing repair prompt"
  in
  let agent = step agent (B.Repair_started token) in
  let agent = step agent (B.Repair_completed { token; at = 100. }) in
  let agent = reply agent (B.Integrated candidate) in
  let agent = reply agent B.Published in
  let agent =
    reply agent (B.Remote { sha = Some candidate; topology = Equal })
  in
  Patch_agent.set_head_oid agent (Some (B.Commit.to_string candidate))

let tests =
  [
    T.make
      ~name:
        "historical repair requires the current base head or Git containment"
      ~count:300
      G.(triple bool (int_range 0 2) bool)
      (fun (mainline, head_state, ancestry) ->
        try
          let graph = graph 1 in
          let mode = if mainline then Execution_mode.mainline else mode graph in
          let terminal = if mainline then "main" else "root" in
          let target = String.make 40 'b' in
          let candidate = String.make 40 'c' in
          let repaired =
            repaired_agent ~base:terminal ~target ~candidate |> fun agent ->
            Patch_agent.set_head_oid agent
              (match head_state with
              | 0 -> None
              | 1 -> Some candidate
              | _ -> Some (String.make 40 'd'))
          in
          let root =
            Patch_agent.create ~branch:(branch terminal) (pid "root")
            |> fun agent -> Patch_agent.set_head_oid agent (Some target)
          in
          let agent_of_branch name =
            if Types.Branch.equal name (branch "1") then Some repaired
            else if Types.Branch.equal name (branch terminal) then Some root
            else None
          in
          let has_merged id =
            List.mem
              [ pid "a"; pid "b"; pid "root" ]
              id ~equal:Types.Patch_id.equal
          in
          let revision_of_branch name =
            if Types.Branch.equal name main then Some target else None
          in
          let ancestor_oracle _ ~descendant:_ = ancestry in
          let contains =
            Base_containment.contains_merged_siblings_with_mode ~mode ~graph
              ~patch_id:(pid "p") ~has_merged
              ~merge_sha:(fun _ -> None)
              ~branch_of ~main ~agent_of_branch ~revision_of_branch
              ~ancestor_oracle
          in
          let frontier =
            Base_containment.stale_chain_rebase_target_with_mode ~mode ~graph
              ~patch_id:(pid "p") ~has_merged
              ~merge_sha:(fun _ -> None)
              ~branch_of ~main ~agent_of_branch ~revision_of_branch
              ~ancestor_oracle
          in
          let expected = Int.equal head_state 1 || ancestry in
          Bool.equal contains expected
          && Option.equal Types.Patch_id.equal frontier
               (if expected then None else Some (pid "1"))
        with _ -> false);
    T.make
      ~name:"accepted repair targets owner publication while forge head lags"
      ~count:100 G.bool (fun current_target ->
        try
          let graph = graph 1 in
          let mode = mode graph in
          let old_head = String.make 40 'b' in
          let new_head = String.make 40 'c' in
          let root =
            repaired_agent ~base:"main" ~target:old_head ~candidate:new_head
            |> fun agent -> Patch_agent.set_head_oid agent (Some old_head)
          in
          let repaired =
            repaired_agent ~base:"root"
              ~target:(if current_target then new_head else old_head)
              ~candidate:(String.make 40 'd')
          in
          let agent_of_branch name =
            if Types.Branch.equal name (branch "root") then Some root
            else if Types.Branch.equal name (branch "1") then Some repaired
            else None
          in
          let contains =
            Base_containment.contains_merged_siblings_with_mode ~mode ~graph
              ~patch_id:(pid "p")
              ~has_merged:(fun id ->
                List.mem [ pid "a"; pid "b" ] id ~equal:Types.Patch_id.equal)
              ~merge_sha:(fun _ -> None)
              ~branch_of ~main
              ~revision_of_branch:(fun _ -> None)
              ~agent_of_branch
              ~ancestor_oracle:(fun _ ~descendant:_ -> false)
          in
          Bool.equal contains current_target
        with _ -> false);
    T.make
      ~name:"accepted agent repairs release containment across terminal changes"
      ~count:300
      G.(pair bool (list bool))
      (fun (mainline, events) ->
        try
          let graph = graph 1 in
          let mode = if mainline then Execution_mode.mainline else mode graph in
          let terminal = if mainline then "main" else "root" in
          let sha n =
            "00000000"
            ^ Stdlib.Digest.to_hex (Stdlib.Digest.string (Int.to_string n))
          in
          let has_merged id =
            List.mem
              [ pid "a"; pid "b"; pid "root" ]
              id ~equal:Types.Patch_id.equal
          in
          let check epoch repaired =
            let root =
              Patch_agent.create ~branch:(branch terminal) (pid "root")
              |> fun agent -> Patch_agent.set_head_oid agent (Some (sha epoch))
            in
            let agent_of_branch name =
              if (not mainline) && Types.Branch.equal name (branch terminal)
              then Some root
              else if Types.Branch.equal name (branch "1") then repaired
              else None
            in
            let contains =
              Base_containment.contains_merged_siblings_with_mode ~mode ~graph
                ~patch_id:(pid "p") ~has_merged
                ~merge_sha:(fun _ -> None)
                ~branch_of ~main
                ~revision_of_branch:(fun name ->
                  if Types.Branch.equal name main then Some (sha epoch)
                  else None)
                ~agent_of_branch
                ~ancestor_oracle:(fun _ ~descendant:_ -> false)
            in
            let target =
              Base_containment.stale_chain_rebase_target_with_mode ~mode ~graph
                ~patch_id:(pid "p") ~has_merged
                ~merge_sha:(fun _ -> None)
                ~branch_of ~main
                ~revision_of_branch:(fun name ->
                  if Types.Branch.equal name main then Some (sha epoch)
                  else None)
                ~agent_of_branch
                ~ancestor_oracle:(fun _ ~descendant:_ -> false)
            in
            (contains, target)
          in
          let repair epoch =
            repaired_agent ~base:terminal ~target:(sha epoch)
              ~candidate:(sha (epoch + 10000))
          in
          let epoch, repaired, accepted_epoch, valid =
            List.fold events ~init:(0, None, None, true)
              ~f:(fun (epoch, repaired, accepted_epoch, valid) repair_now ->
                let epoch, repaired, accepted_epoch =
                  if repair_now then (epoch, Some (repair epoch), Some epoch)
                  else (epoch + 1, repaired, accepted_epoch)
                in
                let contains, target = check epoch repaired in
                let expected =
                  Option.equal Int.equal accepted_epoch (Some epoch)
                in
                ( epoch,
                  repaired,
                  accepted_epoch,
                  valid
                  && Bool.equal contains expected
                  && Option.equal Types.Patch_id.equal target
                       (if expected then None else Some (pid "1")) ))
          in
          ignore (repaired, accepted_epoch);
          let contains, target = check epoch (Some (repair epoch)) in
          valid && contains && Option.is_none target
        with _ -> false);
    T.make ~name:"containment and frontier are total on arbitrary cyclic graphs"
      ~count:500
      G.(
        pair
          (list (pair (int_range 1 6) (int_range 1 6)))
          (triple (list (int_range 1 6)) bool bool))
      (fun (edges, (merged, known, ancestry)) ->
        try
          let original = graph 6 in
          let mode = mode original in
          let graph =
            List.fold edges ~init:original ~f:(fun graph (a, b) ->
                Graph.add_dependency graph
                  (pid (Int.to_string a))
                  ~dep:(pid (Int.to_string b)))
          in
          let has_merged id =
            match Int.of_string_opt (Types.Patch_id.to_string id) with
            | Some n -> List.mem merged n ~equal:Int.equal
            | None ->
                String.equal (Types.Patch_id.to_string id) "a"
                || String.equal (Types.Patch_id.to_string id) "b"
          in
          let merge_sha _ = if known then Some "sha" else None in
          let ancestor_oracle _ ~descendant:_ = ancestry in
          List.iter [ mode; Execution_mode.mainline ] ~f:(fun mode ->
              ignore
                (Base_containment.contains_merged_siblings_with_mode ~mode
                   ~agent_of_branch:(fun _ -> None)
                   ~revision_of_branch:(fun _ -> None)
                   ~graph ~patch_id:(pid "p") ~has_merged ~merge_sha ~branch_of
                   ~main ~ancestor_oracle
                  : bool);
              ignore
                (Base_containment.stale_chain_rebase_target_with_mode ~mode
                   ~agent_of_branch:(fun _ -> None)
                   ~revision_of_branch:(fun _ -> None)
                   ~graph ~patch_id:(pid "p") ~has_merged ~merge_sha ~branch_of
                   ~main ~ancestor_oracle
                  : Types.Patch_id.t option));
          true
        with _ -> false);
    T.make
      ~name:"feature intermediate needs commits or current terminal ancestry"
      ~count:200
      G.(triple bool bool bool)
      (fun (known, commits_present, root_present) ->
        try
          let graph = graph 1 in
          let mode = mode graph in
          let has_merged id =
            List.mem [ pid "a"; pid "b" ] id ~equal:Types.Patch_id.equal
          in
          let merge_sha _ = if known then Some "old-sha" else None in
          let ancestor_oracle ancestor ~descendant:_ =
            if String.equal ancestor "root" then root_present
            else commits_present
          in
          let contains =
            Base_containment.contains_merged_siblings_with_mode ~mode
              ~agent_of_branch:(fun _ -> None)
              ~revision_of_branch:(fun _ -> None)
              ~graph ~patch_id:(pid "p") ~has_merged ~merge_sha ~branch_of ~main
              ~ancestor_oracle
          in
          Bool.equal contains ((known && commits_present) || root_present)
        with _ -> false);
    T.make
      ~name:
        "feature terminal remains ready after arbitrary integrations and \
         rewrites"
      ~count:500
      G.(list (pair bool bool))
      (fun events ->
        try
          let graph = Graph.remove_patch (graph 1) (pid "1") in
          let mode = mode graph in
          let _, valid =
            List.fold events
              ~init:(advance (advance (initial 0) "a") "b", true)
              ~f:(fun (state, valid) (integrate, rebase) ->
                let state =
                  if integrate then advance state "a" else advance state "b"
                in
                let state = if rebase then rewrite state else state in
                let contains, target = decisions graph mode state in
                let eligible =
                  Start_eligibility.decide ~base_is_main:false
                    ~base_branch:"root" ~base_patch_merged:false
                    ~base_patch_busy_rebasing:false
                    ~base_patch_has_conflict:false ~base_structurally_fresh:true
                    ~base_contains_merged_siblings:contains
                in
                ( state,
                  valid && contains && Option.is_none target
                  && Start_eligibility.equal_decision eligible
                       Start_eligibility.Allow ))
          in
          valid
        with _ -> false);
    T.make
      ~name:
        "fan-in interleavings converge after terminal rewrites and missing SHAs"
      ~count:1000
      G.(pair (pair bool bool) (pair (int_range 1 6) (list (int_range 0 9))))
      (fun ((mainline, known), (depth, events)) ->
        try
          let graph = graph depth in
          let mode = if mainline then Execution_mode.mainline else mode graph in
          let valid state =
            let contains, target = decisions ~known graph mode state in
            let content = (List.nth_exn state.layers (depth - 1)).content in
            let deps_ready =
              Set.mem state.merged "a" && Set.mem state.merged "b"
            in
            ((not contains)
            || (deps_ready && Set.is_subset state.merged ~of_:content))
            && ((not contains) || Option.is_none target)
          in
          let state, safe =
            List.fold events
              ~init:(initial depth, true)
              ~f:(fun (state, safe) event ->
                let state =
                  match event with
                  | 0 -> advance state "a"
                  | 1 -> advance state "b"
                  | 2 | 3 -> rewrite state
                  | n -> heal state ((n - 4) % depth)
                in
                (state, safe && valid state))
          in
          let state = rewrite (advance (advance state "a") "b") in
          let state, safe =
            List.fold (List.range 0 depth) ~init:(state, safe)
              ~f:(fun (state, safe) _ ->
                let _, target = decisions ~known graph mode state in
                match target with
                | None ->
                    (state, safe && fst (decisions ~known graph mode state))
                | Some id ->
                    let n = Int.of_string (Types.Patch_id.to_string id) - 1 in
                    let next = heal state n in
                    (next, safe && valid next))
          in
          safe && fst (decisions ~known graph mode state)
        with _ -> false);
  ]

let () = QCheck_base_runner.run_tests_main tests

(* @archlint.module exempt
   @archlint.exempt-reason test-support *)

open Base
open Onton_core
open Types
module B = Branch_reconcile
module P = Publication_fixture
module R = Replay_fixture

let conflict ~base ~step ~state initial =
  match B.operation (state initial) with
  | Some { B.repair = Some repair; _ } when repair.B.conflicts > 0 -> initial
  | Some _ | None ->
      let sequence =
        match B.operation (state initial) with
        | None -> 1
        | Some op -> op.B.id + 1
      in
      let source = R.commit ((sequence * 10) + 1)
      and target = R.commit ((sequence * 10) + 2) in
      let reply = P.reply ~step ~state in
      let t =
        step initial
          (B.Request
             {
               base;
               policy = Preserve_ancestry;
               purpose =
                 Reconcile_request ("fixture-conflict:" ^ Int.to_string sequence);
             })
      in
      let t =
        reply t
          (B.Observed
             (R.observation ~source ~target ~boundary:Plain ~noop:false))
      in
      let t = reply t B.Pinned in
      reply t
        (B.Conflict
           { head = source; sequencer = "fixture-rebase"; conflicts = 1 })

let resolve ~step ~state initial =
  match B.operation (state initial) with
  | Some
      {
        B.repair = Some _;
        source = Some source;
        target = Some target;
        boundary;
        id;
        _;
      } ->
      let reply = P.reply ~step ~state in
      let t = step initial B.Recover in
      let t =
        reply t
          (R.repair_inspection ~source ~target ~boundary
             ~sequencer:"fixture-rebase" ~conflicts:0)
      in
      let candidate = R.commit ((id * 10) + 3) in
      let t = reply t (B.Integrated candidate) in
      let t = reply t B.Published in
      reply t (B.Remote { sha = Some candidate; topology = Equal })
  | Some _ | None -> initial

let agent t =
  conflict
    ~base:
      (Branch.to_string
         (Option.value t.Patch_agent.base_branch
            ~default:(Branch.of_string "main")))
    ~step:(fun t event -> fst (Patch_agent.reconcile_branch t event))
    ~state:(fun t -> t.Patch_agent.branch_reconcile)
    t

let resolved_agent t =
  resolve
    ~step:(fun t event -> fst (Patch_agent.reconcile_branch t event))
    ~state:(fun t -> t.Patch_agent.branch_reconcile)
    t

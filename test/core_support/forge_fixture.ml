(* @archlint.module exempt
   @archlint.exempt-reason test-support *)

open Onton_core
module F = Forge_observation

let base : Pr_state.t =
  {
    status = Pr_state.Open;
    is_draft = false;
    merge_state = Pr_state.Unknown;
    merge_ready = false;
    merge_ready_divergence = None;
    review_decision = None;
    check_status = Pr_state.Pending;
    ci_checks = [];
    ci_checks_truncated = false;
    comments = [];
    unresolved_comment_count = 0;
    findings = [];
    pr_number = Some (Types.Pr_number.of_int 7);
    node_id = None;
    merge_queue_required = false;
    merge_queue_entry = None;
    native_stack = false;
    head_branch = None;
    base_oid = Some (String.make 40 'b');
    head_oid = Some (String.make 40 'a');
    merge_commit_sha = None;
    base_branch = Some (Types.Branch.of_string "main");
    is_fork = false;
  }

let request id = F.{ id; pr_number = Types.Pr_number.of_int 7 }

let scope =
  F.
    {
      pr_number = Some (Types.Pr_number.of_int 7);
      branch = "patch";
      generation = 0;
      head = base.Pr_state.head_oid;
      base_branch = Some "main";
    }

let observe request merge_state =
  F.
    {
      request;
      outcome = Poll_outcome.Ok_pr_state { base with Pr_state.merge_state };
    }

(** Supply valid identities for a positive readiness fixture. Callers testing
    missing identity must remove it after this setup, or omit this helper. *)
let readiness_agent agent =
  let module A = Patch_agent in
  if not (A.is_pr_present agent) then agent
  else
    match (A.pr_number agent, agent.A.base_branch) with
    | Some pr_number, Some base_branch -> (
        let oid label =
          if Git_oid.valid label then label else Publication_fixture.sha label
        in
        let head =
          match agent.A.head_oid with
          | Some head -> oid head
          | None -> String.make 40 'a'
        in
        let agent = A.set_head_oid agent (Some head) in
        let agent =
          A.set_review_requested_for_oid agent
            (Option.map oid agent.A.review_requested_for_oid)
        in
        let base_revision =
          match
            List.find_opt
              (fun receipt ->
                receipt.Branch_reconcile.capture.Branch_reconcile.base_branch
                = Some (Types.Branch.to_string base_branch))
              (Branch_reconcile.integrations agent.A.branch_reconcile)
          with
          | Some receipt ->
              Branch_reconcile.Commit.to_string
                receipt.Branch_reconcile.capture
                  .Branch_reconcile.target_revision
          | None -> String.make 40 'b'
        in
        let request : F.request = { F.id = "readiness-fixture"; F.pr_number } in
        let state =
          {
            base with
            Pr_state.pr_number = Some pr_number;
            head_branch = Some agent.A.branch;
            head_oid = Some head;
            base_branch = Some base_branch;
            base_oid = Some base_revision;
            merge_state =
              (if agent.A.mergeability_unknown then Pr_state.Unknown
               else Pr_state.Mergeable);
          }
        in
        let agent, ticket = A.begin_forge_observation agent ~request in
        match ticket with
        | None -> failwith "readiness fixture requires valid request identity"
        | Some ticket -> (
            let agent, result =
              A.accept_forge_observation ~confirmed_base:base_revision agent
                ~ticket ~confirmed_head:(Some head)
                { F.request; F.outcome = Poll_outcome.Ok_pr_state state }
            in
            match result with Ok _ -> agent | Error reason -> failwith reason))
    | None, _ | _, None -> agent

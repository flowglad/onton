open Base
open Types

(** Pure decision logic for post-action completion.

    Mirrors the pattern of [Poll_applicator]: given orchestrator state and
    context, return the updated state plus a list of side effects for the runner
    to execute. *)

type github_effect =
  | Set_pr_description of { pr_number : Pr_number.t; body : string }
  | Set_pr_draft of { pr_number : Pr_number.t; draft : bool }
[@@deriving show, eq, sexp_of]

let on_start_discovered t ~patch_id ~pr_number ~base_branch ~main_branch
    ~pr_body =
  let description_effect = Set_pr_description { pr_number; body = pr_body } in
  let draft_effect =
    if Branch.equal base_branch main_branch then
      Set_pr_draft { pr_number; draft = false }
    else Set_pr_draft { pr_number; draft = true }
  in
  let t = Orchestrator.set_pr_number t patch_id pr_number in
  let t = Orchestrator.enqueue t patch_id Operation_kind.Implementation_notes in
  let t = Orchestrator.complete t patch_id in
  (t, [ description_effect; draft_effect ])

let on_start_discovery_failed t ~patch_id =
  let t = Orchestrator.on_pr_discovery_failure t patch_id in
  Orchestrator.complete t patch_id

let on_respond_ok t ~patch_id ~kind ~main_branch =
  let t =
    if Operation_kind.equal kind Operation_kind.Merge_conflict then
      Orchestrator.clear_has_conflict t patch_id
    else t
  in
  let t = Orchestrator.complete t patch_id in
  let effects =
    if Operation_kind.equal kind Operation_kind.Implementation_notes then
      let agent = Orchestrator.agent t patch_id in
      match (agent.Patch_agent.base_branch, agent.Patch_agent.pr_number) with
      | Some base, Some pr_number when Branch.equal base main_branch ->
          [ Set_pr_draft { pr_number; draft = false } ]
      | _ -> []
    else []
  in
  (t, effects)

(* -- Property tests -- *)

let make_orchestrator ~patch_id ~main_branch =
  let patch =
    Patch.
      {
        id = patch_id;
        title = "test";
        description = "test";
        branch = Branch.of_string "test-branch";
        dependencies = [];
        spec = "";
        acceptance_criteria = [];
        changes = [];
        files = [];
        classification = "";
        test_stubs_introduced = [];
        test_stubs_implemented = [];
      }
  in
  Orchestrator.create ~patches:[ patch ] ~main_branch

(** Helper: create an orchestrator with a started agent (busy, no PR). *)
let make_started ~patch_id ~main_branch ~base_branch =
  let t = make_orchestrator ~patch_id ~main_branch in
  Orchestrator.fire t (Orchestrator.Start (patch_id, base_branch))

(** Helper: create an orchestrator with a responding agent. *)
let make_responding ~patch_id ~main_branch ~base_branch ~kind =
  let t = make_started ~patch_id ~main_branch ~base_branch in
  (* Simulate the Start → discover → enqueue → complete cycle *)
  let pr = Pr_number.of_int 42 in
  let t = Orchestrator.set_pr_number t patch_id pr in
  let t = Orchestrator.enqueue t patch_id kind in
  let t = Orchestrator.complete t patch_id in
  (* Now fire the Respond *)
  Orchestrator.fire t (Orchestrator.Respond (patch_id, kind))

let pid = Patch_id.of_string "p1"
let main = Branch.of_string "main"
let feature = Branch.of_string "feature"
let pr42 = Pr_number.of_int 42

let%test "on_start_discovered: agent has_pr after discovery" =
  let t = make_started ~patch_id:pid ~main_branch:main ~base_branch:main in
  let t, _effects =
    on_start_discovered t ~patch_id:pid ~pr_number:pr42 ~base_branch:main
      ~main_branch:main ~pr_body:"body"
  in
  let a = Orchestrator.agent t pid in
  a.Patch_agent.has_pr

let%test "on_start_discovered: agent not busy after discovery" =
  let t = make_started ~patch_id:pid ~main_branch:main ~base_branch:main in
  let t, _effects =
    on_start_discovered t ~patch_id:pid ~pr_number:pr42 ~base_branch:main
      ~main_branch:main ~pr_body:"body"
  in
  let a = Orchestrator.agent t pid in
  not a.Patch_agent.busy

let%test "on_start_discovered: Implementation_notes enqueued" =
  let t = make_started ~patch_id:pid ~main_branch:main ~base_branch:main in
  let t, _effects =
    on_start_discovered t ~patch_id:pid ~pr_number:pr42 ~base_branch:main
      ~main_branch:main ~pr_body:"body"
  in
  let a = Orchestrator.agent t pid in
  List.mem a.Patch_agent.queue Operation_kind.Implementation_notes
    ~equal:Operation_kind.equal

let%test "on_start_discovered: pr_number set" =
  let t = make_started ~patch_id:pid ~main_branch:main ~base_branch:main in
  let t, _effects =
    on_start_discovered t ~patch_id:pid ~pr_number:pr42 ~base_branch:main
      ~main_branch:main ~pr_body:"body"
  in
  let a = Orchestrator.agent t pid in
  Option.equal Pr_number.equal a.Patch_agent.pr_number (Some pr42)

let%test "on_start_discovered: un-drafts when base is main" =
  let t = make_started ~patch_id:pid ~main_branch:main ~base_branch:main in
  let _t, effects =
    on_start_discovered t ~patch_id:pid ~pr_number:pr42 ~base_branch:main
      ~main_branch:main ~pr_body:"body"
  in
  List.exists effects ~f:(fun e ->
      equal_github_effect e (Set_pr_draft { pr_number = pr42; draft = false }))

let%test "on_start_discovered: sets draft when base is not main" =
  let t = make_started ~patch_id:pid ~main_branch:main ~base_branch:feature in
  let _t, effects =
    on_start_discovered t ~patch_id:pid ~pr_number:pr42 ~base_branch:feature
      ~main_branch:main ~pr_body:"body"
  in
  List.exists effects ~f:(fun e ->
      equal_github_effect e (Set_pr_draft { pr_number = pr42; draft = true }))

let%test "on_start_discovered: sets PR description" =
  let t = make_started ~patch_id:pid ~main_branch:main ~base_branch:main in
  let _t, effects =
    on_start_discovered t ~patch_id:pid ~pr_number:pr42 ~base_branch:main
      ~main_branch:main ~pr_body:"test body"
  in
  List.exists effects ~f:(fun e ->
      equal_github_effect e
        (Set_pr_description { pr_number = pr42; body = "test body" }))

let%test "on_start_discovery_failed: agent not busy" =
  let t = make_started ~patch_id:pid ~main_branch:main ~base_branch:main in
  let t = on_start_discovery_failed t ~patch_id:pid in
  let a = Orchestrator.agent t pid in
  not a.Patch_agent.busy

let%test "on_start_discovery_failed: agent still has no PR" =
  let t = make_started ~patch_id:pid ~main_branch:main ~base_branch:main in
  let t = on_start_discovery_failed t ~patch_id:pid in
  let a = Orchestrator.agent t pid in
  not a.Patch_agent.has_pr

let%test "on_start_discovery_failed: escalates after two failures" =
  let t = make_started ~patch_id:pid ~main_branch:main ~base_branch:main in
  (* First failure *)
  let t = on_start_discovery_failed t ~patch_id:pid in
  (* Re-fire Start for second attempt *)
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  (* Second failure *)
  let t = on_start_discovery_failed t ~patch_id:pid in
  let a = Orchestrator.agent t pid in
  a.Patch_agent.needs_intervention

let%test "on_start_discovery_failed: first failure does not need intervention" =
  let t = make_started ~patch_id:pid ~main_branch:main ~base_branch:main in
  let t = on_start_discovery_failed t ~patch_id:pid in
  let a = Orchestrator.agent t pid in
  not a.Patch_agent.needs_intervention

let%test "on_respond_ok: agent not busy after respond" =
  let t =
    make_responding ~patch_id:pid ~main_branch:main ~base_branch:main
      ~kind:Operation_kind.Implementation_notes
  in
  let t, _effects =
    on_respond_ok t ~patch_id:pid ~kind:Operation_kind.Implementation_notes
      ~main_branch:main
  in
  let a = Orchestrator.agent t pid in
  not a.Patch_agent.busy

let%test "on_respond_ok: un-drafts after impl notes when base is main" =
  let t =
    make_responding ~patch_id:pid ~main_branch:main ~base_branch:main
      ~kind:Operation_kind.Implementation_notes
  in
  let _t, effects =
    on_respond_ok t ~patch_id:pid ~kind:Operation_kind.Implementation_notes
      ~main_branch:main
  in
  List.exists effects ~f:(fun e ->
      equal_github_effect e (Set_pr_draft { pr_number = pr42; draft = false }))

let%test "on_respond_ok: no draft effect for impl notes when base is not main" =
  let t =
    make_responding ~patch_id:pid ~main_branch:main ~base_branch:feature
      ~kind:Operation_kind.Implementation_notes
  in
  let _t, effects =
    on_respond_ok t ~patch_id:pid ~kind:Operation_kind.Implementation_notes
      ~main_branch:main
  in
  List.is_empty effects

let%test "on_respond_ok: clears has_conflict for merge conflict" =
  let t = make_started ~patch_id:pid ~main_branch:main ~base_branch:main in
  let pr = Pr_number.of_int 42 in
  let t = Orchestrator.set_pr_number t pid pr in
  let t = Orchestrator.set_has_conflict t pid in
  let t = Orchestrator.enqueue t pid Operation_kind.Merge_conflict in
  let t = Orchestrator.complete t pid in
  let t = Orchestrator.fire t (Orchestrator.Respond (pid, Merge_conflict)) in
  let t, _effects =
    on_respond_ok t ~patch_id:pid ~kind:Operation_kind.Merge_conflict
      ~main_branch:main
  in
  let a = Orchestrator.agent t pid in
  not a.Patch_agent.has_conflict

let%test "on_respond_ok: no draft effect for CI feedback" =
  let t =
    make_responding ~patch_id:pid ~main_branch:main ~base_branch:main
      ~kind:Operation_kind.Ci
  in
  let _t, effects =
    on_respond_ok t ~patch_id:pid ~kind:Operation_kind.Ci ~main_branch:main
  in
  List.is_empty effects

let%test "on_respond_ok: no draft effect for review comments" =
  let t =
    make_responding ~patch_id:pid ~main_branch:main ~base_branch:main
      ~kind:Operation_kind.Review_comments
  in
  let _t, effects =
    on_respond_ok t ~patch_id:pid ~kind:Operation_kind.Review_comments
      ~main_branch:main
  in
  List.is_empty effects

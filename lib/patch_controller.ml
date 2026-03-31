open Base
open Types

type github_effect =
  | Set_pr_description of {
      patch_id : Patch_id.t;
      pr_number : Pr_number.t;
      body : string;
    }
  | Set_pr_draft of {
      patch_id : Patch_id.t;
      pr_number : Pr_number.t;
      draft : bool;
    }
[@@deriving show, eq, sexp_of]

let enqueue_notes_if_needed t patch_id (agent : Patch_agent.t) =
  if (not agent.has_pr) || agent.merged || agent.implementation_notes_delivered
  then t
  else
    let already_queued =
      List.mem agent.queue Operation_kind.Implementation_notes
        ~equal:Operation_kind.equal
      || Option.equal Operation_kind.equal agent.current_op
           (Some Operation_kind.Implementation_notes)
    in
    if already_queued then t
    else Orchestrator.enqueue t patch_id Operation_kind.Implementation_notes

let reconcile_patch t ~project_name ~gameplan ~(patch : Patch.t) =
  let patch_id = patch.id in
  let agent = Orchestrator.agent t patch_id in
  let t =
    if
      (not agent.has_pr)
      && agent.start_attempts_without_pr >= 2
      && not agent.needs_intervention
    then Orchestrator.set_needs_intervention t patch_id
    else t
  in
  let agent = Orchestrator.agent t patch_id in
  let t = enqueue_notes_if_needed t patch_id agent in
  let agent = Orchestrator.agent t patch_id in
  let effects = ref [] in
  (match agent.pr_number with
  | Some pr_number ->
      if not agent.pr_description_applied then
        effects :=
          Set_pr_description
            {
              patch_id;
              pr_number;
              body = Prompt.render_pr_description ~project_name patch gameplan;
            }
          :: !effects;
      (match agent.base_branch with
      | Some base_branch ->
          let desired_draft =
            if Branch.equal base_branch (Orchestrator.main_branch t) then
              not agent.implementation_notes_delivered
            else true
          in
          if Bool.(agent.is_draft <> desired_draft) then
            effects :=
              Set_pr_draft { patch_id; pr_number; draft = desired_draft }
              :: !effects
      | None -> ())
  | None -> ());
  (t, List.rev !effects)

let reconcile_all t ~project_name ~gameplan =
  List.fold gameplan.Gameplan.patches ~init:(t, []) ~f:(fun (orch, acc) patch ->
      let orch, effects =
        reconcile_patch orch ~project_name ~gameplan ~patch
      in
      (orch, acc @ effects))

let apply_github_effect_success t = function
  | Set_pr_description { patch_id; _ } ->
      Orchestrator.set_pr_description_applied t patch_id true
  | Set_pr_draft { patch_id; draft; _ } ->
      Orchestrator.set_is_draft t patch_id draft

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
  (patch, Orchestrator.create ~patches:[ patch ] ~main_branch)

let pid = Patch_id.of_string "p1"
let main = Branch.of_string "main"

let%test "reconcile_patch escalates repeated start discovery failures" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t =
    let t = Orchestrator.increment_start_attempts_without_pr t pid in
    Orchestrator.increment_start_attempts_without_pr t pid
  in
  let t, _ = reconcile_patch t ~project_name:"proj"
      ~gameplan:
        Gameplan.
          {
            project_name = "proj";
            problem_statement = "";
            solution_summary = "";
            design_decisions = "";
            patches = [ patch ];
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
          }
      ~patch
  in
  (Orchestrator.agent t pid).Patch_agent.needs_intervention

let%test "reconcile_patch enqueues implementation notes while missing" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.complete t pid in
  let t, _ =
    reconcile_patch t ~project_name:"proj"
      ~gameplan:
        Gameplan.
          {
            project_name = "proj";
            problem_statement = "";
            solution_summary = "";
            design_decisions = "";
            patches = [ patch ];
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
          }
      ~patch
  in
  List.mem (Orchestrator.agent t pid).Patch_agent.queue
    Operation_kind.Implementation_notes ~equal:Operation_kind.equal

let%test "reconcile_patch emits description effect while unapplied" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.complete t pid in
  let _, effects =
    reconcile_patch t ~project_name:"proj"
      ~gameplan:
        Gameplan.
          {
            project_name = "proj";
            problem_statement = "";
            solution_summary = "";
            design_decisions = "";
            patches = [ patch ];
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
          }
      ~patch
  in
  List.exists effects ~f:(function
    | Set_pr_description { patch_id; _ } -> Patch_id.equal patch_id pid
    | Set_pr_draft _ -> false)

let%test "reconcile_patch requests ready-for-review after notes on main" =
  let patch, t = make_orchestrator ~patch_id:pid ~main_branch:main in
  let t = Orchestrator.fire t (Orchestrator.Start (pid, main)) in
  let t = Orchestrator.set_pr_number t pid (Pr_number.of_int 42) in
  let t = Orchestrator.set_implementation_notes_delivered t pid true in
  let t = Orchestrator.complete t pid in
  let _, effects =
    reconcile_patch t ~project_name:"proj"
      ~gameplan:
        Gameplan.
          {
            project_name = "proj";
            problem_statement = "";
            solution_summary = "";
            design_decisions = "";
            patches = [ patch ];
            current_state_analysis = "";
            explicit_opinions = "";
            acceptance_criteria = [];
          }
      ~patch
  in
  List.exists effects ~f:(function
    | Set_pr_draft { patch_id; draft = false; _ } -> Patch_id.equal patch_id pid
    | Set_pr_description _ | Set_pr_draft _ -> false)

(* @archlint.module test
   @archlint.domain backend-routing *)

open Onton_core

let expected = function
  | Some 1 -> ("gpt-6-luna", "medium")
  | Some 2 -> ("gpt-6-luna", "xhigh")
  | Some _ | None -> ("gpt-6-sol", "medium")

let ladder =
  QCheck2.Test.make ~name:"shared auto ladder is total and selects exact tiers"
    ~count:500
    QCheck2.Gen.(
      option (oneof [ int; oneof_list [ 1; 2; 3; 0; -1; min_int; max_int ] ]))
    (fun complexity ->
      let model, effort = expected complexity in
      Openai_auto_settings.auto_model ~complexity = Some model
      && Openai_auto_settings.auto_effort ~complexity = Some effort)

let backend_agreement =
  QCheck2.Test.make ~name:"Codex and Pi use the same model and effort"
    ~count:500
    QCheck2.Gen.(option int)
    (fun complexity ->
      let model = Openai_auto_settings.auto_model ~complexity in
      let effort = Openai_auto_settings.auto_effort ~complexity in
      Codex_event_parser.auto_model ~complexity = model
      && Pi_event_parser.auto_model ~complexity = model
      && Codex_event_parser.auto_effort ~complexity = effort
      && Pi_event_parser.auto_effort ~complexity = effort
      && List.for_all
           (fun backend ->
             let decision =
               Backend_routing.decide ~repo_config:Repo_config.empty
                 ~default_backend:backend ~effective_model:(Some "auto")
                 ~complexity
             in
             decision.Backend_routing.effort = effort)
           [ "codex"; "pi" ])

let boundaries =
  QCheck2.Test.make ~name:"auto ladder boundary cases" QCheck2.Gen.unit
    (fun () ->
      List.for_all
        (fun complexity ->
          let model, effort = expected complexity in
          Openai_auto_settings.settings ~complexity = (model, effort))
        [
          None;
          Some min_int;
          Some (-1);
          Some 0;
          Some 1;
          Some 2;
          Some 3;
          Some 4;
          Some max_int;
        ])

let auto_tiers_enforce_budget =
  QCheck2.Test.make ~name:"every auto tier accrues cost and enforces its cap"
    QCheck2.Gen.unit (fun () ->
      List.for_all
        (fun complexity ->
          let model = Openai_auto_settings.auto_model ~complexity in
          let decision =
            Codex_cost.on_turn_completed ~model ~budget_cap_nano_usd:(Some 0L)
              ~state:Codex_cost.initial_cost_state
              (`Assoc [ ("usage", `Assoc [ ("input_tokens", `Int 1000) ]) ])
          in
          Option.is_some (Codex_cost.model_pricing model)
          && decision.Codex_cost.state.Codex_cost.cumulative_nano_usd > 0L
          && List.length decision.Codex_cost.events = 2)
        [ None; Some 1; Some 2; Some 3; Some 0; Some (-1) ])

let () =
  List.iter
    (fun test -> QCheck2.Test.check_exn test)
    [ ladder; backend_agreement; boundaries; auto_tiers_enforce_budget ]

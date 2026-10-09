(* @archlint.module core
   @archlint.domain reconcile-plan *)

open Base

type obligation = Preserve_work | Finish_work | Integrate | Publish
[@@deriving eq, compare, sexp_of]

type strategy =
  | Observe_evidence
  | Deterministic
  | Content_repair
  | Agent_required
[@@deriving eq, compare, sexp_of]

type readiness =
  | Satisfied
  | Unobserved
  | Required of {
      strategy : strategy;
      deterministic_failures : int;
      content_failures : int;
      recovery_failures : int;
    }
[@@deriving eq, compare, sexp_of]

let required ?(deterministic_failures = 0) ?(content_failures = 0)
    ?(recovery_failures = 0) strategy =
  Required
    { strategy; deterministic_failures; content_failures; recovery_failures }

type authority = Held | Observe_only | Manage
[@@deriving eq, compare, sexp_of]

type execution = Available | In_flight | Backoff | Agent_result of obligation
[@@deriving eq, compare, sexp_of]

type t = {
  authority : authority;
  execution : execution;
  preserve_work : readiness;
  finish_work : readiness;
  integrate : readiness;
  publish : readiness;
}
[@@deriving eq, compare, sexp_of]

type hold = User_hold | Recovery_budget_exhausted
[@@deriving eq, compare, sexp_of]

type agent_scope = Content_only | Full_recovery | Diagnosis
[@@deriving eq, compare, sexp_of]

type decision =
  | Settled
  | Wait
  | Observe
  | Execute of obligation
  | Run_agent of obligation * agent_scope
  | Verify_agent of obligation
  | Hold of hold
[@@deriving eq, compare, sexp_of]

let for_obligation ~authority obligation readiness =
  let t =
    {
      authority;
      execution = Available;
      preserve_work = Satisfied;
      finish_work = Satisfied;
      integrate = Satisfied;
      publish = Satisfied;
    }
  in
  match obligation with
  | Preserve_work -> { t with preserve_work = readiness }
  | Finish_work -> { t with finish_work = readiness }
  | Integrate -> { t with integrate = readiness }
  | Publish -> { t with publish = readiness }

let next t =
  match (t.authority, t.execution) with
  | Held, _ -> Hold User_hold
  | (Observe_only | Manage), (In_flight | Backoff) -> Wait
  | (Observe_only | Manage), Agent_result task -> Verify_agent task
  | (Observe_only | Manage), Available ->
      let rec choose = function
        | [] -> Settled
        | (_, Satisfied) :: rest -> choose rest
        | (_, Unobserved) :: _ -> Observe
        | ( task,
            Required
              {
                strategy;
                deterministic_failures;
                content_failures;
                recovery_failures;
              } )
          :: _ -> (
            if recovery_failures >= 2 then Hold Recovery_budget_exhausted
            else
              match (t.authority, strategy) with
              | ( Held,
                  ( Observe_evidence | Deterministic | Content_repair
                  | Agent_required ) ) ->
                  Hold User_hold
              | (Observe_only | Manage), Observe_evidence ->
                  if deterministic_failures < 2 then Observe
                  else Run_agent (task, Diagnosis)
              | Observe_only, (Deterministic | Content_repair | Agent_required)
                ->
                  Run_agent (task, Diagnosis)
              | Manage, Content_repair when content_failures < 2 ->
                  Run_agent (task, Content_only)
              | Manage, Deterministic when deterministic_failures < 2 ->
                  Execute task
              | Manage, (Deterministic | Content_repair | Agent_required) ->
                  Run_agent (task, Full_recovery))
      in
      choose
        [
          (Preserve_work, t.preserve_work);
          (Finish_work, t.finish_work);
          (Integrate, t.integrate);
          (Publish, t.publish);
        ]

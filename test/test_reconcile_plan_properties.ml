(* @archlint.module test
   @archlint.domain reconcile-plan *)

open Onton_core
module P = Reconcile_plan
open P
module G = QCheck2.Gen

let tasks = [ P.Preserve_work; P.Finish_work; P.Integrate; P.Publish ]

let readiness =
  G.oneof
    [
      G.oneof_list [ P.Satisfied; P.Unobserved ];
      G.map
        (fun (strategy, (deterministic_failures, recovery_failures)) ->
          P.required ~deterministic_failures ~recovery_failures strategy)
        G.(
          pair
            (oneof_list
               [
                 P.Observe_evidence;
                 P.Deterministic;
                 P.Content_repair;
                 P.Agent_required;
               ])
            (pair int int));
    ]

let state =
  G.(
    map
      (fun ( (authority, execution),
             ((preserve_work, finish_work), (integrate, publish)) ) ->
        P.
          {
            authority;
            execution;
            preserve_work;
            finish_work;
            integrate;
            publish;
          })
      (pair
         (pair
            (oneof_list [ P.Held; P.Observe_only; P.Manage ])
            (oneof_list
               ([ P.Available; P.In_flight; P.Backoff ]
               @ List.map (fun task -> P.Agent_result task) tasks)))
         (pair (pair readiness readiness) (pair readiness readiness))))

let ready =
  P.
    {
      authority = Manage;
      execution = Available;
      preserve_work = Satisfied;
      finish_work = Satisfied;
      integrate = Satisfied;
      publish = Satisfied;
    }

let tests =
  [
    QCheck2.Test.make ~count:500
      ~name:
        "evidence failures reach diagnosis under either observation or \
         mutation authority"
      G.(pair bool (int_range 0 1000))
      (fun (manage, failures) ->
        let authority = if manage then P.Manage else P.Observe_only in
        P.next
          (P.for_obligation ~authority P.Preserve_work
             (P.required ~deterministic_failures:failures P.Observe_evidence))
        =
        if failures < 2 then P.Observe
        else P.Run_agent (P.Preserve_work, P.Diagnosis));
    QCheck2.Test.make
      ~name:
        "conflict-only repair escalates to full recovery before a budget hold"
      ~count:500
      G.(pair (int_range 0 30) (int_range 0 30))
      (fun (content_failures, recovery_failures) ->
        P.next
          (P.for_obligation ~authority:Manage Integrate
             (required ~content_failures ~recovery_failures Content_repair))
        =
        if recovery_failures >= 2 then Hold Recovery_budget_exhausted
        else
          Run_agent
            ( Integrate,
              if content_failures >= 2 then Full_recovery else Content_only ));
    QCheck2.Test.make
      ~name:"completed recovery cannot exhaust a later obligation" ~count:500
      G.(pair (oneof_list tasks) (int_range 2 1000))
      (fun (task, failures) ->
        let blocked = required ~recovery_failures:failures Agent_required in
        let pending =
          match task with
          | Preserve_work -> { ready with preserve_work = blocked }
          | Finish_work -> { ready with finish_work = blocked }
          | Integrate -> { ready with integrate = blocked }
          | Publish -> { ready with publish = blocked }
        in
        let completed =
          match task with
          | Preserve_work ->
              {
                pending with
                preserve_work = Satisfied;
                finish_work = required Agent_required;
              }
          | Finish_work ->
              {
                pending with
                finish_work = Satisfied;
                integrate = required Deterministic;
              }
          | Integrate ->
              {
                pending with
                integrate = Satisfied;
                publish = required Deterministic;
              }
          | Publish -> { pending with publish = Satisfied }
        in
        P.next pending = Hold Recovery_budget_exhausted
        && P.next completed
           =
           match task with
           | Preserve_work -> Run_agent (Finish_work, Full_recovery)
           | Finish_work -> Execute Integrate
           | Integrate -> Execute Publish
           | Publish -> Settled);
    QCheck2.Test.make
      ~name:
        "managed pending work never holds before agent recovery is exhausted"
      ~count:2000
      G.(pair int (int_range (-100) 1))
      (fun (deterministic_failures, recovery_failures) ->
        let decision =
          P.next
            {
              ready with
              integrate =
                required ~deterministic_failures ~recovery_failures
                  Deterministic;
            }
        in
        decision
        =
        if deterministic_failures >= 2 then Run_agent (Integrate, Full_recovery)
        else Execute Integrate);
    QCheck2.Test.make
      ~name:
        "repeated deterministic failures escalate instead of replaying forever"
      ~count:500
      G.(int_range 0 1000)
      (fun failures ->
        P.next
          {
            ready with
            integrate = required ~deterministic_failures:failures Deterministic;
          }
        =
        if failures >= 2 then Run_agent (Integrate, Full_recovery)
        else Execute Integrate);
    QCheck2.Test.make
      ~name:
        "planning is total for arbitrary obligation and execution combinations"
      ~count:2000 state (fun s ->
        try
          ignore (P.next s);
          true
        with _ -> false);
    QCheck2.Test.make
      ~name:
        "unfinished local work reaches the agent before integration or \
         publication"
      ~count:500
      G.(pair readiness readiness)
      (fun (integrate, publish) ->
        P.next
          {
            ready with
            finish_work = required Agent_required;
            integrate;
            publish;
          }
        = P.Run_agent (P.Finish_work, Full_recovery));
    QCheck2.Test.make
      ~name:
        "unavailable deterministic recovery routes to an agent for every \
         obligation"
      ~count:100
      G.(oneof_list tasks)
      (fun task ->
        let s =
          match task with
          | P.Preserve_work ->
              { ready with preserve_work = required Agent_required }
          | P.Finish_work ->
              { ready with finish_work = required Agent_required }
          | P.Integrate -> { ready with integrate = required Agent_required }
          | P.Publish -> { ready with publish = required Agent_required }
        in
        P.next s = P.Run_agent (task, Full_recovery));
    QCheck2.Test.make
      ~name:
        "agent completion requires verification even when readiness claims \
         success"
      ~count:500
      G.(pair state (oneof_list tasks))
      (fun (s, task) ->
        P.next { s with authority = Manage; execution = Agent_result task }
        = P.Verify_agent task);
    QCheck2.Test.make
      ~name:
        "in-flight actions and infrastructure waits cannot dispatch a second \
         actor"
      ~count:500
      G.(pair state bool)
      (fun (s, backoff) ->
        P.next
          {
            s with
            authority = Manage;
            execution = (if backoff then Backoff else In_flight);
          }
        = P.Wait);
    QCheck2.Test.make
      ~name:
        "inspection-only authority can observe but cannot mutate through \
         fallback"
      ~count:1000 state (fun s ->
        match P.next { s with authority = Observe_only } with
        | P.Execute _ | P.Run_agent (_, (Content_only | Full_recovery)) -> false
        | P.Run_agent (_, Diagnosis) -> true
        | P.Settled | P.Wait | P.Observe | P.Verify_agent _ | P.Hold _ -> true);
    QCheck2.Test.make
      ~name:
        "captured publication does not invent an obligation to clean unrelated \
         local work" ~count:1 G.unit (fun () ->
        P.next { ready with publish = required Deterministic }
        = P.Execute P.Publish);
    QCheck2.Test.make
      ~name:
        "interrupted local work converges through verification before base \
         reconciliation"
      ~count:500
      G.(list_size (int_range 0 40) bool)
      (fun waits ->
        let dirty =
          {
            ready with
            finish_work = required Agent_required;
            integrate = required Deterministic;
            publish = required Deterministic;
          }
        in
        let in_flight = { dirty with execution = In_flight } in
        let no_duplicate =
          List.for_all
            (fun outage ->
              P.next
                {
                  in_flight with
                  execution = (if outage then Backoff else In_flight);
                }
              = P.Wait)
            waits
        in
        let returned = { dirty with execution = Agent_result Finish_work } in
        let committed = { dirty with finish_work = Satisfied } in
        let integrated = { committed with integrate = Satisfied } in
        no_duplicate
        && P.next dirty = P.Run_agent (Finish_work, Full_recovery)
        && P.next returned = P.Verify_agent Finish_work
        && P.next committed = P.Execute Integrate
        && P.next integrated = P.Execute Publish
        && P.next { integrated with publish = Satisfied } = P.Settled);
  ]

let () = QCheck_base_runner.run_tests_main tests

open Types

type snapshot = {
  orchestrator : Orchestrator.t;
  activity_log : Activity_log.t;
  gameplan : Gameplan.t;
}

type t = { mutex : Eio.Mutex.t; mutable snap : snapshot }

let create ~gameplan ~(main_branch : Branch.t) =
  let orchestrator =
    Orchestrator.create ~patches:gameplan.Gameplan.patches ~main_branch
  in
  let snap = { orchestrator; activity_log = Activity_log.empty; gameplan } in
  { mutex = Eio.Mutex.create (); snap }

let read t f = Eio.Mutex.use_ro t.mutex (fun () -> f t.snap)

let update t f =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.snap <- f t.snap)

let update_orchestrator t f =
  update t (fun s -> { s with orchestrator = f s.orchestrator })

let update_activity_log t f =
  update t (fun s -> { s with activity_log = f s.activity_log })

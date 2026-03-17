open Types

type snapshot = {
  orchestrator : Orchestrator.t;
  activity_log : Activity_log.t;
  gameplan : Gameplan.t;
  transcripts : (Patch_id.t, string) Base.Hashtbl.t;
}

type t = { mutex : Eio.Mutex.t; mutable snap : snapshot }

let create ~gameplan ~(main_branch : Branch.t) ?snapshot () =
  let snap =
    match snapshot with
    | Some s -> s
    | None ->
        let orchestrator =
          Orchestrator.create ~patches:gameplan.Gameplan.patches ~main_branch
        in
        {
          orchestrator;
          activity_log = Activity_log.empty;
          gameplan;
          transcripts = Base.Hashtbl.create (module Patch_id);
        }
  in
  { mutex = Eio.Mutex.create (); snap }

let read t f = Eio.Mutex.use_ro t.mutex (fun () -> f t.snap)

let update t f =
  (* ~protect:false: if [f] raises, [t.snap] is never assigned (the
     record expression [f t.snap] fails before the [<-] executes), so
     the snapshot remains consistent and the mutex stays healthy.
     ~protect:true would poison the mutex on any exception, cascading
     into all other fibers — a disproportionate response to a logic
     bug in one patch agent. *)
  Eio.Mutex.use_rw ~protect:false t.mutex (fun () -> t.snap <- f t.snap)

let update_orchestrator t f =
  update t (fun s -> { s with orchestrator = f s.orchestrator })

let update_activity_log t f =
  update t (fun s -> { s with activity_log = f s.activity_log })

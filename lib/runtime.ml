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

let read t f =
  Eio.Mutex.lock t.mutex;
  match f t.snap with
  | v ->
      Eio.Mutex.unlock t.mutex;
      v
  | exception ex ->
      Eio.Mutex.unlock t.mutex;
      raise ex

let update t f =
  (* Manual lock/unlock instead of [use_rw]: [use_rw] poisons the mutex on
     any exception (including [Cancelled]), which cascades into every other
     fiber and crashes the process. Since [f] is a pure snapshot→snapshot
     transform, the assignment either completes or doesn't — no inconsistent
     state is possible, so poisoning is never warranted. *)
  Eio.Mutex.lock t.mutex;
  match t.snap <- f t.snap with
  | () -> Eio.Mutex.unlock t.mutex
  | exception ex ->
      Eio.Mutex.unlock t.mutex;
      raise ex

let update_orchestrator t f =
  update t (fun s -> { s with orchestrator = f s.orchestrator })

let update_orchestrator_returning t f =
  let result = ref None in
  update t (fun s ->
      let orch', v = f s.orchestrator in
      result := Some v;
      { s with orchestrator = orch' });
  (* [update] guarantees the callback ran exactly once before returning *)
  match !result with
  | Some v -> v
  | None -> assert false

let update_activity_log t f =
  update t (fun s -> { s with activity_log = f s.activity_log })

let snapshot_unsync t = t.snap

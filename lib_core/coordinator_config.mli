(* @archlint.module interface
   @archlint.domain coordinator *)
type t = private {
  version : int;
  capacity : int;
  repo_root : string;
  main_branch : string;
  worker_executable : string;
  backend : string;
  execution_mode : string;
}

val decode : string -> (t, string) result
val yojson_of_t : t -> Yojson.Safe.t
val work_json : Worker_protocol_v3.work -> Yojson.Safe.t
val work : string -> (Worker_protocol_v3.work, string) result
val reconcile : Scheduler_decision.input -> Scheduler_decision.input

val work_of_decision :
  spec:string -> Scheduler_decision.decision -> Worker_protocol_v3.work

val body_delivered : Coordinator_journal.entry list -> Patch_agent.t -> bool

open Types

(** Pure application of poll results to orchestrator state.

    Returns the updated orchestrator and a list of log entries to emit. The CI
    cache update (mutable Hashtbl) remains in the caller. *)

type log_entry = { message : string; patch_id : Patch_id.t }
[@@deriving show, eq]

val apply :
  Orchestrator.t -> Patch_id.t -> Poller.t -> Orchestrator.t * log_entry list
(** [apply orch patch_id poll_result] applies the poll result to the
    orchestrator, returning the updated state and log entries. *)

type application = { poll_result : Poller.t; merge_queue_ejected : bool }
[@@deriving show, eq]

val should_treat_ejection_as_ci_failure :
  previous_entry:Pr_state.merge_queue_entry option -> Poller.t -> bool

val apply :
  previous_entry:Pr_state.merge_queue_entry option -> Poller.t -> application

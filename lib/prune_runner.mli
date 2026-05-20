val run_prune : unit -> int
(** Remove stored projects whose gameplan patches are all merged.

    Returns [0] on success and [1] when one or more projects could not be pruned
    due to lock or snapshot errors. *)

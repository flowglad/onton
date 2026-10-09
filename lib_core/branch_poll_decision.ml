(* @archlint.module core
   @archlint.domain orchestrator *)

open Base

type cached = {
  next_probe_at : float;
  checks_observed_at : float option;
  observed_head : string option;
  expected_head : string option;
}

type action = Skip | Probe of { reuse_checks : bool }

let head_interval = 60.
let checks_interval = 180.

let plan ~now ~expected_head ~checks = function
  | None -> Probe { reuse_checks = false }
  | Some cached ->
      let publication_changed =
        not (Option.equal String.equal expected_head cached.expected_head)
      in
      let terminal_checks =
        (not (List.is_empty checks))
        && List.for_all checks ~f:Types.Ci_check.is_terminal
      in
      if
        (not publication_changed) && terminal_checks
        && Float.(now < cached.next_probe_at)
      then Skip
      else
        let reuse_checks =
          (not publication_changed) && terminal_checks
          && Option.is_none expected_head
          && Option.is_some cached.observed_head
          && Option.value_map cached.checks_observed_at ~default:false
               ~f:(fun checked_at ->
                 Float.(now < checked_at +. checks_interval))
        in
        Probe { reuse_checks }

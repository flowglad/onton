(* @archlint.module test
   @archlint.domain orchestrator *)

open Base
open Onton_core
open Branch_poll_decision
module G = QCheck2.Gen

let timestamp = G.int_range (-1_000_000) 1_000_000
let property name gen f = QCheck2.Test.make ~name ~count:500 gen f

let check conclusion =
  Types.Ci_check.
    {
      name = "CI";
      conclusion;
      details_url = None;
      description = None;
      started_at = None;
      app_id = None;
      check_suite_id = None;
      id = None;
    }

let plan = Branch_poll_decision.plan ~checks:[ check "success" ]

let cached ~now ~checked_at =
  Branch_poll_decision.
    {
      next_probe_at = now +. head_interval;
      checks_observed_at = Some checked_at;
      observed_head = Some "abc";
      expected_head = None;
    }

let tests =
  [
    property "pending and empty checks bypass HEAD throttle on every cycle"
      (G.pair timestamp (G.int_range 1 59))
      (fun (n, offset) ->
        let now = Float.of_int n in
        List.for_all
          [ []; [ check "pending" ]; [ check "failure"; check "in_progress" ] ]
          ~f:(fun checks ->
            match
              Branch_poll_decision.plan
                ~now:(now +. Float.of_int offset)
                ~expected_head:None ~checks
                (Some (cached ~now ~checked_at:now))
            with
            | Probe { reuse_checks = false } -> true
            | Skip | Probe { reuse_checks = true } -> false));
    property "nonterminal checks never reuse results at a probe"
      (G.pair timestamp
         (G.oneof_list
            [ "pending"; "queued"; "in_progress"; "cancelled"; "unknown"; "" ]))
      (fun (n, conclusion) ->
        let now = Float.of_int n in
        let checks = [ check "failure"; check conclusion; check "success" ] in
        match
          Branch_poll_decision.plan ~now:(now +. head_interval)
            ~expected_head:None ~checks
            (Some (cached ~now ~checked_at:now))
        with
        | Probe { reuse_checks = false } -> true
        | Skip | Probe { reuse_checks = true } -> false);
    property "pending-to-failed at unchanged HEAD forces fresh probes" timestamp
      (fun n ->
        let now = Float.of_int n in
        let initial = cached ~now ~checked_at:now in
        let first =
          Branch_poll_decision.plan ~now:(now +. head_interval)
            ~expected_head:None
            ~checks:[ check "pending" ]
            (Some initial)
        in
        let after_pending =
          {
            initial with
            next_probe_at = now +. (2. *. head_interval);
            checks_observed_at = Some (now +. head_interval);
          }
        in
        let second =
          Branch_poll_decision.plan
            ~now:(now +. (2. *. head_interval))
            ~expected_head:None
            ~checks:[ check "pending" ]
            (Some after_pending)
        in
        let after_failure =
          {
            after_pending with
            next_probe_at = now +. (3. *. head_interval);
            checks_observed_at = Some (now +. (2. *. head_interval));
          }
        in
        let third =
          Branch_poll_decision.plan
            ~now:(now +. (3. *. head_interval))
            ~expected_head:None
            ~checks:[ check "failure" ]
            (Some after_failure)
        in
        let fetches = function
          | Probe { reuse_checks = false } -> true
          | Skip | Probe { reuse_checks = true } -> false
        in
        let reuses = function
          | Probe { reuse_checks = true } -> true
          | Skip | Probe { reuse_checks = false } -> false
        in
        fetches first && fetches second && reuses third);
    property "empty checks never reuse a recent observation" timestamp (fun n ->
        let now = Float.of_int n in
        match
          Branch_poll_decision.plan ~now:(now +. head_interval)
            ~expected_head:None ~checks:[]
            (Some (cached ~now ~checked_at:now))
        with
        | Probe { reuse_checks = false } -> true
        | Skip | Probe { reuse_checks = true } -> false);
    property "plan is total over arbitrary cached heads and times"
      (G.pair timestamp (G.pair (G.option G.string) (G.option G.string)))
      (fun (n, (observed_head, expected_head)) ->
        let now = Float.of_int n in
        let state =
          Branch_poll_decision.
            {
              next_probe_at = now +. head_interval;
              checks_observed_at = Some (now -. checks_interval);
              observed_head;
              expected_head;
            }
        in
        match plan ~now ~expected_head (Some state) with
        | Skip | Probe _ -> true);
    property "initial observation always fetches checks" timestamp (fun n ->
        match plan ~now:(Float.of_int n) ~expected_head:None None with
        | Probe { reuse_checks = false } -> true
        | Skip | Probe { reuse_checks = true } -> false);
    property "repeated polls before HEAD deadline are skipped"
      (G.pair timestamp (G.int_range 0 59))
      (fun (n, offset) ->
        let now = Float.of_int n in
        match
          plan
            ~now:(now +. Float.of_int offset)
            ~expected_head:None
            (Some (cached ~now ~checked_at:now))
        with
        | Skip -> true
        | Probe _ -> false);
    property "unchanged HEAD can reuse checks before check deadline" timestamp
      (fun n ->
        let now = Float.of_int n in
        match
          plan
            ~now:(now +. Branch_poll_decision.head_interval)
            ~expected_head:None
            (Some (cached ~now ~checked_at:now))
        with
        | Probe { reuse_checks = true } -> true
        | Skip | Probe { reuse_checks = false } -> false);
    property "check deadline forces complete refresh even at the same HEAD"
      timestamp (fun n ->
        let now = Float.of_int n in
        match
          plan
            ~now:(now +. Branch_poll_decision.checks_interval)
            ~expected_head:None
            (Some (cached ~now ~checked_at:now))
        with
        | Probe { reuse_checks = false } -> true
        | Skip | Probe { reuse_checks = true } -> false);
    property "new publication bypasses both deadlines" timestamp (fun n ->
        let now = Float.of_int n in
        match
          plan ~now ~expected_head:(Some "new-head")
            (Some (cached ~now ~checked_at:now))
        with
        | Probe { reuse_checks = false } -> true
        | Skip | Probe { reuse_checks = true } -> false);
    property "empty observation never reuses checks" timestamp (fun n ->
        let now = Float.of_int n in
        let state =
          Branch_poll_decision.
            {
              next_probe_at = now;
              checks_observed_at = None;
              observed_head = None;
              expected_head = None;
            }
        in
        match plan ~now ~expected_head:None (Some state) with
        | Probe { reuse_checks = false } -> true
        | Skip | Probe { reuse_checks = true } -> false);
  ]

let () = QCheck_base_runner.run_tests_main tests

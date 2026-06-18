(* @archlint.module test
   @archlint.domain activity-log *)

open Base
open Onton_core

(** Property tests for {!Activity_log}.

    The module is a pure ring-buffer-like log of transitions, events, and stream
    entries. The properties below are derived from the .mli contract:

    - [empty] has no entries.
    - [add_*] is monotone in length (never decreases the count).
    - [recent_*] respects [~limit] bounds (returns at most [limit], 0 for
      non-positive).
    - The most recent entry at [recent_*] is the last one added.
    - [trim ~max:n] caps each list at [n] entries; idempotent. *)

(* ─────────────────────────────────────────────────────────────────────────
   Generators
   ───────────────────────────────────────────────────────────────────────── *)

let gen_patch_id =
  let open QCheck2.Gen in
  map (fun i -> Types.Patch_id.of_string (Int.to_string i)) (int_range 1 100)

let gen_timestamp =
  let open QCheck2.Gen in
  map Float.of_int (int_range 1_700_000_000 1_800_000_000)

let gen_status =
  let open QCheck2.Gen in
  oneof_list
    [
      Display_status.Merged;
      Needs_help;
      Approved_idle;
      Approved_running;
      Fixing_ci;
      Addressing_review;
      Resolving_conflict;
      Responding_to_human;
      Writing_pr_body;
      Rebasing;
      Starting;
      Updating;
      Ci_queued;
      Review_queued;
      Awaiting_feedback;
      Blocked_by_dep;
      Pending;
    ]

let gen_transition =
  let open QCheck2.Gen in
  let* timestamp = gen_timestamp in
  let* patch_id = gen_patch_id in
  let* from_status = gen_status in
  let* to_status = gen_status in
  let* action = string_size (int_range 0 16) in
  pure
    (Activity_log.Transition_entry.create ~timestamp ~patch_id ~from_status
       ~to_status ~action)

let gen_event =
  let open QCheck2.Gen in
  let* timestamp = gen_timestamp in
  let* patch_id = option gen_patch_id in
  let* message = string_size (int_range 0 32) in
  pure (Activity_log.Event.create ~timestamp ?patch_id message)

let gen_stream_kind =
  let open QCheck2.Gen in
  oneof
    [
      map2
        (fun a b -> Activity_log.Stream_entry.Tool_use (a, b))
        (string_size (int_range 0 8))
        (string_size (int_range 0 16));
      map (fun s -> Activity_log.Stream_entry.Text_chunk s) string;
      map (fun s -> Activity_log.Stream_entry.Finished s) string;
      map (fun s -> Activity_log.Stream_entry.Stream_error s) string;
    ]

let gen_stream_entry =
  let open QCheck2.Gen in
  let* timestamp = gen_timestamp in
  let* patch_id = gen_patch_id in
  let* kind = gen_stream_kind in
  pure (Activity_log.Stream_entry.create ~timestamp ~patch_id ~kind)

(* Build a log by replaying a random sequence of operations. *)
type op =
  | Add_transition of Activity_log.Transition_entry.t
  | Add_event of Activity_log.Event.t
  | Add_stream of Activity_log.Stream_entry.t

let gen_op =
  let open QCheck2.Gen in
  oneof
    [
      map (fun t -> Add_transition t) gen_transition;
      map (fun e -> Add_event e) gen_event;
      map (fun s -> Add_stream s) gen_stream_entry;
    ]

let apply log = function
  | Add_transition t -> Activity_log.add_transition log t
  | Add_event e -> Activity_log.add_event log e
  | Add_stream s -> Activity_log.add_stream_entry log s

let build_log ops = List.fold ops ~init:Activity_log.empty ~f:apply

let gen_log =
  let open QCheck2.Gen in
  map build_log (list_size (int_range 0 32) gen_op)

(* ─────────────────────────────────────────────────────────────────────────
   Properties
   ───────────────────────────────────────────────────────────────────────── *)

let prop_empty_recent =
  QCheck2.Test.make ~name:"empty: recent_X ~limit:n is empty for any n"
    ~count:200
    QCheck2.Gen.(int_range (-10) 100)
    (fun n ->
      List.is_empty
        (Activity_log.recent_transitions Activity_log.empty ~limit:n)
      && List.is_empty (Activity_log.recent_events Activity_log.empty ~limit:n)
      && List.is_empty
           (Activity_log.recent_stream_entries Activity_log.empty ~limit:n))

let prop_recent_limit_bound =
  QCheck2.Test.make
    ~name:"recent_X returns at most ~limit entries for any non-negative limit"
    ~count:300
    QCheck2.Gen.(pair gen_log (int_range 0 50))
    (fun (log, limit) ->
      List.length (Activity_log.recent_transitions log ~limit) <= limit
      && List.length (Activity_log.recent_events log ~limit) <= limit
      && List.length (Activity_log.recent_stream_entries log ~limit) <= limit)

let prop_recent_negative_limit_empty =
  QCheck2.Test.make ~name:"recent_X with limit <= 0 returns []" ~count:200
    QCheck2.Gen.(pair gen_log (int_range (-20) 0))
    (fun (log, limit) ->
      List.is_empty (Activity_log.recent_transitions log ~limit)
      && List.is_empty (Activity_log.recent_events log ~limit)
      && List.is_empty (Activity_log.recent_stream_entries log ~limit))

let prop_add_transition_head =
  QCheck2.Test.make ~name:"add_transition: most recent is the entry just added"
    ~count:300
    QCheck2.Gen.(pair gen_log gen_transition)
    (fun (log, t) ->
      let log' = Activity_log.add_transition log t in
      match Activity_log.recent_transitions log' ~limit:1 with
      | [ head ] -> Activity_log.Transition_entry.equal head t
      | _ -> false)

let prop_add_event_head =
  QCheck2.Test.make ~name:"add_event: most recent is the entry just added"
    ~count:300
    QCheck2.Gen.(pair gen_log gen_event)
    (fun (log, e) ->
      let log' = Activity_log.add_event log e in
      match Activity_log.recent_events log' ~limit:1 with
      | [ head ] -> Activity_log.Event.equal head e
      | _ -> false)

let prop_add_stream_head =
  QCheck2.Test.make
    ~name:"add_stream_entry: most recent is the entry just added" ~count:300
    QCheck2.Gen.(pair gen_log gen_stream_entry)
    (fun (log, s) ->
      let log' = Activity_log.add_stream_entry log s in
      match Activity_log.recent_stream_entries log' ~limit:1 with
      | [ head ] -> Activity_log.Stream_entry.equal head s
      | _ -> false)

let prop_add_monotone =
  QCheck2.Test.make ~name:"add_X never decreases the count at large limit"
    ~count:200
    QCheck2.Gen.(pair gen_log gen_op)
    (fun (log, op) ->
      let big = 10_000 in
      let before_t =
        List.length (Activity_log.recent_transitions log ~limit:big)
      in
      let before_e = List.length (Activity_log.recent_events log ~limit:big) in
      let before_s =
        List.length (Activity_log.recent_stream_entries log ~limit:big)
      in
      let log' = apply log op in
      let after_t =
        List.length (Activity_log.recent_transitions log' ~limit:big)
      in
      let after_e = List.length (Activity_log.recent_events log' ~limit:big) in
      let after_s =
        List.length (Activity_log.recent_stream_entries log' ~limit:big)
      in
      after_t >= before_t && after_e >= before_e && after_s >= before_s)

let prop_trim_caps_lengths =
  QCheck2.Test.make ~name:"trim ~max:n caps each list at n" ~count:200
    QCheck2.Gen.(pair gen_log (int_range 0 50))
    (fun (log, max) ->
      let log' = Activity_log.trim log ~max in
      let big = 10_000 in
      List.length (Activity_log.recent_transitions log' ~limit:big) <= max
      && List.length (Activity_log.recent_events log' ~limit:big) <= max
      && List.length (Activity_log.recent_stream_entries log' ~limit:big) <= max)

let prop_trim_idempotent =
  QCheck2.Test.make ~name:"trim ~max:n is idempotent" ~count:200
    QCheck2.Gen.(pair gen_log (int_range 0 50))
    (fun (log, max) ->
      let once = Activity_log.trim log ~max in
      let twice = Activity_log.trim once ~max in
      Activity_log.equal once twice)

let prop_trim_zero_empty =
  QCheck2.Test.make ~name:"trim ~max:0 yields empty (vs the original log)"
    ~count:100 gen_log (fun log ->
      let trimmed = Activity_log.trim log ~max:0 in
      let big = 10_000 in
      List.is_empty (Activity_log.recent_transitions trimmed ~limit:big)
      && List.is_empty (Activity_log.recent_events trimmed ~limit:big)
      && List.is_empty (Activity_log.recent_stream_entries trimmed ~limit:big))

let prop_trim_large_max_unchanged =
  QCheck2.Test.make
    ~name:"trim with max larger than any list is a no-op (recent_X stable)"
    ~count:100 gen_log (fun log ->
      let trimmed = Activity_log.trim log ~max:10_000 in
      Activity_log.equal log trimmed)

(* ─────────────────────────────────────────────────────────────────────────
   merged_recent: the user-facing feed = the [limit] newest of the union of
   transitions and events, interleaved by timestamp. Spec derived from the .mli.
   ───────────────────────────────────────────────────────────────────────── *)

let timestamp_descending a b =
  Float.descending
    (Activity_log.Merged_entry.timestamp a)
    (Activity_log.Merged_entry.timestamp b)

let prop_merged_length_bound =
  QCheck2.Test.make ~name:"merged_recent returns at most ~limit entries"
    ~count:300
    QCheck2.Gen.(pair gen_log (int_range (-5) 50))
    (fun (log, limit) ->
      List.length (Activity_log.merged_recent log ~limit) <= Int.max 0 limit)

let prop_merged_descending =
  QCheck2.Test.make ~name:"merged_recent is sorted newest-first by timestamp"
    ~count:300
    QCheck2.Gen.(pair gen_log (int_range 0 50))
    (fun (log, limit) ->
      let entries = Activity_log.merged_recent log ~limit in
      List.is_sorted entries ~compare:timestamp_descending)

(* A log whose insertion order is strictly increasing, distinct timestamps —
   mirroring production, where entries are appended as time advances. Under this
   ordering [recent_*] returns exactly the newest-by-timestamp entries, so
   [merged_recent] must equal the globally newest [limit] rows of the union. The
   random-timestamp [gen_log] above deliberately does not hold that coincidence,
   so it can't express this property. *)
type payload =
  | P_event of Types.Patch_id.t option * string
  | P_transition of
      Types.Patch_id.t * Display_status.t * Display_status.t * string

let gen_payload =
  let open QCheck2.Gen in
  oneof
    [
      map2
        (fun pid msg -> P_event (pid, msg))
        (option gen_patch_id)
        (string_size (int_range 0 32));
      (let* pid = gen_patch_id in
       let* from_status = gen_status in
       let* to_status = gen_status in
       let* action = string_size (int_range 0 16) in
       pure (P_transition (pid, from_status, to_status, action)));
    ]

let build_ordered_log payloads =
  List.foldi payloads ~init:Activity_log.empty ~f:(fun i log p ->
      let timestamp = Float.of_int (i + 1) in
      match p with
      | P_event (patch_id, message) ->
          Activity_log.add_event log
            (Activity_log.Event.create ~timestamp ?patch_id message)
      | P_transition (patch_id, from_status, to_status, action) ->
          Activity_log.add_transition log
            (Activity_log.Transition_entry.create ~timestamp ~patch_id
               ~from_status ~to_status ~action))

let gen_ordered_log =
  QCheck2.Gen.(map build_ordered_log (list_size (int_range 0 32) gen_payload))

let prop_merged_matches_union_take =
  (* The defining property: merging before truncating must yield exactly the
     [limit] newest rows of the *whole* union — never a per-source slice. This
     is what keeps the feed's density uniform rather than dense-at-top. The old
     take-per-source-then-merge implementation returned up to 2*limit rows and
     fails this on length. *)
  QCheck2.Test.make
    ~name:"merged_recent = newest limit of the union (time-ordered log)"
    ~count:300
    QCheck2.Gen.(pair gen_ordered_log (int_range 0 50))
    (fun (log, limit) ->
      let big = 10_000 in
      let union =
        List.map (Activity_log.recent_events log ~limit:big) ~f:(fun e ->
            Activity_log.Merged_entry.Event e)
        @ List.map (Activity_log.recent_transitions log ~limit:big) ~f:(fun t ->
            Activity_log.Merged_entry.Transition t)
      in
      let expected =
        List.take (List.stable_sort union ~compare:timestamp_descending) limit
      in
      List.equal Activity_log.Merged_entry.equal
        (Activity_log.merged_recent log ~limit)
        expected)

let prop_merged_negative_limit_empty =
  QCheck2.Test.make ~name:"merged_recent with limit <= 0 returns []" ~count:200
    QCheck2.Gen.(pair gen_log (int_range (-20) 0))
    (fun (log, limit) -> List.is_empty (Activity_log.merged_recent log ~limit))

let prop_stream_kind_of_raw_total =
  QCheck2.Test.make ~name:"stream_kind_of_raw is total" ~count:200
    QCheck2.Gen.string_small (fun raw ->
      ignore (Activity_log.stream_kind_of_raw ~channel:`Stdout raw);
      true)

let () =
  let suite =
    [
      prop_empty_recent;
      prop_recent_limit_bound;
      prop_recent_negative_limit_empty;
      prop_add_transition_head;
      prop_add_event_head;
      prop_add_stream_head;
      prop_add_monotone;
      prop_trim_caps_lengths;
      prop_trim_idempotent;
      prop_trim_zero_empty;
      prop_trim_large_max_unchanged;
      prop_merged_length_bound;
      prop_merged_descending;
      prop_merged_matches_union_take;
      prop_merged_negative_limit_empty;
      prop_stream_kind_of_raw_total;
    ]
  in
  let exit_code = QCheck_base_runner.run_tests ~verbose:true suite in
  if exit_code <> 0 then Stdlib.exit exit_code

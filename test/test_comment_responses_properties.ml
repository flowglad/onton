(* @archlint.module test
   @archlint.domain comment-responses *)

open Base
open Onton_core

(* Filenames: mix of valid ("<int>.md", "<int>", "<int>.txt") and junk. *)
let gen_filename =
  let open QCheck2.Gen in
  oneof_weighted
    [
      ( 3,
        let* id = int_range (-100) 100_000 in
        let* ext = oneof_list [ ".md"; ".txt"; "" ] in
        return (Int.to_string id ^ ext) );
      (1, oneof_list [ "notes.md"; "comment-12.md"; ".md"; ""; "12a.md"; "a12" ]);
    ]

let gen_contents =
  let open QCheck2.Gen in
  oneof_weighted
    [
      (3, string_size ~gen:printable (int_range 1 200));
      (1, oneof_list [ ""; "   "; " \n\t "; "\n\n" ]);
    ]

let gen_comment_id = QCheck2.Gen.int_range (-5) 40

let gen_comment =
  let open QCheck2.Gen in
  let* id = gen_comment_id in
  let* thread_id = option (map (fun n -> Printf.sprintf "T%d" n) nat_small) in
  let* outdated = bool in
  return
    Types.Comment.
      {
        id = Types.Comment_id.of_int id;
        thread_id;
        body = "body";
        path = None;
        line = None;
        commit_sha = None;
        original_commit_sha = None;
        outdated;
        last_reply_author = None;
      }

let gen_entry =
  let open QCheck2.Gen in
  let* comment_id = gen_comment_id in
  let* response = string_size ~gen:printable (int_range 1 50) in
  let response = "r" ^ response in
  return Comment_responses.{ comment_id; response }

let gen_delivered =
  QCheck2.Gen.list_size (QCheck2.Gen.int_range 0 20) gen_comment

let gen_entries = QCheck2.Gen.list_size (QCheck2.Gen.int_range 0 20) gen_entry

(* CR-TOT-1: entry_of_file is total over arbitrary filename/content pairs. *)
let prop_entry_of_file_total =
  QCheck2.Test.make ~count:500 ~name:"CR-TOT-1: entry_of_file never raises"
    QCheck2.Gen.(pair gen_filename gen_contents)
    (fun (filename, contents) ->
      try
        ignore
          (Comment_responses.entry_of_file ~filename ~contents
            : Comment_responses.entry option);
        true
      with _ -> false)

(* CR-DEC-1: a decoded entry's id round-trips the filename stem and its
   response is the stripped contents (nonblank). *)
let prop_entry_of_file_sound =
  QCheck2.Test.make ~count:500
    ~name:"CR-DEC-1: decoded entry matches stem and stripped contents"
    QCheck2.Gen.(pair gen_filename gen_contents)
    (fun (filename, contents) ->
      match Comment_responses.entry_of_file ~filename ~contents with
      | None -> true
      | Some { comment_id; response } ->
          let stem =
            Stdlib.Filename.remove_extension (Stdlib.Filename.basename filename)
          in
          String.equal (Int.to_string comment_id) stem
          && String.equal response (String.strip contents)
          && not (String.is_empty response))

(* CR-TOT-2: plan is total. *)
let prop_plan_total =
  QCheck2.Test.make ~count:500 ~name:"CR-TOT-2: plan never raises"
    QCheck2.Gen.(pair gen_delivered gen_entries)
    (fun (delivered, entries) ->
      try
        ignore
          (Comment_responses.plan ~delivered ~entries
            : Comment_responses.outcome);
        true
      with _ -> false)

(* CR-PLAN-1: partition — every distinct delivered id is either actioned or
   unanswered, never both; nothing else appears. *)
let prop_plan_partitions_delivered =
  QCheck2.Test.make ~count:500
    ~name:"CR-PLAN-1: actions + unanswered partition distinct delivered ids"
    QCheck2.Gen.(pair gen_delivered gen_entries)
    (fun (delivered, entries) ->
      let o = Comment_responses.plan ~delivered ~entries in
      let action_ids =
        List.map o.Comment_responses.actions ~f:(fun a ->
            Types.Comment_id.to_int a.Comment_responses.comment_id)
      in
      let unanswered_ids =
        List.map o.Comment_responses.unanswered ~f:Types.Comment_id.to_int
      in
      let combined = action_ids @ unanswered_ids in
      let distinct_delivered =
        List.map delivered ~f:(fun c ->
            Types.Comment_id.to_int c.Types.Comment.id)
        |> List.dedup_and_sort ~compare:Int.compare
      in
      List.equal Int.equal
        (List.sort combined ~compare:Int.compare)
        distinct_delivered
      && not
           (List.exists action_ids ~f:(fun id ->
                List.mem unanswered_ids id ~equal:Int.equal)))

(* CR-PLAN-2: an id is actioned iff some entry carries it. *)
let prop_plan_actions_iff_entry =
  QCheck2.Test.make ~count:500
    ~name:"CR-PLAN-2: delivered id actioned iff an entry exists for it"
    QCheck2.Gen.(pair gen_delivered gen_entries)
    (fun (delivered, entries) ->
      let o = Comment_responses.plan ~delivered ~entries in
      let entry_ids =
        List.map entries ~f:(fun e -> e.Comment_responses.comment_id)
      in
      List.for_all delivered ~f:(fun c ->
          let id = Types.Comment_id.to_int c.Types.Comment.id in
          let actioned =
            List.exists o.Comment_responses.actions ~f:(fun a ->
                Types.Comment_id.to_int a.Comment_responses.comment_id = id)
          in
          Bool.equal actioned (List.mem entry_ids id ~equal:Int.equal)))

(* CR-PLAN-3: last entry wins — each action's response equals the response of
   the last entry with that comment_id. *)
let prop_plan_last_entry_wins =
  QCheck2.Test.make ~count:500 ~name:"CR-PLAN-3: last entry wins"
    QCheck2.Gen.(pair gen_delivered gen_entries)
    (fun (delivered, entries) ->
      let o = Comment_responses.plan ~delivered ~entries in
      List.for_all o.Comment_responses.actions ~f:(fun a ->
          let id = Types.Comment_id.to_int a.Comment_responses.comment_id in
          match
            List.filter entries ~f:(fun e ->
                e.Comment_responses.comment_id = id)
          with
          | [] -> false
          | matching ->
              String.equal a.Comment_responses.response
                (List.last_exn matching).Comment_responses.response))

(* CR-PLAN-4: actions preserve first-occurrence delivered order and carry the
   first occurrence's thread_id. *)
let prop_plan_order_and_thread =
  QCheck2.Test.make ~count:500
    ~name:
      "CR-PLAN-4: delivered order preserved; thread_id from first occurrence"
    QCheck2.Gen.(pair gen_delivered gen_entries)
    (fun (delivered, entries) ->
      let o = Comment_responses.plan ~delivered ~entries in
      let first_occurrence_order =
        List.fold delivered ~init:[] ~f:(fun acc c ->
            let id = Types.Comment_id.to_int c.Types.Comment.id in
            if List.mem acc id ~equal:Int.equal then acc else acc @ [ id ])
      in
      let action_ids =
        List.map o.Comment_responses.actions ~f:(fun a ->
            Types.Comment_id.to_int a.Comment_responses.comment_id)
      in
      let expected_order =
        List.filter first_occurrence_order ~f:(fun id ->
            List.mem action_ids id ~equal:Int.equal)
      in
      List.equal Int.equal action_ids expected_order
      && List.for_all o.Comment_responses.actions ~f:(fun a ->
          let id = Types.Comment_id.to_int a.Comment_responses.comment_id in
          match
            List.find delivered ~f:(fun c ->
                Types.Comment_id.to_int c.Types.Comment.id = id)
          with
          | None -> false
          | Some c ->
              Option.equal String.equal a.Comment_responses.thread_id
                c.Types.Comment.thread_id))

(* CR-PLAN-5: unmatched_entries are exactly the entries whose id is not
   delivered, and they never intersect the actioned ids. *)
let prop_unmatched_entries =
  QCheck2.Test.make ~count:500
    ~name:"CR-PLAN-5: unmatched_entries = entries with undelivered ids"
    QCheck2.Gen.(pair gen_delivered gen_entries)
    (fun (delivered, entries) ->
      let unmatched = Comment_responses.unmatched_entries ~delivered ~entries in
      let delivered_ids =
        List.map delivered ~f:(fun c ->
            Types.Comment_id.to_int c.Types.Comment.id)
      in
      List.for_all entries ~f:(fun e ->
          let is_unmatched =
            List.mem unmatched e ~equal:Comment_responses.equal_entry
          in
          Bool.equal is_unmatched
            (not
               (List.mem delivered_ids e.Comment_responses.comment_id
                  ~equal:Int.equal))))

(* CR-PLAN-6: no entries -> no actions, everything unanswered (boundary). *)
let prop_no_entries_all_unanswered =
  QCheck2.Test.make ~count:200
    ~name:"CR-PLAN-6: empty entries -> all distinct delivered unanswered"
    gen_delivered (fun delivered ->
      let o = Comment_responses.plan ~delivered ~entries:[] in
      List.is_empty o.Comment_responses.actions
      && List.length o.Comment_responses.unanswered
         = List.length
             (List.dedup_and_sort ~compare:Int.compare
                (List.map delivered ~f:(fun c ->
                     Types.Comment_id.to_int c.Types.Comment.id))))

let () =
  let suite =
    [
      prop_entry_of_file_total;
      prop_entry_of_file_sound;
      prop_plan_total;
      prop_plan_partitions_delivered;
      prop_plan_actions_iff_entry;
      prop_plan_last_entry_wins;
      prop_plan_order_and_thread;
      prop_unmatched_entries;
      prop_no_entries_all_unanswered;
    ]
  in
  let exit_code = QCheck_base_runner.run_tests ~verbose:true suite in
  if exit_code <> 0 then Stdlib.exit exit_code

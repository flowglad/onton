(* @archlint.module shell
   @archlint.domain comment-responses *)

let read_file path =
  try
    let ic = Stdlib.In_channel.open_text path in
    Ok
      (Fun.protect
         ~finally:(fun () -> Stdlib.In_channel.close ic)
         (fun () -> Stdlib.In_channel.input_all ic))
  with Sys_error msg -> Error msg

(* Sorted for determinism: [Comment_responses.plan] is last-entry-wins when
   duplicate stems map to one comment id ("12.md" and "12.txt"), so a stable
   listing order makes the winning file deterministic. *)
let list_response_files artifact_dir =
  match Stdlib.Sys.readdir artifact_dir with
  | exception Sys_error _ -> []
  | names ->
      let names = Array.to_list names in
      List.sort String.compare names

let respond_after_session ~reply ~resolve ~log ~viewer_login ~artifact_dir
    ~delivered () =
  (* Count of delivered comments fully handled this cycle: reply posted AND
     thread resolved. Everything short of that leaves the thread unresolved,
     so the poller re-delivers it — the caller turns a non-converged cycle
     into [Orchestrator.Respond_review_unresolved] so the retry loop is
     bounded. *)
  let converged = ref 0 in
  let entries =
    List.filter_map
      (fun name ->
        let path = Stdlib.Filename.concat artifact_dir name in
        match read_file path with
        | Error msg ->
            log
              (Printf.sprintf
                 "comment-responses: could not read %s (%s) — skipping" path msg);
            None
        | Ok contents -> (
            match
              Onton_core.Comment_responses.entry_of_file ~filename:name
                ~contents
            with
            | Some _ as e -> e
            | None ->
                log
                  (Printf.sprintf
                     "comment-responses: ignoring %s — filename is not a \
                      comment id or the file is blank"
                     path);
                None))
      (list_response_files artifact_dir)
  in
  (match Onton_core.Comment_responses.unmatched_entries ~delivered ~entries with
  | [] -> ()
  | stray ->
      log
        (Printf.sprintf
           "comment-responses: response file(s) for unknown comment id(s) %s — \
            ignoring"
           (String.concat ", "
              (List.map
                 (fun (e : Onton_core.Comment_responses.entry) ->
                   Int.to_string e.comment_id)
                 stray))));
  let outcome = Onton_core.Comment_responses.plan ~delivered ~entries in
  (* Unanswered comments split by thread state. A resolve-retry thread (its
     last word is onton's own posted reply — the previous cycle's reply
     landed but the resolve didn't) needs no fresh response file: retry the
     resolve directly instead of posting a duplicate reply. Everything else
     genuinely lacks a response and re-delivers on the next poll. *)
  let retry_unanswered, plain_unanswered =
    List.partition
      (fun id ->
        match
          List.find_opt
            (fun (c : Types.Comment.t) ->
              Types.Comment_id.equal c.Types.Comment.id id)
            delivered
        with
        | Some c ->
            Onton_core.Comment_responses.is_resolve_retry ~viewer_login c
        | None -> false)
      outcome.Onton_core.Comment_responses.unanswered
  in
  (match plain_unanswered with
  | [] -> ()
  | ids ->
      log
        (Printf.sprintf
           "comment-responses: no response written for comment(s) %s — leaving \
            unresolved; they will re-deliver on the next poll"
           (String.concat ", "
              (List.map
                 (fun id -> Int.to_string (Types.Comment_id.to_int id))
                 ids))));
  (* Plain unanswered comments deliberately count as non-converged from the
     first Review session. A missing response file means the delivered thread
     remains unresolved and should re-deliver; repeated cycles with any such
     gaps are capped by the caller's review-unresolved counter. *)
  List.iter
    (fun id ->
      let raw_id = Types.Comment_id.to_int id in
      let thread_id =
        List.find_opt
          (fun (c : Types.Comment.t) ->
            Types.Comment_id.equal c.Types.Comment.id id)
          delivered
        |> Option.map (fun (c : Types.Comment.t) -> c.Types.Comment.thread_id)
        |> Option.join
      in
      match thread_id with
      | None ->
          log
            (Printf.sprintf
               "comment-responses: comment %d has a posted reply but carries \
                no thread id — cannot resolve"
               raw_id)
      | Some thread_id -> (
          match resolve ~thread_id with
          | Ok () ->
              incr converged;
              log
                (Printf.sprintf
                   "comment-responses: comment %d already carried onton's \
                    reply — resolve retried without a duplicate reply"
                   raw_id)
          | Error msg ->
              log
                (Printf.sprintf
                   "comment-responses: resolve retry for comment %d (thread \
                    %s) failed — %s"
                   raw_id thread_id msg)))
    retry_unanswered;
  List.iter
    (fun (a : Onton_core.Comment_responses.action) ->
      let raw_id = Types.Comment_id.to_int a.comment_id in
      (* Synthetic ids (negative, issued when GitHub returned no databaseId)
         are not forge-addressable — the REST reply endpoint would 404. *)
      if raw_id <= 0 then
        log
          (Printf.sprintf
             "comment-responses: comment %d has a synthetic id — cannot reply; \
              leaving unresolved"
             raw_id)
      else
        match reply ~comment_id:a.comment_id ~body:a.response with
        | Error msg ->
            log
              (Printf.sprintf
                 "comment-responses: reply to comment %d failed — %s; leaving \
                  thread unresolved"
                 raw_id msg)
        | Ok () -> (
            match a.thread_id with
            | None ->
                log
                  (Printf.sprintf
                     "comment-responses: replied to comment %d but it carries \
                      no thread id — cannot resolve"
                     raw_id)
            | Some thread_id -> (
                match resolve ~thread_id with
                | Ok () ->
                    incr converged;
                    log
                      (Printf.sprintf
                         "comment-responses: replied to and resolved comment %d"
                         raw_id)
                | Error msg ->
                    log
                      (Printf.sprintf
                         "comment-responses: replied to comment %d but \
                          resolving thread %s failed — %s"
                         raw_id thread_id msg))))
    outcome.Onton_core.Comment_responses.actions;
  let distinct_delivered =
    List.length outcome.Onton_core.Comment_responses.actions
    + List.length outcome.Onton_core.Comment_responses.unanswered
  in
  let unresolved = distinct_delivered - !converged in
  if unresolved = 0 then `Converged else `Unresolved unresolved

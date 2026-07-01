(* @archlint.module shell
   @archlint.domain orchestrator *)

open Base
open Types

let log_event runtime ?patch_id msg =
  Runtime_logging.log_event runtime ?patch_id msg

let activity_entries_of_log ?(limit = 10) (log : Activity_log.t) =
  Activity_log.merged_recent log ~limit
  |> List.map ~f:(function
    | Activity_log.Merged_entry.Event (e : Activity_log.Event.t) ->
        Tui.Event
          {
            patch_id =
              Option.map e.Activity_log.Event.patch_id ~f:Patch_id.to_string;
            message = e.Activity_log.Event.message;
            timestamp = e.Activity_log.Event.timestamp;
          }
    | Activity_log.Merged_entry.Transition (t : Activity_log.Transition_entry.t)
      ->
        Tui.Transition
          {
            patch_id =
              Patch_id.to_string t.Activity_log.Transition_entry.patch_id;
            from_label = Tui.label t.Activity_log.Transition_entry.from_status;
            to_status = t.Activity_log.Transition_entry.to_status;
            to_label = Tui.label t.Activity_log.Transition_entry.to_status;
            action = t.Activity_log.Transition_entry.action;
            timestamp = t.Activity_log.Transition_entry.timestamp;
          })

let pluralize ?plural n singular =
  let many = match plural with Some p -> p | None -> singular ^ "s" in
  Printf.sprintf "%d %s" n (if n = 1 then singular else many)

let intervention_reasons_of_log (log : Activity_log.t)
    ~(orchestrator : Orchestrator.t) =
  let agents = Orchestrator.all_agents orchestrator in
  let needs =
    List.filter_map agents ~f:(fun (a : Patch_agent.t) ->
        if Patch_agent.needs_intervention a || a.Patch_agent.branch_blocked then
          Some a.Patch_agent.patch_id
        else None)
    |> Hash_set.of_list (module Patch_id)
  in
  if Hash_set.is_empty needs then Map.Poly.empty
  else
    let events = Activity_log.recent_events log ~limit:1000 in
    List.fold events ~init:Map.Poly.empty ~f:(fun acc e ->
        match e.Activity_log.Event.patch_id with
        | Some pid when Hash_set.mem needs pid ->
            if Map.Poly.mem acc pid then acc
            else Map.Poly.set acc ~key:pid ~data:e.Activity_log.Event.message
        | _ -> acc)

let normalize_paste text =
  let text =
    String.rstrip text ~drop:(fun c -> Char.equal c '\n' || Char.equal c '\r')
  in
  let text = String.tr text ~target:'\n' ~replacement:' ' in
  String.tr text ~target:'\r' ~replacement:' '

module Tui_env = struct
  module type S = sig
    include Run_env.S

    val process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t
    val stdout : Eio_unix.sink_ty Eio.Resource.t
    val owner : string
    val repo : string
    val transcripts : (Patch_id.t, string) Stdlib.Hashtbl.t
    val tui_state : Tui_state.t
    val backend_name : string
    val version : string
    val resolve_routing : complexity:int option -> Backend_routing.decision
    val find_pr_number : patch_id:Patch_id.t -> Pr_number.t option

    val register_pr_number :
      patch_id:Patch_id.t -> pr_number:Pr_number.t -> unit

    val unregister_pr_number : patch_id:Patch_id.t -> unit
  end
end

module Make
    (Forge : Forge.S with type error = Github.error)
    (_ : Worktree.S)
    (Env : Tui_env.S) =
struct
  exception Quit

  let run () =
    let {
      Tui_state.list_selected;
      detail_scroll;
      detail_follow;
      timeline_scroll;
      view_mode;
      sorted_patch_ids;
      input_mode;
      prompt_line;
      show_help;
      show_checks;
      checks_scroll;
      status_msg;
      patches_start_row;
      patches_scroll_offset;
      patches_visible_count;
      dep_select_cursor;
      dep_select_chosen;
      _;
    } =
      Env.tui_state
    in
    Eio.Flow.copy_string (Tui.enter_tui ()) Env.stdout;
    let first = ref true in
    let prev_output = ref "" in
    let rec loop () =
      if !first then first := false
      else if Atomic.exchange Term.Raw.redraw_needed false then ()
      else Eio.Time.sleep Env.clock 0.1;
      let orch, gp, log =
        Runtime.read Env.runtime (fun snap ->
            ( snap.Runtime.orchestrator,
              snap.Runtime.gameplan,
              snap.Runtime.activity_log ))
      in
      let size = Term.get_size () in
      let width = match size with Some s -> s.Term.cols | None -> 80 in
      let height = match size with Some s -> s.Term.rows | None -> 24 in
      let limit =
        match !view_mode with
        | Tui.Timeline_view | Tui.Detail_view _ -> 100
        | Tui.List_view -> 10
      in
      let activity = activity_entries_of_log ~limit log in
      let intervention_reasons =
        intervention_reasons_of_log log ~orchestrator:orch
      in
      let views =
        Tui.views_of_orchestrator ~orchestrator:orch ~gameplan:gp ~activity
          ~resolve_routing:Env.resolve_routing ~intervention_reasons ()
      in
      sorted_patch_ids :=
        List.map views ~f:(fun (pv : Tui.patch_view) -> pv.Tui.patch_id);
      let transcript =
        match !view_mode with
        | Tui.Detail_view pid ->
            Option.value
              (Stdlib.Hashtbl.find_opt Env.transcripts pid)
              ~default:""
        | Tui.List_view | Tui.Timeline_view -> ""
      in
      let now = Unix.gettimeofday () in
      (match !status_msg with
      | Some msg when Tui.msg_expired ~now msg -> status_msg := None
      | Some _ | None -> ());
      let scroll_offset =
        match !view_mode with
        | Tui.Detail_view _ ->
            if !detail_follow then Int.max_value else !detail_scroll
        | Tui.Timeline_view -> !timeline_scroll
        | Tui.List_view -> 0
      in
      let frame =
        Tui.render_frame ~width ~height ~selected:!list_selected ~scroll_offset
          ~view_mode:!view_mode ~activity ~project_name:Env.project_name
          ~backend_name:Env.backend_name ~version:Env.version
          ~show_help:!show_help ~show_checks:!show_checks
          ~checks_scroll:!checks_scroll
          ~show_manage:
            (Tui_input.equal_input_mode !input_mode Tui_input.Manage_patch)
          ~now ~transcript ?status_msg:!status_msg ?prompt_line:!prompt_line
          ?dep_select:
            (if
               Tui_input.equal_input_mode !input_mode
                 Tui_input.Select_patch_deps
             then Some (!dep_select_cursor, !dep_select_chosen)
             else None)
          views
      in
      detail_scroll := Tui.detail_scroll_offset frame;
      checks_scroll := Tui.checks_scroll_offset frame;
      if Tui.detail_at_bottom frame then detail_follow := true;
      patches_start_row := Tui.patches_start_row frame;
      patches_scroll_offset := Tui.patches_scroll_offset frame;
      patches_visible_count := Tui.patch_count frame;
      let output = Tui.paint_frame frame in
      if not (String.equal output !prev_output) then (
        Eio.Flow.copy_string output Env.stdout;
        prev_output := output);
      loop ()
    in
    loop ()

  let run_input () =
    let {
      Tui_state.list_selected;
      detail_scroll;
      detail_follow;
      timeline_scroll;
      detail_scrolls;
      view_mode;
      sorted_patch_ids;
      input_mode;
      prompt_line;
      show_help;
      show_checks;
      checks_scroll;
      status_msg;
      patches_start_row;
      patches_scroll_offset;
      patches_visible_count;
      dep_select_cursor;
      dep_select_chosen;
    } =
      Env.tui_state
    in
    let buf = Tui_input.Edit_buffer.create () in
    let selected_pid () =
      let pids = !sorted_patch_ids in
      let count = List.length pids in
      if count = 0 || !list_selected < 0 then None
      else
        let idx = Int.max 0 (Int.min !list_selected (count - 1)) in
        List.nth pids idx
    in
    let sync_input () =
      match !input_mode with
      | Tui_input.Normal | Tui_input.Manage_patch | Tui_input.Select_patch_deps
        ->
          prompt_line := None
      | Tui_input.Prompt_pr | Tui_input.Prompt_worktree
      | Tui_input.Prompt_message | Tui_input.Prompt_broadcast
      | Tui_input.Prompt_patch_desc ->
          let prefix = Tui_input.prompt_prefix !input_mode in
          let contents = Tui_input.Edit_buffer.contents buf in
          let cursor_col =
            Term.visible_length prefix
            + Term.visible_length
                (String.sub contents ~pos:0
                   ~len:(Tui_input.Edit_buffer.cursor buf))
          in
          prompt_line :=
            Some { Tui.prompt_text = prefix ^ contents; cursor_col };
          Atomic.set Term.Raw.redraw_needed true
    in
    let history = Tui_input.History.create () in
    let saved_draft = ref "" in
    (* Carries the description typed in [Prompt_patch_desc] into the
       [Prompt_patch_deps] step of the add-patch flow. *)
    let pending_patch_desc = ref None in
    let eof_count = ref 0 in
    let last_click_time = ref 0.0 in
    let last_click_row = ref (-1) in
    let rec loop () =
      sync_input ();
      match Term.Key_io.poll () with
      | No_input ->
          Eio.Fiber.yield ();
          loop ()
      | Eof ->
          eof_count := !eof_count + 1;
          if !eof_count >= 10 then
            log_event Env.runtime
              "Stdin closed — input fiber giving up after 10 consecutive EOFs"
          else (
            Eio.Fiber.yield ();
            loop ())
      | Key key -> (
          eof_count := 0;
          if !show_help then (
            show_help := false;
            loop ())
          else if !show_checks then (
            (* Dedicated, self-contained scroll region. The detail transcript
               scroll stays dormant while this is up, preserving the "one live
               scroll offset" invariant. Render clamps [checks_scroll]. *)
            (match key with
            | Term.Key.Escape | Term.Key.Char ('q' | 'c') ->
                show_checks := false
            | Term.Key.Up | Term.Key.Char 'k' ->
                checks_scroll := Int.max 0 (!checks_scroll - 1)
            | Term.Key.Down | Term.Key.Char 'j' ->
                checks_scroll := Int.max 0 (!checks_scroll + 1)
            | Term.Key.Page_up ->
                checks_scroll := Int.max 0 (!checks_scroll - 10)
            | Term.Key.Page_down ->
                checks_scroll := Int.max 0 (!checks_scroll + 10)
            | Term.Key.Mouse (Term_key.Scroll { dir; _ }) ->
                let delta =
                  match dir with Term_key.Up -> -3 | Term_key.Down -> 3
                in
                checks_scroll := Int.max 0 (!checks_scroll + delta)
            | Term.Key.Char _ | Term.Key.Enter | Term.Key.Tab | Term.Key.Paste _
            | Term.Key.Backspace | Term.Key.Left | Term.Key.Right
            | Term.Key.Home | Term.Key.End | Term.Key.Delete | Term.Key.F _
            | Term.Key.Ctrl _
            | Term.Key.Mouse (Term_key.Click _)
            | Term.Key.Unknown _ ->
                ());
            loop ())
          else if Tui_input.equal_input_mode !input_mode Tui_input.Manage_patch
          then (
            (match key with
            | Term.Key.Escape -> input_mode := Tui_input.Normal
            | Term.Key.Char 'p' ->
                (* Add a new gameplan patch. Global action, not tied to the
                   selected patch: begin the two-step description → deps prompt. *)
                Tui_input.Edit_buffer.clear buf;
                pending_patch_desc := None;
                input_mode := Tui_input.Prompt_patch_desc
            | Term.Key.Char 'm' -> (
                input_mode := Tui_input.Normal;
                let target_patch_id =
                  match !view_mode with
                  | Tui.Detail_view patch_id -> Some patch_id
                  | Tui.List_view -> selected_pid ()
                  | Tui.Timeline_view -> None
                in
                match target_patch_id with
                | Some patch_id ->
                    let busy, has_pr =
                      Runtime.read Env.runtime (fun snap ->
                          let agent =
                            Orchestrator.agent snap.Runtime.orchestrator
                              patch_id
                          in
                          ( agent.Patch_agent.busy,
                            Patch_agent.is_pr_present agent ))
                    in
                    if busy then
                      log_event Env.runtime ~patch_id
                        "Cannot force-mark as merged — patch is currently busy"
                    else if not has_pr then
                      log_event Env.runtime ~patch_id
                        "Cannot force-mark as merged — patch has no usable PR \
                         (Missing or Absent)"
                    else (
                      Runtime.update_orchestrator Env.runtime (fun orch ->
                          Orchestrator.mark_merged orch patch_id);
                      log_event Env.runtime ~patch_id "Force-marked as merged")
                | None -> ())
            | Term.Key.Char 'a' -> (
                input_mode := Tui_input.Normal;
                let target_patch_id =
                  match !view_mode with
                  | Tui.Detail_view patch_id -> Some patch_id
                  | Tui.List_view -> selected_pid ()
                  | Tui.Timeline_view -> None
                in
                match target_patch_id with
                | Some patch_id -> (
                    match
                      Runtime.update_orchestrator_returning Env.runtime
                        (fun orch ->
                          match Orchestrator.find_agent orch patch_id with
                          | None -> (orch, None)
                          | Some agent ->
                              let v = not agent.Patch_agent.automerge_enabled in
                              let orch =
                                Orchestrator.set_automerge_enabled orch patch_id
                                  v
                              in
                              (orch, Some v))
                    with
                    | Some true ->
                        log_event Env.runtime ~patch_id "Automerge enabled"
                    | Some false ->
                        log_event Env.runtime ~patch_id "Automerge disabled"
                    | None -> ())
                | None -> ())
            | Term.Key.Char 'b' -> (
                input_mode := Tui_input.Normal;
                let target_patch_id =
                  match !view_mode with
                  | Tui.Detail_view patch_id -> Some patch_id
                  | Tui.List_view -> selected_pid ()
                  | Tui.Timeline_view -> None
                in
                match target_patch_id with
                | Some patch_id -> (
                    match
                      Runtime.update_orchestrator_returning Env.runtime
                        (fun orch ->
                          match Orchestrator.find_agent orch patch_id with
                          | None -> (orch, false)
                          | Some agent ->
                              if Patch_agent.needs_intervention agent then
                                ( Orchestrator.reset_intervention_state orch
                                    patch_id,
                                  true )
                              else (orch, false))
                    with
                    | true ->
                        log_event Env.runtime ~patch_id
                          "Bumped — cleared intervention state"
                    | false ->
                        log_event Env.runtime ~patch_id
                          "Cannot bump — patch is not in needs-intervention")
                | None -> ())
            | Term.Key.Char _ | Term.Key.Enter | Term.Key.Tab | Term.Key.Paste _
            | Term.Key.Backspace | Term.Key.Up | Term.Key.Down | Term.Key.Left
            | Term.Key.Right | Term.Key.Home | Term.Key.End | Term.Key.Page_up
            | Term.Key.Page_down | Term.Key.Delete | Term.Key.F _
            | Term.Key.Ctrl _ | Term.Key.Mouse _ | Term.Key.Unknown _ ->
                ());
            loop ())
          else if
            Tui_input.equal_input_mode !input_mode Tui_input.Select_patch_deps
          then (
            (* Add-patch step 2: multi-select dependencies. Candidates are the
               existing patches in display order (same ordering as [views], so
               the cursor index lines up with the rendered overlay). *)
            let candidates = !sorted_patch_ids in
            let n = List.length candidates in
            (match key with
            | Term.Key.Escape ->
                pending_patch_desc := None;
                input_mode := Tui_input.Normal
            | Term.Key.Up | Term.Key.Char 'k' ->
                if n > 0 then
                  dep_select_cursor := Int.max 0 (!dep_select_cursor - 1)
            | Term.Key.Down | Term.Key.Char 'j' ->
                if n > 0 then
                  dep_select_cursor := Int.min (n - 1) (!dep_select_cursor + 1)
            | Term.Key.Char ' ' -> (
                match List.nth candidates !dep_select_cursor with
                | None -> ()
                | Some pid ->
                    if List.mem !dep_select_chosen pid ~equal:Patch_id.equal
                    then
                      dep_select_chosen :=
                        List.filter !dep_select_chosen ~f:(fun p ->
                            not (Patch_id.equal p pid))
                    else dep_select_chosen := pid :: !dep_select_chosen)
            | Term.Key.Enter -> (
                let description = !pending_patch_desc in
                pending_patch_desc := None;
                input_mode := Tui_input.Normal;
                match description with
                | None ->
                    log_event Env.runtime
                      "Add patch failed — no pending description"
                | Some description -> (
                    (* Present deps in display order, not toggle order. *)
                    let dependencies =
                      List.filter candidates ~f:(fun p ->
                          List.mem !dep_select_chosen p ~equal:Patch_id.equal)
                    in
                    let title =
                      if String.length description <= 72 then description
                      else String.prefix description 71 ^ "…"
                    in
                    dep_select_chosen := [];
                    dep_select_cursor := 0;
                    match
                      Runtime.add_patch Env.runtime ~title ~description
                        ~dependencies
                    with
                    | Error msg ->
                        log_event Env.runtime
                          (Printf.sprintf "Cannot add patch — %s" msg)
                    | Ok patch ->
                        let deps_str =
                          match patch.Patch.dependencies with
                          | [] -> "no dependencies"
                          | ds ->
                              "depends on "
                              ^ (List.map ds ~f:Patch_id.to_string
                                |> String.concat ~sep:", ")
                        in
                        log_event Env.runtime ~patch_id:patch.Patch.id
                          (Printf.sprintf "Added patch %s (%s) — %s"
                             (Patch_id.to_string patch.Patch.id)
                             deps_str title)))
            | Term.Key.Char _ | Term.Key.Tab | Term.Key.Paste _
            | Term.Key.Backspace | Term.Key.Left | Term.Key.Right
            | Term.Key.Home | Term.Key.End | Term.Key.Page_up
            | Term.Key.Page_down | Term.Key.Delete | Term.Key.F _
            | Term.Key.Ctrl _ | Term.Key.Mouse _ | Term.Key.Unknown _ ->
                ());
            loop ())
          else if not (Tui_input.equal_input_mode !input_mode Tui_input.Normal)
          then
            match key with
            | Term.Key.Paste text ->
                Tui_input.Edit_buffer.insert_string buf (normalize_paste text);
                loop ()
            | Term.Key.Escape ->
                Tui_input.Edit_buffer.clear buf;
                saved_draft := "";
                pending_patch_desc := None;
                Tui_input.History.reset_browse history;
                input_mode := Tui_input.Normal;
                loop ()
            | Term.Key.Enter ->
                let line = Tui_input.Edit_buffer.contents buf in
                Tui_input.History.push history line;
                Tui_input.History.reset_browse history;
                let mode = !input_mode in
                Tui_input.Edit_buffer.clear buf;
                saved_draft := "";
                input_mode := Tui_input.Normal;
                let line = String.strip line in
                (match mode with
                | Tui_input.Prompt_message -> (
                    if not (String.is_empty line) then
                      match !view_mode with
                      | Tui.Detail_view patch_id ->
                          let patch_exists =
                            Runtime.read Env.runtime (fun snap ->
                                Map.mem
                                  (Orchestrator.agents_map
                                     snap.Runtime.orchestrator)
                                  patch_id)
                          in
                          if patch_exists then (
                            Runtime.update_orchestrator Env.runtime (fun orch ->
                                Orchestrator.send_human_message orch patch_id
                                  line);
                            log_event Env.runtime ~patch_id
                              (Printf.sprintf "Sent human message — %s" line))
                          else
                            log_event Env.runtime
                              (Printf.sprintf
                                 "Cannot send human message — unknown patch %s"
                                 (Patch_id.to_string patch_id))
                      | Tui.List_view | Tui.Timeline_view -> ())
                | Tui_input.Prompt_broadcast ->
                    if not (String.is_empty line) then begin
                      (* Broadcast goes to every agent that has a live session,
                         including ones parked in needs-intervention — a human
                         broadcast resets their intervention state and gives
                         them direction. Agents that never started a session
                         (Pending, etc.) have nothing to receive it, and merged
                         agents are terminal, so both are skipped via the
                         [has_session]/[merged] predicate rather than by display
                         status. *)
                      let targets =
                        Runtime.read Env.runtime (fun snap ->
                            Orchestrator.all_agents snap.Runtime.orchestrator
                            |> List.filter ~f:(fun (a : Patch_agent.t) ->
                                a.Patch_agent.has_session
                                && not a.Patch_agent.merged)
                            |> List.map ~f:(fun (a : Patch_agent.t) ->
                                a.Patch_agent.patch_id))
                      in
                      let count = List.length targets in
                      List.iter targets ~f:(fun patch_id ->
                          Runtime.update_orchestrator Env.runtime (fun orch ->
                              Orchestrator.send_human_message orch patch_id line));
                      log_event Env.runtime
                        (Printf.sprintf "Broadcast to %s — %s"
                           (pluralize count "patch with a session"
                              ~plural:"patches with a session")
                           line)
                    end
                | Tui_input.Prompt_pr -> (
                    match Int.of_string_opt line with
                    | Some n when n > 0 ->
                        let pr_number = Pr_number.of_int n in
                        let patch_id = Patch_id.of_string (Int.to_string n) in
                        let already_exists =
                          Runtime.read Env.runtime (fun snap ->
                              Option.is_some
                                (Orchestrator.find_agent
                                   snap.Runtime.orchestrator patch_id))
                        in
                        if already_exists then
                          log_event Env.runtime ~patch_id
                            (Printf.sprintf "Ad-hoc PR #%d already registered" n)
                        else (
                          status_msg :=
                            Some
                              {
                                Tui.level = Tui.Info;
                                text = Printf.sprintf "Fetching PR #%d…" n;
                                expires_at = None;
                              };
                          match Forge.pr_state pr_number with
                          | Error err ->
                              status_msg := None;
                              log_event Env.runtime ~patch_id
                                (Printf.sprintf "Cannot add ad-hoc PR #%d — %s"
                                   n (Forge.show_error err))
                          | Ok pr_state when Pr_state.is_fork pr_state ->
                              status_msg := None;
                              log_event Env.runtime ~patch_id
                                (Printf.sprintf
                                   "Cannot add ad-hoc PR #%d — fork PRs not \
                                    supported"
                                   n)
                          | Ok pr_state -> (
                              status_msg := None;
                              match pr_state.Pr_state.head_branch with
                              | None ->
                                  log_event Env.runtime ~patch_id
                                    (Printf.sprintf
                                       "Cannot add ad-hoc PR #%d — no head \
                                        branch"
                                       n)
                              | Some branch ->
                                  Env.register_pr_number ~patch_id ~pr_number;
                                  Runtime.update_orchestrator Env.runtime
                                    (fun orch ->
                                      let base_branch =
                                        Option.value
                                          pr_state.Pr_state.base_branch
                                          ~default:
                                            (Orchestrator.main_branch orch)
                                      in
                                      Orchestrator.add_agent orch ~patch_id
                                        ~branch ~base_branch ~pr_number);
                                  log_event Env.runtime ~patch_id
                                    (Printf.sprintf "Ad-hoc PR #%d added (%s)" n
                                       (Branch.to_string branch))))
                    | _ ->
                        if not (String.is_empty line) then
                          log_event Env.runtime
                            (Printf.sprintf "Invalid PR number — %s" line))
                | Tui_input.Prompt_worktree -> (
                    if not (String.is_empty line) then
                      let path = line in
                      match selected_pid () with
                      | None ->
                          log_event Env.runtime
                            "Cannot add worktree — no selectable patch"
                      | Some patch_id -> (
                          let busy =
                            Runtime.read Env.runtime (fun snap ->
                                (Orchestrator.agent snap.Runtime.orchestrator
                                   patch_id)
                                  .Patch_agent.busy)
                          in
                          if busy then
                            log_event Env.runtime ~patch_id
                              "Warning — patch is currently running, changing \
                               worktree may affect the live session";
                          let expected =
                            Worktree.worktree_dir ~project_name:Env.project_name
                              ~patch_id
                          in
                          try
                            let raw_path = Worktree.normalize_path path in
                            if not (Stdlib.Sys.file_exists raw_path) then
                              failwith ("Worktree path not found: " ^ raw_path);
                            if not (Stdlib.Sys.is_directory raw_path) then
                              failwith
                                ("Worktree path is not a directory: " ^ raw_path);
                            let canonical_real = Unix.realpath raw_path in
                            let git_file =
                              Stdlib.Filename.concat raw_path ".git"
                            in
                            if
                              (not (Stdlib.Sys.file_exists git_file))
                              || Stdlib.Sys.is_directory git_file
                            then
                              failwith
                                ("Path is not a git worktree (no .git file): "
                               ^ raw_path);
                            let canonical_expected =
                              try Unix.realpath expected
                              with Unix.Unix_error (Unix.ENOENT, _, _) ->
                                expected
                            in
                            if
                              not
                                (String.equal canonical_real canonical_expected)
                            then (
                              let parent = Stdlib.Filename.dirname expected in
                              (try Unix.mkdir parent 0o755 with
                              | Unix.Unix_error (Unix.ENOENT, _, _) ->
                                  failwith
                                    ("Cannot create parent directory: " ^ parent)
                              | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
                              (match Unix.lstat expected with
                              | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                                  ()
                              | { Unix.st_kind = Unix.S_LNK; _ } ->
                                  Unix.unlink expected
                              | {
                               Unix.st_kind =
                                 ( Unix.S_REG | Unix.S_DIR | Unix.S_CHR
                                 | Unix.S_BLK | Unix.S_FIFO | Unix.S_SOCK );
                               _;
                              } ->
                                  failwith
                                    (Printf.sprintf
                                       "Cannot overwrite non-symlink at %s"
                                       expected));
                              Unix.symlink canonical_real expected);
                            Runtime.update_orchestrator Env.runtime (fun orch ->
                                let orch =
                                  Orchestrator.reset_intervention_state orch
                                    patch_id
                                in
                                Orchestrator.set_worktree_path orch patch_id
                                  canonical_real);
                            status_msg := None;
                            if String.equal canonical_real canonical_expected
                            then
                              log_event Env.runtime ~patch_id
                                (Printf.sprintf
                                   "Worktree already at expected path %s"
                                   canonical_real)
                            else
                              log_event Env.runtime ~patch_id
                                (Printf.sprintf
                                   "Registered worktree — symlinked %s → %s"
                                   expected canonical_real)
                          with
                          | Failure msg ->
                              status_msg :=
                                Some
                                  {
                                    Tui.level = Tui.Error;
                                    text = msg;
                                    expires_at = None;
                                  };
                              log_event Env.runtime ~patch_id
                                (Printf.sprintf "Failed to add worktree — %s"
                                   msg)
                          | exn ->
                              let msg = Exn.to_string exn in
                              status_msg :=
                                Some
                                  {
                                    Tui.level = Tui.Error;
                                    text =
                                      Printf.sprintf
                                        "Failed to add worktree: %s" msg;
                                    expires_at = None;
                                  };
                              log_event Env.runtime ~patch_id
                                (Printf.sprintf "Failed to add worktree — %s"
                                   msg)))
                | Tui_input.Prompt_patch_desc ->
                    (* Step 1: capture the description, then open the
                       dependency-selection overlay. [input_mode] was forced to
                       [Normal] above, so override it back here. *)
                    if String.is_empty line then (
                      pending_patch_desc := None;
                      log_event Env.runtime
                        "Add patch cancelled — empty description")
                    else (
                      pending_patch_desc := Some line;
                      dep_select_cursor := 0;
                      dep_select_chosen := [];
                      input_mode := Tui_input.Select_patch_deps)
                | Tui_input.Normal | Tui_input.Manage_patch
                | Tui_input.Select_patch_deps ->
                    ());
                loop ()
            | Term.Key.Backspace ->
                Tui_input.Edit_buffer.delete_before buf;
                loop ()
            | Term.Key.Char c ->
                Tui_input.Edit_buffer.insert_char buf c;
                loop ()
            | Term.Key.Ctrl 'z' ->
                Term.Raw.suspend ();
                loop ()
            | Term.Key.Up ->
                let was_browsing = Tui_input.History.is_browsing history in
                (match Tui_input.History.older history with
                | Some s ->
                    if not was_browsing then
                      saved_draft := Tui_input.Edit_buffer.contents buf;
                    Tui_input.Edit_buffer.set buf s
                | None -> ());
                loop ()
            | Term.Key.Down ->
                (if Tui_input.History.is_browsing history then
                   match Tui_input.History.newer history with
                   | Tui_input.History.Entry s ->
                       Tui_input.Edit_buffer.set buf s
                   | Tui_input.History.At_fresh ->
                       Tui_input.Edit_buffer.set buf !saved_draft);
                loop ()
            | Term.Key.Left ->
                Tui_input.Edit_buffer.move_left buf;
                loop ()
            | Term.Key.Right ->
                Tui_input.Edit_buffer.move_right buf;
                loop ()
            | Term.Key.Home ->
                Tui_input.Edit_buffer.move_home buf;
                loop ()
            | Term.Key.End ->
                Tui_input.Edit_buffer.move_end buf;
                loop ()
            | Term.Key.Delete ->
                Tui_input.Edit_buffer.delete_at buf;
                loop ()
            | Term.Key.Ctrl 'a' ->
                Tui_input.Edit_buffer.move_home buf;
                loop ()
            | Term.Key.Ctrl 'e' ->
                Tui_input.Edit_buffer.move_end buf;
                loop ()
            | Term.Key.Ctrl 'k' ->
                ignore (Tui_input.Edit_buffer.kill_to_end buf);
                loop ()
            | Term.Key.Ctrl 'u' ->
                ignore (Tui_input.Edit_buffer.kill_to_start buf);
                loop ()
            | Term.Key.Tab | Term.Key.Page_up | Term.Key.Page_down
            | Term.Key.F _ | Term.Key.Ctrl _ | Term.Key.Mouse _
            | Term.Key.Unknown _ ->
                loop ()
          else if Term.Key.equal key (Term.Key.Ctrl 'z') then (
            Term.Raw.suspend ();
            loop ())
          else
            match key with
            | Term.Key.Paste text -> (
                match !view_mode with
                | Tui.Detail_view _ ->
                    Tui_input.Edit_buffer.clear buf;
                    Tui_input.Edit_buffer.insert_string buf
                      (normalize_paste text);
                    input_mode := Tui_input.Prompt_message;
                    loop ()
                | Tui.List_view | Tui.Timeline_view -> loop ())
            | Term.Key.Mouse ev -> (
                match (ev, !view_mode) with
                | ( Term_key.Click
                      { button = Term_key.Left; row; press = true; _ },
                    Tui.List_view ) ->
                    let start = !patches_start_row in
                    let count = !patches_visible_count in
                    let screen_idx = row - start in
                    let abs_idx = !patches_scroll_offset + screen_idx in
                    if start > 0 && screen_idx >= 0 && screen_idx < count then (
                      let now = Unix.gettimeofday () in
                      let is_double =
                        Float.compare (now -. !last_click_time) 0.3 <= 0
                        && !last_click_row = abs_idx
                      in
                      last_click_time := now;
                      last_click_row := abs_idx;
                      if is_double then (
                        let pids = !sorted_patch_ids in
                        let pid_count = List.length pids in
                        if abs_idx < pid_count then (
                          list_selected := abs_idx;
                          let pid = List.nth_exn pids abs_idx in
                          view_mode := Tui.Detail_view pid;
                          match Stdlib.Hashtbl.find_opt detail_scrolls pid with
                          | Some (offset, follow) ->
                              detail_scroll := offset;
                              detail_follow := follow
                          | None ->
                              detail_scroll := 0;
                              detail_follow := true))
                      else list_selected := abs_idx);
                    loop ()
                | ( Term_key.Click
                      { button = Term_key.Left; row; press = true; _ },
                    Tui.Detail_view pid ) ->
                    let size = Term.get_size () in
                    let height =
                      match size with Some s -> s.Term.rows | None -> 24
                    in
                    if row >= height - 1 then (
                      Stdlib.Hashtbl.replace detail_scrolls pid
                        (!detail_scroll, !detail_follow);
                      view_mode := Tui.List_view);
                    loop ()
                | Term_key.Scroll { dir; _ }, Tui.List_view ->
                    let count = List.length !sorted_patch_ids in
                    let delta =
                      match dir with Term_key.Up -> -1 | Term_key.Down -> 1
                    in
                    list_selected :=
                      Int.max (-1)
                        (Int.min (count - 1) (!list_selected + delta));
                    loop ()
                | Term_key.Scroll { dir; _ }, Tui.Detail_view _ ->
                    let delta =
                      match dir with Term_key.Up -> -3 | Term_key.Down -> 3
                    in
                    detail_scroll := Int.max 0 (!detail_scroll + delta);
                    loop ()
                | Term_key.Scroll { dir; _ }, Tui.Timeline_view ->
                    let delta =
                      match dir with Term_key.Up -> -3 | Term_key.Down -> 3
                    in
                    timeline_scroll := Int.max 0 (!timeline_scroll + delta);
                    loop ()
                | ( Term_key.Click
                      {
                        button =
                          Term_key.Left | Term_key.Middle | Term_key.Right;
                        _;
                      },
                    (Tui.List_view | Tui.Detail_view _ | Tui.Timeline_view) ) ->
                    loop ())
            | Term.Key.Char '*'
              when Tui.equal_view_mode !view_mode Tui.List_view ->
                Tui_input.Edit_buffer.clear buf;
                input_mode := Tui_input.Prompt_broadcast;
                loop ()
            | Term.Key.Char '+'
              when Tui.equal_view_mode !view_mode Tui.List_view ->
                Tui_input.Edit_buffer.clear buf;
                input_mode := Tui_input.Prompt_pr;
                loop ()
            | Term.Key.Char 'w'
              when Tui.equal_view_mode !view_mode Tui.List_view ->
                Tui_input.Edit_buffer.clear buf;
                input_mode := Tui_input.Prompt_worktree;
                loop ()
            | Term.Key.Char 'o'
              when match !view_mode with
                   | Tui.Detail_view _ -> true
                   | Tui.List_view -> Option.is_some (selected_pid ())
                   | Tui.Timeline_view -> false ->
                let target_patch_id =
                  match !view_mode with
                  | Tui.Detail_view patch_id -> Some patch_id
                  | Tui.List_view -> selected_pid ()
                  | Tui.Timeline_view -> None
                in
                (match target_patch_id with
                | Some patch_id -> (
                    match Env.find_pr_number ~patch_id with
                    | Some pr_number -> (
                        let url =
                          Printf.sprintf "https://github.com/%s/%s/pull/%d"
                            Env.owner Env.repo
                            (Pr_number.to_int pr_number)
                        in
                        let open_cmd =
                          if Stdlib.Sys.file_exists "/usr/bin/open" then "open"
                          else "xdg-open"
                        in
                        match
                          Eio.Process.run Env.process_mgr [ open_cmd; url ]
                        with
                        | () -> (
                            match !status_msg with
                            | Some
                                {
                                  Tui.text =
                                    "No PR to open" | "Could not open browser";
                                  _;
                                } ->
                                status_msg := None
                            | Some _ | None -> ())
                        | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
                        | exception _ ->
                            status_msg :=
                              Some
                                {
                                  Tui.level = Tui.Error;
                                  text = "Could not open browser";
                                  expires_at = None;
                                })
                    | None ->
                        status_msg :=
                          Some
                            {
                              Tui.level = Tui.Info;
                              text = "No PR to open";
                              expires_at = None;
                            })
                | None -> ());
                loop ()
            | Term.Key.Char 'm'
              when match !view_mode with
                   | Tui.Detail_view _ -> true
                   | Tui.List_view -> Option.is_some (selected_pid ())
                   | Tui.Timeline_view -> false ->
                input_mode := Tui_input.Manage_patch;
                loop ()
            | Term.Key.Char 'c'
              when match !view_mode with
                   | Tui.Detail_view _ -> true
                   | Tui.List_view -> Option.is_some (selected_pid ())
                   | Tui.Timeline_view -> false ->
                show_checks := true;
                checks_scroll := 0;
                loop ()
            | Term.Key.Char _ | Term.Key.Enter | Term.Key.Tab
            | Term.Key.Backspace | Term.Key.Escape | Term.Key.Up | Term.Key.Down
            | Term.Key.Left | Term.Key.Right | Term.Key.Home | Term.Key.End
            | Term.Key.Page_up | Term.Key.Page_down | Term.Key.Delete
            | Term.Key.F _ | Term.Key.Ctrl _ | Term.Key.Unknown _ -> (
                let cmd = Tui_input.of_key key in
                match cmd with
                | Tui_input.Quit -> raise Quit
                | Tui_input.Move_up | Tui_input.Move_down | Tui_input.Page_up
                | Tui_input.Page_down | Tui_input.Scroll_top
                | Tui_input.Scroll_bottom ->
                    (match !view_mode with
                    | Tui.List_view ->
                        let count = List.length !sorted_patch_ids in
                        list_selected :=
                          Tui_input.apply_move ~count ~selected:!list_selected
                            cmd
                    | Tui.Timeline_view ->
                        let total =
                          Runtime.read Env.runtime (fun snap ->
                              let log = snap.Runtime.activity_log in
                              List.length
                                (activity_entries_of_log ~limit:100 log))
                        in
                        let height =
                          match Term.get_size () with
                          | Some s -> s.Term.rows
                          | None -> 24
                        in
                        let status_rows =
                          match !status_msg with Some _ -> 1 | None -> 0
                        in
                        let reserved = 9 + status_rows in
                        let max_rows = Int.max 0 (height - reserved) in
                        let max_offset = Int.max 0 (total - max_rows) in
                        timeline_scroll :=
                          Tui_input.apply_move ~count:(max_offset + 1)
                            ~selected:(Int.min !timeline_scroll max_offset)
                            cmd
                    | Tui.Detail_view _ -> (
                        match cmd with
                        | Tui_input.Scroll_top ->
                            detail_follow := false;
                            detail_scroll := 0
                        | Tui_input.Scroll_bottom -> detail_follow := true
                        | Tui_input.Move_up | Tui_input.Move_down
                        | Tui_input.Page_up | Tui_input.Page_down
                        | Tui_input.Quit | Tui_input.Help | Tui_input.Select
                        | Tui_input.Back | Tui_input.Timeline | Tui_input.Noop
                        | Tui_input.Send_message _ | Tui_input.Add_pr _
                        | Tui_input.Add_worktree _ | Tui_input.Remove_patch
                        | Tui_input.Open_in_browser ->
                            let delta =
                              match cmd with
                              | Tui_input.Move_up -> -1
                              | Tui_input.Move_down -> 1
                              | Tui_input.Page_up -> -10
                              | Tui_input.Page_down -> 10
                              | Tui_input.Quit | Tui_input.Help
                              | Tui_input.Select | Tui_input.Back
                              | Tui_input.Timeline | Tui_input.Noop
                              | Tui_input.Send_message _ | Tui_input.Add_pr _
                              | Tui_input.Add_worktree _
                              | Tui_input.Remove_patch
                              | Tui_input.Open_in_browser | Tui_input.Scroll_top
                              | Tui_input.Scroll_bottom ->
                                  0
                            in
                            if delta <> 0 then detail_follow := false;
                            detail_scroll := Int.max 0 (!detail_scroll + delta)));
                    loop ()
                | Tui_input.Select -> (
                    match !view_mode with
                    | Tui.List_view ->
                        let pids = !sorted_patch_ids in
                        let count = List.length pids in
                        if count > 0 && !list_selected >= 0 then (
                          let idx = Int.min !list_selected (count - 1) in
                          list_selected := idx;
                          let pid = List.nth_exn pids idx in
                          view_mode := Tui.Detail_view pid;
                          match Stdlib.Hashtbl.find_opt detail_scrolls pid with
                          | Some (offset, follow) ->
                              detail_scroll := offset;
                              detail_follow := follow
                          | None ->
                              detail_scroll := 0;
                              detail_follow := true);
                        loop ()
                    | Tui.Detail_view _ ->
                        input_mode := Tui_input.Prompt_message;
                        loop ()
                    | Tui.Timeline_view -> loop ())
                | Tui_input.Back -> (
                    match !view_mode with
                    | Tui.Detail_view pid ->
                        Stdlib.Hashtbl.replace detail_scrolls pid
                          (!detail_scroll, !detail_follow);
                        view_mode := Tui.List_view;
                        loop ()
                    | Tui.Timeline_view ->
                        view_mode := Tui.List_view;
                        loop ()
                    | Tui.List_view -> loop ())
                | Tui_input.Timeline -> (
                    match !view_mode with
                    | Tui.Timeline_view ->
                        view_mode := Tui.List_view;
                        loop ()
                    | Tui.List_view | Tui.Detail_view _ ->
                        (match !view_mode with
                        | Tui.Detail_view pid ->
                            Stdlib.Hashtbl.replace detail_scrolls pid
                              (!detail_scroll, !detail_follow)
                        | Tui.List_view | Tui.Timeline_view -> ());
                        view_mode := Tui.Timeline_view;
                        timeline_scroll := 0;
                        loop ())
                | Tui_input.Help ->
                    show_help := true;
                    loop ()
                | Tui_input.Remove_patch ->
                    (match !view_mode with
                    | Tui.List_view -> (
                        match selected_pid () with
                        | None ->
                            log_event Env.runtime
                              "Cannot remove patch — no selectable patch"
                        | Some patch_id ->
                            let busy, in_gameplan =
                              Runtime.read Env.runtime (fun snap ->
                                  let agent =
                                    Orchestrator.agent snap.Runtime.orchestrator
                                      patch_id
                                  in
                                  let in_gp =
                                    List.exists
                                      snap.Runtime.gameplan.Gameplan.patches
                                      ~f:(fun (p : Patch.t) ->
                                        Patch_id.equal p.Patch.id patch_id)
                                  in
                                  (agent.Patch_agent.busy, in_gp))
                            in
                            if in_gameplan then
                              log_event Env.runtime ~patch_id
                                "Cannot remove gameplan patch — only ad-hoc \
                                 patches can be removed"
                            else (
                              if busy then
                                log_event Env.runtime ~patch_id
                                  "Warning — patch is currently running, it \
                                   may create a GitHub PR before stopping";
                              Runtime.update_orchestrator Env.runtime
                                (fun orch ->
                                  Orchestrator.remove_agent orch patch_id);
                              Env.unregister_pr_number ~patch_id;
                              log_event Env.runtime ~patch_id
                                "Removed ad-hoc patch"))
                    | Tui.Detail_view _ | Tui.Timeline_view -> ());
                    loop ()
                | Tui_input.Noop | Tui_input.Send_message _ | Tui_input.Add_pr _
                | Tui_input.Add_worktree _ | Tui_input.Open_in_browser ->
                    loop ()))
    in
    loop ()
end

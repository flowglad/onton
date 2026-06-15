(* @archlint.module test
   @archlint.domain markdown-render *)

(** Rendering tests for [Tui.render_frame], focused on the short-terminal
    behavior described in flowglad/onton#267.

    These exercise the actual frame layout — they do not paint to a real
    terminal. The frame is rendered, ANSI is stripped, and the resulting
    plain-text lines are inspected. *)

open Base
open Onton
open Onton_core.Types

let make_view ~id ~title =
  {
    Tui.patch_id = Patch_id.of_string id;
    title;
    branch = Branch.of_string (Printf.sprintf "branch-%s" id);
    status = Tui.Pending;
    queue_len = 0;
    current_op = None;
    current_op_state = Onton_core.Patch_agent.Queued;
    ci_failures = 0;
    dep_ids = [];
    has_pr = false;
    has_conflict = false;
    needs_intervention = false;
    human_messages = 0;
    ci_checks = [];
    recent_stream = [];
    pr_number = None;
    merge_queue_entry = None;
    pr_missing = false;
    base_branch = None;
    worktree_path = None;
    intervention_reason = None;
    automerge_enabled = false;
    automerge_deadline = None;
    automerge_failure_count = 0;
    complexity = None;
    backend = "claude";
    model = None;
  }

let plain_lines frame =
  Tui.frame_to_string frame |> String.split ~on:'\n'
  |> List.map ~f:Onton.Term.strip_ansi

let render ?(width = 80) ?(height = 24) ?(view_mode = Tui.List_view)
    ?(activity = []) ?(project_name = "demo") ?(backend_name = "claude") views =
  Tui.render_frame ~width ~height ~selected:0 ~scroll_offset:0 ~view_mode
    ~activity ~project_name ~backend_name ~show_help:false ~show_checks:false
    ~checks_scroll:0 ~show_manage:false ~now:0.0 views

let one_view = [ make_view ~id:"1" ~title:"first patch" ]

let many_views =
  List.init 8 ~f:(fun i ->
      make_view
        ~id:(Int.to_string (i + 1))
        ~title:(Printf.sprintf "patch %d" (i + 1)))

let one_activity_entry =
  [
    Tui.Event
      { patch_id = Some "1"; message = "something happened"; timestamp = 0.0 };
  ]

let four_activity_entries =
  List.init 4 ~f:(fun i ->
      Tui.Event
        {
          patch_id = Some "1";
          message = Printf.sprintf "event %d" i;
          timestamp = Float.of_int i;
        })

(* --- helpers --------------------------------------------------------------- *)

let line_contains lines needle =
  List.exists lines ~f:(fun line -> String.is_substring line ~substring:needle)

let find_index lines ~f =
  List.findi lines ~f:(fun _ line -> f line) |> Option.map ~f:fst

(* --- tests ----------------------------------------------------------------- *)

let test_header_has_project_and_backend () =
  let frame = render one_view in
  let lines = plain_lines frame in
  let first = List.hd_exn lines in
  assert (String.is_substring first ~substring:"demo");
  assert (String.is_substring first ~substring:"claude");
  (* Backend should appear on the right half *)
  let demo_at = String.substr_index_exn first ~pattern:"demo" in
  let backend_at = String.substr_index_exn first ~pattern:"claude" in
  assert (backend_at > demo_at)

let test_header_truncates_when_too_narrow () =
  (* Width too small for both project name and backend — project name wins. *)
  let frame = render ~width:10 one_view in
  let lines = plain_lines frame in
  let first = List.hd_exn lines in
  assert (String.is_substring first ~substring:"demo");
  (* Visible width of the header line should not exceed the terminal width. *)
  assert (Onton.Term.visible_length first <= 10)

let test_no_summary_row () =
  (* The dedicated " backend | X/Y merged | …" summary line is gone. *)
  let frame = render one_view in
  let lines = plain_lines frame in
  assert (not (line_contains lines "merged │"));
  assert (not (line_contains lines "1/1 merged"))

let test_list_view_shows_activity_when_room () =
  let frame = render ~height:24 ~activity:four_activity_entries one_view in
  let lines = plain_lines frame in
  assert (line_contains lines "Activity");
  assert (line_contains lines "event 0");
  assert (line_contains lines "event 3")

let test_list_view_collapses_activity_when_tight () =
  (* Height that fits header + some patch rows + footer but not the full
     activity block. Activity must collapse, patches must remain. *)
  let frame = render ~height:12 ~activity:four_activity_entries many_views in
  let lines = plain_lines frame in
  assert (line_contains lines "Patches");
  assert (Tui.patch_count frame >= 1);
  (* No activity rows shown — patches won the budget. *)
  assert (not (line_contains lines "event 0"));
  assert (not (line_contains lines "event 3"))

let test_list_view_patches_survive_activity_at_short_height () =
  (* The patch block must keep at least one row at a typical short tmux
     pane height; activity must be the first thing to disappear. *)
  let frame = render ~height:14 ~activity:four_activity_entries many_views in
  assert (Tui.patch_count frame >= 1);
  let lines = plain_lines frame in
  assert (not (line_contains lines "event 0"))

let test_list_view_activity_squeezed_partial () =
  (* At a height where activity barely fits, it should be truncated rather
     than steal from patches. *)
  let frame = render ~height:18 ~activity:four_activity_entries many_views in
  assert (Tui.patch_count frame >= 1);
  let lines = plain_lines frame in
  (* Activity is partially visible: at least one entry shown but not all
     four. Asserting both ends prevents the test from passing vacuously if
     activity collapsed entirely. *)
  let visible_events =
    List.count
      [ "event 0"; "event 1"; "event 2"; "event 3" ]
      ~f:(line_contains lines)
  in
  assert (visible_events >= 1);
  assert (visible_events < 4)

let test_frame_anchored_at_top () =
  (* The header must be on the first emitted line — paint_frame moves the
     cursor to row 1 and then writes [lines.[0]] first. *)
  let frame = render one_view in
  let lines = plain_lines frame in
  let header = List.hd_exn lines in
  assert (String.is_substring header ~substring:"demo")

let test_detail_view_no_summary () =
  let pv = make_view ~id:"1" ~title:"focus me" in
  let frame = render ~view_mode:(Tui.Detail_view pv.Tui.patch_id) [ pv ] in
  let lines = plain_lines frame in
  assert (line_contains lines "focus me");
  (* Detail view used to repeat the summary line; that line is gone. *)
  assert (not (line_contains lines "1/1 merged"))

let test_timeline_view_no_summary () =
  let frame =
    render ~view_mode:Tui.Timeline_view ~activity:one_activity_entry one_view
  in
  let lines = plain_lines frame in
  assert (not (line_contains lines "1/1 merged"));
  assert (line_contains lines "something happened")

let test_patches_render_above_activity () =
  let frame = render ~activity:one_activity_entry many_views in
  let lines = plain_lines frame in
  let patches_idx =
    find_index lines ~f:(String.is_substring ~substring:"Patches")
  in
  let activity_idx =
    find_index lines ~f:(String.is_substring ~substring:"Activity")
  in
  match (patches_idx, activity_idx) with
  | Some p, Some a -> assert (p < a)
  | _ -> assert false

let make_checks n =
  List.init n ~f:(fun i ->
      {
        Ci_check.name = Printf.sprintf "check-%d" i;
        conclusion = (if i = 0 then "failure" else "pending");
        details_url = None;
        description = None;
        started_at = None;
        id = None;
      })

let count_check_rows lines =
  List.count lines ~f:(fun line -> String.is_substring line ~substring:"check-")

(* Inline detail section: with more than the cap, only the cap is shown plus an
   ellipsis pointing at the overlay; metadata and footer survive. *)
let test_detail_inline_ci_checks_capped () =
  let pv =
    {
      (make_view ~id:"1" ~title:"many checks") with
      Tui.ci_checks = make_checks 40;
    }
  in
  let frame =
    render ~height:60 ~view_mode:(Tui.Detail_view pv.Tui.patch_id) [ pv ]
  in
  let lines = plain_lines frame in
  assert (count_check_rows lines = 8);
  assert (line_contains lines "32 more");
  assert (line_contains lines "press c to view all");
  (* Metadata and footer are not crowded out. *)
  assert (line_contains lines "Patch ID:");
  assert (line_contains lines "h:help")

(* At or below the cap, every check is shown inline and there is no ellipsis. *)
let test_detail_inline_ci_checks_under_cap () =
  let pv =
    {
      (make_view ~id:"1" ~title:"few checks") with
      Tui.ci_checks = make_checks 5;
    }
  in
  let frame =
    render ~height:60 ~view_mode:(Tui.Detail_view pv.Tui.patch_id) [ pv ]
  in
  let lines = plain_lines frame in
  assert (count_check_rows lines = 5);
  assert (not (line_contains lines "more — press c"))

(* The overlay lists more than the inline cap and reports the total. *)
let test_checks_overlay_lists_all () =
  let pv =
    { (make_view ~id:"1" ~title:"overlay") with Tui.ci_checks = make_checks 40 }
  in
  let lines, _ =
    Tui.render_checks_overlay ~width:80 ~height:50 ~scroll_offset:0 pv
  in
  let lines = List.map lines ~f:Onton.Term.strip_ansi in
  assert (count_check_rows lines > 8);
  assert (line_contains lines "of 40")

(* A large scroll offset clamps to the last page. *)
let test_checks_overlay_clamps_offset () =
  let pv =
    { (make_view ~id:"1" ~title:"overlay") with Tui.ci_checks = make_checks 40 }
  in
  let _, clamped =
    Tui.render_checks_overlay ~width:80 ~height:20 ~scroll_offset:9999 pv
  in
  assert (clamped > 0);
  assert (clamped <= 40)

let test_patch_5_merge_queue_badge () =
  let pv =
    {
      (make_view ~id:"1" ~title:"queued patch") with
      Tui.status = Tui.Approved_idle;
      Tui.pr_number = Some (Pr_number.of_int 42);
      Tui.merge_queue_entry =
        Some
          {
            Onton_core.Pr_state.id = "mqe_123";
            state = Onton_core.Pr_state.Mq_awaiting_checks;
            position = 3;
          };
    }
  in
  let frame = render [ pv ] in
  let lines = plain_lines frame in
  assert (line_contains lines "mq-awaiting-checks #3")

let () =
  test_patch_5_merge_queue_badge ();
  test_header_has_project_and_backend ();
  test_header_truncates_when_too_narrow ();
  test_no_summary_row ();
  test_list_view_shows_activity_when_room ();
  test_list_view_collapses_activity_when_tight ();
  test_list_view_patches_survive_activity_at_short_height ();
  test_list_view_activity_squeezed_partial ();
  test_frame_anchored_at_top ();
  test_detail_view_no_summary ();
  test_detail_inline_ci_checks_capped ();
  test_detail_inline_ci_checks_under_cap ();
  test_checks_overlay_lists_all ();
  test_checks_overlay_clamps_offset ();
  test_timeline_view_no_summary ();
  test_patches_render_above_activity ();
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"render_frame handles generated dimensions"
       ~count:100
       QCheck2.Gen.(pair (int_range 20 120) (int_range 4 40))
       (fun (width, height) ->
         let frame = render ~width ~height one_view in
         Tui.patch_count frame <= List.length one_view));
  (* [is_tool_marker] is true exactly when the (stripped) line starts with the
     injected "[tool: " prefix; build both shapes from generated text. *)
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"is_tool_marker detects the tool prefix" ~count:200
       QCheck2.Gen.(pair bool (string_size (int_range 0 12)))
       (fun (mark, rest) ->
         (* Keep a non-space token after the prefix: is_tool_marker strips the
            line first, so a bare "[tool: " would lose its trailing space. *)
         let line = if mark then "[tool: read " ^ rest else "plain " ^ rest in
         Bool.equal (Markdown_render.is_tool_marker line) mark));
  (* [style_tool_marker] preserves the underlying text (only adds ANSI), so
     stripping ANSI recovers the original line. *)
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"style_tool_marker preserves text under strip_ansi"
       ~count:200
       (* Plain letters only: arbitrary bytes could include ESC sequences that
          strip_ansi would also remove, which is not what this property is about. *)
       QCheck2.Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 0 16))
       (fun line ->
         String.equal
           (Onton.Term.strip_ansi (Markdown_render.style_tool_marker line))
           line));
  (* [render_to_lines] is total over arbitrary markdown-ish input and never
     introduces embedded newlines within a single emitted line. *)
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"render_to_lines is total and line-split"
       ~count:200
       QCheck2.Gen.(string_size (int_range 0 40))
       (fun s ->
         try
           let lines = Markdown_render.render_to_lines s in
           List.for_all lines ~f:(fun l -> not (String.contains l '\n'))
         with _ -> false));
  (* [render_block] is total over the block parsed from generated text. *)
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"render_block is total over parsed blocks"
       ~count:200
       QCheck2.Gen.(string_size (int_range 0 40))
       (fun s ->
         try
           let block =
             Cmarkit.Doc.block (Cmarkit.Doc.of_string ~strict:false s)
           in
           let lines = Markdown_render.render_block block in
           List.length lines >= 0
         with _ -> false));
  (* [render_inline] on a plain Text inline returns the text verbatim (it is the
     identity branch of the renderer). *)
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"render_inline returns plain text verbatim"
       ~count:200
       QCheck2.Gen.(string_size (int_range 0 24))
       (fun s ->
         let inline = Cmarkit.Inline.Text (s, Cmarkit.Meta.none) in
         String.equal (Markdown_render.render_inline inline) s));
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~name:"markdown render public surface is linked"
       QCheck2.Gen.unit (fun () ->
         ignore Markdown_render.is_tool_marker;
         ignore Markdown_render.render_block;
         ignore Markdown_render.render_inline;
         ignore Markdown_render.render_to_lines;
         ignore Markdown_render.style_tool_marker;
         true));
  Stdlib.print_endline "PASS: tui render_frame short-height tests"

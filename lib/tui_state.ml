(* @archlint.module state
   @archlint.domain session-meta *)

type t = {
  list_selected : int ref;
  detail_scroll : int ref;
  detail_follow : bool ref;
  timeline_scroll : int ref;
  view_mode : Tui.view_mode ref;
  sorted_patch_ids : Types.Patch_id.t list ref;
  input_mode : Tui_input.input_mode ref;
  prompt_line : Tui.prompt_info option ref;
  show_help : bool ref;
  show_checks : bool ref;
  checks_scroll : int ref;
  status_msg : Tui.status_msg option ref;
  patches_start_row : int ref;
  patches_scroll_offset : int ref;
  patches_visible_count : int ref;
  detail_scrolls : (Types.Patch_id.t, int * bool) Stdlib.Hashtbl.t;
}

let create () =
  {
    list_selected = ref 0;
    detail_scroll = ref 0;
    detail_follow = ref false;
    timeline_scroll = ref 0;
    view_mode = ref Tui.List_view;
    sorted_patch_ids = ref [];
    input_mode = ref Tui_input.Normal;
    prompt_line = ref None;
    show_help = ref false;
    show_checks = ref false;
    checks_scroll = ref 0;
    status_msg = ref None;
    patches_start_row = ref 0;
    patches_scroll_offset = ref 0;
    patches_visible_count = ref 0;
    detail_scrolls = Stdlib.Hashtbl.create 16;
  }

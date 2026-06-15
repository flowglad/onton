(* @archlint.module interface
   @archlint.domain tui-state *)

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

val create : unit -> t

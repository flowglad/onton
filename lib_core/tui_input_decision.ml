(* @archlint.module core
   @archlint.domain tui-input *)

open Base

(** Pure keyboard input -> TUI command translation. *)

type command =
  | Quit
  | Help
  | Move_up
  | Move_down
  | Page_up
  | Page_down
  | Scroll_top
  | Scroll_bottom
  | Select
  | Back
  | Noop
  | Timeline
  | Send_message of string
  | Add_pr of Types.Pr_number.t
  | Add_worktree of string
  | Remove_patch
  | Open_in_browser
[@@deriving show, eq]

type input_mode =
  | Normal
  | Prompt_pr
  | Prompt_worktree
  | Prompt_message
  | Prompt_broadcast
  | Manage_patch
[@@deriving show, eq]

let prompt_prefix = function
  | Normal -> ""
  | Prompt_pr -> "PR #: "
  | Prompt_worktree -> "Worktree: "
  | Prompt_message -> "> "
  | Prompt_broadcast -> "broadcast> "
  | Manage_patch -> ""

let of_key (key : Term_key.t) : command =
  match key with
  | Char 'q' | Ctrl 'c' -> Quit
  | Char 'h' | Char '?' -> Help
  | Char 't' -> Timeline
  | Up | Char 'k' -> Move_up
  | Down | Char 'j' -> Move_down
  | Page_up -> Page_up
  | Page_down -> Page_down
  | Home -> Scroll_top
  | End -> Scroll_bottom
  | Enter -> Select
  | Escape | Backspace -> Back
  | Char '-' | Char 'x' -> Remove_patch
  | Char 'o' -> Open_in_browser
  | Char _ | Tab | Delete | Left | Right | F _ | Ctrl _ | Paste _ | Mouse _
  | Unknown _ ->
      Noop

(** Apply a command to the selected index. Returns [-1] when no patch should be
    selected (arrowing past the first or last item deselects). Page moves clamp
    to boundaries instead of deselecting. Valid range: [-1, count-1]. *)
let apply_move ~count ~selected (cmd : command) =
  if count <= 0 then -1
  else
    let clamp n = Int.max 0 (Int.min (count - 1) n) in
    let selected =
      if selected < -1 || selected >= count then -1 else selected
    in
    match cmd with
    | Move_up ->
        if selected = -1 then count - 1
        else
          let n = selected - 1 in
          if n < -1 then -1 else n
    | Move_down ->
        let n = if selected = -1 then 0 else selected + 1 in
        if n >= count then -1 else n
    | Page_up -> if selected = -1 then -1 else clamp (selected - 5)
    | Page_down -> if selected = -1 then 0 else clamp (selected + 5)
    | Scroll_top -> 0
    | Scroll_bottom -> count - 1
    | Quit | Help | Select | Back | Timeline | Noop | Send_message _ | Add_pr _
    | Add_worktree _ | Remove_patch | Open_in_browser ->
        if selected >= count then -1 else selected

let%test "q maps to Quit" = equal_command (of_key (Char 'q')) Quit
let%test "ctrl-c maps to Quit" = equal_command (of_key (Ctrl 'c')) Quit
let%test "r maps to Noop" = equal_command (of_key (Char 'r')) Noop
let%test "h maps to Help" = equal_command (of_key (Char 'h')) Help
let%test "? maps to Help" = equal_command (of_key (Char '?')) Help
let%test "up arrow maps to Move_up" = equal_command (of_key Up) Move_up
let%test "k maps to Move_up" = equal_command (of_key (Char 'k')) Move_up
let%test "down arrow maps to Move_down" = equal_command (of_key Down) Move_down
let%test "j maps to Move_down" = equal_command (of_key (Char 'j')) Move_down
let%test "page_up maps to Page_up" = equal_command (of_key Page_up) Page_up

let%test "page_down maps to Page_down" =
  equal_command (of_key Page_down) Page_down

let%test "home maps to Scroll_top" = equal_command (of_key Home) Scroll_top
let%test "end maps to Scroll_bottom" = equal_command (of_key End) Scroll_bottom
let%test "enter maps to Select" = equal_command (of_key Enter) Select
let%test "escape maps to Back" = equal_command (of_key Escape) Back
let%test "backspace maps to Back" = equal_command (of_key Backspace) Back

let%test "- maps to Remove_patch" =
  equal_command (of_key (Char '-')) Remove_patch

let%test "x maps to Remove_patch" =
  equal_command (of_key (Char 'x')) Remove_patch

let%test "o maps to Open_in_browser" =
  equal_command (of_key (Char 'o')) Open_in_browser

let%test "unknown key maps to Noop" = equal_command (of_key (Char 'z')) Noop
let%test "tab maps to Noop" = equal_command (of_key Tab) Noop

let%test "apply_move down from 0" =
  apply_move ~count:5 ~selected:0 Move_down = 1

let%test "apply_move up from 0 deselects" =
  apply_move ~count:5 ~selected:0 Move_up = -1

let%test "apply_move down at end deselects" =
  apply_move ~count:5 ~selected:4 Move_down = -1

let%test "apply_move page_down" = apply_move ~count:20 ~selected:3 Page_down = 8

let%test "apply_move page_up clamps at 0" =
  apply_move ~count:20 ~selected:2 Page_up = 0

let%test "apply_move with count 0" =
  apply_move ~count:0 ~selected:0 Move_down = -1

let%test "apply_move noop preserves" = apply_move ~count:5 ~selected:3 Noop = 3

let%test "apply_move noop deselects stale selection" =
  apply_move ~count:3 ~selected:9 Noop = -1

let%test "apply_move down past last deselects" =
  apply_move ~count:5 ~selected:4 Move_down = -1

let%test "apply_move down from deselected selects first" =
  apply_move ~count:5 ~selected:(-1) Move_down = 0

let%test "apply_move up from deselected selects last" =
  apply_move ~count:5 ~selected:(-1) Move_up = 4

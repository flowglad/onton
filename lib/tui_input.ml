open Base

(** Pure keyboard input → TUI command translation.

    This module contains no I/O. It maps parsed key events to semantic TUI
    commands, making the input handling independently testable. *)

type command =
  | Quit
  | Refresh
  | Help
  | Move_up
  | Move_down
  | Page_up
  | Page_down
  | Select
  | Back
  | Noop
  | Send_message of Types.Patch_id.t * string
  | Add_pr of Types.Pr_number.t
[@@deriving show, eq]

let of_key (key : Term.Key.t) : command =
  match key with
  | Char 'q' | Ctrl 'c' -> Quit
  | Char 'r' -> Refresh
  | Char 'h' | Char '?' -> Help
  | Up | Char 'k' -> Move_up
  | Down | Char 'j' -> Move_down
  | Page_up -> Page_up
  | Page_down -> Page_down
  | Enter -> Select
  | Escape | Backspace -> Back
  | Char _ | Tab | Delete | Home | End | Left | Right | F _ | Ctrl _ | Unknown _
    ->
      Noop

(** Apply a command to the selected index, clamping to [0, count-1]. *)
let apply_move ~count ~selected (cmd : command) =
  if count <= 0 then 0
  else
    let clamp n = Int.max 0 (Int.min (count - 1) n) in
    let next =
      match cmd with
      | Move_up -> selected - 1
      | Move_down -> selected + 1
      | Page_up -> selected - 5
      | Page_down -> selected + 5
      | Quit | Refresh | Help | Select | Back | Noop | Send_message _
      | Add_pr _ ->
          selected
    in
    clamp next

let equal_command_option = Option.equal equal_command

(** Parse a text-mode input line into a command.

    Supported formats:
    - ["N> message"] — send a human message to patch N
    - ["+123"] — register ad-hoc PR #123 for the currently selected patch

    Returns [None] for empty or unrecognised input. *)
let parse_line (line : string) : command option =
  let line = String.strip line in
  if String.is_empty line then None
  else if String.is_prefix line ~prefix:"+" then
    let rest = String.drop_prefix line 1 in
    match Int.of_string_opt rest with
    | Some n when n > 0 -> Some (Add_pr (Types.Pr_number.of_int n))
    | _ -> None
  else
    match String.lsplit2 line ~on:'>' with
    | Some (num_str, msg) -> (
        let num_str = String.strip num_str in
        let msg = String.strip msg in
        match Int.of_string_opt num_str with
        | Some n when n > 0 && not (String.is_empty msg) ->
            Some
              (Send_message (Types.Patch_id.of_string (Int.to_string n), msg))
        | _ -> None)
    | None -> None

let%test "parse_line: message input" =
  equal_command_option
    (parse_line "3> fix the tests")
    (Some (Send_message (Types.Patch_id.of_string "3", "fix the tests")))

let%test "parse_line: add PR" =
  equal_command_option (parse_line "+123")
    (Some (Add_pr (Types.Pr_number.of_int 123)))

let%test "parse_line: empty" = Option.is_none (parse_line "")
let%test "parse_line: whitespace" = Option.is_none (parse_line "   ")
let%test "parse_line: invalid" = Option.is_none (parse_line "hello")

let%test "parse_line: message with spaces in number" =
  equal_command_option
    (parse_line " 5 > rewrite module")
    (Some (Send_message (Types.Patch_id.of_string "5", "rewrite module")))

let%test "parse_line: zero PR rejected" = Option.is_none (parse_line "+0")
let%test "parse_line: negative PR rejected" = Option.is_none (parse_line "+-1")

let%test "parse_line: empty message rejected" =
  Option.is_none (parse_line "3>  ")

let%test "q maps to Quit" = equal_command (of_key (Char 'q')) Quit
let%test "ctrl-c maps to Quit" = equal_command (of_key (Ctrl 'c')) Quit
let%test "r maps to Refresh" = equal_command (of_key (Char 'r')) Refresh
let%test "h maps to Help" = equal_command (of_key (Char 'h')) Help
let%test "? maps to Help" = equal_command (of_key (Char '?')) Help
let%test "up arrow maps to Move_up" = equal_command (of_key Up) Move_up
let%test "k maps to Move_up" = equal_command (of_key (Char 'k')) Move_up
let%test "down arrow maps to Move_down" = equal_command (of_key Down) Move_down
let%test "j maps to Move_down" = equal_command (of_key (Char 'j')) Move_down
let%test "page_up maps to Page_up" = equal_command (of_key Page_up) Page_up

let%test "page_down maps to Page_down" =
  equal_command (of_key Page_down) Page_down

let%test "enter maps to Select" = equal_command (of_key Enter) Select
let%test "escape maps to Back" = equal_command (of_key Escape) Back
let%test "backspace maps to Back" = equal_command (of_key Backspace) Back
let%test "unknown key maps to Noop" = equal_command (of_key (Char 'z')) Noop
let%test "tab maps to Noop" = equal_command (of_key Tab) Noop

let%test "apply_move down from 0" =
  apply_move ~count:5 ~selected:0 Move_down = 1

let%test "apply_move up from 0 clamps" =
  apply_move ~count:5 ~selected:0 Move_up = 0

let%test "apply_move down at end clamps" =
  apply_move ~count:5 ~selected:4 Move_down = 4

let%test "apply_move page_down" = apply_move ~count:20 ~selected:3 Page_down = 8

let%test "apply_move page_up clamps at 0" =
  apply_move ~count:20 ~selected:2 Page_up = 0

let%test "apply_move with count 0" =
  apply_move ~count:0 ~selected:0 Move_down = 0

let%test "apply_move noop preserves" = apply_move ~count:5 ~selected:3 Noop = 3

let%test "apply_move noop clamps stale selection" =
  apply_move ~count:3 ~selected:9 Noop = 2

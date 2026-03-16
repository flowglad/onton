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
  | Timeline
  | Send_message of Types.Patch_id.t * string
  | Add_pr of Types.Pr_number.t
[@@deriving show, eq]

let of_key (key : Term.Key.t) : command =
  match key with
  | Char 'q' | Ctrl 'c' -> Quit
  | Char 'r' -> Refresh
  | Char 'h' | Char '?' -> Help
  | Char 't' -> Timeline
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
      | Quit | Refresh | Help | Select | Back | Timeline | Noop | Send_message _
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

(** Input history for the text-mode prompt.

    Stores previously entered lines and supports up/down navigation. The history
    is bounded; oldest entries are dropped when the capacity is exceeded. *)
module History = struct
  type t = {
    entries : string array;
    capacity : int;
    mutable size : int;
    mutable cursor : int;
        (** Points one past the newest entry (the slot where the next entry will
            be written). *)
    mutable browse_depth : int;
        (** How many steps back from [cursor] the user has browsed. 0 means at
            the fresh-input position (not browsing). *)
  }

  let create ?(capacity = 100) () =
    {
      entries = Array.create ~len:capacity "";
      capacity;
      size = 0;
      cursor = 0;
      browse_depth = 0;
    }

  let push t line =
    let line = String.strip line in
    if String.is_empty line then ()
    else begin
      (* Don't push duplicates of the most recent entry *)
      let last_idx = (t.cursor - 1 + t.capacity) % t.capacity in
      if t.size > 0 && String.equal t.entries.(last_idx) line then ()
      else begin
        t.entries.(t.cursor) <- line;
        t.cursor <- (t.cursor + 1) % t.capacity;
        t.size <- Int.min (t.size + 1) t.capacity
      end;
      t.browse_depth <- 0
    end

  let older t =
    if t.browse_depth >= t.size then None
    else begin
      t.browse_depth <- t.browse_depth + 1;
      let idx = (t.cursor - t.browse_depth + t.capacity) % t.capacity in
      Some t.entries.(idx)
    end

  let newer t =
    if t.browse_depth <= 0 then None
    else begin
      t.browse_depth <- t.browse_depth - 1;
      if t.browse_depth = 0 then None
      else
        let idx = (t.cursor - t.browse_depth + t.capacity) % t.capacity in
        Some t.entries.(idx)
    end

  let reset_browse t = t.browse_depth <- 0
  let is_browsing t = t.browse_depth > 0

  let%test "empty history returns None for older" =
    let h = create ~capacity:5 () in
    Option.is_none (older h)

  let%test "push and recall" =
    let h = create ~capacity:5 () in
    push h "hello";
    match older h with Some s -> String.equal s "hello" | None -> false

  let%test "duplicate suppression" =
    let h = create ~capacity:5 () in
    push h "hello";
    push h "hello";
    match older h with
    | Some s ->
        String.equal s "hello"
        &&
        (* only one entry, so next older is None *)
        Option.is_none (older h)
    | None -> false

  let%test "empty line ignored" =
    let h = create ~capacity:5 () in
    push h "   ";
    Option.is_none (older h)

  let%test "newer returns None at bottom" =
    let h = create ~capacity:5 () in
    push h "a";
    Option.is_none (newer h)

  let%test "older then newer round-trips" =
    let h = create ~capacity:5 () in
    push h "a";
    push h "b";
    let _ = older h in
    (* at "b" *)
    let _ = older h in
    (* at "a" *)
    match newer h with
    | Some s -> String.equal s "b"
    | None -> false

  let%test "newer past newest returns None" =
    let h = create ~capacity:5 () in
    push h "a";
    push h "b";
    let _ = older h in
    let _ = older h in
    let _ = newer h in
    (* at "b" *)
    Option.is_none (newer h)

  let%test "capacity overflow drops oldest" =
    let h = create ~capacity:3 () in
    push h "a";
    push h "b";
    push h "c";
    push h "d";
    (* should have b, c, d — "a" dropped *)
    let e1 = older h in
    let e2 = older h in
    let e3 = older h in
    let e4 = older h in
    Option.is_none e4
    && Option.value_map e1 ~default:false ~f:(String.equal "d")
    && Option.value_map e2 ~default:false ~f:(String.equal "c")
    && Option.value_map e3 ~default:false ~f:(String.equal "b")

  let%test "push resets browse position" =
    let h = create ~capacity:5 () in
    push h "a";
    push h "b";
    let _ = older h in
    push h "c";
    (* browse should be reset *)
    not (is_browsing h)
end

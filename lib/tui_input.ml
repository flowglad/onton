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
      | Quit | Refresh | Help | Select | Back | Noop | Send_message _ | Add_pr _
        ->
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

(** Input history — a pure zipper over previously entered lines.

    [prev] is older entries (most recent first), [next] is newer entries (oldest
    first), and [draft] holds the in-progress line the user was typing before
    navigating history. *)
module History = struct
  type t = {
    prev : string list;
    next : string list;
    draft : string;
    max_size : int;
  }
  [@@deriving show, eq]

  let create ?(max_size = 100) () =
    { prev = []; next = []; draft = ""; max_size }

  let add entry t =
    (* Skip duplicates of the most recent entry and empty strings *)
    if String.is_empty entry then t
    else
      let prev =
        match t.prev @ List.rev t.next with
        | top :: _ when String.equal top entry -> t.prev @ List.rev t.next
        | _ -> (entry :: t.prev) @ List.rev t.next
      in
      (* Flatten and truncate to max_size *)
      let prev = List.take prev t.max_size in
      { prev; next = []; draft = ""; max_size = t.max_size }

  let up ~current t =
    match t.prev with
    | [] -> (current, t)
    | top :: rest ->
        let draft = if List.is_empty t.next then current else t.draft in
        (top, { t with prev = rest; next = top :: t.next; draft })

  let down ~current:_ t =
    match t.next with
    | [] -> (t.draft, t)
    | [ only ] ->
        (t.draft, { t with next = []; prev = only :: t.prev; draft = "" })
    | top :: next_entry :: rest ->
        (next_entry, { t with next = next_entry :: rest; prev = top :: t.prev })

  let reset t =
    { t with prev = t.prev @ List.rev t.next; next = []; draft = "" }

  let entries t = List.rev t.prev @ t.next

  let%test "empty history up returns current" =
    let h = create () in
    let line, h' = up ~current:"foo" h in
    String.equal line "foo" && equal h h'

  let%test "add then up recalls entry" =
    let h = create () |> add "hello" in
    let line, _ = up ~current:"" h in
    String.equal line "hello"

  let%test "up then down restores draft" =
    let h = create () |> add "hello" in
    let _, h = up ~current:"typing" h in
    let line, _ = down ~current:"hello" h in
    String.equal line "typing"

  let%test "duplicate suppression" =
    let h = create () |> add "hello" |> add "hello" in
    equal_list String.equal (entries h) [ "hello" ]

  let%test "empty string not added" =
    let h = create () |> add "" in
    equal_list String.equal (entries h) []

  let%test "max_size truncation" =
    let h = create ~max_size:3 () in
    let h = h |> add "a" |> add "b" |> add "c" |> add "d" in
    List.length (entries h) = 3

  let%test "multiple entries in order" =
    let h = create () |> add "first" |> add "second" |> add "third" in
    let line1, h = up ~current:"" h in
    let line2, h = up ~current:line1 h in
    let line3, _ = up ~current:line2 h in
    String.equal line1 "third"
    && String.equal line2 "second"
    && String.equal line3 "first"

  let%test "reset flattens navigation" =
    let h = create () |> add "a" |> add "b" in
    let _, h = up ~current:"" h in
    let h = reset h in
    List.is_empty h.next

  let%test "entries returns chronological order" =
    let h = create () |> add "first" |> add "second" in
    equal_list String.equal (entries h) [ "first"; "second" ]
end

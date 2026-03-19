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
  | Send_message of string
  | Add_pr of Types.Pr_number.t
  | Add_worktree of string
  | Remove_patch
[@@deriving show, eq]

type input_mode = Normal | Prompt_pr | Prompt_worktree | Prompt_message
[@@deriving show, eq]

let prompt_prefix = function
  | Normal -> ""
  | Prompt_pr -> "PR #: "
  | Prompt_worktree -> "Worktree: "
  | Prompt_message -> "> "

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
  | Char '-' | Char 'x' -> Remove_patch
  | Char _ | Tab | Delete | Home | End | Left | Right | F _ | Ctrl _ | Paste _
  | Mouse _ | Unknown _ ->
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
      | Add_pr _ | Add_worktree _ | Remove_patch ->
          selected
    in
    clamp next

let equal_command_option = Option.equal equal_command
let _ = equal_command_option
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

let%test "- maps to Remove_patch" =
  equal_command (of_key (Char '-')) Remove_patch

let%test "x maps to Remove_patch" =
  equal_command (of_key (Char 'x')) Remove_patch

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

  type newer_result = At_fresh | Entry of string

  let create ?(capacity = 50) () =
    if capacity < 1 then invalid_arg "History.create: capacity must be >= 1";
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
    if t.browse_depth <= 0 then At_fresh
    else begin
      t.browse_depth <- t.browse_depth - 1;
      if t.browse_depth = 0 then At_fresh
      else
        let idx = (t.cursor - t.browse_depth + t.capacity) % t.capacity in
        Entry t.entries.(idx)
    end

  let reset_browse t = t.browse_depth <- 0
  let is_browsing t = t.browse_depth > 0

  let%test "zero capacity raises" =
    try
      let _ = create ~capacity:0 () in
      false
    with Invalid_argument _ -> true

  let%test "negative capacity raises" =
    try
      let _ = create ~capacity:(-1) () in
      false
    with Invalid_argument _ -> true

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

  let%test "newer returns At_fresh at bottom" =
    let h = create ~capacity:5 () in
    push h "a";
    match newer h with At_fresh -> true | Entry _ -> false

  let%test "older then newer round-trips" =
    let h = create ~capacity:5 () in
    push h "a";
    push h "b";
    let _ = older h in
    (* at "b" *)
    let _ = older h in
    (* at "a" *)
    match newer h with
    | Entry s -> String.equal s "b"
    | At_fresh -> false

  let%test "newer past newest returns At_fresh" =
    let h = create ~capacity:5 () in
    push h "a";
    push h "b";
    let _ = older h in
    let _ = older h in
    let _ = newer h in
    (* at "b" *)
    match newer h with
    | At_fresh -> true
    | Entry _ -> false

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

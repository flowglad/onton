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
  | Char 'o' -> Open_in_browser
  | Char _ | Tab | Delete | Home | End | Left | Right | F _ | Ctrl _ | Paste _
  | Mouse _ | Unknown _ ->
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
        let n = selected - 1 in
        if n < -1 then -1 else n
    | Move_down ->
        let n = if selected = -1 then 0 else selected + 1 in
        if n >= count then -1 else n
    | Page_up -> if selected = -1 then -1 else clamp (selected - 5)
    | Page_down -> if selected = -1 then 0 else clamp (selected + 5)
    | Quit | Refresh | Help | Select | Back | Timeline | Noop | Send_message _
    | Add_pr _ | Add_worktree _ | Remove_patch | Open_in_browser ->
        if selected >= count then -1 else selected

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

let%test "apply_move up from deselected stays deselected" =
  apply_move ~count:5 ~selected:(-1) Move_up = -1

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

module Edit_buffer = struct
  type t = { mutable text : string; mutable pos : int }

  let create () = { text = ""; pos = 0 }
  let contents t = t.text
  let cursor t = t.pos
  let length t = String.length t.text

  let clear t =
    t.text <- "";
    t.pos <- 0

  let set t s =
    t.text <- s;
    t.pos <- String.length s

  let insert_char t c =
    let len = String.length t.text in
    let s = String.make 1 c in
    t.text <-
      String.sub t.text ~pos:0 ~len:t.pos
      ^ s
      ^ String.sub t.text ~pos:t.pos ~len:(len - t.pos);
    t.pos <- t.pos + 1

  let insert_string t s =
    let slen = String.length s in
    if slen > 0 then begin
      let len = String.length t.text in
      t.text <-
        String.sub t.text ~pos:0 ~len:t.pos
        ^ s
        ^ String.sub t.text ~pos:t.pos ~len:(len - t.pos);
      t.pos <- t.pos + slen
    end

  let delete_before t =
    if t.pos > 0 then begin
      let len = String.length t.text in
      t.text <-
        String.sub t.text ~pos:0 ~len:(t.pos - 1)
        ^ String.sub t.text ~pos:t.pos ~len:(len - t.pos);
      t.pos <- t.pos - 1
    end

  let delete_at t =
    let len = String.length t.text in
    if t.pos < len then
      t.text <-
        String.sub t.text ~pos:0 ~len:t.pos
        ^ String.sub t.text ~pos:(t.pos + 1) ~len:(len - t.pos - 1)

  let move_left t = if t.pos > 0 then t.pos <- t.pos - 1
  let move_right t = if t.pos < String.length t.text then t.pos <- t.pos + 1
  let move_home t = t.pos <- 0
  let move_end t = t.pos <- String.length t.text

  let kill_to_end t =
    let len = String.length t.text in
    let killed = String.sub t.text ~pos:t.pos ~len:(len - t.pos) in
    t.text <- String.sub t.text ~pos:0 ~len:t.pos;
    killed

  let kill_to_start t =
    let len = String.length t.text in
    let killed = String.sub t.text ~pos:0 ~len:t.pos in
    t.text <- String.sub t.text ~pos:t.pos ~len:(len - t.pos);
    t.pos <- 0;
    killed

  let%test "create is empty with cursor at 0" =
    let b = create () in
    String.equal (contents b) "" && cursor b = 0

  let%test "insert_char at end" =
    let b = create () in
    insert_char b 'a';
    insert_char b 'b';
    String.equal (contents b) "ab" && cursor b = 2

  let%test "insert_char at middle" =
    let b = create () in
    insert_char b 'a';
    insert_char b 'c';
    move_left b;
    insert_char b 'b';
    String.equal (contents b) "abc" && cursor b = 2

  let%test "insert_string at middle" =
    let b = create () in
    set b "ac";
    move_left b;
    insert_string b "bb";
    String.equal (contents b) "abbc" && cursor b = 3

  let%test "delete_before at middle" =
    let b = create () in
    set b "abc";
    move_left b;
    delete_before b;
    String.equal (contents b) "ac" && cursor b = 1

  let%test "delete_before at start is no-op" =
    let b = create () in
    set b "abc";
    move_home b;
    delete_before b;
    String.equal (contents b) "abc" && cursor b = 0

  let%test "delete_at removes char under cursor" =
    let b = create () in
    set b "abc";
    move_home b;
    delete_at b;
    String.equal (contents b) "bc" && cursor b = 0

  let%test "delete_at at end is no-op" =
    let b = create () in
    set b "abc";
    delete_at b;
    String.equal (contents b) "abc" && cursor b = 3

  let%test "move_left clamps at 0" =
    let b = create () in
    set b "ab";
    move_home b;
    move_left b;
    cursor b = 0

  let%test "move_right clamps at length" =
    let b = create () in
    set b "ab";
    move_right b;
    cursor b = 2

  let%test "kill_to_end returns and removes suffix" =
    let b = create () in
    set b "abcde";
    move_home b;
    move_right b;
    move_right b;
    let killed = kill_to_end b in
    String.equal killed "cde" && String.equal (contents b) "ab" && cursor b = 2

  let%test "kill_to_start returns and removes prefix" =
    let b = create () in
    set b "abcde";
    move_home b;
    move_right b;
    move_right b;
    let killed = kill_to_start b in
    String.equal killed "ab" && String.equal (contents b) "cde" && cursor b = 0

  let%test "set puts cursor at end" =
    let b = create () in
    set b "hello";
    cursor b = 5 && String.equal (contents b) "hello"

  let%test "clear resets everything" =
    let b = create () in
    set b "hello";
    clear b;
    cursor b = 0 && String.equal (contents b) ""
end

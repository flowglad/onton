open Base

(** Low-level ANSI terminal primitives. *)

(** ANSI SGR (Select Graphic Rendition) codes. *)
module Sgr = struct
  let reset = "\027[0m"
  let bold = "\027[1m"
  let dim = "\027[2m"
  let italic = "\027[3m"
  let underline = "\027[4m"
  let strikethrough = "\027[9m"
  let fg_black = "\027[30m"
  let fg_red = "\027[31m"
  let fg_green = "\027[32m"
  let fg_yellow = "\027[33m"
  let fg_blue = "\027[34m"
  let fg_magenta = "\027[35m"
  let fg_cyan = "\027[36m"
  let fg_white = "\027[37m"
  let fg_default = "\027[39m"
  let bg_black = "\027[40m"
  let bg_red = "\027[41m"
  let bg_green = "\027[42m"
  let bg_yellow = "\027[43m"
  let bg_blue = "\027[44m"
  let bg_magenta = "\027[45m"
  let bg_cyan = "\027[46m"
  let bg_white = "\027[47m"
  let bg_default = "\027[49m"
  let fg_rgb r g b = Printf.sprintf "\027[38;2;%d;%d;%dm" r g b
  let bg_rgb r g b = Printf.sprintf "\027[48;2;%d;%d;%dm" r g b
  let fg_256 n = Printf.sprintf "\027[38;5;%dm" n
  let bg_256 n = Printf.sprintf "\027[48;5;%dm" n
end

(** Cursor movement and screen control. *)
module Cursor = struct
  let up n = Printf.sprintf "\027[%dA" n
  let down n = Printf.sprintf "\027[%dB" n
  let forward n = Printf.sprintf "\027[%dC" n
  let back n = Printf.sprintf "\027[%dD" n
  let move_to ~row ~col = Printf.sprintf "\027[%d;%dH" row col
  let save = "\027[s"
  let restore = "\027[u"
  let hide = "\027[?25l"
  let show = "\027[?25h"
end

(** Screen clearing. *)
module Clear = struct
  let screen = "\027[2J"
  let to_end = "\027[0J"
  let to_start = "\027[1J"
  let line = "\027[2K"
  let line_to_end = "\027[0K"
  let line_to_start = "\027[1K"
end

type style = string list
(** Style type for composing multiple attributes. *)

let style codes = String.concat codes
let styled codes text = String.concat codes ^ text ^ Sgr.reset

(** Strip all ANSI escape sequences from a string. *)
let strip_ansi s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else if Char.equal (String.get s i) '\027' then skip_seq (i + 1)
    else begin
      Buffer.add_char buf (String.get s i);
      loop (i + 1)
    end
  and skip_seq i =
    if i >= len then Buffer.contents buf
    else if Char.equal (String.get s i) '[' then skip_params (i + 1)
    else begin
      (* Not a CSI sequence, emit the char *)
      Buffer.add_char buf (String.get s i);
      loop (i + 1)
    end
  and skip_params i =
    if i >= len then Buffer.contents buf
    else
      let c = String.get s i in
      (* Parameters and intermediates: 0x20-0x3F, final byte: 0x40-0x7E *)
      if Char.to_int c >= 0x20 && Char.to_int c <= 0x3F then skip_params (i + 1)
      else
        (* Final byte consumed *)
        loop (i + 1)
  in
  loop 0

(** Visible character width of a string (strips ANSI codes). *)
let visible_length s = String.length (strip_ansi s)

(** Pad or truncate to fit a visible width. *)
let fit_width width s =
  let vis = strip_ansi s in
  let vlen = String.length vis in
  if vlen <= width then s ^ String.make (width - vlen) ' '
  else String.prefix vis width

(** Repeat a string n times. *)
let repeat n s =
  let buf = Buffer.create (n * String.length s) in
  for _ = 1 to n do
    Buffer.add_string buf s
  done;
  Buffer.contents buf

(** Draw a horizontal rule. *)
let hrule ?(ch = "─") width = repeat width ch

let%test "strip_ansi removes SGR" =
  String.equal (strip_ansi (styled [ Sgr.bold; Sgr.fg_red ] "hello")) "hello"

let%test "strip_ansi preserves plain text" =
  String.equal (strip_ansi "plain text") "plain text"

let%test "visible_length ignores ANSI" =
  visible_length (styled [ Sgr.bold ] "test") = 4

let%test "fit_width pads short strings" =
  String.equal (fit_width 10 "hi") "hi        "

let%test "fit_width truncates long strings" =
  String.equal (fit_width 3 "hello") "hel"

let%test "hrule default" = String.equal (hrule 3) "───"
let%test "repeat" = String.equal (repeat 3 "ab") "ababab"

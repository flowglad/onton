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

(** Number of bytes in a UTF-8 sequence starting with byte [b]. *)
let utf8_char_width b =
  let b = Char.to_int b in
  if b land 0x80 = 0 then 1
  else if b land 0xE0 = 0xC0 then 2
  else if b land 0xF0 = 0xE0 then 3
  else if b land 0xF8 = 0xF0 then 4
  else 1

(** Visible character width of a string (strips ANSI codes, counts UTF-8
    codepoints rather than bytes). *)
let visible_length s =
  let raw = strip_ansi s in
  let len = String.length raw in
  let rec loop i count =
    if i >= len then count
    else loop (i + utf8_char_width (String.get raw i)) (count + 1)
  in
  loop 0 0

(** Pad or truncate to fit a visible width, preserving ANSI formatting.
    Truncation walks the original string, copying escape sequences verbatim and
    counting only visible characters until [width] is reached. *)
let fit_width width s =
  let vlen = visible_length s in
  if vlen <= width then s ^ String.make (width - vlen) ' '
  else
    let buf = Buffer.create (width * 2) in
    let len = String.length s in
    let rec loop i visible =
      if visible >= width || i >= len then Buffer.contents buf
      else if Char.equal (String.get s i) '\027' then
        copy_escape (i + 1) visible
      else
        let w = utf8_char_width (String.get s i) in
        let () =
          for j = 0 to w - 1 do
            if i + j < len then Buffer.add_char buf (String.get s (i + j))
          done
        in
        loop (i + w) (visible + 1)
    and copy_escape i visible =
      Buffer.add_char buf '\027';
      if i >= len then Buffer.contents buf
      else if Char.equal (String.get s i) '[' then begin
        Buffer.add_char buf '[';
        copy_params (i + 1) visible
      end
      else begin
        Buffer.add_char buf (String.get s i);
        loop (i + 1) visible
      end
    and copy_params i visible =
      if i >= len then Buffer.contents buf
      else
        let c = String.get s i in
        Buffer.add_char buf c;
        if Char.to_int c >= 0x20 && Char.to_int c <= 0x3F then
          copy_params (i + 1) visible
        else loop (i + 1) visible
    in
    let result = loop 0 0 in
    result ^ Sgr.reset

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
  String.equal (strip_ansi (fit_width 3 "hello")) "hel"

let%test "fit_width truncation preserves ANSI" =
  let s = styled [ Sgr.bold ] "hello" in
  let result = fit_width 3 s in
  String.equal (strip_ansi result) "hel" && not (String.equal result "hel")

let%test "visible_length counts multibyte glyphs as one" =
  visible_length "⚠" = 1

let%test "visible_length multibyte with ANSI" =
  visible_length (styled [ Sgr.fg_yellow ] "⚠ hello") = 7

let%test "visible_length cross mark glyph" = visible_length "✗" = 1

let%test "fit_width pads multibyte string" =
  let s = fit_width 5 "⚠" in
  visible_length s = 5 && String.is_prefix s ~prefix:"⚠"

let%test "fit_width truncates multibyte string" =
  let s = styled [ Sgr.fg_red ] "⚠ hello" in
  visible_length (fit_width 3 s) = 3

let%test "hrule default" = String.equal (hrule 3) "───"
let%test "repeat" = String.equal (repeat 3 "ab") "ababab"

type size = { rows : int; cols : int } [@@deriving show, eq]
(** Terminal size as rows × cols. *)

(** Query terminal size via stty. Returns None if the terminal size cannot be
    determined. *)
let get_size () =
  try
    let ic = Unix.open_process_in "stty size 2>/dev/null </dev/tty" in
    let line = In_channel.input_line ic in
    let _ = Unix.close_process_in ic in
    match line with
    | Some s -> (
        match String.split s ~on:' ' with
        | [ rows; cols ] ->
            Some { rows = Int.of_string rows; cols = Int.of_string cols }
        | _ -> None)
    | None -> None
  with _ -> None

(** Raw mode management. Saves/restores original termios settings. *)
module Raw = struct
  type state = { original : Unix.terminal_io }

  let make_raw_settings (original : Unix.terminal_io) : Unix.terminal_io =
    {
      original with
      Unix.c_icanon = false;
      c_echo = false;
      c_icrnl = false;
      c_ixon = false;
      c_vmin = 1;
      c_vtime = 0;
      c_isig = false;
    }

  let enter () =
    let fd = Unix.stdin in
    let original = Unix.tcgetattr fd in
    Unix.tcsetattr fd Unix.TCSAFLUSH (make_raw_settings original);
    { original }

  let leave state = Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH state.original

  (** Shared mutable state for suspend/resume across signal boundaries. Uses
      [Atomic.t] because it is written by the SIGCONT handler and read by the
      main fiber. *)
  let _saved_state : state option Atomic.t = Atomic.make None

  (** Saved previous signal handlers, restored by {!clear_suspend_handlers}. *)
  let _saved_handlers :
      (Stdlib.Sys.signal_behavior * Stdlib.Sys.signal_behavior) option ref =
    ref None

  (** Flag set by the SIGCONT handler to request an immediate TUI redraw. The
      TUI render loop should check and clear this each iteration. *)
  let redraw_needed : bool Atomic.t = Atomic.make false

  (** Write a string to stdout, handling short writes and EINTR. Uses
      [single_write_substring] which maps directly to [write(2)] — safe to call
      from signal handlers unlike buffered I/O. *)
  let write_stdout_all s =
    let len = String.length s in
    let rec go off =
      if off < len then
        try
          let n = Unix.single_write_substring Unix.stdout s off (len - off) in
          go (off + n)
        with
        | Unix.Unix_error (Unix.EINTR, _, _) -> go off
        | Unix.Unix_error (_, _, _) -> ()
    in
    go 0

  (** Suspend the terminal: restore original settings, show cursor, then send
      SIGSTOP to ourselves. A SIGCONT handler (installed via
      {!install_suspend_handlers}) re-enters raw mode automatically. *)
  let suspend () =
    let current = Atomic.get _saved_state in
    match current with
    | None -> () (* not in raw mode, nothing to do *)
    | Some state ->
        (* Block SIGCONT before CAS so the handler cannot race between the
           state clear and the sigprocmask call. *)
        let old_mask = Unix.sigprocmask Unix.SIG_BLOCK [ Stdlib.Sys.sigcont ] in
        (* CAS ensures only one concurrent suspend() proceeds — if the SIGTSTP
           handler races with a Ctrl+Z suspend, the loser's CAS fails. *)
        if Atomic.compare_and_set _saved_state current None then (
          leave state;
          write_stdout_all
            (Clear.screen ^ Cursor.move_to ~row:1 ~col:1 ^ Cursor.show);
          Unix.kill (Unix.getpid ()) Stdlib.Sys.sigstop;
          (* SIGCONT is unblocked here — handler runs and re-enters raw mode *)
          ignore (Unix.sigprocmask Unix.SIG_SETMASK old_mask))
        else ignore (Unix.sigprocmask Unix.SIG_SETMASK old_mask)

  (** Install SIGTSTP/SIGCONT handlers for proper terminal suspend/resume. Must
      be called after {!enter} — pass the {!state} so we can restore and
      re-enter raw mode around the stop. *)
  let install_suspend_handlers (state : state) =
    Atomic.set _saved_state (Some state);
    let prev_tstp =
      Stdlib.Sys.signal Stdlib.Sys.sigtstp
        (Stdlib.Sys.Signal_handle (fun _signum -> suspend ()))
    in
    let prev_cont =
      Stdlib.Sys.signal Stdlib.Sys.sigcont
        (Stdlib.Sys.Signal_handle
           (fun _signum ->
             (* Only re-enter raw mode if we actually suspended: suspend()
                clears _saved_state before sending SIGSTOP, so None means
                a real resume. CAS ensures only one SIGCONT wins if racing. *)
             if Atomic.compare_and_set _saved_state None (Some state) then (
               Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH
                 (make_raw_settings state.original);
               write_stdout_all
                 (Clear.screen ^ Cursor.move_to ~row:1 ~col:1 ^ Cursor.hide);
               Atomic.set redraw_needed true)))
    in
    _saved_handlers := Some (prev_tstp, prev_cont)

  (** Clean up suspend handlers, restoring previous handlers and clearing saved
      state. *)
  let clear_suspend_handlers () =
    (match !_saved_handlers with
    | None -> ()
    | Some (prev_tstp, prev_cont) ->
        ignore (Stdlib.Sys.signal Stdlib.Sys.sigtstp prev_tstp);
        ignore (Stdlib.Sys.signal Stdlib.Sys.sigcont prev_cont));
    _saved_handlers := None;
    match Atomic.exchange _saved_state None with
    | Some state -> leave state
    | None -> ()
end

(** Keyboard input types and parsing. *)
module Key = struct
  type t =
    | Char of char
    | Enter
    | Tab
    | Backspace
    | Escape
    | Up
    | Down
    | Left
    | Right
    | Home
    | End
    | Page_up
    | Page_down
    | Delete
    | F of int
    | Ctrl of char
    | Paste of string
    | Unknown of string
  [@@deriving show, eq]

  (** Read a single byte from stdin, returning None on EOF/error. Runs in a
      systhread so it does not block the Eio event loop. *)
  let read_byte () =
    Eio_unix.run_in_systhread (fun () ->
        let buf = Bytes.create 1 in
        try
          let n = Unix.read Unix.stdin buf 0 1 in
          if n = 0 then None else Some (Bytes.get buf 0)
        with _ -> None)

  (** Try to read a byte with a short timeout (for escape sequence detection).
      Uses Unix.select with a 50ms timeout. Runs in a systhread so it does not
      block the Eio event loop. *)
  let read_byte_timeout () =
    Eio_unix.run_in_systhread (fun () ->
        let ready, _, _ = Unix.select [ Unix.stdin ] [] [] 0.05 in
        match ready with
        | [] -> None
        | _ -> (
            let buf = Bytes.create 1 in
            try
              let n = Unix.read Unix.stdin buf 0 1 in
              if n = 0 then None else Some (Bytes.get buf 0)
            with _ -> None))

  (** Read bracketed paste content until ESC[201~ (paste end). *)
  let read_bracketed_paste () =
    let buf = Buffer.create 256 in
    let rec consume () =
      match read_byte () with
      | None -> Paste (Buffer.contents buf)
      | Some '\027' -> (
          (* Possible end sequence: ESC [ 2 0 1 ~ *)
          match read_byte_timeout () with
          | Some '[' -> (
              match read_byte_timeout () with
              | Some '2' -> (
                  match read_byte_timeout () with
                  | Some '0' -> (
                      match read_byte_timeout () with
                      | Some '1' -> (
                          match read_byte_timeout () with
                          | Some '~' -> Paste (Buffer.contents buf)
                          | Some c ->
                              Buffer.add_string buf "\027[20";
                              Buffer.add_char buf c;
                              consume ()
                          | None ->
                              Buffer.add_string buf "\027[20";
                              Paste (Buffer.contents buf))
                      | Some c ->
                          Buffer.add_string buf "\027[2";
                          Buffer.add_char buf c;
                          consume ()
                      | None ->
                          Buffer.add_string buf "\027[2";
                          Paste (Buffer.contents buf))
                  | Some c ->
                      Buffer.add_string buf "\027[";
                      Buffer.add_char buf c;
                      consume ()
                  | None ->
                      Buffer.add_string buf "\027[";
                      Paste (Buffer.contents buf))
              | Some c ->
                  Buffer.add_string buf "\027";
                  Buffer.add_char buf c;
                  consume ()
              | None ->
                  Buffer.add_string buf "\027";
                  Paste (Buffer.contents buf))
          | Some c ->
              Buffer.add_string buf "\027";
              Buffer.add_char buf c;
              consume ()
          | None ->
              Buffer.add_string buf "\027";
              Paste (Buffer.contents buf))
      | Some c ->
          Buffer.add_char buf c;
          consume ()
    in
    consume ()

  (** Parse a CSI escape sequence (after ESC and bracket have been read). *)
  let parse_csi () =
    match read_byte_timeout () with
    | None -> Escape
    | Some c -> (
        match c with
        | 'A' -> Up
        | 'B' -> Down
        | 'C' -> Right
        | 'D' -> Left
        | 'H' -> Home
        | 'F' -> End
        | '1' -> (
            match read_byte_timeout () with
            | Some '~' -> Home
            | Some ';' ->
                (* skip modifier + final byte *)
                let _ = read_byte_timeout () in
                let _ = read_byte_timeout () in
                Unknown "modified"
            | Some c2 -> (
                (* Could be F5-F8: 1 5~ through 1 9~ *)
                match read_byte_timeout () with
                | Some '~' -> (
                    match c2 with
                    | '5' -> F 5
                    | '7' -> F 6
                    | '8' -> F 7
                    | '9' -> F 8
                    | _ -> Unknown (Printf.sprintf "1%c~" c2))
                | _ -> Unknown (Printf.sprintf "1%c" c2))
            | None -> Unknown "1")
        | '2' -> (
            match read_byte_timeout () with
            | Some '~' -> Unknown "insert"
            | Some '0' -> (
                match read_byte_timeout () with
                | Some '0' -> (
                    (* CSI 200~ = bracketed paste start *)
                    match read_byte_timeout () with
                    | Some '~' -> read_bracketed_paste ()
                    | _ -> Unknown "200")
                | Some '~' -> F 9
                | _ -> Unknown "20")
            | Some '1' -> (
                match read_byte_timeout () with
                | Some '~' -> F 10
                | _ -> Unknown "21")
            | Some '3' -> (
                match read_byte_timeout () with
                | Some '~' -> F 11
                | _ -> Unknown "23")
            | Some '4' -> (
                match read_byte_timeout () with
                | Some '~' -> F 12
                | _ -> Unknown "24")
            | _ -> Unknown "2")
        | '3' -> (
            match read_byte_timeout () with
            | Some '~' -> Delete
            | _ -> Unknown "3")
        | '4' -> (
            match read_byte_timeout () with Some '~' -> End | _ -> Unknown "4")
        | '5' -> (
            match read_byte_timeout () with
            | Some '~' -> Page_up
            | _ -> Unknown "5")
        | '6' -> (
            match read_byte_timeout () with
            | Some '~' -> Page_down
            | _ -> Unknown "6")
        | _ -> Unknown (Printf.sprintf "[%c" c))

  (** Parse an escape sequence after ESC has been read. *)
  let parse_escape () =
    match read_byte_timeout () with
    | None -> Escape (* bare escape *)
    | Some '[' -> parse_csi ()
    | Some 'O' -> (
        (* SS3 sequences: F1-F4 *)
        match read_byte_timeout () with
        | Some 'P' -> F 1
        | Some 'Q' -> F 2
        | Some 'R' -> F 3
        | Some 'S' -> F 4
        | Some c -> Unknown (Printf.sprintf "O%c" c)
        | None -> Unknown "O")
    | Some c -> Unknown (Printf.sprintf "\\e%c" c)

  (** Read and parse a single key press. Blocks until input is available. Must
      be called while in raw mode. *)
  let read () =
    match read_byte () with
    | None -> None
    | Some '\027' -> Some (parse_escape ())
    | Some '\r' | Some '\n' -> Some Enter
    | Some '\t' -> Some Tab
    | Some '\127' | Some '\008' -> Some Backspace
    | Some c ->
        let code = Char.to_int c in
        if code >= 1 && code <= 26 then
          Some (Ctrl (Char.of_int_exn (code + 96)))
        else Some (Char c)
end

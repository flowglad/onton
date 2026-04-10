(** Low-level ANSI terminal primitives.

    Provides SGR styling, cursor/screen control, keyboard input parsing, mouse
    event decoding, raw-mode management, and text-width utilities that account
    for ANSI escape sequences and multi-byte UTF-8. *)

(** {1 SGR (Select Graphic Rendition)} *)

module Sgr : sig
  val reset : string
  val bold : string
  val dim : string
  val italic : string
  val underline : string
  val reverse : string
  val strikethrough : string
  val fg_black : string
  val fg_red : string
  val fg_green : string
  val fg_yellow : string
  val fg_blue : string
  val fg_magenta : string
  val fg_cyan : string
  val fg_white : string
  val fg_default : string
  val bg_black : string
  val bg_red : string
  val bg_green : string
  val bg_yellow : string
  val bg_blue : string
  val bg_magenta : string
  val bg_cyan : string
  val bg_white : string
  val bg_default : string
  val fg_rgb : int -> int -> int -> string
  val bg_rgb : int -> int -> int -> string
  val fg_256 : int -> string
  val bg_256 : int -> string
end

(** {1 Cursor movement and screen control} *)

module Cursor : sig
  val up : int -> string
  val down : int -> string
  val forward : int -> string
  val back : int -> string
  val move_to : row:int -> col:int -> string
  val save : string
  val restore : string
  val hide : string
  val show : string
end

(** {1 Screen clearing} *)

module Clear : sig
  val screen : string
  val to_end : string
  val to_start : string
  val line : string
  val line_to_end : string
  val line_to_start : string
end

(** {1 Styling} *)

type style = string list
(** A composable list of SGR codes. *)

val color_enabled : bool Lazy.t
(** Whether color output is enabled. [false] when [$NO_COLOR] is set to a
    non-empty value (see {{:https://no-color.org} no-color.org}). *)

val styled : style -> string -> string
(** [styled codes text] wraps [text] in the given SGR codes and appends a reset.
    Returns plain [text] when color is disabled. *)

(** {1 Text width utilities} *)

val strip_ansi : string -> string
(** Remove all ANSI escape sequences from a string. *)

val visible_length : string -> int
(** Visible character count (strips ANSI, counts UTF-8 codepoints). *)

val fit_width : int -> string -> string
(** Pad or truncate to a visible width, preserving ANSI formatting. *)

val byte_offset_of_visible_col : string -> int -> int
(** [byte_offset_of_visible_col s col] returns the byte offset in plain string
    [s] of the [col]-th visible character. Returns [String.length s] when
    [col >= visible_length s]. *)

val wrap_lines : int -> string -> string list
(** Hard-wrap a plain string at [width] visible characters. Each line is padded
    to [width]. Returns at least one line. *)

val hrule : ?ch:string -> int -> string
(** Draw a horizontal rule of [width] characters (default ["─"]). *)

(** {1 Terminal size} *)

type size = { rows : int; cols : int } [@@deriving show, eq]

val get_size : unit -> size option
(** Query terminal size via stty. *)

(** {1 Raw mode} *)

module Raw : sig
  type state

  val enter : unit -> state
  (** Enter raw mode. Returns saved terminal state for {!leave}. *)

  val leave : state -> unit
  (** Restore original terminal settings. *)

  val suspend : unit -> unit
  (** Suspend the terminal (restore settings, show cursor, SIGSTOP). *)

  val install_suspend_handlers : state -> unit
  (** Install SIGTSTP/SIGCONT handlers for proper suspend/resume. *)

  val clear_suspend_handlers : unit -> unit
  (** Remove suspend handlers and restore original terminal settings. *)

  val redraw_needed : bool Atomic.t
  (** Set by the SIGCONT handler when the TUI should redraw. *)
end

(** {1 Mouse input} *)

type mouse_button = Left | Middle | Right [@@deriving show, eq]
type scroll_dir = Up | Down [@@deriving show, eq]

type mouse_event =
  | Click of { button : mouse_button; row : int; col : int; press : bool }
  | Scroll of { dir : scroll_dir; row : int; col : int }
[@@deriving show, eq]

val enable_mouse : string
val disable_mouse : string

(** {1 Keyboard input} *)

module Key : sig
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
    | Mouse of mouse_event
    | Unknown of string
  [@@deriving show, eq]

  val read : unit -> t option
  (** Read and parse a single key press. Blocks until input is available. Must
      be called while in raw mode. *)
end

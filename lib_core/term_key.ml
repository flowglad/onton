(* @archlint.module value
   @archlint.domain term-key *)

(** Pure types for keyboard input events.

    The effectful read functions (stdin polling, escape-sequence parsing) live
    in [Term.Key] in the effectful library. This module owns the data types so
    that [Tui_input] can map keys to commands without depending on the terminal
    runtime. *)

type mouse_button = Left | Middle | Right [@@deriving show, eq]
type scroll_dir = Up | Down [@@deriving show, eq]

type mouse_event =
  | Click of { button : mouse_button; row : int; col : int; press : bool }
  | Scroll of { dir : scroll_dir; row : int; col : int }
[@@deriving show, eq]

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

open Onton
open Onton.Tui_input

(** Standalone tests for Tui_input — supplements the inline tests in
    [lib/tui_input.ml] with scenario-based coverage. *)

let () =
  (* Vim-style navigation round-trip *)
  let selected = ref 0 in
  let count = 10 in
  (* Move down 3 times *)
  for _ = 1 to 3 do
    let cmd = Tui_input.of_key (Term.Key.Char 'j') in
    selected := Tui_input.apply_move ~count ~selected:!selected cmd
  done;
  assert (!selected = 3);
  (* Move up once *)
  let cmd = Tui_input.of_key Term.Key.Up in
  selected := Tui_input.apply_move ~count ~selected:!selected cmd;
  assert (!selected = 2);
  (* Page down from 2 *)
  let cmd = Tui_input.of_key Term.Key.Page_down in
  selected := Tui_input.apply_move ~count ~selected:!selected cmd;
  assert (!selected = 7);
  (* Page down again, should clamp to 9 *)
  selected := Tui_input.apply_move ~count ~selected:!selected cmd;
  assert (!selected = 9);
  (* All keys that should be Quit *)
  assert (Tui_input.equal_command (Tui_input.of_key (Term.Key.Char 'q')) Quit);
  assert (Tui_input.equal_command (Tui_input.of_key (Term.Key.Ctrl 'c')) Quit);
  (* All keys that should be Help *)
  assert (Tui_input.equal_command (Tui_input.of_key (Term.Key.Char 'h')) Help);
  assert (Tui_input.equal_command (Tui_input.of_key (Term.Key.Char '?')) Help);
  (* Non-navigation commands don't change selection *)
  let s = Tui_input.apply_move ~count ~selected:5 Tui_input.Refresh in
  assert (s = 5);
  let s = Tui_input.apply_move ~count ~selected:5 Tui_input.Quit in
  assert (s = 5);
  Printf.printf "test_tui_input: all tests passed\n"

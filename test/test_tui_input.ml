(* @archlint.module stateTest
   @archlint.domain tui-input *)

open Onton_core
open Tui_input

(** Standalone tests for Tui_input — supplements the inline tests in
    [lib/tui_input.ml] with scenario-based coverage. *)

let () =
  (* Vim-style navigation round-trip *)
  let selected = ref 0 in
  let count = 10 in
  (* Move down 3 times *)
  for _ = 1 to 3 do
    let cmd = Tui_input.of_key (Term_key.Char 'j') in
    selected := Tui_input.apply_move ~count ~selected:!selected cmd
  done;
  assert (!selected = 3);
  (* Move up once *)
  let cmd = Tui_input.of_key Term_key.Up in
  selected := Tui_input.apply_move ~count ~selected:!selected cmd;
  assert (!selected = 2);
  (* Page down from 2 *)
  let cmd = Tui_input.of_key Term_key.Page_down in
  selected := Tui_input.apply_move ~count ~selected:!selected cmd;
  assert (!selected = 7);
  (* Page down again, should clamp to 9 *)
  selected := Tui_input.apply_move ~count ~selected:!selected cmd;
  assert (!selected = 9);
  (* All keys that should be Quit *)
  assert (Tui_input.equal_command (Tui_input.of_key (Term_key.Char 'q')) Quit);
  assert (Tui_input.equal_command (Tui_input.of_key (Term_key.Ctrl 'c')) Quit);
  (* All keys that should be Help *)
  assert (Tui_input.equal_command (Tui_input.of_key (Term_key.Char 'h')) Help);
  assert (Tui_input.equal_command (Tui_input.of_key (Term_key.Char '?')) Help);
  (* Non-navigation commands don't change selection *)
  let s = Tui_input.apply_move ~count ~selected:5 Tui_input.Noop in
  assert (s = 5);
  let s = Tui_input.apply_move ~count ~selected:5 Tui_input.Quit in
  assert (s = 5);
  let command_gen =
    QCheck2.Gen.oneof_list
      [
        Tui_input.Move_up;
        Tui_input.Move_down;
        Tui_input.Page_up;
        Tui_input.Page_down;
        Tui_input.Scroll_top;
        Tui_input.Scroll_bottom;
        Tui_input.Noop;
      ]
  in
  let selection_stays_in_valid_range =
    QCheck2.Test.make ~name:"selection command interleavings stay in range"
      ~count:500
      QCheck2.Gen.(
        pair (int_range 0 30) (list_size (int_range 0 80) command_gen))
      (fun (count, commands) ->
        let selected = ref (-1) in
        List.for_all
          (fun cmd ->
            selected := Tui_input.apply_move ~count ~selected:!selected cmd;
            if count <= 0 then !selected = -1
            else !selected >= -1 && !selected < count)
          commands)
  in
  QCheck2.Test.check_exn selection_stays_in_valid_range;
  let key_translation_is_total =
    QCheck2.Test.make ~name:"key translation is total" ~count:500
      QCheck2.Gen.char (fun c ->
        ignore (Tui_input.of_key (Term_key.Char c) : Tui_input.command);
        true)
  in
  QCheck2.Test.check_exn key_translation_is_total;
  let prompt_prefix_is_total =
    QCheck2.Test.make ~name:"prompt prefixes are total" ~count:10
      (QCheck2.Gen.oneof_list
         [
           Tui_input.Normal;
           Tui_input.Prompt_pr;
           Tui_input.Prompt_worktree;
           Tui_input.Prompt_message;
           Tui_input.Prompt_broadcast;
           Tui_input.Manage_patch;
         ])
      (fun mode ->
        ignore (Tui_input.prompt_prefix mode : string);
        true)
  in
  QCheck2.Test.check_exn prompt_prefix_is_total;
  let history_browse_is_bounded =
    QCheck2.Test.make ~name:"history interleavings browse only stored entries"
      ~count:500
      QCheck2.Gen.(list_size (int_range 0 80) string_small)
      (fun entries ->
        let h = Tui_input.History.create ~capacity:7 () in
        List.iter (Tui_input.History.push h) entries;
        let rec drain seen =
          match Tui_input.History.older h with
          | None -> seen
          | Some _ -> drain (seen + 1)
        in
        drain 0 <= 7)
  in
  QCheck2.Test.check_exn history_browse_is_bounded;
  Printf.printf "test_tui_input: all tests passed\n"

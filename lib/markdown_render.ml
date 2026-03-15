open Base

(** Markdown-to-ANSI terminal renderer.

    Handles a useful subset of markdown: headings, bold, italic, code spans,
    fenced code blocks, lists, horizontal rules, and blockquotes. Not a full
    CommonMark parser — just enough for rendering orchestrator output. *)

(** Render inline markdown formatting to ANSI. *)
let render_inline s =
  let buf = Buffer.create (String.length s * 2) in
  let len = String.length s in
  let rec loop i =
    if i >= len then ()
    else
      match String.get s i with
      | '`' ->
          let j = find_closing '`' (i + 1) in
          if j > i + 1 then begin
            Buffer.add_string buf Term.Sgr.fg_cyan;
            Buffer.add_string buf (String.sub s ~pos:(i + 1) ~len:(j - i - 1));
            Buffer.add_string buf Term.Sgr.reset;
            loop (j + 1)
          end
          else begin
            Buffer.add_char buf '`';
            loop (i + 1)
          end
      | '*' when i + 1 < len && Char.equal (String.get s (i + 1)) '*' ->
          let j = find_double_closing '*' (i + 2) in
          if j > i + 2 then begin
            Buffer.add_string buf Term.Sgr.bold;
            Buffer.add_string buf (String.sub s ~pos:(i + 2) ~len:(j - i - 2));
            Buffer.add_string buf Term.Sgr.reset;
            loop (j + 2)
          end
          else begin
            Buffer.add_char buf '*';
            Buffer.add_char buf '*';
            loop (i + 2)
          end
      | '*' ->
          let j = find_closing '*' (i + 1) in
          if j > i + 1 then begin
            Buffer.add_string buf Term.Sgr.italic;
            Buffer.add_string buf (String.sub s ~pos:(i + 1) ~len:(j - i - 1));
            Buffer.add_string buf Term.Sgr.reset;
            loop (j + 1)
          end
          else begin
            Buffer.add_char buf '*';
            loop (i + 1)
          end
      | '~' when i + 1 < len && Char.equal (String.get s (i + 1)) '~' ->
          let j = find_double_closing '~' (i + 2) in
          if j > i + 2 then begin
            Buffer.add_string buf Term.Sgr.strikethrough;
            Buffer.add_string buf (String.sub s ~pos:(i + 2) ~len:(j - i - 2));
            Buffer.add_string buf Term.Sgr.reset;
            loop (j + 2)
          end
          else begin
            Buffer.add_char buf '~';
            Buffer.add_char buf '~';
            loop (i + 2)
          end
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  and find_closing delim i =
    if i >= len then i - 1
    else if Char.equal (String.get s i) delim then i
    else find_closing delim (i + 1)
  and find_double_closing delim i =
    if i + 1 >= len then i - 1
    else if
      Char.equal (String.get s i) delim
      && Char.equal (String.get s (i + 1)) delim
    then i
    else find_double_closing delim (i + 1)
  in
  loop 0;
  Buffer.contents buf

(** Render a full markdown string to ANSI-formatted text. *)
let render s =
  let lines = String.split_lines s in
  let buf = Buffer.create (String.length s * 2) in
  let in_code_block = ref false in
  let first = ref true in
  let add_line line =
    if not !first then Buffer.add_char buf '\n';
    first := false;
    Buffer.add_string buf line
  in
  List.iter lines ~f:(fun line ->
      if String.is_prefix line ~prefix:"```" then begin
        if !in_code_block then begin
          in_code_block := false
        end
        else begin
          in_code_block := true
        end
      end
      else if !in_code_block then
        add_line (Term.Sgr.dim ^ "  " ^ line ^ Term.Sgr.reset)
      else
        let trimmed = String.lstrip line in
        if String.is_prefix trimmed ~prefix:"### " then
          add_line
            (Term.Sgr.bold ^ Term.Sgr.fg_yellow
            ^ String.drop_prefix trimmed 4
            ^ Term.Sgr.reset)
        else if String.is_prefix trimmed ~prefix:"## " then
          add_line
            (Term.Sgr.bold ^ Term.Sgr.fg_green
            ^ String.drop_prefix trimmed 3
            ^ Term.Sgr.reset)
        else if String.is_prefix trimmed ~prefix:"# " then
          add_line
            (Term.Sgr.bold ^ Term.Sgr.underline ^ Term.Sgr.fg_magenta
            ^ String.drop_prefix trimmed 2
            ^ Term.Sgr.reset)
        else if
          String.is_prefix trimmed ~prefix:"---"
          || String.is_prefix trimmed ~prefix:"***"
          || String.is_prefix trimmed ~prefix:"___"
        then add_line (Term.Sgr.dim ^ Term.hrule 40 ^ Term.Sgr.reset)
        else if String.is_prefix trimmed ~prefix:"> " then
          add_line
            (Term.Sgr.dim ^ "│ "
            ^ render_inline (String.drop_prefix trimmed 2)
            ^ Term.Sgr.reset)
        else if
          String.is_prefix trimmed ~prefix:"- "
          || String.is_prefix trimmed ~prefix:"* "
        then add_line ("  • " ^ render_inline (String.drop_prefix trimmed 2))
        else if String.is_empty trimmed then add_line ""
        else add_line (render_inline line));
  Buffer.contents buf

let%expect_test "render headings" =
  let s = render "# Title\n## Section\n### Sub" in
  let stripped = Term.strip_ansi s in
  Stdio.print_string stripped;
  [%expect {|
    Title
    Section
    Sub |}]

let%expect_test "render inline formatting" =
  let s = render "hello **bold** and *italic* and `code`" in
  let stripped = Term.strip_ansi s in
  Stdio.print_string stripped;
  [%expect {| hello bold and italic and code |}]

let%expect_test "render code block" =
  let s = render "```\nlet x = 1\n```" in
  let stripped = Term.strip_ansi s in
  Stdio.print_string stripped;
  [%expect {|   let x = 1 |}]

let%expect_test "render list" =
  let s = render "- one\n- two" in
  let stripped = Term.strip_ansi s in
  Stdio.print_string stripped;
  [%expect {|
    • one
    • two |}]

let%expect_test "render blockquote" =
  let s = render "> quoted text" in
  let stripped = Term.strip_ansi s in
  Stdio.print_string stripped;
  [%expect {| │ quoted text |}]

let%expect_test "render hrule" =
  let s = render "---" in
  let stripped = Term.strip_ansi s in
  Stdio.print_string stripped;
  [%expect {| ──────────────────────────────────────── |}]

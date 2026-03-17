open Base

(** Markdown-to-ANSI terminal renderer using cmarkit (CommonMark parser).

    Walks the cmarkit AST and produces styled text with ANSI escape sequences
    for bold, italic, code, headings, etc.

    cmarkit constructors carry ['a node = 'a * Meta.t]. We use [fst] to extract
    the payload. *)

(** Render an inline to ANSI-styled text. *)
let rec render_inline (inline : Cmarkit.Inline.t) : string =
  match inline with
  | Cmarkit.Inline.Text (s, _) -> s
  | Cmarkit.Inline.Inlines (inlines, _) ->
      List.map inlines ~f:render_inline |> String.concat
  | Cmarkit.Inline.Emphasis n ->
      let inner = render_inline (Cmarkit.Inline.Emphasis.inline (fst n)) in
      Term.Sgr.italic ^ inner ^ Term.Sgr.reset
  | Cmarkit.Inline.Strong_emphasis n ->
      let inner = render_inline (Cmarkit.Inline.Emphasis.inline (fst n)) in
      Term.Sgr.bold ^ inner ^ Term.Sgr.reset
  | Cmarkit.Inline.Code_span n ->
      let lines = Cmarkit.Inline.Code_span.code_layout (fst n) in
      let text =
        List.map lines ~f:(fun (_, bl) -> Cmarkit.Block_line.to_string bl)
        |> String.concat ~sep:" "
      in
      Term.Sgr.fg_cyan ^ text ^ Term.Sgr.reset
  | Cmarkit.Inline.Break n -> (
      match Cmarkit.Inline.Break.type' (fst n) with
      | `Hard -> "\n"
      | `Soft -> " ")
  | Cmarkit.Inline.Link n ->
      let text = render_inline (Cmarkit.Inline.Link.text (fst n)) in
      Term.Sgr.underline ^ text ^ Term.Sgr.reset
  | Cmarkit.Inline.Autolink n ->
      let uri = fst (Cmarkit.Inline.Autolink.link (fst n)) in
      Term.Sgr.underline ^ Term.Sgr.fg_blue ^ uri ^ Term.Sgr.reset
  | Cmarkit.Inline.Raw_html _ -> ""
  | Cmarkit.Inline.Image _ -> "[image]"
  | Cmarkit.Inline.Ext_strikethrough n ->
      let inner = render_inline (Cmarkit.Inline.Strikethrough.inline (fst n)) in
      Term.Sgr.strikethrough ^ inner ^ Term.Sgr.reset
  | _ -> ""

(** Render a block to a list of lines. *)
let rec render_block (block : Cmarkit.Block.t) : string list =
  match block with
  | Cmarkit.Block.Paragraph n ->
      let inline = Cmarkit.Block.Paragraph.inline (fst n) in
      let text = render_inline inline in
      String.split_lines text @ [ "" ]
  | Cmarkit.Block.Heading n ->
      let h = fst n in
      let level = Cmarkit.Block.Heading.level h in
      let text = render_inline (Cmarkit.Block.Heading.inline h) in
      let color =
        match level with
        | 1 -> Term.Sgr.fg_magenta ^ Term.Sgr.underline
        | 2 -> Term.Sgr.fg_green
        | _ -> Term.Sgr.fg_yellow
      in
      let marker = String.make level '#' in
      [ Term.Sgr.bold ^ color ^ marker ^ " " ^ text ^ Term.Sgr.reset; "" ]
  | Cmarkit.Block.Code_block n ->
      let cb = fst n in
      let info_label =
        match Cmarkit.Block.Code_block.info_string cb with
        | Some (s, _) -> (
            match Cmarkit.Block.Code_block.language_of_info_string s with
            | Some (lang, _) -> " " ^ lang
            | None -> "")
        | None -> ""
      in
      let code_lines = Cmarkit.Block.Code_block.code cb in
      let code =
        List.map code_lines ~f:(fun bl ->
            "  " ^ Term.Sgr.dim
            ^ Cmarkit.Block_line.to_string bl
            ^ Term.Sgr.reset)
      in
      [ Term.Sgr.dim ^ "```" ^ info_label ^ Term.Sgr.reset ]
      @ code
      @ [ Term.Sgr.dim ^ "```" ^ Term.Sgr.reset; "" ]
  | Cmarkit.Block.Block_quote n ->
      let inner = Cmarkit.Block.Block_quote.block (fst n) in
      let inner_lines = render_block inner in
      List.map inner_lines ~f:(fun l ->
          Term.Sgr.dim ^ "│ " ^ Term.Sgr.reset ^ l)
      @ [ "" ]
  | Cmarkit.Block.List n ->
      let lst = fst n in
      let items = Cmarkit.Block.List'.items lst in
      let is_ordered =
        match Cmarkit.Block.List'.type' lst with
        | `Ordered _ -> true
        | `Unordered _ -> false
      in
      let result =
        List.concat_mapi items ~f:(fun i item ->
            let marker =
              if is_ordered then Printf.sprintf "%d. " (i + 1) else "• "
            in
            let pad = String.make (String.length marker) ' ' in
            let block = Cmarkit.Block.List_item.block (fst item) in
            let lines = render_block block in
            match lines with
            | [] -> [ marker ]
            | first :: rest ->
                (marker ^ first) :: List.map rest ~f:(fun l -> pad ^ l))
      in
      result @ [ "" ]
  | Cmarkit.Block.Thematic_break _ ->
      [ Term.Sgr.dim ^ "───" ^ Term.Sgr.reset; "" ]
  | Cmarkit.Block.Blank_line _ -> [ "" ]
  | Cmarkit.Block.Blocks (blocks, _) -> List.concat_map blocks ~f:render_block
  | Cmarkit.Block.Html_block _ -> []
  | _ -> []

(** Render a full markdown string to a single ANSI-styled string. *)
let render s =
  let doc = Cmarkit.Doc.of_string ~strict:false s in
  let lines = render_block (Cmarkit.Doc.block doc) in
  String.concat ~sep:"\n" lines

(** Render a full markdown string to a list of styled lines. Consecutive blank
    lines are collapsed to at most one. *)
let render_to_lines s =
  let doc = Cmarkit.Doc.of_string ~strict:false s in
  let lines = render_block (Cmarkit.Doc.block doc) in
  let rec collapse acc prev_blank = function
    | [] -> List.rev acc
    | line :: rest ->
        let is_blank = String.is_empty (String.strip line) in
        if is_blank && prev_blank then collapse acc true rest
        else collapse (line :: acc) is_blank rest
  in
  collapse [] false lines

let%expect_test "render headings" =
  let s = render "# Title\n## Section\n### Sub" in
  let stripped = Term.strip_ansi s in
  Stdio.print_string stripped;
  [%expect {|
    # Title

    ## Section

    ### Sub
    |}]

let%expect_test "render inline formatting" =
  let s = render "hello **bold** and *italic* and `code`" in
  let stripped = Term.strip_ansi s in
  Stdio.print_string stripped;
  [%expect {| hello bold and italic and code |}]

let%expect_test "render code block" =
  let s = render "```\nlet x = 1\n```" in
  let stripped = Term.strip_ansi s in
  Stdio.print_string stripped;
  [%expect {|
    ```
      let x = 1
    ```
    |}]

let%expect_test "render list" =
  let s = render "- one\n- two" in
  let stripped = Term.strip_ansi s in
  Stdio.print_string stripped;
  [%expect {|
    • one

    • two
    |}]

let%expect_test "render blockquote" =
  let s = render "> quoted text" in
  let stripped = Term.strip_ansi s in
  Stdio.print_string stripped;
  [%expect {|
    │ quoted text
    │
    |}]

let%expect_test "render hrule" =
  let s = render "---" in
  let stripped = Term.strip_ansi s in
  Stdio.print_string stripped;
  [%expect {|
    ───
    |}]

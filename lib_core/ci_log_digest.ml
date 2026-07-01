open Base

type annotation = {
  path : string option;
  line : int option;
  level : string;
  message : string;
}

type source = { annotations : annotation list; log : string option }
type enrichment = { summary_md : string; teaser : string option; signal : int }

let summary_total_cap_bytes = 49_152
let marker_context_lines = 30
let error_window_lines = 100
let collapse_signal_threshold = 200
let is_digit = function '0' .. '9' -> true | _ -> false

let is_timestamp_prefix s =
  let len = String.length s in
  if len < 22 then false
  else
    let char_at i = String.get s i in
    is_digit (char_at 0)
    && is_digit (char_at 1)
    && is_digit (char_at 2)
    && is_digit (char_at 3)
    && Char.equal (char_at 4) '-'
    && is_digit (char_at 5)
    && is_digit (char_at 6)
    && Char.equal (char_at 7) '-'
    && is_digit (char_at 8)
    && is_digit (char_at 9)
    && Char.equal (char_at 10) 'T'
    && is_digit (char_at 11)
    && is_digit (char_at 12)
    && Char.equal (char_at 13) ':'
    && is_digit (char_at 14)
    && is_digit (char_at 15)
    && Char.equal (char_at 16) ':'
    && is_digit (char_at 17)
    && is_digit (char_at 18)

let drop_timestamp_prefix line =
  if not (is_timestamp_prefix line) then line
  else
    match String.lfindi line ~f:(fun _ c -> Char.equal c 'Z') with
    | Some z
      when z + 1 < String.length line
           && Char.equal (String.get line (z + 1)) ' ' ->
        String.drop_prefix line (z + 2)
    | _ -> line

let is_ansi_final c =
  let code = Char.to_int c in
  code >= 0x40 && code <= 0x7e

let strip_ansi line =
  let len = String.length line in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else
      let c = String.get line i in
      if
        Char.equal c '\027'
        && i + 1 < len
        && Char.equal (String.get line (i + 1)) '['
      then (
        let rec skip j =
          if j >= len then None
          else if is_ansi_final (String.get line j) then Some (j + 1)
          else skip (j + 1)
        in
        match skip (i + 2) with
        | Some next -> loop next
        | None ->
            Buffer.add_char buf c;
            loop (i + 1))
      else (
        Buffer.add_char buf c;
        loop (i + 1))
  in
  loop 0

let strip_log log =
  String.split log ~on:'\n'
  |> List.map ~f:(fun line ->
      line |> strip_ansi |> drop_timestamp_prefix |> strip_ansi)
  |> String.concat ~sep:"\n"

let contains s needle = String.is_substring s ~substring:needle

let is_failure_marker line =
  contains line "(fail)" || contains line "=== FAIL" || contains line "✕"
  || contains line "❌"

let is_error_line line = contains (String.strip line) "::error"
let is_hash_error_line line = contains line "##[error]"

let annotation_line (a : annotation) =
  let path = Option.value a.path ~default:"<unknown>" in
  let line = match a.line with Some n -> Int.to_string n | None -> "?" in
  "- " ^ path ^ ":" ^ line ^ ": " ^ a.message

let failure_annotations annotations =
  annotations
  |> List.filter ~f:(fun (a : annotation) ->
      String.Caseless.equal (String.strip a.level) "failure")
  |> List.map ~f:annotation_line

let range_indices ~line_count ~before idx =
  let first = Int.max 0 (idx - before) in
  List.range first (Int.min line_count (idx + 1))

let unique_indices indices = indices |> List.dedup_and_sort ~compare:Int.compare

let lines_at lines indices =
  List.filter_map indices ~f:(fun i -> List.nth lines i)

let first_matching_line lines ~f = List.find lines ~f

let error_message_part line =
  match String.substr_index line ~pattern:"::error" with
  | None -> String.strip line
  | Some idx ->
      let rest = String.drop_prefix line (idx + String.length "::error") in
      let rest =
        match String.substr_index rest ~pattern:"::" with
        | Some msg_idx -> String.drop_prefix rest (msg_idx + 2)
        | None -> rest
      in
      let stripped = String.strip rest in
      if String.is_empty stripped then String.strip line else stripped

let diagnostic_bytes lines = lines |> String.concat ~sep:"\n" |> String.length

let non_boilerplate_section4_lines lines =
  List.filter lines ~f:(fun line ->
      let stripped = String.strip line in
      (not (String.is_empty stripped))
      && not (contains stripped "Process completed with exit code"))

let section title lines =
  if List.is_empty lines then None
  else Some ("## " ^ title ^ "\n\n" ^ String.concat lines ~sep:"\n" ^ "\n")

let append_capped pieces =
  let truncation_note = "\n\n[diagnostic summary truncated]\n" in
  let note_len = String.length truncation_note in
  let rec loop acc used = function
    | [] -> String.concat (List.rev acc) ~sep:""
    | piece :: rest ->
        let len = String.length piece in
        if used + len <= summary_total_cap_bytes then
          loop (piece :: acc) (used + len) rest
        else
          let remaining = summary_total_cap_bytes - used in
          if remaining <= 0 then String.concat (List.rev acc) ~sep:""
          else if remaining <= note_len then
            let clipped = String.prefix truncation_note remaining in
            String.concat (List.rev (clipped :: acc)) ~sep:""
          else
            let prefix_len = remaining - note_len in
            let clipped = String.prefix piece prefix_len ^ truncation_note in
            ignore rest;
            String.concat (List.rev (clipped :: acc)) ~sep:""
  in
  loop [] 0 pieces

let digest (source : source) =
  let stripped_log = source.log |> Option.value ~default:"" |> strip_log in
  let lines = String.split_lines stripped_log in
  let line_count = List.length lines in
  let annotation_lines = failure_annotations source.annotations in
  let error_lines = List.filter lines ~f:is_error_line in
  let marker_indices =
    List.filter_mapi lines ~f:(fun i line ->
        if is_failure_marker line then Some i else None)
  in
  let hash_error_indices =
    List.filter_mapi lines ~f:(fun i line ->
        if is_hash_error_line line then Some i else None)
  in
  let marker_context =
    marker_indices
    |> List.concat_map
         ~f:(range_indices ~line_count ~before:marker_context_lines)
    |> unique_indices |> lines_at lines
  in
  let hash_error_window =
    hash_error_indices
    |> List.concat_map ~f:(range_indices ~line_count ~before:error_window_lines)
    |> unique_indices |> lines_at lines
  in
  let teaser =
    match first_matching_line lines ~f:is_error_line with
    | Some line -> Some (error_message_part line)
    | None ->
        first_matching_line lines ~f:is_failure_marker
        |> Option.map ~f:(fun line -> String.strip line)
  in
  let signal =
    diagnostic_bytes annotation_lines
    + diagnostic_bytes error_lines
    + diagnostic_bytes marker_context
    + diagnostic_bytes (non_boilerplate_section4_lines hash_error_window)
  in
  let pieces =
    [
      section "Failure annotations" annotation_lines;
      section "Literal ::error lines" error_lines;
      section "Failure marker context" marker_context;
      section "Pre-##[error] window" hash_error_window;
    ]
    |> List.filter_opt
  in
  { summary_md = append_capped pieces; teaser; signal }
